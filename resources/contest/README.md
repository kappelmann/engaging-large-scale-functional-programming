# Programming Contest Framework

## Test Server

Both `run.sh` and `upload.php` can be used for this (TODO I didnt make these, what do they do?).

## Test Server Setup

TODO Kevin

## Scoreboard Server

The scoreboard server consists of two main components
1. the webserver (`app.py`) and
2. the scoreboard manager (`scoreboard_manager.py`).

### Webserver

The webserver shows the scoreboard as well as the problem statements.
The scoreboard data is loaded from the scoreboard manager process.

### Scoreboard Manager

This process controls a worker thread which crawls the submissions in a regular interval.
The port of the manager is set in `ADDRESS`
in `scoreboard_manager.py`
and the update interval in `UPDATE_INTERVAL` in `scoreboard.py`.
The data is exposed to other processes via sockets/named pipes (Unix/Windows).

Each team's submission is expected in a separate directory with the corresponding teamname. 
Inside the directory, two files must exist:
1. `timestamp`: containing the submission timestamp in iso-8601.
   You can use this bash command to create such a timestamp: `timestamp=$(date --iso-8601=seconds)`.
2. `results.xml`: containing the test results in Ant JUnit XML format.
   Passing tests contain no children inside their tag while failing tests contain an error message.

### Running

You need to install `python3` as well as `pip3`.
Then install the python packages as follows:

```bash
python3 -m venv venv_app
source venv_app/bin/activate
pip install -r requirements.txt
```

You then need to run the webserver and scoreboard manager in two separate processes (e.g. in separate terminals).
For both processes, first source the python environment:
```
source venv_app/bin/activate
```
Then start the scoreboard manager:
```
python3 scoreboard_manager.py
```
And finally the webserver:
```
gunicorn wsgi:app
```
Note that outside of local testing, `gunicorn` should be deployed behind a proxy server;
see [here](https://docs.gunicorn.org/en/stable/deploy.html).

### Starting From A Backup

After each crawl, the scoreboard manager serialises its data as a backup.
In case of a system crash, you can restart the manager from this backup by running
```
python3 scoreboard_manager.py restore_backup
```

### Freezing The Scoreboard

To make the contest more exciting,
the scoreboard can be frozen (e.g. 1 hour before finish),
in which case new uploads are still processed but not published to the frontend. 

To freeze the scoreboard, you have to send a command to the scoreboard manager.
In a third process, first source the python environment
```
source venv_app/bin/activate
```
You can then freeze the scoreboard by running
```
python3 send_cmd.py freeze
```
and unfreeze the it again by running
```
python3 send_cmd.py unfreeze
```

### Adding Problem Statements

Problem statements must be added to the scoreboard manager and the webserver.

#### Webserver

Add your problem to the `problems` node in `problem.yaml` as exemplified below:
``` yaml
problems:
  example_problem:
    path: "path/to/problem"
    testhalf: 
      - "PartialA"
      - "PartialB"
    testfull: 
      - "TotalA"
      - "TotalB"
  another_problem:
    path: "path/to/another_problem"
    testhalf: 
      - "PartialA"
    testfull: 
      - "TotalA"
```
Explanation:
- `path`: the path where the problem data can be found.
  The scoreboard manager crawls the `uploads` directory in this path.
- `testhalf`: list of test names that are required to pass 50%
- `testfull`: list of test names that are required to pass 100%
Note that the test names must correspond to the ones configured in the test server.
To obtain 100%, the tests for 50% and 100% must be passed.

#### Webserver

The problem statement must be added as an `html` file in the directory `templates`.
The file must be named `problem<id>.html`, where `id` is the integer index of the problem in `problems.yaml`.
For the first problem, this would be `problem0.html`, for the second problem `problem1.html`, and so on.
The content of the file may be arbitrary.
For convenience, [mathjax](https://www.mathjax.org/) is loaded automatically by the webserver.

**Hint**: You may use any document format you like to write the problem statements
and convert them to html using [pandoc](https://pandoc.org/).

