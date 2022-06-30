# Programming Contest Framework

This folder contains a simple application to run an [ACM-ICPC-like programming contest](https://en.wikipedia.org/wiki/International_Collegiate_Programming_Contest).
An example instance can be found [here](https://vmnipkow16.in.tum.de/contest/).

To run the contest, you need
1. a test server, accepting and testing submissions (e.g. [Artemis](https://github.com/ls1intum/Artemis)),
2. the scoreboard server contained in this directory, and
3. some way to transfer results from the former to the latter.

## Test Server

The test server needs to
1. accept submissions,
2. test submissions, producing a result file in Ant JUnit XML format, and
3. publish the results to the scoreboard server.

## Transferring Results To The Scoreboard Server

In principle, you may upload the result data to the scoreboard server in any way you like.
You only need to make sure to include all data required by the scoreboard server (see below).

In `run.sh`, you can find an example script that tests a Haskell submission on Artemis
(like the ones that can be found in [this repository](https://github.com/kappelmann/engaging-large-scale-functional-programming/tree/main/resources))
uploading its result to the scoreboard server, serving the `upload.php` script (e.g. by using nginx).
If your test and scoreboard server run on the same machine, you may simply copy the data to the appropriate location.

## Scoreboard Server

The scoreboard server consists of two main components
1. the webserver (`app.py`) and
2. the scoreboard manager (`scoreboard_manager.py`).

### Webserver

The webserver shows the scoreboard as well as the problem statements.
The scoreboard data is loaded from the scoreboard manager process.

### Scoreboard Manager

This process controls a thread which crawls the submissions in a regular interval.
The port of the manager is set in `ADDRESS` in `scoreboard_manager.py`
and the update interval in `UPDATE_INTERVAL` in `scoreboard.py`.
The data is exposed to other processes via sockets/named pipes (Unix/Windows).

Each team's submission is expected in a separate directory named by the corresponding name of the team. 
Inside the directory, two files must exist:
1. `timestamp`: containing the submission's timestamp in iso-8601.
   You can use this bash command to create such a timestamp: `timestamp=$(date --iso-8601=seconds)`.
2. `results.xml`: containing the test results in Ant JUnit XML format.
   Passing tests contain no children inside their tag while failing tests contain an error message.
The repository comes with some example data in `example_data`.

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
gunicorn then prints the URL of the webserver that you can visit.
Note that outside of local testing, `gunicorn` should be deployed behind a proxy server;
see [here](https://docs.gunicorn.org/en/stable/deploy.html).

### Starting From A Backup

After each crawl, the scoreboard manager serialises its data and saves it in the folder `backups`.
In case of a system crash, you can restart the manager from these backups by running
```
python3 scoreboard_manager.py restore_backup
```
Note that the backups in particular contain transient data like the number of retries,
which are counted by the scoreboard manager while running.

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
and unfreeze it again by running
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
    path: "/path/to/problem"
    testhalf: 
      - "PartialA"
      - "PartialB"
    testfull: 
      - "TotalA"
      - "TotalB"
  another_problem:
    path: "/path/to/another_problem"
    testhalf: 
      - "PartialA"
    testfull: 
      - "TotalA"
```
Explanation:
- `path`: the path where the problem data can be found.
- `testhalf`: list of test names that are required to pass 50%
- `testfull`: list of test names that are required to pass 100%
Note that to obtain 100%, the tests for 50% and 100% must be passed.
The repository comes with some example entries that matches the data in `example_data`.

#### Webserver

The problem statement must be added as an `html` file in the directory `templates`.
The file must be named `problem<id>.html`, where `id` is the integer index of the problem in `problems.yaml`.
For the first problem, this would be `problem0.html`, for the second problem `problem1.html`, and so on.
The content of the file may be arbitrary.
For convenience, [mathjax](https://www.mathjax.org/) is loaded automatically by the webserver.

**Hint**: You may use any document format you like to write the problem statements
and convert them to html using [pandoc](https://pandoc.org/).

