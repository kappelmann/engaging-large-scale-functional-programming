# Programming Contest Framework

## Adding Problem Statements

To add a problem statement, it must be added to the
backend **and** frontend.

### Adding to backend
Add your problem **under** `problem` in `problem.yaml`
as seen below:
``` yaml
problems:
  example_problem:
    path: "path/to/problem/root"
    testhalf: 
      - "PartialA"
      - "PartialB"
    testfull: 
      - "TotalA"
      - "TotalB"
```

- `path` : where problem data is uploaded. the server crawls the `upload` directory in this path
- `testhalf`: list of test names that are required to pass for 50% (must correspond to artemis)
- `testfull`: list of test names that are required to pass for 100% (must correspond to artemis)

Tests for 50% must be passed to get 100%.

## Adding to frontend
After adding the problem to the backend, create a corresponding `html` file
in `templates`. This file is names `problem<id>.html` where `id` is just
a counter starting from 0 to number of problems -1. For the first problem, this
would be `problem0`. 
Inside that file, create a `div` with `id="problem<id>` 
(for automatically setting up links in navigation).

The contents of this div are arbitrary. For convenience, `mathjax` is already 
loaded in the server and must not be loaded seperately.

```html
<div id="problem0">
        <p>I am a placeholder</p>
</div>
```

TODO kevin automate div ids

## Workflow

This project runs via two main components, the `flask` webserver
(`app.py`). And the scoreboard manager (`scoreboard_manager.py`).

### Scoreboard Manager

This process controls a worker thread which crawls the contest entries in a regular
interval. The data is exposed via socket/named pipes (Linux/Windows).

### App

Uses `flask` to serve the contents of the scoreboard. The data is sourced via connection
to the manager process.

### Artemis

For each problem, the manager crawls the configured directory in `uploads`.
Each team is expected as a seperate directory with the corresponding teamname. 
Inside that directory, two entries must exist:

- `timestamp`: File containing a string corresponding to iso-8601.
Use this bash command to create: `timestamp=$(date --iso-8601=seconds -d "+1 hour")`.

- `results.xml`: File containing the artemis results as `xml`. Tests that are passed
are expected to have no children inside their tag.

Both `run.sh` and `upload.php` can be used for this (TODO I didnt make these, what do they do?).

## Test Server Setup

TODO Kevin

## Running th application

The server is written in python and requires
a valid python3 installation as well as pip3.
First install the packages as follows:

```
python3 -m venv venv_app
source venv_app/bin/activate
pip install -r requirements.txt
```

You then need to run two different processes (e.g. in separate terminals).

First run the manager process:
```
python3 scoreboard_manager.py
```

And then run the webserver (source env first)
```
gunicorn wsgi:app
```

Outside of local testing, `gunicorn` should be deployed; see
[here](https://docs.gunicorn.org/en/stable/deploy.html).

### Starting from backup
After each crawl through all problems, the server saves backups as `pickle`. 
In case of system crash, these backups can be sourced by running:

```
python3 scoreboard_manager.py restore_backup
```

This will restore all data from backup and then run the manager normally.

### Toggling scoreboard freeze

The scoreboard can be frozen, in which case new uploads are still processed, but
not reflected in the frontend. 

Freezing:
```
python3 send_cmd.py freeze
```

Unfreezing:
```
python3 send_cmd.py unfreeze
```