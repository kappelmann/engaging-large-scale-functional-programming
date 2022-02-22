# Programming Contest Framework

## Workflow

TODO describe general workflow:
test server executes test,
data is sent to contest server's upload.php,
secret keys etc. are compared,
files are stored at ???,
and then somehow(?) the files get picked up by some contest server process,
that process does something?
and then magic.

What about the backup stuff?

## Adding Problem Statements

Where do problems need to be registered?
problems.yaml, templates/, index.html (the javascript),
anywhere else?

## Test Server Setup

TODO Kevin

## Accepting Server Setup

The server is written in python and requires
a valid python3 installation as well as pip3.
First install the packages as follows:

```
python3 -m venv venv_app
source venv_app/bin/activate
pip install -r requirements.txt
```

You then need to run two different processes (e.g. in separate terminals).
First run the scoreboard-manager process:
```
python3 scoreboard_manager.py
```
And then run the ??? process:
```
gunicorn wsgi:app
```

How to stop the server and what about the backup stuff?
