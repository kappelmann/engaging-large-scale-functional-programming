from flask import Flask, render_template, g
from scoreboard_manager import ScoreboardManager, ADDRESS, SKEY
from multiprocessing.connection import Client

app = Flask(__name__, static_url_path='/static')


@app.route('/')
def home():
    conn = Client(address=ADDRESS, authkey=SKEY)
    conn.send("req")
    (frozen, team_values, n_problems) = conn.recv()
    conn.close()

    return render_template('index.html',
                           teams=team_values,
                           frstr=frozen,
                           n_probs = n_problems)

@app.route('/scoreboard')
def render_scoreboard():
    conn = Client(address=ADDRESS, authkey=SKEY)
    conn.send("req")
    (frozen, team_values, n_problems) = conn.recv()
    conn.close()

    return render_template('scoreboard.html',
                           teams=team_values,
                           frstr=frozen,
                           n_probs = n_problems)
