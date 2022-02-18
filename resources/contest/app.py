from flask import Flask, render_template, g
from scoreboard_manager import ScoreboardManager, ADDRESS, SKEY
from multiprocessing.connection import Client

app = Flask(__name__, static_url_path='/static')


@app.route('/')
def home():
    return render_template('index.html')


@app.route('/scoreboard')
def render_scoreboard():
    conn = Client(address=ADDRESS, authkey=SKEY)
    conn.send("req")
    (frozen, team_values) = conn.recv()
    conn.close()

    return render_template('scoreboard.html',
                           teams=team_values,
                           frstr=frozen)


@app.route('/problem1')
def render_problem1():
    return render_template('problem1.html')


@app.route('/problem2')
def render_problem2():
    return render_template('problem2.html')


@app.route('/problem3')
def render_problem3():
    return render_template('problem3.html')


@app.route('/problem4')
def render_problem4():
    return render_template('problem4.html')


@app.route('/problem5')
def render_problem5():
    return render_template('problem5.html')


@app.route('/problem6')
def render_problem6():
    return render_template('problem6.html')


@app.route('/problem7')
def render_problem7():
    return render_template('problem7.html')
