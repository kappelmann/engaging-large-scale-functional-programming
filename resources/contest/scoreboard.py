import time
import os
import multiprocessing
import yaml
from copy import deepcopy
from threading import Lock, Thread
from problem import Problem
from collections import defaultdict
from datetime import datetime
from operator import itemgetter
from itertools import groupby
from functools import cmp_to_key
import pickle

# crawl/update interval in seconds
UPDATE_INTERVAL = 15

team_lock = Lock()

# we really weren't sure what we wanted
def allNeutral(x):
    if x == 1:
        return 1
    return 0

def isValid(val):
    if val[0] > 0:
        return val[1]
    else:
        return 0

def cmp(a, b):
    return (a>b) - (a<b)


# TEAM tuple layout:
# 0: name (id)
# 1: score (just int)
# 2: list of problem triples (see `problem.py`)
def cmpTeams(t1, t2):

    scorediff = cmp(t1[1], t2[1])

    if scorediff:
        return scorediff

    teamHalfSolves = lambda t: sum([allNeutral(val[0]) for val in t[2]])

    t1Hs = teamHalfSolves(t1)
    t2Hs = teamHalfSolves(t2)

    hsdiff = cmp(t1Hs, t2Hs)

    if hsdiff:
        return hsdiff

    teamLastTs = lambda t: -1 * max([isValid(val) for val in t[2]])

    t1Ls = teamLastTs(t1)
    t2Ls = teamLastTs(t2)

    return cmp(t1Ls, t2Ls)

class Scoreboard:

    def __init__(self, hasCrashed=False):
        try:
            # create backups folder
            os.mkdir("backups")
        except FileExistsError:
            pass
        except:
            print("Error creating backups folder!")
            exit(1)

        # parse problem configs from yaml
        cfgs = None
        with open("problems.yaml", "r") as f:
            cfgs = yaml.safe_load(f)

        # load into Problem objects
        cfg_problems = [Problem(prob["path"], prob["testhalf"], prob["testfull"])
            for prob in cfgs["problems"].values()]

        self.n_problems = len(cfg_problems)

        self.teams_results = []
        self.problems = deepcopy(cfg_problems)
        self.ts = time.localtime()
        self.hasCrashed = hasCrashed

        # start worker thread
        self.t_shouldRun = True
        self.t = Thread(target=self._updateLoop)
        self.t.start()

    def _updateLoop(self):

        # dictionary mapping teams to their score -
        # details about attempts are saved inside the problems!
        team_scores = None

        # shadowed copy of the problems
        shadow_problems = None

        # restore from last backup
        if self.hasCrashed:
            team_scores = pickle.load(open("backups/scores.p", "rb"))
            shadow_problems = [pickle.load(open("backups/problem{}.p".format(i), "rb"))
                for i in range(len(self.problems))]

        else:
            team_scores = defaultdict(int)
            shadow_problems = deepcopy(self.problems)

        while self.t_shouldRun:

            # update from uploaded data; increments team scores on solve
            for (i, p) in enumerate(shadow_problems):
                p.updatePoints(team_scores)

                #after update, pickle problems for backup
                pickle.dump(p, open("backups/problem{}.p".format(i), "wb"), pickle.HIGHEST_PROTOCOL)


            # pickle team scores for backup after all problems are backed up
            pickle.dump(team_scores, open("backups/scores.p", "wb"))

            ts = time.localtime()

            # need to sort before rendering; do it in worker thread
            shadow_results = []
            for (tid,score) in team_scores.items():
                triples = []
                for p in shadow_problems:
                    triples.append(p.getTriple(tid))

                shadow_results.append((tid, score, triples))

            shadow_results.sort(key=cmp_to_key(cmpTeams), reverse=True)

            # convert a team's timestamp to string for frontend
            tsToStr =lambda t: (t[0], t[1],
                list(map(lambda tr: (tr[0], datetime.fromtimestamp(tr[1]).strftime("%H:%M:%S"),tr[2]),t[2])))

            # convert all timestamps to string
            shadow_results = list(map(tsToStr, shadow_results))


            # deepcopy shadowed values in a threadsafe manner
            try:
                team_lock.acquire()
                self.teams_results = deepcopy(shadow_results)
                self.problems = deepcopy(shadow_problems)
                self.ts = ts
            finally:
                team_lock.release()


            time.sleep(UPDATE_INTERVAL)

    # get teams with all values we decided we wanted
    def getTeamValues(self):
        try:
            team_lock.acquire()
            return self.teams_results
        finally:
            team_lock.release()

    # get the problems
    def getProblemSolves(self):
        try:
            team_lock.acquire()
            return self.problems
        finally:
            team_lock.release()

    # get timestamp
    def getTs(self):
        try:
            team_lock.acquire()
            return self.ts
        finally:
            team_lock.release()

    # value is never modified and set at construction, no race condition here :)
    def getNProbs(self):
        return self.n_problems

    def shutdownWorker(self):
        self.t_shouldRun = False
        self.t.join()

if __name__ == "__main__":
    # Just a test querying the scoreboard a few times
    sb = Scoreboard(hasCrashed=False)
    n = 0
    try:
        while True:
            i = 0
            for x in sb.getTeamValues():
                if(i == 15):
                    break
                print(x)
                i+= 1

            time.sleep(3)
    except:
        sb.shutdownWorker
