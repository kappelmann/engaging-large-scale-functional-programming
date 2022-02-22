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

UPDATE_INTERVAL = 15

team_lock = Lock()

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

        # parse problem configs from yaml
        cfgs = None
        with open("problems.yaml", "r") as f:
            cfgs = yaml.safe_load(f)

        # load into Problem objects
        cfg_problems = [Problem(prob["path"], prob["testhalf"], prob["testfull"]) 
            for prob in cfgs["problems"].values()]
            


        self.teams = []
        self.problems = deepcopy(cfg_problems)
        self.ts = time.localtime()
        self.hasCrashed = hasCrashed

        # python threads don't play that nicely, but whatever
        self.t_shouldRun = True
        self.t = Thread(target=self._updateLoop)
        self.t.start()

    # decided I don't care wether or not the GIL exists, what matters is
    # that this is
    def _updateLoop(self):

        # restore from last backup
        shadow_teams = None
        shadow_problems = None

        if self.hasCrashed:
            shadow_teams = pickle.load(open("backups/teams.p", "rb"))
            #print(shadow_teams)
            shadow_problems = [pickle.load(open("backups/problem{}.p".format(i), "rb"))
                for i in range(len(self.problems))]

        else:
            shadow_teams = defaultdict(int)
            shadow_problems = [Problem(p["path"],p["testhalf"],p["testfull"]) for p in cfgs["problems"].values()]

        while self.t_shouldRun:
            for (i, p) in enumerate(shadow_problems):
                p.updatePoints(shadow_teams)

                #after update pickle
                pickle.dump(p, open("backups/problem{}.p".format(i), "wb"), pickle.HIGHEST_PROTOCOL)


            pickle.dump(shadow_teams, open("backups/teams.p", "wb"))
                
            ts = time.localtime()

            # python3 iterators are kinda neat
            # need to sort before rendering, do it in worker thread
            values = []
            for (tid,score) in shadow_teams.items():
                triples = []
                for p in shadow_problems:
                    triples.append(p.getTriple(tid))
                
                values.append((tid, score, triples))
            
            values.sort(key=cmp_to_key(cmpTeams), reverse=True)

            tsToStr =lambda t: (t[0], t[1], 
                list(map(lambda tr: (tr[0], datetime.fromtimestamp(tr[1]).strftime("%H:%M:%S"),tr[2]),t[2]))) 
            
            values = list(map(tsToStr, values))

            try:
                team_lock.acquire()
                self.teams = deepcopy(values)
                self.problems = deepcopy(shadow_problems)
                self.ts = ts
            finally:
                team_lock.release()
            
            time.sleep(UPDATE_INTERVAL)

    def getTeamValues(self):
        try:
            team_lock.acquire()
            return self.teams
        finally:
            team_lock.release()

    def getProblemSolves(self):
        try:
            team_lock.acquire()
            return self.problems
        finally:
            team_lock.release()
    
    def getTs(self):
        try:
            team_lock.acquire()
            return self.ts
        finally:
            team_lock.release()

    def shutdownWorker(self):
        self.t_shouldRun = False
        self.t.join()

if __name__ == "__main__":
    sb = Scoreboard(hasCrashed=True)
    n = 0
    while True:
        print("Teams: {}".format(sb.getTeamValues()))
        for p in sb.getProblemSolves():
            #print(p.team_attempts)
            pass
        time.sleep(3)