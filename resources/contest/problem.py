from typing import Sequence
from collections import defaultdict
import xml.etree.ElementTree as ET
import os
from datetime import datetime
import time

EXCEPTION = -1
NOT_PASSED = 0
HALF_PASSED = 1
PASSED = 2

class Problem:

    def __init__(self, path, testhalf, testfull):
        self.path = path
        self.testhalf = testhalf
        self.testfull = testfull
        self.testhalf_len = len(testhalf)
        self.testfull_len = len(testfull)
        self.team_attempts = {}
        self.last_checked = 0

    def getAttempt(self, teamid):
        if not teamid in self.team_attempts:
            notest = [False, False]

            self.team_attempts[teamid] = [0, notest, "", int(time.mktime(datetime.max.timetuple()))]

        return self.team_attempts[teamid]

    # triple layout
    # status of problem
    # 1: status : -1 not attempted, 0 not passed, 1 half passed, 2 full passed
    # 2: timestamp
    # 3: number of attempts before solve
    def getTriple(self, teamid):
        # has not attempted this problem yet
        if not teamid in self.team_attempts:
            # dirty but works
            return (-1, -1, 0)

        attempt = self.team_attempts[teamid]

        status = 0
        if attempt[1][0]:
            status = sum(map(int, attempt[1]))

        timesf = attempt[0]

        timestamp = attempt[3]

        # failsave -> timestamp somehow could not get parsed, but team was added
        if timestamp == int(time.mktime(datetime.max.timetuple())):
            # see above
            return (-1,-1,0)

        return (status, timestamp, timesf)


    def updatePoints(self, teamscores):
        self.last_checked = time.localtime()

        fullpath = self.path

        # failsave
        if not os.path.isdir(fullpath):
            return

        for teamname in os.scandir(fullpath):
            teamid = teamname.name

            attempt = self.getAttempt(teamid)

            # already got the point but for some reason resubmitted; ignore
            if all(attempt[1]):
                continue

            ts = getTimestamp(fullpath+"/"+teamid+"/timestamp")

            if ts != self.team_attempts[teamid][3]:
                self.team_attempts[teamid][3] = ts
            else:
                continue

            try:
                tree = ET.parse(fullpath+"/"+teamid+"/results.xml")
            except:
                # some weird race condition might occur; try again in next update
                continue

            halfp = 0
            fullp = 0
            for t in tree.iter():
                if t.tag == "testcase":

                    # testcase failed
                    if t.getchildren():
                        continue

                    testname = t.get("name")
                    if testname in self.testhalf:
                        halfp += 1

                    if testname in self.testfull:
                        fullp += 1

            if halfp == self.testhalf_len:
                self.team_attempts[teamid][1][0] = True

            if fullp == self.testfull_len and halfp == self.testhalf_len:
                self.team_attempts[teamid][1][1] = True


            # default dict; if this isn't yet an element, value is 0;
            # also add teams with 0 points (if not yet existing)
            if all(self.team_attempts[teamid][1]):
                teamscores[teamid] += 1
            else:
                # didn't get full points, so failed attempt!
                self.team_attempts[teamid][0] += 1
                teamscores[teamid] += 0

def getTimestamp(path):
    try:
        with open(path, "r") as f:
            return int(time.mktime(datetime.fromisoformat(f.read()).timetuple()))
    except:
        return int(time.mktime(datetime.max.timetuple()))


if __name__ == "__main__":
    # Just a test printing all parsed tests in the specified XML
    tree = ET.parse("/some/path/teaching_fpv/resources/contest/example_data/0/uploads/foobar/results.xml")
    print("start dump")
    l = []
    for t in tree.iter():
        if t.tag == "testcase":
                l.append(t.get("name"))
    print(l)
