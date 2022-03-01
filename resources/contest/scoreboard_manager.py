from scoreboard import Scoreboard
from multiprocessing.managers import BaseManager
from multiprocessing.connection import Listener
from copy import deepcopy
from os import getpid

class ScoreboardManager(BaseManager):
    pass

ADDRESS = ('localhost', 50001)
SKEY = b'thisrunsonlylocally'

def main():

    listener = Listener(address=ADDRESS, authkey=SKEY)
    print("Listener running with PID: {}".format(getpid()))

    try:
        shared_scoreboard = Scoreboard()
        frozen_vals = None
        is_frozen = False
        n_problems = shared_scoreboard.getNProbs()

        while True:
            conn = listener.accept()
            req = conn.recv()
            if (req == "freeze"):
                if is_frozen:
                    pass
                else:
                    vals = shared_scoreboard.getTeamValues()
                    frozen_vals = deepcopy(vals)
                    is_frozen = True
                    print("FREEZE")
            elif (req == "unfreeze"):
                is_frozen = False
                print("UNFREEZE")
            elif (req == "debug"):
                for t in shared_scoreboard.getTeamValues():
                    print(t)
            elif (req == "req"):
                if is_frozen:
                    conn.send(("(FROZEN)", frozen_vals, n_problems))
                else:
                    conn.send(("", shared_scoreboard.getTeamValues(), n_problems))
            else:
                print("Unknown request: {}".format(req))

            conn.close()
    
    finally:
        listener.close()
        shared_scoreboard.shutdownWorker()

if __name__ == '__main__':
    main()