from scoreboard_manager import ADDRESS, SKEY
from multiprocessing.connection import Client
from sys import argv

if len(argv) < 2:
    print("Missing cmd")
else:
    conn = Client(address=ADDRESS, authkey=SKEY)
    conn.send(argv[1])
    conn.close()