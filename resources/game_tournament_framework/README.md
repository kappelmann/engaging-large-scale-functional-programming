# Game Tournament Framework

This folder contains a framework to run round-robin game tournaments,
continously accepting and executing games in parallel while collecting statistics for each game and player.
The generated score data can then easily be displayed, for example, on a tournament website.

Some of the code in the framework is specific to run the game [Chain Reaction](https://brilliant.org/wiki/chain-reaction-game/).
An example instance can be found [here](https://vmnipkow16.in.tum.de/christmas2020/)
and corresponding instructions can be found [here](instructions.pdf).
We provide guidance how to adapt the framework to other games below.

To run the framework, you need
1. a test server, accepting and testing submissions (e.g. [Artemis](https://github.com/ls1intum/Artemis)),
2. a server running the services contained in this directory, and
3. some way to transfer results from the former to the latter.

## Test Server

The job of the test server is to check whether a student's submission satisifes
some minimum requirements, e.g. having correctly implemented the required game strategy function
to run games against other students' submissions.
The test server needs to
1. accept submissions,
2. test submissions, and
3. forward the passing submissions to the server running the tournament framework.

### Transferring Results To The Tournament Server

In principle, you may upload the submission data to the tournament server in any way you like.
You only need to make sure to include all data required by the tournament server (see below).

In `run.sh`, you can find an example script that tests a Haskell submission on Artemis
(like the ones that can be found in [this repository](https://github.com/kappelmann/engaging-large-scale-functional-programming/tree/main/resources))
uploading its result to the tournament server, serving the `upload.php` script (e.g. by using nginx).
If your test and tournament server run on the same machine, you may simply copy the data to the appropriate location
and then pass it to `tournament-runner` (cf final lines of `upload.php`).

## Tournament Server

The tournament server contained in the subfolder `backend` consists of three main components
1. `tournament-app`: Contains the html and JavaScript sources to display the results of the tournament.
2. `tournament-runner`: Contains the code to run the round-robin simulations, creating the result data that
   is then displayed by the web server.
3. `tournament-web-server`: Contains the web server that
  1. takes the submission uploaded from the test server, forwarding them to the tournament runner,
  2. (optionally) calls the `tournament-runner` to simulate new submissions against existing ones, and
  3. uses the files created by `tournament-app` to display the results created by `tournament-runner`.

### tournament-app

This component is quite specific to the ChainReaction game. You will have to create your own 
sources if you want to use another game. The existing files may provide you some inspirations and ideas
(e.g. creating videos and gifs from game states).

**Deployment**:
0. Navigate to `tournament-app`
1. Install [npm](https://docs.npmjs.com/downloading-and-installing-node-js-and-npm)
2. Run `npm install`.
3. Build the app `npm run build`

By default, the created files will be put into the folder `backend/www`.

### tournament-runner

This component caches and simulates submissions in a round-robin fashion.
A simulation is started using `run.sh`.
The resulting data will be put into `backend/www/data` by default.
This component is usually automatically called from `tournament-web-server` on new submissions.

Much of the framework is agnostic to the game to be simulated.
However, specifics to the games, e.g. type of the game board,
game state, interfaces and allowed modules, etc. have to be adapted.
It may also collect statistics while simulating the game; obviously this
also needs to be adjusted in case you want to use the framework for a different game.

The runner requires [stack](https://docs.haskellstack.org/en/stable/README/) to run the simulations.
Note that the runner dynamically has to change its `.yaml` and `.cabal` files.
If you want to clean the runner's state, do not forget to reset those files too (cf. `cleanup.sh`).

### tournament-web-server

**Deployment**:
0. Navigate to `tournament-web-server`
1. Install [npm](https://docs.npmjs.com/downloading-and-installing-node-js-and-npm)
2. Run `npm install`.
  - If it fails, check the error output. You may need to install some image manipulation libraries,
    e.g. on Debian: `apt install libjpeg-dev libgif-dev`.
3. Start the server `npm run start`

By default, the server accepts new submissions and calls `tournament-runner` to update the tournament results.
If you are expecting a large number of submissions, you may move the simulation to an external server.
In that case, disable the autorun  option of the server, upload the user data to the external machine
on arrival, run `tournament-runner` on the external machine, and then sync back the resulting data
to the web server. 

The main parts of the server are agnostic to the game to be simulated.
It also contains code that dynamically creates MP4 videos of a game if users ask for it on the tournament website.
This code has to be adapted if you also want to support this for another game.

## Chain Reaction

### Test Requirements

Tests are run using [stack](https://docs.haskellstack.org/en/stable/README/).

### Test Setup

The executables specified in `test.cabal` expect the solution repository checked out in the `solution`
subdirectory and the submission checked out in the `assignment` subdirectory.
Moreover, `test.cabal` provides an executable to test the template repository locally.
For this, it expects the template repository in the `template` subdirectory.

### Running Tests

Refer to `test.cabal` for detailed information about the targets and flags provided.

#### Locally

You can run executables specified in `test.cabal` using `stack run <executableName>`.

#### On Artemis

By default, Artemis runs `./run.sh -s` to execute the tests.
You can modify `run.sh` to adapt the build and test process.

