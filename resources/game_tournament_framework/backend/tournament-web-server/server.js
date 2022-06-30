const childProcess = require("child_process")
const fs = require("fs");
const minimist = require("minimist");
const express = require("express");
const bodyParser = require("body-parser");
const util = require("util");
const crypto = require("crypto");
const app = express();

// TODO: adapt if needed
const uploadDir = "../tournament-runner/uploads/";
const dataDir = "../www/data/";
const animDir = dataDir + "animations/";
const gameDir = dataDir + "games/";

const args = minimist(process.argv.slice(2), {
    alias: {
        u: "accept",
        a: "autorun",
        p: "port",
        r: "register",
        s: "static"
    },
    boolean: ["register", "accept", "static"],
    default: {
        port: 1234,
        autorun: true,
        register: false,
        accept: true,
        static: true
    }
});

const httpPort = Number(args.port);
const canRegister = Boolean(args.register);
const acceptUploads = Boolean(args.accept);
const autorunTournament = Boolean(args.autorun);
const serveStatic = Boolean(args.static);
const maxAnimGenProcesses = 1;
let animGenQueue = []; // {fileName, started, finished}
let newUploadsSubmitted = true; // Run tournament once upon startup (if autorunTournament is set)

async function handleError(err, response) {
    try {
        console.log(err);
        response.send(500, "An error occurred: " + err);
    }
    catch (newErr) {

    }
}

function exec(process, args, opt = {}) {
    return new Promise((resolve, reject) => {
        opt.stdio = opt.stdio || "inherit"
        const proc = childProcess.spawn(process, args, opt);
        proc.on('close', code => {
            if (code === 0) resolve();
            else reject(`process ${process} failed with code ${code}`);
        })
    });
};

const execFile = util.promisify(childProcess.execFile);
const exists = util.promisify(fs.exists);
const writeFile = util.promisify(fs.writeFile);
const readdir = util.promisify(fs.readdir);
const rmdir = util.promisify(fs.rmdir);

async function updateRepos() {
    for (const obj of await readdir("repo-cache", { withFileTypes: true })) {
        if (obj.isDirectory()) {
            try {
                await exec("git", ["pull"], { cwd: `repo-cache/${obj.name}`});
                await writeFile(`repo-cache/${obj.name}/commit`, (await execFile("git", ["rev-parse", "@{0}"], { cwd: `repo-cache/${obj.name}` })).stdout.trim());
            } catch (ex) {
                console.error(ex);
            }
        }
    }
}

const delay = (n) => new Promise(resolve => setTimeout(resolve, n).unref());

async function tournamentBatch() {
    while (true) {
        const start = new Date();

        try {
            if(canRegister) {
                console.log("updating repositories");
                await updateRepos();
            }
            
            if(canRegister || !acceptUploads || newUploadsSubmitted) {
                console.log("running tournament");
                await exec("./run.sh", { cwd: "../tournament-runner", shell: true });
                console.log("done");
                newUploadsSubmitted = false;
            }
        } catch (ex) {
            console.error(ex);
        }

        const wait = 10 * 1000 - (new Date() - start);

        await delay(Math.max(wait, 1000));
    }
}

if (autorunTournament && (canRegister || acceptUploads))
    tournamentBatch();

function startNextAnimGen() {
    let generating = animGenQueue.filter(a => a.started && !a.finished);
    if(generating.length < maxAnimGenProcesses) {
        let toGenerateIdx = animGenQueue.findIndex(a => !a.started && !a.finished);
        if(toGenerateIdx > -1) {
            let fileName = animGenQueue[toGenerateIdx].fileName;
            animGenQueue[toGenerateIdx].started = true;
            let child = childProcess.fork("./mp4-gen", [fileName.replace(/\.mp4/gi, "")]);
            child.on("exit", code => {
                animGenQueue[toGenerateIdx].finished = true;
                startNextAnimGen();
            });
        }
    }
}

async function runServer() {
    let sanitizeRequestFn = fn => fn.replace(/\.\.\//g, "").replace(/^(\/)+/, ""); // Remove "../" from entire string, and sequence of "/" from start of string

    if (canRegister || acceptUploads)
        app.use(bodyParser.urlencoded({ extended: false }));

    app.get("/", async function (request, response) {
        try {
            response.sendFile("index.html", { root: "../www" });
        }
        catch {
            await handleError(err, response);
        }
    });

    app.get("^/data/animations/:fileName((([a-zA-Z0-9\-_\. ]+).(mp4|MP4)))", async function (request, response) {
        try {
            let fileName = sanitizeRequestFn(request.params.fileName);

            if (!(/\.mp4$/i).test(fileName) || (!await exists(animDir + fileName) && !await exists(gameDir + fileName.replace(/\.mp4/gi, ".json")))) { // data may have been removed but MP4 might still exist
                response.status(400);
                response.write(`Bad request.`);
                response.end();
            }
            else if (await exists(animDir + fileName)) {
                response.sendFile(fileName, { root: animDir });
            }
            else {
                if(!animGenQueue.find(a => a.fileName == fileName && !a.finished)) {
                    animGenQueue.push({ fileName: fileName, started: false, finished: false });
                    startNextAnimGen(); // Process limit checks inside function
                }
                response.redirect(303, "../../#animation?game=" + fileName.replace(/\.mp4/gi, ""));
            }
        }
        catch (err) {
            await handleError(err, response);
        }
    });

    if (canRegister)
        app.post("^/api/register", async function (request, response) {
            try {
                const name = String(request.body.name);
                const repo = String(request.body.repo);
                const branch = String(request.body.branch) || "master";

                const path = crypto.randomBytes(20).toString("hex");
                await exec("git", ["clone", "-b", branch, repo, `repo-cache/${path}`]);
                const commit = (await execFile("git", ["rev-parse", "@{0}"], { cwd: `repo-cache/${path}` })).stdout.trim();
                await writeFile(`repo-cache/${path}/commit`, commit);
                await writeFile(`repo-cache/${path}/name`, name);

                if (commit) {
                    response.send("Success");
                } else {
                    await rmdir(`repo-cache/${path}`, { recursive: true });
                    response.send(400, "Failed to clone")
                }
            } catch (err) {
                await handleError(err, response);
            }
        });

    if(acceptUploads)
        app.post("^/api/upload", async function (request, response) {
            try {
                if([request.body.student_id, request.body.commit, request.body.src].indexOf(undefined) > -1) {
                    response.send(400, "Missing parameters");
                    return;
                }

                const studentId = sanitizeRequestFn(String(request.body.student_id));
                const commit = String(request.body.commit);
                const src = String(request.body.src);
                const results = String(request.body.results) || "";
                
                let studentDir = `${uploadDir}${studentId}/`;
                let studentSrcDir = `${studentDir}`;
                if(!fs.existsSync(studentDir))
                    fs.mkdirSync(studentDir, { recursive: true });
                if(!fs.existsSync(studentSrcDir))
                    fs.mkdirSync(studentSrcDir, { recursive: true });
                
                fs.writeFileSync(`${studentDir}commit`, commit);
                fs.writeFileSync(`${studentDir}results.xml`, results);
                fs.writeFileSync(`${studentSrcDir}Exercise08.hs`, src);

                newUploadsSubmitted = true;
                response.send("Success");
            } catch (err) {
                await handleError(err, response);
            }
        });

    if (serveStatic)
        app.get("^:fileName((((\/[a-zA-Z0-9\-_\. ]+)+).(js(on)?|JS(ON)?|html?|HTML?|css|CSS|png|PNG|ico|ICO|jpe?g|JPE?G|bmp|BMP|gif|GIF|txt|TXT|hs|HS|bin|BIN)))", async function (request, response) {
            try {
                let fileName = sanitizeRequestFn(request.params.fileName);

                if (await exists("../www/" + fileName)) {
                    response.sendFile(fileName, { root: "../www" });
                }
                else {
                    response.send(404, `${request.params.fileName} not found.`);
                }
            }
            catch (err) {
                await handleError(err, response);
            }
        });

    try {
        app.listen(httpPort, () => console.log(`Started HTTP server at port ${httpPort}.`));
    }
    catch (err) {
        console.log(`Error attempting to start HTTP server at port ${httpPort}:`);
        console.log(err);
    }
}

runServer();
