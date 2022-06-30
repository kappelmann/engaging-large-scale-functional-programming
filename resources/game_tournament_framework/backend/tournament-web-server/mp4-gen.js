const { createCanvas } = require("canvas");
const fileSystem = require("fs");
const { Worker } = require("worker_threads");

// TODO: adapt if needed
const dataDir = "../www/data/";
const animDir = dataDir + "animations/";
const gameDir = dataDir + "games/";

async function generateAnimations() {
  try {
    let games = process.argv.filter(arg => (/^[^\/\\]+_[^\/\\]+_[^\/\\]+$/i).test(arg)).map(arg => arg.replace(/.mp4/gi, ""));
    games = games.filter((arg, idx) => games.indexOf(arg) == idx); // Remove duplicates
    if(!games.length) {
      console.log("No game IDs supplied.");
      return;
    }
    let animsExist = games.filter(gameId => fileSystem.existsSync(`${animDir + gameId.toString()}.mp4`));
    let animsToGen = games.filter(gameId => !(animsExist.indexOf(gameId) + 1));
    if(animsExist.length)
      console.log("Animations of the following games already exist: ", animsExist);
    if(!animsToGen.length) {
      console.log("Nothing to be done.");
      return;
    }
    else
      console.log("Found the following games to generate animations of: ", animsToGen);

    for(const gameId of animsToGen) // Process sequentially
      await generateGameAnimation(gameId);
  }
  catch(err) {
    console.log("An error occurred while generating animations:");
    console.log(err);
  }
}

async function generateGameAnimation(gameId) {
  if(typeof Game !== "object")
    loadGameClass();

  console.log(`Generating animation of game ${gameId}...`);

  const arr = fileSystem.readFileSync(`${gameDir + gameId.toString()}.bin`);
  const game = new Game(arr);
  const stateDigits = Math.floor(Math.log10(game.state.length)) + 1;

  const cellSize = 40;
  const borderSize = 4;
  const orbSize = 8;
  const orbBorderSize = 1;
  const boardColor = "#fff";
  const borderColor = "#000";
  const orbBorderColor = "#000";
  const playerPlusColor = "#ff0000";
  const playerMinusColor = "#008000";
  const spawnColor = "#add8e6";
  const reactionSourceColor = "#ffb6c1";
  const canvasWidth = game.width * cellSize + (game.width + 1) * borderSize;
  const canvasHeight = game.height * cellSize + (game.height + 1) * borderSize;
  const canvas = createCanvas(canvasWidth, canvasHeight);
  const ctx = canvas.getContext('2d');

  let clearCanvas = () => {
    ctx.fillStyle = boardColor;
    ctx.fillRect(0, 0, canvasWidth, canvasHeight);
  };

  let clearCell = (x, y, bgCol) => {
    ctx.fillStyle = bgCol || boardColor;
    ctx.fillRect(borderSize + x * (cellSize + borderSize), borderSize + y * (cellSize + borderSize), cellSize, cellSize);
  }

  let drawOrb = (centerX, centerY, bgCol) => {
    ctx.fillStyle = bgCol;
    ctx.beginPath();
    ctx.arc(centerX, centerY, orbSize, 0.0, 2.0 * Math.PI);
    ctx.fill();
    ctx.strokeStyle = orbBorderColor;
    ctx.lineWidth = orbBorderSize;
    ctx.beginPath();
    ctx.arc(centerX, centerY, orbSize, 0.0, 2.0 * Math.PI);
    ctx.stroke();
  };

  let drawCell = (x, y, orbs) => {
    let cellX = x * (cellSize + borderSize) + borderSize, cellY = y * (cellSize + borderSize) + borderSize, halfCell = cellSize / 2, margin = 0.67 * orbSize;
    let orbBg = orbs < 0 ? playerMinusColor : playerPlusColor;
    switch(Math.abs(orbs)) {
      case 0:
        break;
      case 1:
        drawOrb(cellX + halfCell, cellY + halfCell, orbBg);
        break;
      case 2:
        drawOrb(cellX + halfCell - margin, cellY + halfCell, orbBg);
        drawOrb(cellX + halfCell + margin, cellY + halfCell, orbBg);
        break;
      case 3:
        drawOrb(cellX + halfCell - margin, cellY + halfCell - margin, orbBg);
        drawOrb(cellX + halfCell + margin, cellY + halfCell - margin, orbBg);
        drawOrb(cellX + halfCell, cellY + halfCell + margin, orbBg);
        break;
      case 4:
        drawOrb(cellX + halfCell - margin, cellY + halfCell - margin, orbBg);
        drawOrb(cellX + halfCell + margin, cellY + halfCell - margin, orbBg);
        drawOrb(cellX + halfCell - margin, cellY + halfCell + margin, orbBg);
        drawOrb(cellX + halfCell + margin, cellY + halfCell + margin, orbBg);
        break;
      case 5:
        drawOrb(cellX + halfCell - margin, cellY + halfCell - margin, orbBg);
        drawOrb(cellX + halfCell + margin, cellY + halfCell - margin, orbBg);
        drawOrb(cellX + halfCell - margin, cellY + halfCell + margin, orbBg);
        drawOrb(cellX + halfCell + margin, cellY + halfCell + margin, orbBg);
        drawOrb(cellX + halfCell, cellY + halfCell, orbBg);
        break;
      default:
        let text = Math.abs(orbs).toString();
        ctx.font = "20px sans-serif";
        ctx.fillStyle = orbBg;
        ctx.fillText(text, cellX + halfCell - 5, cellY + halfCell + 8);
        break;
    }
  };
  
  let updateCell = (x, y, orbs, bgCol) => {
    clearCell(x, y, bgCol);
    if(orbs != 0)
      drawCell(x, y, orbs);
  }

  let drawGrid = () => {
    ctx.strokeStyle = borderColor;
    ctx.lineWidth = borderSize;
    for(let y = 0; y <= game.height; y++) {
      ctx.beginPath();
      ctx.lineTo(0, y * (cellSize + borderSize) + borderSize / 2);
      ctx.lineTo(canvasWidth * (cellSize + borderSize), y * (cellSize + borderSize) + borderSize / 2);
      ctx.stroke();
    }
    
    for(let x = 0; x <= game.width; x++) {
      ctx.beginPath();
      ctx.lineTo(x * (cellSize + borderSize) + borderSize / 2, 0);
      ctx.lineTo(x * (cellSize + borderSize) + borderSize / 2, canvasHeight);
      ctx.stroke();
    }
  };

  let images = [];
  const worker = new Worker("./ffmpeg-worker-mp4.js");
  
  let pad = (n, width, z) => {
    z = z || "0";
    n = n + "";
    return n.length >= width ? n : new Array(width - n.length + 1).join(z) + n;
  };

  let finalizeVideo = async () => {
    await new Promise((resolve, reject) => {

      // Timeout if generation fails/takes too long
      let timeout = setTimeout(() => {
        reject();
      }, 60000);

      let done = output => { // output: Uint8Array
        console.log(`Finished generating animation of ${gameId}`);
    
        fileSystem.writeFileSync(`${animDir + gameId.toString()}.mp4`, output);
        clearTimeout(timeout);
        resolve();
      };

      worker.on("message", msg => {
        switch (msg.type) {
          case "stdout":
              console.log(msg.data);
              break;
          case "stderr":
              console.log(msg.data);
              break;
          case "exit": // Does not mean something went wrong!
              console.log("Process exited with code " + msg.data);
              // worker.terminate();
              break;
          case "done":
              done(msg.data.MEMFS[0].data);
          break;
        }
      });

      // https://trac.ffmpeg.org/wiki/Slideshow
      // https://semisignal.com/tag/ffmpeg-js/
      worker.postMessage({
          type: "run",
          TOTAL_MEMORY: 268435456,
          arguments: ["-r", "2", "-f", "image2", "-i", `img%0${stateDigits}d.jpg`, "-vcodec", "libx264", "-crf", "1", "-s", `${canvasWidth}:${canvasHeight}`, "-pix_fmt", "yuv420p", "out.mp4"],
          MEMFS: images
      });
      
      // Updated recommented arguments
      /*
      worker.postMessage({
        type: 'run',
        TOTAL_MEMORY: 268435456,
        arguments: [
            //"-r", opts.state.frameRate.toString(),
            "-framerate", opts.state.frameRate.toString(),
            "-frames:v", imgs.length.toString(),
            "-an", // disable sound
            "-i", "img%03d.jpeg",
            "-c:v", "libx264",
            "-crf", "17", // https://trac.ffmpeg.org/wiki/Encode/H.264
            "-filter:v",
            `scale=${w}:${h}`,
            "-pix_fmt", "yuv420p",
            "-b:v", "20M",
            "out.mp4"],
        MEMFS: imgs
      });
      */
    });
  }

  clearCanvas();
  drawGrid();
  const frameMimeType = "image/jpeg";

  /*
  function highlightCurrent(game, currentState, x, y) {
  return node => {
    currentState.subscribe(n => {
      let state = game.state[n];
      if (state.type === "move" && state.action.pos[0] == x && state.action.pos[1] == y) {
        node.classList.add("spawn");
      } else if (state.type === "reaction" && state.action.some(([rx, ry]) => rx == x && ry == y)) {
        node.classList.add("reactionSource");
      } else {
        node.classList.remove("reactionSource");
        node.classList.remove("spawn");
      }
    })
  };
}
  */

  let boardStateTracker = new Array(game.width * game.height).fill(0);
  let lastHighlightedCells = [];
  
  for(let i = 0; i < game.state.length; i++) {
    let state = game.state[i];
    let board = state.cells;
    let newHighlightedCells = [];

    for(let y = 0; y < game.height; y++) {
      for(let x = 0; x < game.width; x++) {
        let idx = y * game.width + x;
        let s = board[idx];
        if(boardStateTracker[idx] != s || lastHighlightedCells.indexOf(idx) > -1) {
          boardStateTracker[idx] = s;
          let bgCol = (() => {
            if(state.type === "move" && state.action.pos[0] == x && state.action.pos[1] == y) {
              newHighlightedCells.push(idx);
              return spawnColor;
            }
            else if(state.type === "reaction" && state.action.some(([rx, ry]) => rx == x && ry == y)) {
              newHighlightedCells.push(idx);
              return reactionSourceColor;
            }
            else
              return boardColor;
          })();
          updateCell(x, y, s, bgCol);
        }
      }
    }

    lastHighlightedCells = newHighlightedCells;

    const data = canvas.toBuffer(frameMimeType, { quality: 1.0, progressive: false, chromaSubsampling: true });
    images.push({
      name: `img${pad(images.length, stateDigits)}.jpg`,
      data: data
    });
    
    // Save individual frames:
    // fileSystem.writeFileSync(`${animDir + gameId.toString()}_img${pad(i, stateDigits)}.jpg`, data);
  }
  await finalizeVideo();

  let lockPath = `${animDir + gameId.toString()}.mp4.lock`;
  if(fileSystem.existsSync(lockPath))
    fileSystem.unlinkSync(lockPath);
}

function loadGameClass() {
  // This solves the ES6/CommonJS conflict without having to use Babel, assuming "require" works in the client's browser, or duplicating code
  let gameSource = "Game = " + fileSystem.readFileSync("../tournament-app/src/app/game-class.js").toString().replace("export ", "");
  eval(gameSource);
}

generateAnimations().then(() => process.exit()); // process.exit() needed if started by server.js
