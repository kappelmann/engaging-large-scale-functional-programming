import { e, render } from "./e.js";
import { Notifier } from "./notifier.js";
import { Game } from "./game-class.js";

import "../styles/renderer.css";

/**
 * 
 * @param {ArrayBuffer} data 
 */
export function renderGame(data, gameId) {
  let game = new Game(data)
  let currentState = new Notifier(0);

  return e("div", { className: "gameContainer " + playbackSpeedClassName(loadPlaybackSpeed()) },
    e("div", { className: "gameEmbed" },
      e("table", { className: "board" },
        new Array(game.height).fill(0).map((_, y) => e("tr", { className: "boardRow" },
          new Array(game.width).fill(0).map((_, x) => e("td", { className: "boardCell" }, highlightCurrent(game, currentState, x, y)))
        )),
        makeState(game, currentState),
      ),
      playbackControl(game, currentState, gameId)
    )
  );
}

/**
 * 
 * @param {Game} game 
 * @param {Notifier} currentState 
 */
function playbackControl(game, currentState, gameId) {
  const baseSpeedMs = 500;
  const availableSpeeds = [0.25, 0.5, 1.0, 1.5, 2.0, 3.0, 4.0, 5.0, 7.5, 10.0];

  let iv = 0;
  let num = 0;
  let playbackSliderContainer = null;
  let playbackSliderBar = null;
  let currentFrameBox = null;
  let currentSpeedLabel = null;
  let isDragging = false;
  let wasPlayingBeforeDrag = false;
  let currentSpeed = loadPlaybackSpeed();

  function setFrame(n) {
    if (n < 0) n = 0;
    if (n >= game.state.length) {
      n = game.state.length - 1;
    }
    if (n == game.state.length - 1) {
      pause();
    }

    num = n;
    currentState.value = n;
  }

  function start(speed) {
    if (iv !== 0) clearInterval(iv);
    iv = setInterval(() => {
      setFrame(num + 1);
    }, baseSpeedMs / (currentSpeed = speed || currentSpeed));
  }

  function pause() {
    if (iv !== 0) clearInterval(iv);
    iv = 0;
  }

  function selectSpeed(s) {
    storePlaybackSpeed(s);
    document.querySelectorAll(".gameContainer").forEach(el => { availableSpeeds.forEach(sp => { if(sp != s) el.classList.remove(playbackSpeedClassName(sp)); else el.classList.add(playbackSpeedClassName(sp)); })});
    currentSpeedLabel.innerText = s.toLocaleString() + "x";
    if (iv !== 0)
      start(s);
    else
      currentSpeed = s;
  }

  function adjustNumByEvent(event) {
    let posPx = event.pageX - playbackSliderContainer.getBoundingClientRect().left;
    let pos = Math.min(Math.max(posPx, 0), playbackSliderContainer.offsetWidth) / playbackSliderContainer.offsetWidth;

    let newNum = Math.round(pos * (game.state.length - 1));
    setFrame(newNum);
  }

  function drag(event) {
    if (!isDragging)
      return;

    adjustNumByEvent(event);
    event.stopPropagation();
  }

  function beginDragging() {
    if (isDragging)
      return;
    isDragging = true;
    if (wasPlayingBeforeDrag = (iv !== 0))
      pause();
    currentFrameBox.classList.add("noSelect");
  }

  function endDragging() {
    if (!isDragging)
      return;
    isDragging = false;
    if (wasPlayingBeforeDrag)
      start();
    currentFrameBox.classList.remove("noSelect");
  }

  let updateSliderBtnPosition = node => currentState.subscribe(n => node.style.left = Math.round((n / (game.state.length - 1)) * playbackSliderBar.getBoundingClientRect().width) + "px");
  let updateSliderBarWidth = node => currentState.subscribe(n => node.style.width = Math.round((n / (game.state.length - 1)) * playbackSliderBar.getBoundingClientRect().width) + "px");

  // User may move/release mouse outside of playbackControlContainer
  document.documentElement.addEventListener("mousemove", drag);
  document.documentElement.addEventListener("mouseup", endDragging);

  start();
  return e("div", { className: "playbackControlContainer" },
    e("div", { className: "playbackSliderOuterContainer" },
      (playbackSliderContainer = e("div", { className: "playbackSliderContainer", onmousedown: adjustNumByEvent },
        (playbackSliderBar = e("div", { id: "playbackSliderBar" }, e("div", { id: "playbackSliderBarFilling" }, e("div", { id: "playbackSliderBtn", onmousedown: beginDragging, style: { left: 0 } }, updateSliderBtnPosition), updateSliderBarWidth)))))
    ),
    e("div", { className: "playbackControlRow" },
      e("div", { className: "controlButton", title: "First", onclick() { pause(); setFrame(0) } }, "⏮️"),
      e("div", { className: "controlButton", title: "Previous", onclick() { pause(); setFrame(num - 1) } }, "◀️"),
      e("div", { className: "controlButton", title: "Play/Pause", onclick() { if (iv !== 0) pause(); else start(); } }, "⏯️"),
      currentFrameBox = e("input", { className: "currentFrameBox", title: "Current Frame", value: currentState.value + 1, oninput(ev) { pause(); let val = Number.parseInt(ev.target.value); if (!Number.isNaN(val) && num + 1 !== val) setFrame(val - 1); } }, node => currentState.subscribe(n => node.value = n + 1)),
      e("span", "/"),
      e("span", String(game.state.length)),
      e("div", { className: "controlButton", title: "Next", onclick() { pause(); setFrame(num + 1) } }, "▶️"),
      e("div", { className: "controlButton", title: "Last", onclick() { setFrame(game.state.length) } }, "⏭️"),
      gameId ? e("div", { className: "controlButton", title: "Full Game Page", onclick() { location.href = "index.html#game?id=" + gameId } }, "ℹ️") : []
    ),
    e("div", { className: "speedControlRow" },
      e("div", { className: "controlButton", title: "Decrease Speed", onclick() { selectSpeed(availableSpeeds[Math.max(0, availableSpeeds.indexOf(currentSpeed) - 1)]) } }, "🔽"),
      e("div", { className: "currentSpeedLabelContainer" },
        currentSpeedLabel = e("span", { className: "currentSpeedLabel" }, currentSpeed.toLocaleString() + "x")
      ),
      e("div", { className: "controlButton", title: "Increase Speed", onclick() { selectSpeed(availableSpeeds[Math.min(availableSpeeds.length - 1, availableSpeeds.indexOf(currentSpeed) + 1)]) } }, "🔼"),
    )
  );
}

function loadPlaybackSpeed() {
  let s = Number(localStorage.getItem("playbackSpeed"));
  return Number.isNaN(s) || s <= 0.1 ? 1.0 : s;
}

function storePlaybackSpeed(s) {
  localStorage.setItem("playbackSpeed", String(s));
}

function playbackSpeedClassName(s) {
  return "currentSpeed_" + s.toString().replace(".", "_");
}

/**
 * 
 * @param {Game} game 
 * @param {Notifier} currentState 
 * @param {number} x 
 * @param {number} y 
 */
function highlightCurrent(game, currentState, x, y) {
  return node => {
    currentState.subscribe(n => {
      let state = game.state[n];
      if (state.type === "move" && state.action.pos[0] == x && state.action.pos[1] == y) {
        node.classList.remove("reactionSource");
        node.classList.add("spawn");
      } else if (state.type === "reaction" && state.action.some(([rx, ry]) => rx == x && ry == y)) {
        node.classList.remove("spawn");
        node.classList.add("reactionSource");
      } else {
        node.classList.remove("reactionSource");
        node.classList.remove("spawn");
      }
    })
  };
}

function playerToColor(p) {
  return ["green", "", "red"][Math.sign(p) + 1];
}

/**
 * 
 * @param {Game} game 
 * @param {Notifier} state 
 */
function makeState(game, state) {
  const container = e("div");
  function getOrbs(num) {
    let result = [];
    game.forEach((x, y, v) => {
      if (v == 0) return;
      result.push(...dots(x, y, Math.abs(v), playerToColor(v)))
    }, num);
    return result;
  }
  function clear(elem) {
    while (elem.firstChild !== null) elem.removeChild(elem.firstChild);
  }
  function alignLists(prev, now) {
    if (now.length == 1 && !Array.isArray(now[0])) {
      prev.forEach(v => v.remove());
      container.appendChild(now[0]);
      console.error("cell with too many orbs (" + now.innerText + ")!! Add more patterns");
      return;
    }
    for (let i = 0; ; i++) {
      const p = i < prev.length;
      const n = i < now.length;
      if (!p && !n) {
        break;
      } if (p && n) {
        prev[i].setData(...now[i]);
      } else if (p) {
        prev[i].remove();
      } else {
        const el = dot(...cellCenter(now[i][3], now[i][4]), "", now[i][3], now[i][4]);
        container.appendChild(el);
        setTimeout(() => el.setData(...now[i]), 0);
      }
    }
  }
  function selectCell(pos) {
    return v => v.cellPos && v.cellPos[0] == pos[0] && v.cellPos[1] == pos[1]
  }
  function index(pos) {
    return pos[0] + pos[1] * game.width;
  }
  function nb([x, y]) {
    return [[x + 1, y], [x - 1, y], [x, y + 1], [x, y - 1]].filter(([x, y]) => x >= 0 && y >= 0 && x < game.width && y < game.height);
  }
  function ensurePosExists(cells, cellMaps, state, pos) {
    const idx = index(pos);
    if (!cellMaps[idx]) {
      const value = state.cells[idx];
      cellMaps[idx] = [cells.filter(selectCell(pos)), dots(pos[0], pos[1], Math.abs(value), playerToColor(value))];
    }
    return cellMaps[idx][0];
  }
  function makeElem(num, prev) {
    if (num != prev + 1) {
      clear(container);
      render(getOrbs(num).map(v => Array.isArray(v) ? dot(...v) : v), container, null);
    } else {
      const state = game.state[num];
      switch (state.type) {
        case "initial": break;
        case "move": {
          const pos = state.action.pos;
          const previousOrbs = [...container.children].filter(selectCell(pos));
          const value = state.cells[index(pos)];
          const nextOrbs = dots(pos[0], pos[1], Math.abs(value), playerToColor(value));
          alignLists(previousOrbs, nextOrbs);
          break;
        }
        case "reaction": {
          const cells = [...container.children];
          const cellMaps = new Array(game.width * game.height);
          for (const pos of state.action) {
            const from = ensurePosExists(cells, cellMaps, state, pos);
            for (const n of nb(pos)) {
              const to = ensurePosExists(cells, cellMaps, state, n);
              to.push(from.pop());
            }
          }
          cellMaps.forEach(([p, n]) => alignLists(p, n));
          break;
        }
      }
    }
  }
  state.subscribe(makeElem);
  makeElem(state.value, undefined);
  return container;
}

function cellCenter(x, y) {
  return [
    24 + x * 44,
    24 + y * 44
  ];
}

function dots(x, y, n, color) {
  const [centerX, centerY] = cellCenter(x, y);

  switch (n) {
    case 0: return [];
    case 1: return [[centerX, centerY, color, x, y]];
    case 2: return [
      [centerX - 5, centerY, color, x, y],
      [centerX + 5, centerY, color, x, y]
    ];
    case 3: return [
      [centerX - 5, centerY - 4, color, x, y],
      [centerX + 5, centerY - 4, color, x, y],
      [centerX, centerY + 4, color, x, y],
    ];
    case 4: return [
      [centerX - 5, centerY - 5, color, x, y],
      [centerX + 5, centerY - 5, color, x, y],
      [centerX - 5, centerY + 5, color, x, y],
      [centerX + 5, centerY + 5, color, x, y],
    ];
    case 5: return [
      [centerX - 5, centerY - 5, color, x, y],
      [centerX + 5, centerY - 5, color, x, y],
      [centerX - 5, centerY + 5, color, x, y],
      [centerX + 5, centerY + 5, color, x, y],
      [centerX, centerY, color, x, y],
    ];
    case 6: return [
      [centerX - 8, centerY, color, x, y],
      [centerX - 4, centerY - 7, color, x, y],
      [centerX + 4, centerY - 7, color, x, y],
      [centerX + 8, centerY, color, x, y],
      [centerX + 4, centerY + 7, color, x, y],
      [centerX - 4, centerY + 7, color, x, y],
    ];
    case 7: return [
      [centerX - 8, centerY, color, x, y],
      [centerX - 4, centerY - 7, color, x, y],
      [centerX + 4, centerY - 7, color, x, y],
      [centerX + 8, centerY, color, x, y],
      [centerX + 4, centerY + 7, color, x, y],
      [centerX - 4, centerY + 7, color, x, y],
      [centerX, centerY, color, x, y],
    ];
    default: return [e("div", { class: "atomContainer", style: { top: centerY + "px", left: centerX + "px" } }, e("span", { class: ["fallback", color] }, String(n)))];
  }
}

function dot(x, y, color, cellX, cellY) {
  let container = undefined;
  let core = undefined;
  let shadow = undefined;

  function setData(x, y, color, cellX, cellY) {
    container.style.top = y + "px";
    container.style.left = x + "px";
    container.cellPos = [cellX, cellY];
    core.classList.remove("red");
    core.classList.remove("green");
    if (color == "") {
      core.style.borderWidth = "0";
      shadow.style.borderWidth = "0";
    } else {
      core.style.borderWidth = "";
      shadow.style.borderWidth = "";
      core.classList.add(color);
    }
  }
  return e("div", { class: "atomContainer", style: { top: y + "px", left: x + "px" }, cellPos: [cellX, cellY], setData },
    e => { container = e },
    e("div", { class: "atom shadow", style: color == "" ? { borderWidth: "0" } : {} }, e => { shadow = e }),
    e("div", { class: ["atom core", color], style: color == "" ? { borderWidth: "0" } : {} }, e => { core = e })
  );
}
