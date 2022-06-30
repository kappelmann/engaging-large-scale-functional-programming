import { async, bind, e, render } from "./e.js";
import { Notifier } from "./notifier.js";
import { renderGame } from "./renderer.js";
import { snowflakes } from "./snow.js";

function embed() {
  const hashChanged = new Notifier(location.hash);
  window.addEventListener("hashchange", () => hashChanged.value = location.hash);
  return bind(hashChanged, fullData => {
    if (fullData === "") { location.href = "index.html"; return; }

    const data = fullData.substring(2);
    const type = fullData[1];

    if (type === "g") {
      const moves = fetch(`data/games/${data}.bin`).then(r => r.arrayBuffer());
      return async(e("div", { className: "loading" }, "⏳ Loading moves..."), moves.then(d => renderGame(d, data)));
    } else if (type === "i") {
      return [renderGame(decode(data)), snowflakes];
    } else {
      location.href = "index.html"; return;
    }
  });
}

// https://github.com/niklasvh/base64-arraybuffer/blob/master/lib/base64-arraybuffer.js
const chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";

// Use a lookup table to find the index.
const lookup = new Uint8Array(256);
for (let i = 0; i < chars.length; i++) {
  lookup[chars.charCodeAt(i)] = i;
}

function decode(base64) {
  let bufferLength = base64.length * 0.75,
    len = base64.length, i, p = 0,
    encoded1, encoded2, encoded3, encoded4,
    swapLeftLeft, swapLeftRight, swapRightLeft, swapRightRight;

  if (base64[base64.length - 1] === "=") {
    bufferLength--;
    if (base64[base64.length - 2] === "=") {
      bufferLength--;
    }
  }

  let arraybuffer = new ArrayBuffer(bufferLength),
    bytes = new Uint8Array(arraybuffer);

  for (i = 0; i < len; i += 4) {
    encoded1 = lookup[base64.charCodeAt(i)];
    encoded2 = lookup[base64.charCodeAt(i + 1)];
    encoded3 = lookup[base64.charCodeAt(i + 2)];
    encoded4 = lookup[base64.charCodeAt(i + 3)];

    bytes[p++] = (encoded1 << 2) | (encoded2 >> 4);
    bytes[p++] = ((encoded2 & 15) << 4) | (encoded3 >> 2);
    bytes[p++] = ((encoded3 & 3) << 6) | (encoded4 & 63);
  }

  // Reverse moves
  for (i = 3; i < 3 + (bufferLength - 3) / 2; i += 2) {
    swapLeftLeft = bytes[i];
    swapLeftRight = bytes[i + 1];
    swapRightLeft = bytes[bufferLength - i + 1];
    swapRightRight = bytes[bufferLength - i + 2];

    bytes[i] = swapRightLeft;
    bytes[i + 1] = swapRightRight;
    bytes[bufferLength - i + 1] = swapLeftLeft;
    bytes[bufferLength - i + 2] = swapLeftRight;
  }
  
  return arraybuffer;
};
// end

render(embed, document.body);