import { e, render } from "./e.js";
import { initTheme } from "./theme.js";
import { game } from "./game.js";
import { index } from "./index.js";
import { route } from "./matcher.js";
import { user } from "./user.js";
import { genAnimation } from "./gen-animation.js";
import { snowflakes } from "./snow.js";

import "../styles/app.css";

const root = [
  e("div", { id: "outerContainer" }, e("div", { id: "innerContainer" },
    route([
      {
        match: "",
        content: index
      },
      {
        match: "animation",
        content: genAnimation
      },
      {
        match: "user",
        content: user
      },
      {
        match: "game",
        content: game
      }
    ], index),
    snowflakes()
  ))
];

render(root, document.body);
initTheme();
