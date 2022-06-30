import { e } from "./e.js";
import "../styles/snow.css";

export function snowflake() {
  const dur1 = rnd(8, 15);
  const dur2 = rnd(2, 5);
  const delay1 = rnd(0, dur1);
  const delay2 = - dur2 + rnd(0, dur2);
  return e("div",
    {
      class: "snowflake",
      style: {
        left: rnd(1, 100) + "%",
        animationDuration: `${dur1}s, ${dur2}s`,
        animationDelay: `${delay1}s, ${delay2}s`
      }
    },
    el => void setTimeout(() => { el.parentNode.appendChild(snowflake()); el.remove(); }, (dur1 + delay1) * 1000),
    "❅"
  );
}

function rnd(min, max, asInt = false) {
  const v = Math.random() * (max - min) + min;
  if (asInt) return Math.floor(v);
  return v;
}

export function snowflakes() {
  const heavySnow = Math.random() < 0.1;
  return e("div", { class: "snowflakes" }, Array.from({ length: heavySnow ? rnd(100, 200, true) : rnd(10, 20, true) }, () => snowflake()));
}