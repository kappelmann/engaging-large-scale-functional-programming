import { e } from "./e.js";

export function genAnimation() {
  window.setTimeout(() => {
    let url = window.location.hash;
    if (url.length > 0 && url[0] == "#") url = url.substring(1);
    const [a, b] = url.split("?", 2);
    const params = Object.fromEntries([... new URLSearchParams(b)]);
    if("game" in params)
      window.location.href = "data/animations/" + params["game"] + ".mp4";
    else
      window.location.href = "";
  }, 5000);
  return [
      e("h1", "Generating animation..."),
      e("div", { className: "largeUnicodeImage" }, "📽️"),
      e("div", "We have started generating an animation for this game, which may take some time. This page will refresh automatically until the animation has been generated.")
  ];
}
