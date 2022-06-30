import { async, e } from "./e.js";
import { renderGame } from "./renderer.js";
import { gameStats } from "./stats.js";
import { topPanel } from "./top.js";
import { themeControl } from "./theme.js";

export function game(data) {
  const info = fetch(`data/games/${data.id}.json`).then(r => r.json());
  const moves = fetch(`data/games/${data.id}.bin`).then(r => r.arrayBuffer());
  return async(e("div", { className: "loading" }, "⏳ Loading game " + data.id + "..."),
    info.then(game => [
      topPanel(),
        e("h1", "🕹️ ", game.players.flatMap(p => [" vs. ", e("a", { href: "#user?id=" + p.userId }, p.name)]).slice(1)),
        e("div", { className: "gameWinnerData" + (game.winner == 0 ? " undecided" : "") }, ...(game.winner == 0 ? ["⚖️ Undecided"] : [e("span", { className: "gameWinnerLabel" }, "🏅 Winner: "), e("div", { className: "playerColorIndicator " + (game.winner == 1 ? "red" : "green") }), (winnerData => e("a", { href: "#user?id=" + winnerData.userId }, winnerData.name))(game.players[[1, 0, 0][game.winner + 1]])])),
        async(e("div", { className: "loading" }, "⏳ Loading moves..."), moves.then(renderGame)),
        showReason(game.reason),
        e("h2", { className: "gameSubHeader" }, "📊 Stats"),
        gameStats(game),
        e("h2", { className: "gameSubHeader" }, "🔗 Embed"),
        e("div", { className: "animGenOuterContainer" },
          e("div", { className: "animGenLinkContainer" }, "🧩", e("a", { className: "animGenLinkText", href: `embed.html#g${data.id}` }, "Embeddable version")),
          e("span", { className: "animGenLinkSeparator" }, "|"),
          e("div", { className: "animGenLinkContainer" }, "📽️", e("a", { className: "animGenLinkText", href: `data/animations/${data.id}.mp4` }, "MP4 animation"))
        ),
        themeControl(),
        e("style", { type: "text/css" }, "#lastUpdated { margin-top: 20px !important; }")
      ])
      .catch(ex => console.log(ex), e("div", "Unable to load game " + data.id))
  );
}

function showReason(reason) {
  if (reason === "Won") return [];
  const [msg, ...data] = reason.split("\n");
  let explain = [];

  switch (msg) {
    case "Loop":
      explain = ["a non-winning loop during game evaluation.", e("br"), "THIS SHOULD HAVE BEEN IMPOSSIBLE." ];
      break;
    case "OutOfBoundsMove":
      explain = ["an attempt to place an orb at ", data, ", which is out-of-bounds"];
      break;
    case "InvalidMove":
      explain = ["an attempt to place an orb on top of the enemy at ", data];
      break;
    case "Timeout":
      explain = "a timeout while evaluating a strategy";
      break;
    case "Exception":
      explain = ["an exception while evaluating a strategy", e("pre", data.join('\n'))];
      break;
  }

  return e("div", { class: "game-abnormal-end" }, "The game was terminated abnormally because of ", explain);
}