import { async, e } from "./e.js";
import { userStats } from "./stats.js";
import { topPanel } from "./top.js";
import { themeControl } from "./theme.js";
import { linkLatest } from "./lastestSubmission.js";

export function user(data) {
  const info = fetch(`data/users/${data.id}.json`).then(r => r.json());
  return async(e("div", { className: "loading" }, "⏳ Loading user " + data.id + "..."),
    info
      .then(user => [
        topPanel(),
        e("h1", "🕹️ ", user.name),
        e("div", {class: "userCommit"}, "commit: ", user.commit),
        linkLatest(user.name, data.id),
        user.games.map((games, i) =>
          [
            e("h2", (i == 0 ? "1️⃣" : "2️⃣") + " Games as player " + (i + 1)),
            e("div", { className: "dataTableContainer" }, e("table", { className: "userGamesTable dataTable", cellPadding: 0, cellSpacing: 0 }, [
              e("tr", { className: "dataTableHeader" }, e("td", { className: "userGameOpponent" }, "🎮 Opponent"), e("td", { className: "userGameWinner" }, "⚖️ Result"), e("td", { className: "userGameReplay" }, "📽️ Replay")),
              ...games.map(game => e("tr", 
                e("td", { className: "userGameOpponent" },
                  e("span", { className: "opponent" }, game.enemy)
                ),
                e("td", { className: "userGameResult " + (game.result == 1 ? "won" : "lost") }, game.result == 1 ? "Won" : "Lost"),
                e("td", { className: "userGameReplay nonHeader" }, e("a", { href: "#game?id=" + game.gameId }, "▶️"))
              ))
            ]))
          ]
        ),
        e("h2", "📊 Stats"),
        userStats(user),
        themeControl()
      ])
      .catch(ex => e("div", "Unable to load user " + data.id))
  );
}