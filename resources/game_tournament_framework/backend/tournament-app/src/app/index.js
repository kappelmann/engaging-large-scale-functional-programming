import { async, bind, e } from "./e.js";
import { Notifier } from "./notifier.js";
import { tournamentStats } from "./stats.js";
import { themeControl } from "./theme.js";

const loader = e("div", { class: "loading" }, "⏳ Loading...");
const indexData = fetch("data/index.json").then(r => r.json());

export function index() {
  return [
    e("div", { class: "siteHeader" }, e("span", { class: "lozenge left" }, "🔸🔹🔸"), e("span", { class: "siteName" }, "Virus vs. Antibodies"), e("span", { class: "lozenge right" }, "🔸🔹🔸")),
    e("h2", "🏆 Ranking"),
    async(loader, indexData.then(makeRanking)),
    e("h2", "📊 Stats"),
    async(loader, indexData.then(result => tournamentStats(result, makeGameTable, makeUserTable))),
    e("h2", "🕹️ All games"),
    async(loader, indexData.then(makeAllGames)),
    lastUpdated(),
    themeControl()
  ];
}

function makeRanking(data) {
  let place = 0;
  return e("div", { class: "dataTableContainer" },
    e("table", { class: "rankingTable dataTable", cellPadding: 0, cellSpacing: 0 }, [
      e("tr", { class: "dataTableHeader" },
        e("td", { class: "userRank" }, "⚖️ Rank"),
        e("td", { class: "userName" }, "😄 Name"),
        e("td", { class: "userScore" }, "🎲 Score")
      ),
      (data.ranking.sort((rankA, rankB) => Math.sign(rankB.score - rankA.score)).reduce((accumulator, currentRank) => {
        var accLast = accumulator.length - 1;
        if (!accumulator.length || accumulator[accLast][0].score > currentRank.score)
          return [...accumulator, [currentRank]];
        else
          return [...accumulator.slice(0, -1), [...accumulator[accLast], currentRank].sort((rankA, rankB) => rankA.name.localeCompare(rankB.name))];
      }, []).map(group => {
        let n = place;
        place += group.length;
        return group.map((user, idx) =>
          e("tr", { class: (!(n % 2) ? "evenGroupUser " : "oddGroupUser ") + (["gold", "silver", "bronze", "regular"][Math.min(n, 3)]) + "User" },
            !idx ? e("td", { class: "userRank", rowSpan: group.length }, (n + 1).toString()) : [],
            e("td", { class: "userName" }, e("div", { class: "userLinkContainer" }, e("a", { href: "#user?id=" + user.userId }, user.name))),
            !idx ? e("td", { class: "userScore", rowSpan: group.length }, user.score.toString()) : []
          )
        );
      }))
    ])
  );
}

function makeAllGames(data) {
  const page = new Notifier(1);
  return [
    e("div", { class: "pageButtonContainer" },
      gamePageSelector(page, data.pages)
    ),
    e("div", bind(page, gamesPage))
  ];
}

function gamePageSelector(notifier, maxPage, maxItems = 11, ends = 2) {
  const pad = (maxItems - ends - ends - 2) / 2;
  const pages = new Array(maxPage).fill(0).map((_, n) => selectGamePage(notifier, n + 1));
  const [elisionA, elisionB] = [e("div", "..."), e("div", "...")];
  return bind(notifier, page => {
    // all buttons fit, just show them
    if (maxPage <= maxItems) return pages;
    // withing range of the start
    if (page < maxItems - ends - pad) return [pages.slice(0, maxItems - ends - 1), elisionA, pages.slice(-2)];
    // withing range of the end
    if (page > maxPage - maxItems + ends + pad + 1) return [pages.slice(0, 2), elisionA, pages.slice(- maxItems + ends + 1)];
    // somewhere in the middle
    return [pages.slice(0, 2), elisionA, pages.slice(page - pad, page + pad), elisionB, pages.slice(-2)];
  });
}

/**
 * 
 * @param {Notifier} notifier 
 * @param {number} n 
 */
function selectGamePage(notifier, n) {
  return e("button",
    {
      class: ["pageButton", notifier.value == n ? "active" : ""],
      onclick: () => { notifier.value = n; }
    },
    e => void notifier.subscribe((v, p) => { if (v == n) e.classList.add("active"); else if (p == n) e.classList.remove("active") }),
    String(n)
  )
}

function gamesPage(page) {
  return e("div", { class: "dataTableContainer" },
    async(loader, fetch(`data/games-pages/${page}.json`).then(r => r.json()).then(r => makeWinnerGameTable(r)))
  );
}

function makeWinnerGameTable(games) {
  return makeGameTable(games,
    [e("td", { class: "gameWinnerHeader" }, "🏅 Winner")],
    [game => e("td", { class: "gameWinner" }, e("a", { href: "#user?id=" + game.gameId.split("_")[[1, 0, 0][game.winner + 1]] }, game.winner == 0 ? "Undecided" : game.players[[1, 0, 0][game.winner + 1]]))]);
}

function makeGameTable(games, extraHeaders = [], extraDataFuncs = []) {
  return e("table", { class: "gamesTable dataTable", cellPadding: 0, cellSpacing: 0 }, [
    e("tr", { class: "dataTableHeader" }, [e("td", { class: "gamePlayersHeader" }, "🎮 Players"), extraHeaders, e("td", { class: "gameReplayHeader" }, "📽️ Replay")]),
    games.map(game => {
      if (game.players.length > 0 && typeof game.players[0].name !== "undefined")
        game.players = game.players.map(p => p.name);
      return e("tr", { class: "gameTableEntry" },
        e("td", { class: "gamePlayersContainer" },
          e("div", { class: "gamePlayers" },
            e("div", { class: "gamesTablePlayer playerA " + (game.winner == 1 ? "winner" : (game.winner == 0 ? "tiePlayer" : "loser")), onclick() { window.location.href = "#user?id=" + game.gameId.split("_")[0] } }, game.players[0]),
            e("div", { class: "vsLabel" }, "vs."),
            e("div", { class: "gamesTablePlayer playerB " + (game.winner == -1 ? "winner" : (game.winner == 0 ? "tiePlayer" : "loser")), onclick() { window.location.href = "#user?id=" + game.gameId.split("_")[1] } }, game.players[1])
          )
        ),
        extraF(extraDataFuncs, game),
        e("td", { class: "gameReplay nonHeader", onclick() { window.location.href = "#game?id=" + game.gameId } }, e("a", { href: "#game?id=" + game.gameId }, "▶️"))
      );
    })
  ]);
}

function makeUserTable(users, extraHeaders = [], extraDataFuncs = []) {
  return e("table", { class: "tournamentStatsUserTable dataTable", cellPadding: 0, cellSpacing: 0 }, [
    e("tr", { class: "dataTableHeader" }, e("td", { class: "tournamentStatsUserHeader" }, "🎮 Player"), extraHeaders),
    users.map(user => {
      return e("tr", { class: "tournamentStatsUserTableEntry" },
        e("td", { class: "tournamentStatsUserContainer" },
          e("a", { href: "#user?id=" + user.userId }, user.name || user.userName || "")
        ),
        extraF(extraDataFuncs, user)
      );
    })
  ]);
}

function extraF(extraDataFuncs = [], ...args) {
  return extraDataFuncs.map(f => f(...args));
}

function makeUpdate(data) {
  return new Date(data.date).toLocaleString();
}

export function lastUpdated() {
  return e("div", { id: "lastUpdated" }, "Last updated: ", async(loader, indexData.then(makeUpdate)))
}
