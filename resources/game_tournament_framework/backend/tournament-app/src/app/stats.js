import { e } from "./e.js";

const decStatPrecision = 4;
const pctStatPrecision = 2;

// variance converted to std
const numStats = [["statAvg", "Ø", "Avg.", "Average"], ["statMin", "⊥", "Min.", "Minimum"], ["statMax", "⊤", "Max.", "Maximum"], ["statSum", "Σ", "Sum", "Sum"], ["statVariance", "σ", "Std. dev.", "Standard deviation"]];

const fl = i => showAsInf(i) ? "∞" : i.toFixed(decStatPrecision);
const fs = i => showAsInf(i) ? "∞" : Math.sqrt(i).toFixed(decStatPrecision); // variance converted to std
const pc = i => (showAsInf(i) ? "∞" : (i * 100.0).toFixed(pctStatPrecision)) + "%";

function showAsInf(i) {
  return typeof i === "undefined" || i == null || isNaN(i) || i > 1e+30;
}

function numericStatTable(stat, props) {
  let hasProps = typeof props !== "undefined";
  let horizontal = hasProps && typeof props.horizontal !== "undefined" ? props.horizontal : true;
  let defaultFormatFunc = hasProps && typeof props.defaultFormatFunc !== "undefined" ? props.defaultFormatFunc : (i => showAsInf(i) ? "∞" : i.toFixed(decStatPrecision));
  let customFormatFuncs = hasProps && typeof props.customFormatFuncs !== "undefined" ? props.customFormatFuncs : {};

  let findFormatFunc = stName => typeof customFormatFuncs[stName] !== "undefined" ? customFormatFuncs[stName] : defaultFormatFunc;
  let format = stName => (findFormatFunc(stName))(stat[stName]);

  let filtered = numStats.filter(st => typeof stat[st[0]] !== "undefined" && findFormatFunc(st[0]) != null)
  let nameTds = filtered.map(st => e("td", { className: "numericStatSubvalueHeader", title: st[3] }, st[1]));
  let subvalueTds = filtered.map(st => e("td", { className: "numericStatSubvalueValue", title: format(st[0]) }, format(st[0])));

  return e("table", { className: "numericStatTable numericStat"+ (horizontal ? "H" : "V") +"Table" }, horizontal ? [
    e("tr", { className: "numericStatTableHeaderRow" }, ...nameTds),
    e("tr", { className: "numericStatTableSubvalueRow" }, ...subvalueTds),
  ] :
  // Vertical 
    nameTds.map((td, idx) => e("tr", { className: "numericStatTableJointRow" }, [td, subvalueTds[idx]]))
  );
}

export function gameStats(game) {
  if(!game.stats)
    return e("div", "No stats available for this game");
  let stats = game.stats;
  let statList = [];
  let statOrder = ["movesMade", "orbsCaptured", "orbsLost", "drvKillDeathRatio", "drvAvgBoardControl", "drvMaxBoardControl", "drvCloseCalls", "chainReactionsStarted", "explosionsTriggered", "drvExplosions", "drvAvgExplosions", "drvAvgTargetCellFilling", "drvAvgCapturesPerMove", "drvBoardControl", "drvTargetCellFilling", "drvMoveDurations"];
  statOrder.forEach(stat => {
    let statValues = [];
    let rn = "";
    let numStat = false;
    let numStatProps = undefined;
    let ff = i => i.toString();
    let valPreprocessor = v => v;

    switch(stat) {
      case "movesMade":
        rn = "Moves made";
        break;
      case "orbsCaptured":
        rn = "Orbs captured";
        break;
      case "drvKillDeathRatio":
        rn = "Capture/loss ratio";
        ff = fl;
        break;
      case "drvAvgCapturesPerMove":
        // Turn this into a numStat for nicer display
        rn = "Captures per move";
        numStat = true;
        numStatProps = { customFormatFuncs: { statAvg: pc, statMin: null, statMax: pc, statSum: null, statVariance: null } };
        valPreprocessor = v => ({ statAvg: v });
        break;
      case "drvBoardControl":
        rn = "Board control";
        numStat = true;
        numStatProps = { customFormatFuncs: { statAvg: pc, statMin: null, statMax: pc, statSum: null, statVariance: null } };
        break;
      case "drvAvgBoardControl":
        rn = "Avg. board control";
        ff = pc;
        break;
      case "drvMaxBoardControl":
        rn = "Max. board control";
        ff = pc;
        break;
      case "chainReactionsStarted":
        rn = "Chain reactions started";
        break;
      case "explosionsTriggered":
        rn = "Explosions triggered";
        break;
      case "drvExplosions":
        rn = "Explosions per move";
        numStat = true;
        numStatProps = { customFormatFuncs: { statAvg: pc, statMin: null, statMax: pc, statSum: null, statVariance: null } };
        break;
      case "drvAvgExplosions":
        // Turn this into a numStat for nicer display
        rn = "Explosions per move";
        numStat = true;
        numStatProps = { customFormatFuncs: { statAvg: pc, statMin: null, statMax: pc, statSum: null, statVariance: null } };
        valPreprocessor = v => ({ statAvg: v });
        break;
      case "drvTargetCellFilling":
        rn = "Orbs in targeted cells";
        numStat = true;
        numStatProps = { customFormatFuncs: { statAvg: fl, statMin: null, statMax: null, statSum: null, statVariance: fs } };
        break;
      case "drvAvgTargetCellFilling":
        rn = "Avg. orbs in targeted cells";
        ff = fl;
        break;
      case "drvMoveDurations":
        rn = "Move durations [ms]";
        numStat = true;
        numStatProps = { customFormatFuncs: { statMin: ff, statMax: ff, statSum: ff, statVariance: fs } };
        break;
      case "drvCloseCalls":
        rn = "Close calls";
        break;
      default:
        return;
    }

    stats.playerStats.forEach(ps => statValues.push(Object.keys(ps).indexOf(stat) > -1 ? (numStat ? {values: valPreprocessor(ps[stat]), props: valPreprocessor(numStatProps)} : ff(ps[stat])) : ""));

    if(statValues.filter(v => !!v).length > 0)
      statList.push({ key: stat, readableName: rn, values: statValues, numericStat: numStat });
  });

  return e("div", { className: "gameStatsContainer dataTableContainer" },
    e("table", { className: "gameStatsTable dataTable", cellPadding: 0, cellSpacing: 0 }, [
      e("tr", { className: "dataTableHeader" }, e("td", { className: "gameStatsStatistic" }, "📈 Statistic"), game.players.map((player, idx) => e("td", { className: "gameStatsPlayerNameContainer" }, e("div", { className: "playerColorIndicator " + (idx == 0 ? "red" : "green") }), e("span", { className: "gameStatsPlayerName" }, player.name)))),
      ...statList.map(stat => e("tr", [
        e("td", { className: "gameStatsStatReadableName" }, stat.readableName),
        ...stat.values.map(val => e("td", { className: "gameStatsStatValue" + (stat.numericStat ? " numericStat" : "") }, (stat.numericStat ? numericStatTable(val.values, { horizontal: false, ...val.props }) : e("span", val || "-"))))
      ]))
    ])
  )
}

export function userStats(user) {
  // NOTE: we do not use numericStatTable here, to keep the width of TDs equal across rows

  if(!user.stats)
    return e("div", "No stats available for this user");
  
    let stats = user.stats;
    let statKeys = Object.keys(stats);
    let statList = [];
    let statOrder = ["gamesWon", "gamesLost", "efficiency", "movesMade", "gameFrames", "orbsCaptured", "orbsLost", "boardControl", "killDeathRatio", "targetCellFilling", "moveDurations"];
    
    let ff = i => i.toString();
    let sv = i => ({ statValue: ff(i) });
    let sp = i => ({ statValue: pc(i) });
    let nsNoSum =   i => ({ statAvg: fl(i.statAvg), statMin: fl(i.statMin), statMax: fl(i.statMax), statVariance: fs(i.statVariance) });
    let niNoSum =   i => ({ statAvg: fl(i.statAvg), statMin: ff(i.statMin), statMax: ff(i.statMax), statVariance: fs(i.statVariance) });
    let npNoSum =   i => ({ statAvg: pc(i.statAvg), statMin: pc(i.statMin), statMax: pc(i.statMax), statVariance: fs(i.statVariance) });
    let nsWithSum = i => Object.assign({ statSum: fl(i.statSum) }, nsNoSum(i));
    let niWithSum = i => Object.assign({ statSum: ff(i.statSum) }, niNoSum(i));
    let npWithSum = i => Object.assign({ statSum: pc(i.statSum) }, npNoSum(i));

    statOrder.forEach(stat => {
      let rn = "";
      let numStat = false;
      let includeSum = false;
      let statData = stats[stat];
      let pp = {};
      let hasSupOne = false;
      let hasSupTwo = false;

      switch(stat) {
        case "gamesWon":
          rn = "Games won";
          pp = sv(statData);
          break;
        case "gamesLost":
          rn = "Games lost";
          pp = sv(statData);
          break;
        case "efficiency":
          rn = "Efficiency";
          pp = sp(statData);
          hasSupOne = true;
          break;
        case "movesMade":
          rn = "Moves made";
          pp = niWithSum(statData);
          numStat = true;
          break;
        case "gameFrames":
          rn = "Game frames";
          pp = niWithSum(statData);
          numStat = true;
          break;
        case "orbsCaptured":
          rn = "Orbs captured";
          pp = niWithSum(statData);
          numStat = true;
          break;
        case "orbsLost":
          rn = "Orbs lost";
          pp = niWithSum(statData);
          numStat = true;
          break;
        case "boardControl":
          rn = "Board control";
          pp = npNoSum(statData);
          numStat = true;
          hasSupTwo = true;
          break;
        case "killDeathRatio":
          rn = "Capture/loss ratio";
          pp = nsNoSum(statData);
          numStat = true;
          hasSupTwo = true;
          break;
        case "targetCellFilling":
          rn = "Orbs in targeted cells";
          pp = nsNoSum(statData);
          numStat = true;
          hasSupTwo = true;
          break;
        case "moveDurations":
          rn = "Move durations [ms]";
          pp = nsNoSum(statData);
          numStat = true;
          hasSupTwo = true;
          break;
        default:
          return;
      }
      if(statKeys.indexOf(stat) > -1)
        statList.push(Object.assign({ key: stat, readableName: rn, numStat: numStat, includeSum: includeSum, hasSupOne: hasSupOne, hasSupTwo: hasSupTwo }, pp));
    });
  
    // Two versions: one for desktop, one for mobile

    return e("div", { className: "userStatsResponsiveToggle" }, 
    e("div", { className: "userStatsContainer dataTableContainer nonMobile" },
      e("table", { className: "userStatsTable dataTable", cellPadding: 0, cellSpacing: 0 }, [
        e("tr", { className: "dataTableHeader" }, e("td", { className: "userStatsStatistic userStatsHighlighted" }, "📈 Statistic"), e("td", { className: "userStatsValue userStatsHighlighted", colSpan: 1000 }, "🎲 Values")),
        ...([].concat.apply([], statList.map(stat => [e("tr", [
          e("td", { className: "userStatsStatReadableName" + (stat.hasSupOne ? " hasSupOne" : "") + (stat.hasSupTwo ? " hasSupTwo" : ""), rowSpan: stat.numStat ? 2 : 1 }, stat.readableName),
          ...(stat.numStat ? numStats.filter(st => typeof stat[st[0]] !== "undefined").map((st, idx, arr) => e("td", { className: "userStatsStatValueHeader userStatsHighlighted", title: st[3], colSpan: idx == arr.length-1 ? 1000 : 1 }, st[1])) : [e("td", { className: "gameStatsStatValue", colSpan: 5 }, stat.statValue)])
        ]),
        ...(stat.numStat ? [e("tr", numStats.filter(st => typeof stat[st[0]] !== "undefined").map((st, idx, arr) => e("td", { className: "userStatsStatValue", colSpan: idx == arr.length-1 ? 1000 : 1 }, stat[st[0]])))] : [])])))
      ])
    ),
    e("div", { className: "userStatsContainer dataTableContainer mobile" },
      e("table", { className: "userStatsTable dataTable", cellPadding: 0, cellSpacing: 0 }, [
        e("tr", { className: "dataTableHeader" }, e("td", { className: "userStatsStatistic userStatsHighlighted" }, "📈 Statistic"), e("td", { className: "userStatsValue userStatsHighlighted", colSpan: 1000 }, "🎲 Values")),
        ...([].concat.apply([], statList.map(stat => [e("tr", [
          e("td", { className: "userStatsStatReadableName" + (stat.hasSupOne ? " hasSupOne" : "") + (stat.hasSupTwo ? " hasSupTwo" : ""), rowSpan: Math.max(numStats.filter(st => typeof stat[st[0]] !== "undefined").length, 1) }, stat.readableName),
          ...(stat.numStat ? [].concat.apply([], numStats.filter(st => typeof stat[st[0]] !== "undefined").filter((_, idx) => idx == 0).map(st => [e("td", { className: "userStatsStatValueHeader userStatsHighlighted", title: st[3] }, st[1]), e("td", { className: "userStatsStatValue" }, stat[st[0]])])) : [e("td", { className: "gameStatsStatValue", colSpan: 5 }, stat.statValue)])
        ]),
        ...(stat.numStat ? numStats.filter(st => typeof stat[st[0]] !== "undefined").filter((_, idx) => idx > 0).map(st => e("tr", e("td", { className: "userStatsStatValueHeader userStatsHighlighted", title: st[3] }, st[1]), e("td", { className: "userStatsStatValue" }, stat[st[0]]))) : [])])))
      ])
    ),
    e("div", { className: "footnote first" }, "¹ efficiency: (1 - ", e("i", "avg. move duration"), " ÷ ", e("i", "timeout"), ") × (", e("i", "games won"), " ÷ ", e("i", "games played"), ")"),
    e("div", { className: "footnote second" }, "² calculated from game-wise averages of stat, without ∞ values"))
}

export function tournamentStats(tournament, makeGameTableFunc, makeUserTableFunc) {
  if(!tournament.stats)
    return e("div", "No stats available for this tournament");
  
  let stats = tournament.stats;
  return e("div", { className: "tournamentStatsContainer" }, [
    e("h3", { className: "tournamentStatsSubHeader" }, "⏱️ Longest games"),
    makeGameTableFunc(stats.longestGames, [e("td", { className: "gameLengthHeader" }, "🎞️ Frames")], [game => e("td", { className: "gameLength" }, game.stats.gameFrames.toString())]),
    
    e("h3", { className: "tournamentStatsSubHeader hasSupOne" }, "➗ Highest capture/loss ratios"),
    makeUserTableFunc(stats.highestKillDeathRatio, [e("td", "📈 Value")], [user => e("td", { className: "tournamentStatsKillDeathRatio" }, fl(user.statValue))]),
    
    e("h3", { className: "tournamentStatsSubHeader hasSupOne" }, "👑 Highest board control"),
    makeUserTableFunc(stats.highestBoardControl, [e("td", "📈 Value")], [user => e("td", { className: "tournamentStatsBoardControl" }, pc(user.statValue))]),
    
    e("h3", { className: "tournamentStatsSubHeader hasSupTwo" }, "💡 Most efficient"),
    makeUserTableFunc(stats.mostEfficient, [e("td", "📈 Value")], [user => e("td", { className: "tournamentStatsEfficiency" }, pc(user.statValue))]),

    e("div", { className: "footnote first" }, "¹ calculated from game-wise averages of stat, without ∞ values"),
    e("div", { className: "footnote second" }, "² efficiency: (1 - ", e("i", "avg. move duration"), " ÷ ", e("i", "timeout"), ") × (", e("i", "games won"), " ÷ ", e("i", "games played"), ")"),
  ])
}