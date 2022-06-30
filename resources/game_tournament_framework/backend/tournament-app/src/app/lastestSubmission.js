import { async, e } from "./e";

const latestSubmissions = fetch("data/latest.json").then(v => v.json());

export function linkLatest(name, id) {
  return async([], latestSubmissions.then(data => {
    const latestId = data[name];
    if (latestId === undefined || latestId == id) return [];
    return e("div", { class: "userLatest" }, e("a", { href: `#user?id=${latestId}` }, "View the latest submission of this student"));
  }).catch(ex => []));
}