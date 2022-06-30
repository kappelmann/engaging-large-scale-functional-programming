import { bind } from "./e.js";
import { Notifier } from "./notifier.js";

const cached = new Map();

function buildData(template) {
  if (cached.has(template)) return cached.get(template);

  const groups = {};
  let idx = 1;
  let name = "";
  let inName = false;
  for (let i = 0; i < template.length; i++) {
    const c = template[i];

    if (c == "(") idx++;
    else if (c == "{") {
      name = "";
      inName = true;
    } else if (c == "}" || c == ",") {
      groups[name] = idx++;
      inName = false;
    }
    if (inName) name += c;
  }
  const regex = new RegExp("^" + template.replace(/\//g, "\\/").replace(/{([a-zA-Z0-9\-]+),([^}]*?)}/, "($2)").replace(/{([a-zA-Z0-9\-]+)}/, "([a-z0-9\-]+)") + "$", "i")
  const data = { regex, groups };
  cached.set(template, data);
  return data;
}

export function matchPath(template, value) {
  const data = buildData(template);
  const result = data.regex.exec(value);
  if (result === null) return false;
  const obj = {};
  for (const key in data.groups) {
    obj[key] = result[data.groups[key]];
  }
  return obj;
}


export function route(pages, fail) {
  let n = 0;
  const notifier = new Notifier(n++);
  window.addEventListener("hashchange", () => notifier.value = n++);
  return bind(notifier, routeContent.bind(undefined, pages, fail));
}

function testMatch(match, url = location.hash) {
  if (url.length > 0 && url[0] == "#") url = url.substring(1);
  const [a, b] = url.split("?", 2);
  const params = Object.fromEntries([... new URLSearchParams(b)]);
  if (typeof match === "string") {
    return addParams(params, matchPath(match, a));
  }
  return addParams(params, match(a));
}

function addParams(a, b) {
  if (b === false) return false;
  return { ...a, ...b };
}

function routeContent(pages, fail) {
  for (const page of pages) {
    const matched = testMatch(page.match);
    if (matched !== false) {
      return page.content(matched);
    }
  }
  return fail;
}