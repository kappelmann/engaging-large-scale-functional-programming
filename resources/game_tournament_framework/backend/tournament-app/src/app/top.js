import { e } from "./e.js";
import { lastUpdated } from "./index.js";

export function topPanel() {
    return e("div", { className: "topPanel" }, e("a", { href: "#" }, "🏠"), lastUpdated());
}