import { e } from "./e.js";

export function initTheme() {
    const currentTheme = getCurrentTheme();
    const body = document.querySelector('body');
    body.classList.remove("auto-theme");
    body.classList.add(currentTheme + "-theme");
}

export function themeControl() {
    return e(
        "div",
        { id: "switchTheme" },
        "Theme: ",
        e(
            "span",
            { id: "currentTheme" }
        ),
        e(
            "button",
            {
                id: "switchThemeButton",
                className: "pageButton",
                onclick: () => switchTheme()
            }
        )
    )
}

function switchTheme() {
    const oldTheme = getCurrentTheme();
    const newTheme = getNextTheme(oldTheme);
    const body = document.querySelector('body');
    localStorage.setItem("theme", newTheme);
    body.classList.remove(oldTheme + "-theme");
    body.classList.add(newTheme + "-theme");
}

function getCurrentTheme() {
    let theme = localStorage.getItem("theme");
    if (!theme) {
        theme = "auto";
        localStorage.setItem("theme", theme);
    }
    return theme;
}

function getNextTheme(currentTheme) {
    const nextThemeMap = {
        "auto": "dark",
        "dark": "light",
        "light": "auto"
    };
    return nextThemeMap[currentTheme];
}
