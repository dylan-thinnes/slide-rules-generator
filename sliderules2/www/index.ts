// Import app code
import * as wasm from "sliderules";
import App from "./Main.tsx";

// Import renderer
import { render } from 'preact';
render(App, document.getElementById("renderer"))

/*
// Import Ace editor
import ace from "ace-builds/src-noconflict/ace";

// Initialize Ace editor on #editor
window["ace"] = ace;
ace.config.set("basePath", "/node_modules/ace-builds/src-noconflict/");
let editor = ace.edit("editor", {
    mode: "ace/mode/javascript",
});
editor.setKeyboardHandler("ace/keyboard/vim");
editor.setTheme("ace/theme/tomorrow_night_eighties");
*/

wasm.main();
