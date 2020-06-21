import * as wasm from "sliderules";

// Import Ace editor
import ace from "ace-builds/src-noconflict/ace";
import ace_theme from "ace-builds/src-noconflict/theme-tomorrow_night_eighties";

// Initialize Ace editor on #editor
window["ace"] = ace;
ace.config.set("basePath", "/node_modules/ace-builds/src-noconflict/");
let editor = ace.edit("editor", {
    mode: "ace/mode/javascript",
});
editor.setKeyboardHandler("ace/keyboard/vim");
editor.setTheme(ace_theme);

wasm.main();
