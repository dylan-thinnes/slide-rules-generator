import ace from "ace-builds/src-noconflict/ace";
import ace_theme from "ace-builds/src-noconflict/theme-tomorrow_night_eighties";
import opentype from "opentype.js";
import * as wasm from "sliderules";
import makerjs from "makerjs";
import { debounce } from "lodash";
const { Line } = makerjs.paths;

function qr (incl_start: boolean, incl_end: boolean) {
    return function (count: number, height: any, sub_specs?: any) {
        return {
            count,
            incl_start,
            incl_end,
            template: isNaN(height) ? height : {
                height: height,
                show: ns()
            },
            sub_specs
        }
    }
}

let qt = qr(true, true)
let qf = qr(false, false)
let qtf = qr(true, false)
let qft = qr(false, true)

window["ace"] = ace;
ace.config.set("basePath", "/node_modules/ace-builds/src-noconflict/");
let editor = ace.edit("editor", {
    mode: "ace/mode/javascript",
});
editor.setKeyboardHandler("ace/keyboard/vim");
editor.setTheme(ace_theme);
window["editor"] = editor;

let handler = () => {
    try {
        let val = eval(editor.getValue());
        render(genScales(val));
    } catch (e) {
        //console.log(e);
    }
    console.log("Rendered.")
}

let lastHandlerId = null;
let debounced = () => {
    if (lastHandlerId == null) {
        console.log("Leading...")
        handler();
        lastHandlerId = setTimeout(() => {
            lastHandlerId = null;
        }, 500);
    } else {
        console.log("Clearing...")
        clearTimeout(lastHandlerId);
        lastHandlerId = setTimeout(() => {
            handler();
            lastHandlerId = null;
        }, 500);
    }
}

editor.on("change", debounced);
debounced();

window["font"] = null;

function tickToModel (tick: any) {
    let models : any = {};
    let paths : any = {};

    paths.tick = new Line([0,0],[0,tick.height*0.05]);
    //if (typeof tick.name == "string" && tick.name.length > 0) {
    //    models.name = new makerjs.models.Text(
    //        window["font"],
    //        tick.name,
    //        0.02*Math.abs(tick.height)
    //    );
    //    makerjs.model.center(models.name, true, true);
    //    makerjs.model.moveRelative(models.name, [0,tick.height*0.05]);
    //}

    let model = { models, paths };
    makerjs.model.moveRelative(model, [tick.position,0]);
    return model;
}

window["wasm"] = wasm;
console.log("Font loading...")
opentype.load("./fonts/OpenSans-Light.ttf", (err, font) => {
console.log("Font loaded.")
window["font"] = font;
});

function genScale (json) {
    let scale = wasm.scale(json);

    console.log("Generating SVG...")
    let time = Date.now();
    let svg = wasm.scale_to_svg_js(scale);
    console.log(Date.now() - time);
    //console.log("SVG generated.", svg);

    return svg;
}

function genScales (jsons) {
    return jsons.map(genScale).join("");
}

function render (str) {
    document.getElementById("result").innerHTML = str;
}

function tp (height, show) {
    return {
        height,
        show
    }
}

function ns (show?, preshow?) {
    return {
        show: show || "Nothing",
        preshow: preshow || "Nothing"
    }
}
