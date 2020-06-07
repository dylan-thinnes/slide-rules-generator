import { XSCALE, YSCALE } from "./Globals"

import opentype from "opentype.js"
import makerjs from "makerjs"

import * as Tick from "./Tick"
import Scale from "./Scale"

let c = new Scale("C", Math.log10, {
    start: 1,
    end: 10,
    group: Tick.group(
        9, // Number of values
        {  // Height + show
            height: 1,
            show: x => (x == 10 ? 1 : x).toString(),
        },
        // Subgroups
        Tick.group(
            2, { height: 0.75 },
            Tick.group(
                5, { height: 0.5 }
            )
        )
        , true, true
    )
});


opentype.load("./fonts/OpenSans-Light.ttf", (err, font) => {

Tick.Tick.font = font;

let model = c.toModel();
window["model"] = model;
let svg = makerjs.exporter.toSVG(model);
// makerjs.exporter.toDXF(model);

document.body.innerHTML = svg;
document.body.children[0].setAttribute("width", "800");
document.body.children[0].setAttribute("height", "400");

console.log(Tick.unfold(c.spec, 0));

});
