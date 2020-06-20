import { XSCALE, YSCALE } from "./Globals"

import opentype from "opentype.js"
import numeral from "numeral"
import { saveAs } from "file-saver";
import _ from "lodash"
import makerjs from "makerjs"

import * as Tick from "./Tick"
import Scale from "./Scale"

let clampWith10 = x => x < 10 ? x : clampWith10(x / 10);

let ag1 = Tick.group(
    9, // Number of values
    {  // Height + show
        height: 1,
        show: x => clampWith10(x).toString(),
    },
    // Subgroups
    {
        0: Tick.group(
            2, { height: 0.75 },
            Tick.group(
                5, { height: 0.5 },
            )
        ),
        4: Tick.group(
            2, { height: 0.75 },
            Tick.group(
                2, { height: 0.5 },
            )
        ),
    },
    true, true
);

let ag2 = _.cloneDeep(ag1);
ag2.includeInitial = false;

let a = new Scale("A", x => Math.log10(x) / 2, {
    subs: [
        {
            start: 1,
            end: 10,
            group: ag1,
        },
        {
            start: 10,
            end: 100,
            group: ag2,
        }
    ]
});

let b = _.cloneDeep(a);
b.name = "B";
b.hscale = -1;

let c = new Scale("C", Math.log10, {
    start: 1,
    end: 10,
    group: Tick.group(
        9, // Number of values
        {  // Height + show
            height: 1,
            show: x => clampWith10(x).toString(),
        },
        // Subgroups
        {
            0: Tick.group(
                2, { height: 0.75, show: x => numeral(x).format("0.0") },
                Tick.group(
                    5, { height: 0.5 },
                    Tick.group(
                        5, { height: 0.25 }
                    )
                )
            ),
            1: Tick.group(
                2, { height: 0.75 },
                Tick.group(
                    5, { height: 0.5 },
                    Tick.group(
                        2, { height: 0.25 }
                    )
                )
            ),
            5: Tick.group(
                2, { height: 0.75 },
                Tick.group(
                    5, { height: 0.5 },
                )
            )
        },
        true, true
    )
});

let d = _.cloneDeep(c);
d.name = "D";
d.hscale = -1;

let ln = new Scale("Ln", x => x / 10 / Math.log(10), {
    start: 0,
    end: 23,
    group: Tick.group(
        23, { height: 1, show: x => numeral(x / 10).format("0.0") },
        Tick.group(
            2, { height: 0.75 },
            Tick.group(
                5, { height: 0.5 }
            )
        ),
        true, true
    ),
});
ln.hscale = -0.5;

let l = new Scale("L", x => x / 10, {
    start: 0,
    end: 10,
    group: Tick.group(
        10, { height: 1, show: x => numeral(x / 10).format("0.0") },
        Tick.group(
            2, { height: 0.75 },
            Tick.group(
                5, { height: 0.5 }
            )
        ),
        true, true
    ),
});
l.hscale = 0.5;

opentype.load("./fonts/OpenSans-Light.ttf", (err, font) => {
    console.log("Font loaded.");
    Tick.Tick.font = font;
    console.log("Generating model...");

    let model = {
        models: {
            a: makerjs.model.moveRelative(a.toModel(), [0,6]),
            b: makerjs.model.moveRelative(b.toModel(), [0,6]),
            l: makerjs.model.moveRelative(l.toModel(), [0,3]),
            ln: makerjs.model.moveRelative(ln.toModel(), [0,3]),
            c: makerjs.model.moveRelative(c.toModel(), [0,0]),
            d: d.toModel(),
        }
    }
    window["model"] = model;
    console.log("Exporting to SVG...");
    let svg = makerjs.exporter.toSVG(model);

    console.log("Inserting into document...");
    document.body.innerHTML = svg;
    document.body.children[0].setAttribute("width", "800");
    document.body.children[0].setAttribute("height", "400");

    window["exportToDXF"] = () => {
        let dxf = makerjs.exporter.toDXF(model);
        let file = new File([dxf], "slide-rule.dxf", { type: "text/plain;charset=utf-8" });
        saveAs(file);
    }
});
