import opentype from "opentype.js";
import makerjs from "makerjs";
import numeral from "numeral";
const { Line } = makerjs.paths;
import { saveAs } from "file-saver";

window["makerjs"] = makerjs;
window["saveAs"] = saveAs;

const XSCALE : number = 40;
const YSCALE : number = 1;

function id<T>(x: T) : T {
    return x;
}

numeral.zeroFormat("0")
function nf (format: string) {
    return x => numeral(x).format(format);
}

function string (x: number) : string {
    return x.toString();
}

function log10 (x: number) : number {
    return Math.log(x) / Math.log(10);
}

class TickGroup {
    start: number;
    end: number;
    intervals: TickSpec[];
    show?: (x: number) => string;

    constructor (args: { start: number, end: number, intervals: TickSpec[], show?: (x: number) => string }) {
        this.start = args.start;
        this.end = args.end;
        this.intervals = args.intervals;
        this.show = args.show;
    }

    toTicks () : Tick[] {
        let scale = this.end - this.start;
        let ticks : Tick[] = [];

        for (let ii = 0; ii < this.intervals.length; ii++) {
            let { count, show, height } = this.intervals[ii];
            scale /= count;

            let newTicks : Tick[][];
            if (ii == 0) {
                let start = this.start;
                newTicks = [];
                for (let ii = 0; ii < count; ii++) {
                    newTicks.push([new Tick(start, show, height)]);
                    start += scale;
                }
            } else {
                newTicks = ticks.map(tick => {
                    let start = tick.value;
                    let xs : Tick[] = [];
                    for (let ii = 0; ii < count - 1; ii++) {
                        start += scale;
                        xs.push(new Tick(start, show, height));
                    }
                    return xs;
                });
            }

            ticks = ticks.concat(...newTicks);
        }

        return ticks;
    }

    toModel (precompute: (x: number) => number, scale: number = 1): makerjs.IModel {
        let result = {
            models: {}
        }

        let ticks = this.toTicks()
        for (let tickId in ticks) {
            let tick = ticks[tickId];
            let model = tick.toModel(precompute, scale);
            result.models[tickId.toString()] = model;
        }

        return result;
    }
}

class TickSpec {
    count: number;
    height: number;
    show?: (x: number) => string;

    constructor (args: { count: number, height: number, show?: (x: number) => string }) {
        this.count = args.count;
        this.height = args.height;
        this.show = args.show;
    }
}

class Tick {
    static font: any;
    readonly value: number;
    readonly height: number;
    readonly show?: (x: number) => string;

    constructor (value: number, show?: (x: number) => string, height: number = 1) {
        this.value = value;
        this.height = height;
        this.show = show;
    }

    toModel (precompute: (x: number) => number, scale: number = 1): makerjs.IModel {
        let x = precompute(this.value);
        let h = this.height * scale;

        let model : makerjs.IModel = {
            origin: [x*XSCALE,h*YSCALE],
            paths: {
                tick: new Line([0,0],[0,-h*YSCALE]),
            },
            models: {
            }
        }

        if (this.show) {
            let rawText = this.show(this.value);

            let number = new makerjs.models.Text(Tick.font, rawText, 0.5*YSCALE*Math.abs(this.height));
            makerjs.model.center(number, true, true);
            makerjs.model.moveRelative(number, [0, YSCALE * (h > 0 ? 0.3 : -0.3)]);
            model.models.number = number;
        }

        return model;
    }
}

class Scale {
    readonly tickGroups: TickGroup[];
    readonly precompute: (x: number) => number;
    name: string;
    underline: boolean;
    hscale: number;

    constructor (name: string, precompute: (x: number) => number, tickGroups: TickGroup[]) {
        this.name = name;
        this.tickGroups = tickGroups;
        this.precompute = precompute;
        this.underline = true;
        this.hscale = 1;
    }

    toModel () {
        let result : makerjs.IModel = {
            models: {
                tickGroups: {
                    models: {}
                },
            }
        }

        for (let tickGroupId in this.tickGroups) {
            let tickGroup = this.tickGroups[tickGroupId];
            let model = tickGroup.toModel(this.precompute, this.hscale);
            result.models.tickGroups.models[tickGroupId.toString()] = model;
        }

        if (this.underline) {
            let extents = makerjs.measure.modelExtents(result.models.tickGroups);
            let maxX = 1*XSCALE//extents.high[0]
            let minX = 0//extents.low[0]

            result.models.baseline = {
                paths: {
                    underline: new Line([minX,0],[maxX,0]),
                }
            }
        }

        let name = new makerjs.models.Text(Tick.font, this.name, 0.8*YSCALE);
        makerjs.model.center(name, true, true);
        makerjs.model.moveRelative(name, [-1, 0.75*this.hscale]);
        result.models.name = name;
        
        return result;
    }
}

opentype.load("./fonts/OpenSans-Light.ttf", (err, font) => {

Tick.font = font;
let scale1 = new Scale("C", log10, [
    new TickGroup({
        start: 1,
        end: 2,
        intervals: [
            new TickSpec({ count: 1, height: 1, show: nf("0") }),
            new TickSpec({ count: 2, height: 0.75, show: nf("0.0") }),
            new TickSpec({ count: 5, height: 0.5 }),
            new TickSpec({ count: 5, height: 0.25 }),
        ],
    }),
    new TickGroup({
        start: 2,
        end: 6,
        intervals: [
            new TickSpec({ count: 4, height: 1, show: nf("0") }),
            new TickSpec({ count: 2, height: 0.75 }),
            new TickSpec({ count: 5, height: 0.5 }),
            new TickSpec({ count: 2, height: 0.25 }),
        ],
    }),
    new TickGroup({
        start: 6,
        end: 10,
        intervals: [
            new TickSpec({ count: 4, height: 1, show: nf("0") }),
            new TickSpec({ count: 2, height: 0.75 }),
            new TickSpec({ count: 5, height: 0.5 }),
        ],
    }),
    new TickGroup({
        start: 10,
        end: 10,
        intervals: [new TickSpec({ count: 1, height: 1, show: x => Math.floor(x / 10).toString()})],
    }),
]);

let scale2 = makerjs.cloneObject(scale1);
scale2.name = "D";
scale2.underline = false;
scale2.hscale = -1;

let scale3 = new Scale("L", id, [
    new TickGroup({
        start: 0,
        end: 1,
        intervals: [
            new TickSpec({ count: 10, height: 1, show: nf(".0") }),
            new TickSpec({ count: 2, height: 0.75 }),
            new TickSpec({ count: 5, height: 0.5 }),
        ],
    }),
    new TickGroup({
        start: 1,
        end: 1,
        intervals: [new TickSpec({ count: 1, height: 1, show: _ => "1" })],
    }),
]);

let scale3_2 = new Scale("Ln", x => x / Math.log(10), [
    new TickGroup({
        start: 0,
        end: 2.3,
        intervals: [
            new TickSpec({ count: 23, height: 1, show: nf("0.0") }),
            new TickSpec({ count: 5, height: 0.5 }),
        ],
    }),
    new TickGroup({
        start: 2.3,
        end: 2.3,
        intervals: [
            new TickSpec({ count: 1, height: 1, show: _ => "2.3" }),
        ],
    }),
    /*
    new TickGroup({
        start: 2.32,
        end: 2.32,
        intervals: [
            new TickSpec({ count: 1, height: 0.5 }),
        ],
    }),
    */
]);
scale3_2.hscale = -1;

let scale4 = new Scale("CF", x => log10(x / Math.PI), [
    new TickGroup({
        start: Math.PI,
        end: Math.PI,
        intervals: [
            new TickSpec({ count: 1, height: 1, show: _ => "π" }),
        ],
    }),
    new TickGroup({
        start: 3.2,
        end: 3.5,
        intervals: [
            new TickSpec({ count: 3, height: 0.5 }),
        ],
    }),
    new TickGroup({
        start: 3.5,
        end: 4,
        intervals: [
            new TickSpec({ count: 1, height: 0.75, show: nf("0.0") }),
            new TickSpec({ count: 5, height: 0.5 }),
        ],
    }),
    new TickGroup({
        start: 4,
        end: 10,
        intervals: [
            new TickSpec({ count: 6, height: 1, show: nf("0") }),
            new TickSpec({ count: 2, height: 0.75 }),
            new TickSpec({ count: 5, height: 0.5 }),
        ],
    }),
    new TickGroup({
        start: 10,
        end: 20,
        intervals: [
            new TickSpec({ count: 1, height: 1, show: x => nf("0")(x / 10) }),
            new TickSpec({ count: 2, height: 1, show: _ => "1.5" }),
            new TickSpec({ count: 5, height: 0.75 }),
            new TickSpec({ count: 5, height: 0.5 }),
        ],
    }),
    new TickGroup({
        start: 20,
        end: 30,
        intervals: [
            new TickSpec({ count: 1, height: 1, show: x => nf("0")(x / 10) }),
            new TickSpec({ count: 2, height: 1, show: _ => "2.5" }),
            new TickSpec({ count: 5, height: 0.75 }),
            new TickSpec({ count: 2, height: 0.5 }),
        ],
    }),
    new TickGroup({
        start: 30,
        end: 32,
        intervals: [
            new TickSpec({ count: 1, height: 1, show: _ => "3" }),
            new TickSpec({ count: 2, height: 0.75 }),
        ],
    }),
    new TickGroup({
        start: 30.5,
        end: 30.5,
        intervals: [
            new TickSpec({ count: 1, height: 0.5 }),
        ],
    }),
    new TickGroup({
        start: Math.PI * 10,
        end: Math.PI * 10,
        intervals: [
            new TickSpec({ count: 1, height: 1, show: _ => "π" }),
        ],
    }),
]);

let scale5 = new Scale("A", x => log10(Math.sqrt(x)), [
    new TickGroup({
        start: 1,
        end: 2,
        intervals: [
            new TickSpec({ count: 1, height: 1, show: nf("0") }),
            new TickSpec({ count: 2, height: 0.75 }),
            new TickSpec({ count: 5, height: 0.5 }),
            new TickSpec({ count: 2, height: 0.25 }),
        ],
    }),
    new TickGroup({
        start: 2,
        end: 5,
        intervals: [
            new TickSpec({ count: 3, height: 1, show: nf("0") }),
            new TickSpec({ count: 2, height: 0.75 }),
            new TickSpec({ count: 5, height: 0.5 }),
        ],
    }),
    new TickGroup({
        start: 5,
        end: 10,
        intervals: [
            new TickSpec({ count: 5, height: 1, show: nf("0") }),
            new TickSpec({ count: 2, height: 0.75 }),
            new TickSpec({ count: 2, height: 0.5 }),
        ],
    }),
    new TickGroup({
        start: 10,
        end: 20,
        intervals: [
            new TickSpec({ count: 1, height: 1, show: x => nf("0")(x / 10) }),
            new TickSpec({ count: 2, height: 0.75 }),
            new TickSpec({ count: 5, height: 0.5 }),
            new TickSpec({ count: 2, height: 0.25 }),
        ],
    }),
    new TickGroup({
        start: 20,
        end: 50,
        intervals: [
            new TickSpec({ count: 3, height: 1, show: x => nf("0")(x / 10) }),
            new TickSpec({ count: 2, height: 0.75 }),
            new TickSpec({ count: 5, height: 0.5 }),
        ],
    }),
    new TickGroup({
        start: 50,
        end: 100,
        intervals: [
            new TickSpec({ count: 5, height: 1, show: x => nf("0")(x / 10) }),
            new TickSpec({ count: 2, height: 0.75 }),
            new TickSpec({ count: 2, height: 0.5 }),
        ],
    }),
    new TickGroup({
        start: 100,
        end: 100,
        intervals: [
            new TickSpec({ count: 1, height: 1, show: x => nf("0")(x / 10) }),
        ],
    }),
]);

let scale6 = makerjs.cloneObject(scale5);
scale6.name = "B";
scale6.underline = false;
scale6.hscale = -1;

let scale7 = new Scale("CI", x => 1 - log10(x), [
    new TickGroup({
        start: 1,
        end: 2,
        intervals: [
            new TickSpec({ count: 1, height: 1, show: nf("0") }),
            new TickSpec({ count: 2, height: 0.75, show: nf("0.0") }),
            new TickSpec({ count: 5, height: 0.5 }),
            new TickSpec({ count: 5, height: 0.25 }),
        ],
    }),
    new TickGroup({
        start: 2,
        end: 6,
        intervals: [
            new TickSpec({ count: 4, height: 1, show: nf("0") }),
            new TickSpec({ count: 2, height: 0.75 }),
            new TickSpec({ count: 5, height: 0.5 }),
            new TickSpec({ count: 2, height: 0.25 }),
        ],
    }),
    new TickGroup({
        start: 6,
        end: 10,
        intervals: [
            new TickSpec({ count: 4, height: 1, show: nf("0") }),
            new TickSpec({ count: 2, height: 0.75 }),
            new TickSpec({ count: 5, height: 0.5 }),
        ],
    }),
    new TickGroup({
        start: 10,
        end: 10,
        intervals: [
            new TickSpec({ count: 1, height: 1, show: x => "1" })
        ],
    }),
]);
scale7.hscale = -1;

let model = {
    models: {
        scale1: scale1.toModel(),
        scale2: scale2.toModel(),
        scale3: makerjs.model.move(scale3.toModel(), [0, 3.4]),
        scale3_2: makerjs.model.move(scale3_2.toModel(), [0, 3.4]),
        scale4: makerjs.model.move(scale4.toModel(), [0, 6.8]),
        scale7: makerjs.model.move(scale7.toModel(), [0, 6.8]),
        scale5: makerjs.model.move(scale5.toModel(), [0, 10.2]),
        scale6: makerjs.model.move(scale6.toModel(), [0, 10.2]),
    }
}

window["model"] = model;

let svg = makerjs.exporter.toSVG(model);
makerjs.exporter.toDXF(model);

document.body.innerHTML = svg;
document.body.children[0].setAttribute("width", "800");
document.body.children[0].setAttribute("height", "400");

});
