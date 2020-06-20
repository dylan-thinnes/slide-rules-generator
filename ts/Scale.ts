import { XSCALE, YSCALE } from "./Globals"

import makerjs from "makerjs";

import * as Tick from "./Tick"

export default class Scale {
    readonly spec: Tick.Spec;
    readonly precompute: (x: number) => number;
    name: string;
    underline: boolean;
    hscale: number;

    constructor (name: string, precompute: (x: number) => number, spec: Tick.Spec) {
        this.name = name;
        this.spec = spec;
        this.precompute = precompute;
        this.underline = true;
        this.hscale = 1;
    }

    toModel () {
        let result : makerjs.IModel = {
            models: {
                ticks: {
                    models: {}
                },
                numbers: {
                    models: {}
                },
            }
        }

        let ticks = Tick.unfold(this.spec, 0);
        ticks.forEach((tick, ii) => {
            let line = tick.getTickModel(this.precompute, this.hscale);
            result.models.ticks.models[ii.toString()] = line;

            let number = tick.getNumberModel(this.precompute, this.hscale);
            if (number != null) {
                result.models.numbers.models[ii.toString()] = number;
            }
        })

        if (this.underline) {
            let extents = makerjs.measure.modelExtents(result.models.ticks);
            let maxX = extents.high[0];
            let minX = extents.low[0];

            result.models.baseline = {
                paths: {
                    underline: new makerjs.paths.Line([minX,0],[maxX,0]),
                }
            }
        }

        let name = new makerjs.models.Text(Tick.Tick.font, this.name, 0.8*YSCALE);
        makerjs.model.center(name, true, true);
        makerjs.model.moveRelative(name, [-1, 0.75*this.hscale]);
        result.models.name = name;
        
        return result;
    }
}
