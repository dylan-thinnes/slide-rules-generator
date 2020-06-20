import { XSCALE, YSCALE } from "./Globals"

import makerjs from "makerjs";

export class Tick {
    static font: any;
    readonly value: number;
    readonly height: number;
    readonly show?: (x: number) => string;

    static fromTemplate (value, template: Template) {
        return new this(value, template.show, template.height);
    }

    constructor (value: number, show?: (x: number) => string, height: number = 1) {
        this.value = value;
        this.height = height;
        this.show = show;
    }

    getXH (precompute, hscale) {
        let x = precompute(this.value);
        let h = this.height * hscale;

        return [x*XSCALE,h*YSCALE];
    }

    getTickModel (precompute: (x: number) => number, hscale: number = 1): makerjs.IModel {
        let [x,h] = this.getXH(precompute, hscale);

        let model : makerjs.IModel = {
            origin: [x,0],
            paths: {
                tick: new makerjs.paths.Line([0,0],[0,h]),
            },
        }

        return model;
    }

    getNumberModel (precompute: (x: number) => number, hscale: number = 1): makerjs.IModel {
        let [x,h] = this.getXH(precompute, hscale);

        let model : makerjs.IModel = {
            origin: [x,h],
            models: {}
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

// Specs for producing ticks
export type Spec = SpecAll | SpecRanged | SpecSingleton
export type SpecAll = {
    subs: Spec[]
}
export type SpecRanged = {
    start: number,
    end: number,
    group: Group,
}
export type SpecSingleton = {
    value: number,
    template: Template
}
export function isAll       (spec: Spec): spec is SpecAll       { return "subs" in spec; }
export function isRanged    (spec: Spec): spec is SpecRanged    { return "group" in spec; }
export function isSingleton (spec: Spec): spec is SpecSingleton { return "template" in spec; }

export type Template = {
    height: number,
    show?: (x: number) => string,
}

export type Group = {
    count: number,
    template: Template,
    subs?: Map<number, Group>,
    includeInitial?: boolean,
    includeFinal?: boolean,
}
export function isGroup (group: any): group is Group {
    return group instanceof Object && "count" in group && "template" in group;
}

export function isArr (arg: Spec[] | SpecRanged | SpecSingleton): arg is Spec[] {
    return arg instanceof Array;
}
export function spec (arg: Spec[] | SpecRanged | SpecSingleton): Spec {
    if (isArr(arg)) {
        return { subs: arg };
    } else {
        return arg;
    }
}
export const s = spec;

export function template (height, show?): Template {
    return { height, show };
}
export const t = template;

export function group (count, template, subs?: any, includeInitial?, includeFinal?): Group {
    let subsMap: Map<number, Group> = new Map();
    if (isGroup(subs)) {
        subsMap.set(0, subs);
    } else if (subs != undefined && subs instanceof Object) {
        for (let key in subs) {
            let n = parseInt(key);
            if (isNaN(n)) continue;
            subsMap.set(n, subs[key]);
        }
    } else {
        subsMap = null;
    }

    return { count, template, subs: subsMap, includeInitial, includeFinal };
}
export const g = group;

export function unfold (spec: Spec, index: number): Tick[] {
    if (isAll(spec)) {
        return [].concat(...spec.subs.map(unfold));
    } else if (isRanged(spec)) {
        let { start, end, group } = spec;
        return split(start, end, group);
    } else if (isSingleton(spec)) {
        return [Tick.fromTemplate(spec.value, spec.template)]
    }
}

export function split (start: number, end: number, group: Group): Tick[] {
    let { count, subs, template, includeInitial = false, includeFinal = false } = group;
    let results = [];

    let delta = (end - start) / count;
    let pos = start;
    let currSubGroup = null;

    for (let ii = 0; ii < count; ii++) {
        if (subs && subs.has(ii)) {
            currSubGroup = subs.get(ii);
        }

        if (includeInitial || ii > 0) {
            results.push(Tick.fromTemplate(pos, template));
        }

        if (currSubGroup) {
            results.push(...split(pos, pos + delta, currSubGroup));
        }

        pos += delta;
    }

    if (includeFinal) {
        results.push(Tick.fromTemplate(pos, template));
    }

    return results;
}
