"use strict";
var __importDefault = (this && this.__importDefault) || function (mod) {
    return (mod && mod.__esModule) ? mod : { "default": mod };
};
Object.defineProperty(exports, "__esModule", { value: true });
var Globals_1 = require("./Globals");
var makerjs_1 = __importDefault(require("makerjs"));
var Tick = /** @class */ (function () {
    function Tick(value, show, height) {
        if (height === void 0) { height = 1; }
        this.value = value;
        this.height = height;
        this.show = show;
    }
    Tick.fromTemplate = function (value, template) {
        return new this(value, template.show, template.height);
    };
    Tick.prototype.getXH = function (precompute, hscale) {
        var x = precompute(this.value);
        var h = this.height * hscale;
        return [x * Globals_1.XSCALE, h * Globals_1.YSCALE];
    };
    Tick.prototype.getTickModel = function (precompute, hscale) {
        if (hscale === void 0) { hscale = 1; }
        var _a = this.getXH(precompute, hscale), x = _a[0], h = _a[1];
        var model = {
            origin: [x, 0],
            paths: {
                tick: new makerjs_1.default.paths.Line([0, 0], [0, h]),
            },
        };
        return model;
    };
    Tick.prototype.getNumberModel = function (precompute, hscale) {
        if (hscale === void 0) { hscale = 1; }
        var _a = this.getXH(precompute, hscale), x = _a[0], h = _a[1];
        var model = {
            origin: [x, h],
            models: {}
        };
        if (this.show) {
            var rawText = this.show(this.value);
            var number = new makerjs_1.default.models.Text(Tick.font, rawText, 0.5 * Globals_1.YSCALE * Math.abs(this.height));
            makerjs_1.default.model.center(number, true, true);
            makerjs_1.default.model.moveRelative(number, [0, Globals_1.YSCALE * (h > 0 ? 0.3 : -0.3)]);
            model.models.number = number;
        }
        return model;
    };
    return Tick;
}());
exports.Tick = Tick;
function isAll(spec) { return "subs" in spec; }
exports.isAll = isAll;
function isRanged(spec) { return "group" in spec; }
exports.isRanged = isRanged;
function isSingleton(spec) { return "template" in spec; }
exports.isSingleton = isSingleton;
function isGroup(group) {
    return group instanceof Object && "count" in group && "template" in group;
}
exports.isGroup = isGroup;
function isArr(arg) {
    return arg instanceof Array;
}
exports.isArr = isArr;
function spec(arg) {
    if (isArr(arg)) {
        return { subs: arg };
    }
    else {
        return arg;
    }
}
exports.spec = spec;
exports.s = spec;
function template(height, show) {
    return { height: height, show: show };
}
exports.template = template;
exports.t = template;
function group(count, template, subs, includeInitial, includeFinal) {
    var subsMap = new Map();
    if (isGroup(subs)) {
        subsMap.set(0, subs);
    }
    else if (subs != undefined && subs instanceof Object) {
        for (var _i = 0, subs_1 = subs; _i < subs_1.length; _i++) {
            var key = subs_1[_i];
            var n = parseInt(key);
            if (isNaN(n))
                continue;
            subsMap.set(n, subs[key]);
        }
    }
    else {
        subsMap = null;
    }
    return { count: count, template: template, subs: subsMap, includeInitial: includeInitial, includeFinal: includeFinal };
}
exports.group = group;
exports.g = group;
function unfold(spec, index) {
    if (isAll(spec)) {
        return [].concat.apply([], spec.subs.map(unfold));
    }
    else if (isRanged(spec)) {
        var start = spec.start, end = spec.end, group_1 = spec.group;
        return split(start, end, group_1);
    }
    else if (isSingleton(spec)) {
        return [Tick.fromTemplate(spec.value, spec.template)];
    }
}
exports.unfold = unfold;
function split(start, end, group) {
    var count = group.count, subs = group.subs, template = group.template, _a = group.includeInitial, includeInitial = _a === void 0 ? false : _a, _b = group.includeFinal, includeFinal = _b === void 0 ? false : _b;
    var results = [];
    var delta = (end - start) / count;
    var pos = start;
    var currSubGroup = null;
    for (var ii = 0; ii < count; ii++) {
        if (subs && subs.has(ii)) {
            currSubGroup = subs.get(ii);
        }
        if (includeInitial || ii > 0) {
            results.push(Tick.fromTemplate(pos, template));
        }
        if (currSubGroup) {
            results.push.apply(results, split(pos, pos + delta, currSubGroup));
        }
        pos += delta;
    }
    if (includeFinal) {
        results.push(Tick.fromTemplate(pos, template));
    }
    return results;
}
exports.split = split;
