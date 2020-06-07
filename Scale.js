"use strict";
var __importDefault = (this && this.__importDefault) || function (mod) {
    return (mod && mod.__esModule) ? mod : { "default": mod };
};
var __importStar = (this && this.__importStar) || function (mod) {
    if (mod && mod.__esModule) return mod;
    var result = {};
    if (mod != null) for (var k in mod) if (Object.hasOwnProperty.call(mod, k)) result[k] = mod[k];
    result["default"] = mod;
    return result;
};
Object.defineProperty(exports, "__esModule", { value: true });
var Globals_1 = require("./Globals");
var makerjs_1 = __importDefault(require("makerjs"));
var Tick = __importStar(require("./Tick"));
var Scale = /** @class */ (function () {
    function Scale(name, precompute, spec) {
        this.name = name;
        this.spec = spec;
        this.precompute = precompute;
        this.underline = true;
        this.hscale = 1;
    }
    Scale.prototype.toModel = function () {
        var _this = this;
        var result = {
            models: {
                ticks: {
                    models: {}
                },
                numbers: {
                    models: {}
                },
            }
        };
        var ticks = Tick.unfold(this.spec, 0);
        ticks.forEach(function (tick, ii) {
            var line = tick.getTickModel(_this.precompute, _this.hscale);
            result.models.ticks.models[ii.toString()] = line;
            var number = tick.getNumberModel(_this.precompute, _this.hscale);
            if (number != null) {
                result.models.numbers.models[ii.toString()] = number;
            }
        });
        if (this.underline) {
            var extents = makerjs_1.default.measure.modelExtents(result.models.ticks);
            var maxX = extents.high[0];
            var minX = extents.low[0];
            result.models.baseline = {
                paths: {
                    underline: new makerjs_1.default.paths.Line([minX, 0], [maxX, 0]),
                }
            };
        }
        var name = new makerjs_1.default.models.Text(Tick.Tick.font, this.name, 0.8 * Globals_1.YSCALE);
        makerjs_1.default.model.center(name, true, true);
        makerjs_1.default.model.moveRelative(name, [-1, 0.75 * this.hscale]);
        result.models.name = name;
        return result;
    };
    return Scale;
}());
exports.default = Scale;
