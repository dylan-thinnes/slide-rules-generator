opentype.load("./fonts/OpenSans-Light.ttf", (err, font) => {

const makerjs = require("makerjs");
const { Line } = makerjs.paths;

function log10 (x) {
    return Math.log(x) / Math.log(10);
}

const scale = 10000;
function position (x,h) {
    var d = log10(x) * scale;
    return [d,h * scale];
}

function tick (x,h) {
    var [x,h] = position(x,h);
    return new Line([x,0],[x,h]);
}

function range (min, max, step, inclusive = true) {
    var xs = [];
    var cond = inclusive ? () => min <= max : () => min < max;
    while (cond()) {
        xs.push(min);
        min += step;
    }
    return xs;
}

var ticks = [];
//ticks.push(new Line([1,1],[1.1,1.1]));
//ticks.push(new Line([0,1],[-0.1,1.1]));
//ticks.push(new Line([1,0],[1.1,-0.1]));
//ticks.push(new Line([0,0],[-0.1,-0.1]));

ticks.push(...range(1,10,1));
ticks.push(...range(1.1,2,0.1,false));
ticks.push(...range(1,2,0.1,false).flatMap(o => range(o+0.02,o+0.1,0.02,false)));
ticks.push(...range(2,10,1,false).flatMap(o => range(o+0.1,o+1,0.1,false)));
ticks.push(...range(2,5,1,false).flatMap(o => range(o+0.05,o+1,0.1,false)));
//ticks.push(...range(2,10,1,false).flatMap(o => range(o+0.25,o+1,0.25,false)));

ticks = ticks.flatMap(x => {
    var mx = Math.round(x * 1000000);
    var trailing = mx.toString().match(/0*$/)[0];
    var sigdigs = Math.max(0, 6 - trailing.length);
    var h = 0.03 * Math.pow(0.5, sigdigs);

    var model = {
        type: "model",
        paths: [],
        models: [],
    }

    if (sigdigs === 0) {
        var x = Math.round(x);

        var pos = position(x,h);
        pos[0] -= 30;
        pos[1] += 30;
        var str = Math.round(x).toString()
        str = str.slice(0,1);
        var text = new makerjs.models.Text(font, str, scale * 0.01);
        text.origin = pos;
        model.models.push(text);
    }
    model.paths.push(tick(x,h));

    return model;
});

var model = {
    type: "model",
    models: ticks,
}
model = makerjs.model.move(model, [1,1]);
console.log(model);
var svg = makerjs.exporter.toSVG(model);
document.body.innerHTML = svg;
document.body.children[0].setAttribute("width", 800);
document.body.children[0].setAttribute("height", 400);

/*
var svg = document.getElementById("rule");

for (var ii = 1; ii < 10; ii++) {
    var d = log10(ii) * 1000;
    var rect = document.createElementNS("http://www.w3.org/2000/svg", "path");
    //rect.setAttribute("stroke-width", "1");
    rect.setAttribute("vector-effect", "non-scaling-size");
    rect.setAttribute("stroke", "black");
    rect.setAttribute("d", `M${d} 0 l0 100`);
    svg.appendChild(rect);
}
*/

});
