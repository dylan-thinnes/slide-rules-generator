// example data
let exampleData = {
    slideruleId: 0,
    scaleId: 0,
    generatorId: 0,
    addControlPoints: "",
    sliderules: [
        mkSliderule("Sliderule 1",
            mkScale("Scale A",
                mkGenerator("Gen O")
            )
        ),
        mkSliderule("sliderule with really really long name...",
            mkScale("scale A",
                mkGenerator("Gen W")
            ),
            mkScale("scale B",
                mkGenerator("Gen Z")
            ),
            mkScale("scale C",
                mkGenerator("Gen E")
            )
        )
    ]
}

// Persistence
function saveLocal () {
    console.log("Saving local.");
    localStorage.setItem("appData", JSON.stringify(app._data));
}

function loadLocal () {
    try {
        return JSON.parse(localStorage.getItem("appData")) || exampleData;
    } catch (e) {
        return exampleData;
    }
}

// Debugging only
function clearLocal () {
    localStorage.removeItem("appData");
}

// Create dropdown entries from array of vals
function toVueSelectEntries (arr, selectPrompt = "Select one.") {
    if (arr == null || arr.length == 0) {
        return [{ value: null, text: `No options available.`, disabled: true }];
    }

    let entries = arr.map((a, idx) => ({ value: idx, text: a.name }));
    entries.unshift({ value: null, text: selectPrompt, disabled: true });

    return entries;
}

Vue.use(BootstrapVue)

function mkSliderule(name, ...scales) {
    return {
        name, scales,
        description: "",
    }
}

function mkScale(name, ...generators) {
    return {
        name, generators,
        description: "",
        type: "Linear",
        radius: 1,
        velocity: 0.1,
        minimumTickDistance: 0.03,
        baseTickHeight: 0.1,
        flipped: false,
        baseTextHeight: 0.1,
        textBaseline: "Top",
    }
}

function mkGenerator(name) {
    return {
        name,
        type: "HardcodedPoints",
        controlPoints: [],
        transformations: []
    }
}

var app = new Vue({
    el: "#app",
    data: loadLocal(),
    methods: {
        newSliderule: function () {
            let idx = this.sliderules.length;
            let name = "Unnamed sliderule #" + (idx + 1).toString();
            this.sliderules.push(mkSliderule(name));
            this.slideruleId = idx;
        },

        newScale: function () {
            let idx = this.scales.length;
            let name = "Unnamed scale #" + (idx + 1).toString();
            this.scales.push(mkScale(name));
            this.scaleId = idx;
        },

        newGenerator: function () {
            let idx = this.generators.length;
            let name = "Unnamed generator #" + (idx + 1).toString();
            this.generators.push(mkGenerator(name));
            this.generatorId = idx;
        },

        isHSNumber: function (x) {
            return /^\d+(\.\d*)?(e[+-]?\d+)?$/.test(x);
        },

        addControlPointsSubmit: function () {
            let wordsOnly = this.addControlPointsParsedNumbers.map(x => x.word);
            this.generator.controlPoints.push(...wordsOnly);
            this.sortControlPoints();
            this.addControlPoints = "";
        },

        removeControlPoint: function (idx) {
            this.generator.controlPoints.splice(idx, 1);
            this.sortControlPoints();
        },

        sortControlPoints: function () {
            this.generator.controlPoints.sort((x,y) => parseFloat(x) - parseFloat(y))
        }
    },
    computed: {
        // sliderules
        atLeastOneSliderule: function () {
            return this.sliderules == null || this.sliderules.length == 0;
        },
        indexedSliderules: function () {
            return toVueSelectEntries(this.sliderules, "Select a slide rule.");
        },
        sliderule: function () {
            return this.sliderules[this.slideruleId];
        },
        slideruleExists: function () {
            return this.sliderule != null;
        },

        // scales
        scales: function () {
            return this.sliderule?.scales;
        },
        atLeastOneScale: function () {
            return this.scales == null || this.scales.length == 0;
        },
        indexedScales: function () {
            return toVueSelectEntries(this.scales, "Select a scale.");
        },
        scale: function () {
            return this.scales?.[this.scaleId];
        },
        scaleExists: function () {
            return this.scale != null;
        },

        // generators
        generators: function () {
            return this.scale?.generators;
        },
        atLeastOneGenerator: function () {
            return this.generators == null || this.generators.length == 0;
        },
        indexedGenerators: function () {
            return toVueSelectEntries(this.generators, "Select a generator.");
        },
        generator: function () {
            return this.generators?.[this.generatorId];
        },
        generatorExists: function () {
            return this.generator != null;
        },

        // UI-specific
        addControlPointsParsedNumbers: function () {
            if (this.addControlPoints === "") return [];
            let allWords = this.addControlPoints.split(/\s+/).filter(x => x != "");
            return allWords.map(word => ({ word, valid: this.isHSNumber(word) }));
        },
        addControlPointsState: function () {
            if (this.addControlPointsParsedNumbers.length == 0) return null;
            return this.addControlPointsParsedNumbers.every(x => x.valid);
        }
    },
    watch: {
        slideruleId: saveLocal,
        scaleId: saveLocal,
        generatorId: saveLocal,
        sliderules: {
            handler: saveLocal,
            deep: true
        }
    }
});

function serializeGenerator () {
    let { generator } = app;
    if (generator == null) return null;

    let tag = generator.type;
    let transformations = generator.transformations || [];
    let controlPoints = app.generatorCalculatedControlPoints;

    return { tag, transformations, controlPoints };
}
