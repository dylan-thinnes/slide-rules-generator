import * as wasm from "sliderules";
import { h, Component } from 'preact';
import { cloneDeep } from 'lodash';

const trace = x => {
    console.log(x);
    return x;
}

const defaultSpec = (extras) => {
    return {
        count: 2,
        template: {
            height: 1.0
        },
        incl_start: false,
        incl_end: false,
        sub_specs: {},
        ...extras
    }
}

interface Dictionary<T> {
    [key: number]: T;
}

type SpecRow = {
    count : number,
    template : {
        height : number,
        format? : {
            preshow : any,
            show : any,
        }
    },
    incl_start : boolean,
    incl_end : boolean,
    sub_specs : Dictionary<SpecRow>
}

type Preshow = "Id" | {"LogNClamp":number}
type Show = "Display" | "Int" | {"Float":[number, boolean]} | {"Only":string}

interface SpecProps {
    onChange : (x: SpecRow) => void;
    initialSpec? : any;
}

const withOCValue = (spec, onChange) => (converter, f) => e => {
    let valueOrig;
    if (e.target.type === "text") valueOrig = e.target.value;
    if (e.target.type === "number") valueOrig = parseFloat(e.target.value);
    if (e.target.type === "checkbox") valueOrig = e.target.checked;
    if (e.target.type === "radio") valueOrig = e.target.checked;
    if (e.target.type === "select-one") valueOrig = e.target.value;
    console.log(e, e.target.type, valueOrig);
    let value = converter ? converter(valueOrig) : valueOrig;
    if (isNaN(value) || value == null) value = valueOrig;

    let newSpec = cloneDeep(spec);
    f(value, newSpec);
    return onChange(newSpec);
}

const addStartingFrom = (onChange, spec : SpecRow, startingIndex : number, subspec? : SpecRow) => {
    let existingValues = new Set(Object.keys(spec.sub_specs));
    let candidateIndex = startingIndex;
    while (existingValues.has(candidateIndex.toString())) {
        candidateIndex++;
    }
    
    let newSpec = cloneDeep(spec);
    newSpec.sub_specs[candidateIndex] = subspec || defaultSpec({});
    onChange(newSpec)
}

function Spec (props : { onChange : (spec : SpecRow) => void, spec: SpecRow }) {
    const { onChange, spec } = props;
    const { count, template, incl_start, incl_end } = spec;
    let withValue = withOCValue(spec, onChange);

    let id = Math.random().toString();
    return <div id={id}>
        <div>
            <label for={"count" + id} className="mx-2">Count: </label>
            <input type="number"
                   name={"count" + id}
                   id={"count" + id}
                   min="1"
                   onInput={withValue(parseInt, (value, spec) => spec.count = value)}
                   value={count}/>
            <label for={"height" + id} className="mx-2"> Height: </label>
            <input type="number"
                   step={0.05}
                   pattern="(\d+|0)(|.\d+)"
                   name={"height" + id}
                   id={"height" + id}
                   onInput={withValue(parseFloat, (value, spec) => spec.template.height = value)}
                   value={template.height}/>
        </div>

        <div>
            <label for={"incl_start"+id}>
                <input type="checkbox"
                       className="mx-2"
                       name={"incl_start"+id}
                       id={"incl_start"+id}
                       onInput={withValue(null, (value, spec) => spec.incl_start = value)}
                       checked={incl_start}/>
                Include Starting Tick?
            </label>

            <label for={"incl_end"+id}>
                <input type="checkbox"
                       className="mx-2"
                       name={"incl_end"+id}
                       id={"incl_end"+id}
                       onInput={withValue(null, (value, spec) => spec.incl_end = value)}
                       checked={incl_end}/>
                Include Ending Tick?
            </label>
        </div>

        <div>
            Show values on ticks:
            <input type="checkbox"
                   onInput={withValue(null, (value, spec) => {
                       if (value) {
                           spec.template.format = {};
                       } else {
                           spec.template.format = null;
                       }
                   })} />
        </div>

        { spec.template.format != null &&
            <div>
                <div>
                    <span>Preshow </span>
                    <select id={"format_preshow"+id} name={"format_show"+id}
                            onInput={withValue(parseInt, (value, spec) => {
                                spec.template.format = spec.template.format || {};
                                if (value == "LogNClamp") {
                                    spec.template.format.preshow = {"LogNClamp":10};
                                } else {
                                    spec.template.format.preshow = "Id";
                                }
                            })}>
                        <option value="Id" selected>No preformatting</option>
                        <option value="LogNClamp">Clamp value logarithmically between 1 and x</option>
                    </select>
                    {(spec.template?.format?.preshow?.LogNClamp) != null &&
                        <input type="number" value={spec.template.format.preshow["LogNClamp"]}
                               onInput={withValue(parseInt, (value, spec) => {
                                   spec.template.format.preshow = {"LogNClamp":value};
                               })}/>
                    }
                </div>

                <span>Show </span>
                <select id={"format_show"+id} name={"format_show"+id}
                        onInput={withValue(null, (value, spec) => {
                            spec.template.format = spec.template.format || {};
                            switch (value) {
                                case "Display":
                                    spec.template.format.show = "Display";
                                    break;
                                case "Int":
                                    spec.template.format.show = "Int";
                                    break;
                                case "Float":
                                    spec.template.format.show = {"Float":[2, true]};
                                    break;
                                case "Only":
                                    spec.template.format.show = {"Only":"{Custom}"};
                                    break;
                            }
                        })}>
                    <option value="Display" selected>Print as normal</option>
                    <option value="Int">Only print integer</option>
                    <option value="Float">Print decimal, to a certain precision</option>
                    <option value="Only">Print a hardcoded value</option>
                </select>
                {(spec.template?.format?.show?.Float) != null &&
                    <input type="number" step={1}
                           value={spec.template.format.show["Float"][0]}
                           onInput={withValue(parseInt, (value, spec) => {
                               spec.template.format.show["Float"][0] = value;
                           })}/>
                }
            </div>
        }

        {Object.entries(spec.sub_specs).length > 0 &&
         <div style="margin-top: 1ch; font-weight: bold; margin-left: 1ch;">
             Subspecs
         </div>
        }
        <div style="margin-left: 4ch;">
            {...Object.entries(spec.sub_specs).map(
                ([keyStr, subspec]) => {
                    let key = parseInt(keyStr);
                    return <div style="margin: 5px; border: 1px solid black; padding: 5px;">
                        <div>
                            <div style="margin-bottom: 5px; padding-bottom: 5px; border-bottom: 1px solid grey;">
                                <b>Starts From: </b>
                                <input type="number"
                                       min={1}
                                       max={count}
                                       value={key+1}
                                       onInput={(e:any) => {
                                           let newSpec = cloneDeep(spec);
                                           delete newSpec.sub_specs[key];
                                           console.log("Changing...")
                                           newSpec.sub_specs[parseInt(e.data) - 1] = subspec;
                                           onChange(newSpec);
                                       }} />
                                <button onClick={() => {
                                            let newSpec = cloneDeep(spec);
                                            delete newSpec.sub_specs[key];
                                            onChange(newSpec);
                                        }}>
                                    Delete
                                </button>
                                <button onClick={() => addStartingFrom(onChange, spec, key, subspec)}>
                                    Duplicate
                                </button>
                            </div>
                        </div>
                        <Spec
                            onChange={subspec => {
                                let newSpec = cloneDeep(spec);
                                newSpec.sub_specs[key] = subspec;
                                onChange(newSpec);
                            }}
                            spec={subspec}
                        />
                    </div>
                }
            )}
            <button onClick={() => addStartingFrom(onChange, spec, 0)}>
                Create New Subspec
            </button>
        </div>
    </div>;
}

type RangeRow = {
    start : number,
    end : number,
    spec : SpecRow,
}

function Range (props : { onChange : (range : RangeRow) => void, range : RangeRow }) {
    let { onChange, range } = props;
    let { start, end, spec } = range;

    let withValue = withOCValue(range, onChange);

    let id = Math.random().toString();
    return <div>
        <div>
            <span>Start: </span>
            <input type="number"
                   name={"start" + id}
                   id={"start" + id}
                   value={start}
                   onInput={withValue(parseInt, (value, range) => range.start = value)}
                   />
            <span> End: </span>
            <input type="number"
                   name={"end" + id}
                   id={"end" + id}
                   value={end}
                   onInput={withValue(parseInt, (value, range) => range.end = value)}
                   />
        </div>
        <div style="margin-left: 2ch; margin-top: 1ch;">
            <Spec spec={spec} onChange={newSpec => {
                console.log("Spec changed.")
                let newRange = cloneDeep(range);
                newRange.spec = newSpec;
                onChange(newRange);
            }}/>
        </div>
    </div>
}

type ScaleRow = {
    name : string,
    y_ratio : number,
    transformation : string,
    ranges: RangeRow[]
}

function Scale (props : { onChange : (range : ScaleRow) => void, scale : ScaleRow }) {
    let { onChange, scale } = props;
    let { name, y_ratio, transformation, ranges } = scale;

    let withValue = withOCValue(scale, onChange);

    let id = Math.random().toString();
    return <div id={id}>
        <input type="text"
               name={"name" + id}
               id={"name" + id}
               step={0.005}
               value={name}
               onInput={withValue(null, (value, scale) => scale.name = value)}
               />
        <input type="number"
               name={"y_ratio" + id}
               id={"y_ratio" + id}
               step={0.005}
               value={y_ratio}
               onInput={withValue(parseFloat, (value, scale) => scale.y_ratio = value)}
               />
        {scale.ranges.map((range, i) => <Range range={range} onChange={(range) => {
            scale = cloneDeep(scale);
            if (i > scale.ranges.length) {
                scale.ranges.push(range);
            } else {
                scale.ranges[i] = range;
            }
            onChange(scale);
        }}/>)}
    </div>
}

class Main extends Component<{}, { scale : ScaleRow }> {
    updateScale = scale => { this.setState({ scale }); }

    constructor (props) {
        super();
        this.state = {
            scale: {
                name: "C",
                y_ratio: 0.10,
                transformation: "Log",
                ranges: [
                    {
                        start: 1.0,
                        end: 10.0,
                        spec: defaultSpec({
                            incl_start: true,
                            incl_end: true,
                            sub_specs: {}
                        })
                    }
                ]
            }
        }
    }

    render (_, { scale }) {
        console.log(scale);
        let svg = scale == null ? null : wasm.scale_to_svg_js(scale);

        return <div style="max-height: 100vh; overflow-y: hidden; display: flex; flex-direction: column;">
            <div className="mt-1" dangerouslySetInnerHTML={{ __html: svg }} />
            <div style="flex: 1 1 auto; overflow-y: auto;">
                <Scale onChange={this.updateScale} scale={scale} />
            </div>
        </div>;
    }
}

const App = <Main />;
export default App;
