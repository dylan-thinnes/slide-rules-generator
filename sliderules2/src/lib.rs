use wasm_bindgen::prelude::*;
use serde::{Serialize, Deserialize};
use std::collections::HashMap;

// Scale struct / class
#[derive(Serialize, Deserialize)]
pub struct Scale {
    name: String,
    y_ratio: f64,
    transformation: Transformation,
    ranges: Vec<Range>,
}

// A Transform denotes how
#[derive(Debug, Serialize, Deserialize)]
pub enum Transformation {
    Id,
    Inv,
    Linear(f64, f64),
    Pow,
    PowN(f64),
    Exp,
    ExpN(f64),
    Log,
    LogN(f64),
    LogLog,
    Sin,
    Cos,
    Tan,
    Compose(Vec<Transformation>)
}

impl Transformation {
    fn run(&self, x: f64) -> f64 {
        use Transformation::*;

        match self {
            Id => x,
            Inv => 1.0 / x,
            Linear(a,b) => x * a + b,
            Pow => x.powf(10.0),
            PowN(n) => x.powf(*n),
            Exp => (10.0 as f64).powf(x),
            ExpN(n) => n.powf(x),
            Log => x.log(10.0),
            LogN(n) => x.log(*n),
            LogLog => x.log(10.0).log(10.0),
            Sin => x.sin(),
            Cos => x.cos(),
            Tan => x.tan(),
            Compose(transforms) => {
                let mut res = x;
                for transform in transforms {
                    res = transform.run(res);
                }
                res
            }
        }
    }
}

// A range delimits a range over which a TickSpec operates
#[derive(Serialize, Deserialize)]
pub struct Range {
    start: f64,
    end: f64,
    spec: TickSpec,
}

// A tick spec defines how to generate ticks
#[derive(Serialize, Deserialize)]
pub struct TickSpec {
    count: u32, // How many sections in this range, count+1 ticks created
    template: Template, // What template to use on each tick
    incl_start: bool,
    incl_end: bool, // Whether to include starting/ending tick
    #[serde(default)]
    sub_specs: HashMap<u32, TickSpec>, // Specs to run on subticks
}

/*
// Newtype for a HashMap that defaults to index 0 when only a single value is passed.
struct AutoNestHashMap<I, V>(HashMap<I, V>);

impl<I: Hash + Eq, V> Default for AutoNestHashMap<I, V> {
    fn default () -> Self {
        AutoNestHashMap(Default::default())
    }
}

impl<I: Hash + Eq + Serialize, V: Serialize> Serialize for AutoNestHashMap<I, V> {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        match self {
            AutoNestHashMap(hashmap) => {
                Serialize::serialize(hashmap, serializer)
            }
        }
    }
}

impl<'de, I: Hash + Eq + Deserialize<'de> + Default, V: Deserialize<'de>> Deserialize<'de> for AutoNestHashMap<I, V> {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>
    {
        let v_match : Result<V, D::Error> = Deserialize::deserialize::<D>(deserializer);
        let map : HashMap<I, V> = match v_match {
            Ok(v) => {
                let hm = HashMap::new();
                hm.insert(Default::default(), v);
                hm
            },
            Err(_) => {
                let hm : HashMap<I, V> = Deserialize::deserialize::<D>(deserializer)?;
                hm
            }
        };

        Ok(AutoNestHashMap(map))
    }
}
*/

// A template defines metadata about generating a tick given its position
#[derive(Serialize, Deserialize)]
pub struct Template {
    height: f64, // How tall the tick should be
    #[serde(default)]
    format: Option<Format>, // How to show the tick, if at all
}

// A format expresses how to convert a number to a string, using an initial "preshow"
// transformation on the number, and then a "show" transformation to turn it into a string
#[derive(Default, Serialize, Deserialize)]
pub struct Format {
    #[serde(default)]
    preshow: Preshow,
    #[serde(default)]
    show: Show,
}

impl Format {
    fn run (&self, val: f64) -> String {
        self.show.run(self.preshow.run(val))
    }
}

#[derive(Serialize, Deserialize)]
pub enum Preshow {
    Id,             // Make no pretransformation
    LogClamp,       // Clamp it to the range [1,10) with successive division/multiplication
    LogNClamp(f64), // Clamp it to the range [1,n) with successive division/multiplication
}

impl Default for Preshow { fn default() -> Self { Preshow::Id } }

fn log_clamp (r : f64, mut val : f64) -> f64 {
    if r == 1.0 {
        return 1.0;
    } else {
        while val < 1.0 { val *= r; }
        while val >= r  { val /= r; }
    }

    return val;
}

impl Preshow {
    fn run (&self, val: f64) -> f64 {
        match &self {
            Preshow::Id => val,
            Preshow::LogClamp => log_clamp(10.0, val),
            Preshow::LogNClamp(r) => log_clamp(*r, val)
        }
    }
}

#[derive(Serialize, Deserialize)]
pub enum Show {
    Display,            // Display the float using Display (`format!("{}")`)
    Int,                // Display the float as an int, with rounding
    Float(usize, bool), // Display the float as a float with <n> digits precision
                        // The bool indicates whether to exclude everything before the digit
    Only(String),       // Only output this string - useful for things like pi
}

impl Default for Show { fn default() -> Self { Show::Display } }

impl Show {
    fn run (&self, val: f64) -> String {
        match self {
            Show::Display => format!("{}", val),
            Show::Int => format!("{:.0}", val),
            Show::Float(prec, _) => format!("{:.*}", prec, val),
            Show::Only(string) => string.to_string()
        }
    }
}

// "Unfolding" specs into ticks

// Ticks & their SVG representations
#[wasm_bindgen]
#[derive(Debug, Serialize, Deserialize)]
pub struct Tick {
    value: f64,
    position: f64,
    height: f64,
    name: Option<String>,
}

fn to_tick(value: f64, template: &Template, transformation: &Transformation) -> Tick {
    log(&format!("{:?}", transformation));

    Tick {
        value: value,
        position: transformation.run(value),
        height: template.height,
        name: template.format.as_ref().map(|format| format.run(value)),
    }
}

impl Tick {
    fn render_height (&self, y_ratio: f64) -> f64 { self.height * 1000.0 * y_ratio }
    fn render_position (&self) -> f64 { self.position * 1000.0 }
    fn to_line (&self, y_ratio: f64) -> String {
        format!(" M {} 0 l 0 {} ", self.render_position(), self.render_height(y_ratio))
    }
    fn font_height (&self, y_ratio: f64) -> f64 { self.render_height(y_ratio) * 0.4 }
    fn full_height (&self, y_ratio: f64) -> f64 { self.render_height(y_ratio) + self.font_height(y_ratio) }
    fn to_text (&self, y_ratio: f64) -> Option<String> {
        Some(format!("<text text-anchor=\"middle\" x=\"{x}\" y=\"{y}\" font-size=\"{font_size}\">{content}</text>",
            x=self.render_position(),
            y=self.render_height(y_ratio)+self.font_height(y_ratio),
            font_size=self.font_height(y_ratio),
            content=self.name.as_ref()?
        ))
    }
}

pub fn ticks_to_path (ticks: &Vec<Tick>, y_ratio: f64) -> String {
    let mut result = String::from("<path d=\"");

    for tick in ticks {
        result.push_str(&tick.to_line(y_ratio));
    }
    result.push_str("\" vector-effect=\"non-scaling-stroke\"></path>");

    return result;
}

pub fn ticks_to_texts (ticks: &Vec<Tick>, y_ratio: f64) -> String {
    let mut result = String::from("");

    for tick in ticks {
        if let Some(str) = tick.to_text(y_ratio) {
            result.push_str(&str);
        }
    }

    return result;
}

// Unfolding requires three stages: (de)serialization, initialization, recursion
#[wasm_bindgen]
pub fn unfold_js(scale: JsValue) -> JsValue {
    JsValue::from_serde(&unfold(&scale.into_serde().unwrap())).unwrap()
}

pub fn unfold(scale: &Scale) -> Vec<Tick> {
    let mut vec = vec![];
    for range in &scale.ranges {
        unfold_f(&mut vec, range.start, range.end, &scale.transformation, &range.spec);
    }
    return vec;
}

pub fn unfold_f(vec: &mut Vec<Tick>, mut start: f64, end: f64, transformation: &Transformation, spec: &TickSpec) {
    let delta = (end - start) / spec.count as f64;
    let mut curr_sub_spec: Option<&TickSpec> = None;

    for x in 0..spec.count {
        if spec.incl_start || x != 0 {
            vec.push(to_tick(start, &spec.template, transformation));
        }

        match spec.sub_specs.get(&x) {
            None => {},
            Some(sub_spec) => {
                curr_sub_spec = Some(sub_spec);
            }
        }

        match &curr_sub_spec {
            None => {},
            Some(sub_spec) => {
                unfold_f(vec, start, start+delta, transformation, sub_spec);
            },
        }
        start += delta;
    }

    if spec.incl_end {
        vec.push(to_tick(start, &spec.template, transformation));
    }
}

#[wasm_bindgen]
pub fn scale_to_svg_js (val: JsValue) -> String {
    match val.into_serde() {
        Ok(v)  => scale_to_svg(&v),
        Err(_) => String::from("<div>Invalid scale.</div>")
    }
}

pub fn scale_to_svg (scale: &Scale) -> String {
    let ticks = unfold(scale);

    //let mut min_x : f64 = 0.0;
    let mut min_y : f64 = 0.0;
    //let mut max_x : f64 = 0.0;
    let mut max_y : f64 = 0.0;

    let min_x : f64 = -100.0;
    let max_x : f64 = 1020.0;

    for tick in &ticks {
        // Update x bounds
        // min_x = min_x.min(tick.render_position());
        // max_x = max_x.max(tick.render_position());

        // Update y bounds
        min_y = min_y.min(tick.full_height(scale.y_ratio));
        max_y = max_y.max(tick.full_height(scale.y_ratio));
    }

    // let delta_x = (max_x - min_x) * 0.02;
    // min_x -= delta_x;
    // max_x += delta_x;
    let width = max_x-min_x;
    let height = max_y-min_y;

    format!("<svg xmlns=\"http://www.w3.org/2000/svg\" viewBox=\"{min_x} {min_y} {width} {height}\">
                <g id=\"svgGroup\" stroke-linecap=\"round\" fill-rule=\"evenodd\"
                   stroke=\"#000\" stroke-width=\"0.25mm\" fill=\"none\"
                   style=\"stroke:#000;stroke-width:0.25mm;fill:none\">
                   {line}
                </g>
                {texts}
                <text text-anchor=\"middle\" x=\"-50\" y=\"{name_offset}\" font-size=\"{name_height}\">
                    {name}
                </text>
            </svg>",
        min_x=min_x,
        min_y=min_y,
        width=width,
        height=height,
        line=ticks_to_path(&ticks, scale.y_ratio),
        texts=ticks_to_texts(&ticks, scale.y_ratio),
        name_offset=height*0.7,
        name_height=height*0.5,
        name=scale.name
    )
}


// Main + External function bindings
#[wasm_bindgen]
pub fn main() {
    log("V2 WASM started.");
}

#[wasm_bindgen]
extern "C" {
    // Use `js_namespace` here to bind `console.log(..)` instead of just
    // `log(..)`
    #[wasm_bindgen(js_namespace = console)]
    fn log(s: &str);
}
