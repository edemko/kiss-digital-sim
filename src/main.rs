use std::collections::BTreeMap;
use std::collections::HashMap;
use std::collections::HashSet;
use std::fmt;
use std::ops::Add;

////// Basic Types //////

#[derive(Debug)]
#[derive(PartialEq,Eq)]
#[derive(PartialOrd,Ord)]
struct Ticks {
    picoseconds: u64,
}

impl Ticks {

    const ZERO: Ticks = Ticks { picoseconds: 0 };
    const MAX: Ticks = Ticks { picoseconds: u64::MAX };

    fn from_picos(t: u64) -> Self {
        Ticks { picoseconds: t }
    }

    fn from_nanos(t: u64) -> Self {
        Ticks::from_picos(1000*t)
    }

    fn from_micros(t: u64) -> Self {
        Ticks::from_nanos(1000*t)
    }

    fn from_millis(t: u64) -> Self {
        Ticks::from_micros(1000*t)
    }
}

impl Add for &Ticks {
    type Output = Ticks;
    fn add(self, rhs: &Ticks) -> Ticks {
        Ticks { picoseconds: self.picoseconds + rhs.picoseconds }
    }
}

impl fmt::Display for Ticks {

    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        self.picoseconds.fmt(f)
    }
}

// possibly to be replaced by Ieee1164
#[derive(Debug)]
#[derive(PartialEq,Eq)]
#[derive(PartialOrd,Ord)]
#[derive(Clone,Copy)]
enum Signal {
    DL, // driven low
    PD, // pull-down
    HZ, // high impedance, a.k.a. open
    PU, // pull-up
    DH, // driven high
}

impl fmt::Display for Signal {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        use Signal::*;
        let c = match self {
            DH => "1",
            PU => "H",
            HZ => "Z",
            PD => "L",
            DL => "0",
        };
        f.write_str(c)
    }
}

////// Machine Topology //////

#[derive(Debug)]
struct Comp {
    rd_pins: Vec<usize>,
    logic: CompLogic,
    wr_pins: Vec<usize>,
}
#[derive(Debug)]
enum CompLogic {
    Osc(Oscillator),
}
#[derive(Debug)]
struct Oscillator {
    frequency: Ticks
}

#[derive(Debug)]
struct Schedule {
    upcoming: BTreeMap<Ticks, HashMap<usize, Signal>>,
}

#[derive(Debug)]
struct Machine {
    components: Vec<Comp>,
    pin_states: Vec<Signal>,
    current_time: Ticks,
    schedule: Schedule,
}

////// Machine Engine //////

trait CompUpdate {
    fn update(&self, inp_pins: Vec<Signal>) -> Vec<Signal>;
}

impl Schedule {
    fn next_events(&mut self) -> (Ticks, HashMap<usize, Signal>) {
        match self.upcoming.pop_first() {
            None => (Ticks::MAX, HashMap::new()),
            Some(fst) => fst,
        }
    }
}

fn trigger_pins(pin_states: &mut Vec<Signal>, evs: HashMap<usize, Signal>) -> HashSet<usize> {
    let mut acc = HashSet::new();
    for (pin, next_signal) in evs.iter() {
        if pin_states[*pin] != *next_signal {
            pin_states[*pin] = *next_signal;
            acc.insert(*pin);
        }
    }
    acc
}

impl Machine {
    fn update_components(&mut self, changed_pins: HashSet<usize>) {
        for comp in self.components.iter() {
            if comp.rd_pins.iter().any(|p| changed_pins.contains(p)) {
                let rd_signals = comp.rd_pins.iter().map(|p| self.pin_states[*p]).collect();
                let (dt, wr_signals) = comp.logic.update(rd_signals);
                let t = &self.current_time + &dt;
                let evs = self.schedule.upcoming.entry(t).or_insert_with(HashMap::new);
                for (i, s) in wr_signals.iter().enumerate() {
                    let pin = comp.wr_pins[i];
                    evs.insert(pin, *s);
                }
            }
        }
    }
}

impl CompLogic {
    fn update(&self, inp: Vec<Signal>) -> (Ticks, Vec<Signal>) {
        use Signal::*;
        use CompLogic::*;
        match self {
            Osc(osc) => {
                let dt = Ticks::from_picos(osc.frequency.picoseconds/2);
                let new_signal = if inp[0] > HZ { DL } else { DH };
                (dt, vec![new_signal])
            },
        }
    }
}

////// Main //////


fn main() {
    use CompLogic::*;
    use Signal::*;

    println!("Hello, world! {}", DH < PU);
    println!("Hello, world! {} {} {} {} {}", DH, PU, HZ, PD, DL);
    println!("Ticks: {}", Ticks::from_millis(4));
    let mut m = Machine {
        components: vec![
            Comp {
                rd_pins: vec![0],
                logic: Osc(Oscillator{frequency: Ticks::from_nanos(1)}),
                wr_pins: vec![0],
            }
        ],
        pin_states: vec![HZ],
        current_time: Ticks::ZERO,
        schedule: Schedule  {
            upcoming: BTreeMap::from([
                (Ticks::ZERO, HashMap::from([
                    (0, DL)
                ]))
            ]),
        }
    };

    dump_pin_states(&m);
    loop {
        let (t, evs) = m.schedule.next_events();
        if t == Ticks::MAX { break; }
        m.current_time = t;
        let changed_pins = trigger_pins(&mut m.pin_states, evs);
        m.update_components(changed_pins);
        dump_pin_states(&m);
    }
}

fn dump_pin_states(m: &Machine) {
    print!("t = {}ps: [", m.current_time,);
    for s in m.pin_states.iter() { print!("{}", s); }
    print!("] ");
    println!("sched: {:?}", m.schedule);
}
