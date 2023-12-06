#![feature(iter_array_chunks)]
use std::time;
fn main() {
    const TST: &str = include_str!("../input.txt");
    let t = time::Instant::now();
    let res = TST.lines().nth(0).unwrap().split_whitespace().zip(TST.lines().nth(1).unwrap().split_whitespace()).skip(1).map(|(t, d)| {
        let tt: i64 = t.trim().parse().unwrap();
        let dist: i64 = d.trim().parse().unwrap();
        let a = 1.0;
        let b = -tt as f32;
        let c = dist as f32;
        let d = (b*b - 4.0 * a * c).sqrt();
        let lo = (-b - d) / 2.0;
        let hi = (-b + d) / 2.0;
        let lo = if lo.ceil() > lo { lo.ceil() } else { lo.ceil() + 1.0 };
        let hi = if hi.floor() < hi { hi.floor() } else { hi.floor() - 1.0 };
        lo..hi
    }).fold(1.0, |acc, range| acc * (1.0 + range.end - range.start));
    let e = t.elapsed();
    println!("{:?} in {} ns", res, e.as_nanos());
}
