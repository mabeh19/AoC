#![feature(iter_array_chunks)]

#[derive(Debug, Ord, Eq, PartialOrd, PartialEq)]
struct Range {
    first:  i64,
    last:   i64
}

fn main() {
    const INP: &str = include_str!("../input.txt");
    println!("{:?}", solve1(INP));
}


fn overlap(a: &Range, b: &Range) -> Option<Range> {
    if a.first >= b.first && a.first < b.last {
        Some(Range { first: a.first, last: b.last })
    } else if b.first >= a.first && b.first < a.last {
        Some(Range { first: b.first, last: a.last })
    } else {
        None
    }
}

fn solve1(s: &str) -> i64 {
    let seeds = s.split("\n\n").nth(0).unwrap().split_whitespace().skip(1).array_chunks::<2>().map(|[s, l]| {
        let a = s.trim().parse::<i64>().unwrap();
        Range {
            first: a,
            last: a + l.trim().parse::<i64>().unwrap()
        }
    }).collect::<Vec<Range>>();

    let maps: Vec<Vec<Vec<i64>>> = s.split("\n\n").skip(1).map(|m| {
        m.lines().skip(1).map(|l| {
            l.split_whitespace().filter_map(|w| {
                w.trim().parse::<i64>().ok()
            }).collect()
        }).collect()
    }).collect();


    let mut lows = seeds;
    for mp in maps {
        let mut res = Vec::new();

        for e in mp {
            let r = Range { first: e[1], last: e[1] + e[2] };
            for sr in &lows {
                if let Some(inter) = overlap(&sr, &r) {
                    res.push(Range {
                        first: e[0] - e[1] + inter.first,
                        last:  e[0] - e[1] + inter.last
                    });
                }           
            }
        }
        lows = res;
    }

    lows.sort();
    println!("{:?}", lows);
    lows[0].first
}
