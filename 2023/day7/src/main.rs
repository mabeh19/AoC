use std::collections::HashMap;
use std::cmp::Ordering;
use std::time;
use phf::phf_map;

#[derive(Debug, Eq, Ord, PartialEq, PartialOrd)]
enum HandType {
    FiveOfAKind,
    FourOfAKind,
    FullHouse,
    ThreeOfAKind,
    TwoPair,
    OnePair,
    HighCard
}

#[derive(Debug)]
struct Player {
    hand: [char; 5],
    bid: usize,
    handtype : HandType
}


const MAPPING: phf::Map<char, i32> = phf_map! {
    'A' => 14,
    'K' => 13,
    'Q' => 12,
    'J' =>  0,
    'T' => 10,
    '9' =>  9,
    '8' =>  8,
    '7' =>  7,
    '6' =>  6,
    '5' =>  5,
    '4' =>  4,
    '3' =>  3,
    '2' =>  2,
};

fn main() {
    let tst = include_str!("../test.txt");
    let inp = include_str!("../input.txt");

    let start = time::Instant::now();
    let mut game = inp.lines().map(|l| {
        let (hand, bid) = l.split_once(" ").unwrap();
        let mut player = Player{
            hand: ['0'; 5],
            bid: 0,
            handtype: HandType::HighCard
        };
        for (i, c) in hand.chars().enumerate() {
            player.hand[i] = c;
        }
        player.bid = bid.parse().unwrap();
        player.handtype = get_handtype(&player.hand);

        player
    }).collect::<Vec<Player>>();
    game.sort_by(|a, b| {
        cmp_hand(&a.hand, &b.hand)
    });
    let ans = game.iter().enumerate().fold(0, |acc, (i, p)| {
        acc + p.bid * (i + 1)
    });
    let end = start.elapsed();
    println!("ans: {ans}, time: {} us", (end.as_nanos() as f32) / 1000.);
}


fn get_handtype(s: &[char]) -> HandType {
    let mut hm = HashMap::new();
    let mut jokers = 0;

    for i in 0..s.len() {
        if s[i] == 'J' {
            jokers += 1;
            continue;
        }
        if !hm.contains_key(&s[i]) {
            hm.insert(&s[i], 0);
        }
        let c = hm[&s[i]];
        hm.insert(&s[i], c + 1); 
    }

    if hm.len() == 1 {
        return HandType::FiveOfAKind;
    }

    if jokers > 0 {
        let mut h = [' '; 5];
        for i in 0..5 {
            h[i] = s[i];
        }
        let h = get_highest_value_hand(jokers, h);
        return get_handtype(&h);
    }

    if hm.len() == 5 {
        return HandType::HighCard;
    }

    if hm.len() == 2 {
        for (_, v) in &hm {
            if *v == 4 {
                return HandType::FourOfAKind;
            } 
            if *v == 3 {
                return HandType::FullHouse;
            }
        }
    }

    let mut pairs = 0;
    for (_,v) in &hm {
        if *v == 3 {
            return HandType::ThreeOfAKind;
        }

        if *v == 2 {
            pairs += 1;
        }
    }

    match pairs {
        1 => HandType::OnePair,
        2 => HandType::TwoPair,
        _ => HandType::HighCard
    }
}

fn get_highest_value_hand(jokers: i32, hand: [char; 5]) -> [char; 5] {
    let mut hand = hand;
    let mut best_hand = hand;
    let mut first = true;
    for (c, v) in &MAPPING {
        if *c == 'J' {
            continue;
        }
        for _ in 0 .. jokers {
            let newhand: Vec<char> = hand.iter().map(|hc| if *hc == 'J' { *c } else { *hc }).collect();
            if first || cmp_hand(&best_hand, &newhand).is_lt() {
                for i in 0..5 {
                    best_hand[i] = newhand[i];
                }
                first = false;
            }
        }
    }

    best_hand
}

fn cmp_hand(h1: &[char], h2: &[char]) -> Ordering { 
    let h1_ht = get_handtype(h1);
    let h2_ht = get_handtype(h2);
    if h1_ht == h2_ht {
        h1.iter().map(|c| MAPPING[&c]).cmp(h2.iter().map(|c| MAPPING[&c]))
    } else {
        h2_ht.cmp(&h1_ht)
    }
}
