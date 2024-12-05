#![feature(hash_extract_if)]
use std::{cmp::Ordering, collections::{HashMap, VecDeque}};

#[derive(Debug, Clone)]
struct PageRule {
    page: i32,
    is_before: Vec<i32>,
    is_after: Vec<i32>
}



fn main() {
    let test = include_str!("../test.txt");
    let input = include_str!("../input.txt");

    solve1(test);
    solve1(input);
}


fn solve1(input: &str) {
    let mut parts = input.split("\n\n");
    let ordering_rules = parts.nth(0).unwrap();
    let pages_to_produce = parts.nth(0).unwrap();

    let rules = parse_rules(ordering_rules);
    let pages = parse_pages(pages_to_produce);

    let correctly_ordered_updates = get_correctly_ordered_updates(rules, &pages);

    let total: i32 = correctly_ordered_updates.iter().map(|u| u[u.len() / 2]).sum();

    println!("total: {}", total);
}

fn parse_rules(input: &str) -> HashMap<i32, PageRule> {
    let mut page_map: HashMap<i32, PageRule> = HashMap::new();

    for line in input.lines() {
        let pages = line.split("|").collect::<Vec<&str>>();
        
        let first = pages[0].parse::<i32>().unwrap();
        let second = pages[1].parse::<i32>().unwrap();


        if let Some(page) = page_map.get_mut(&first) {
            page.is_before.push(second);
        }
        else {
            page_map.insert(first, PageRule {
                page: first,
                is_before: vec![second],
                is_after: vec![],
            });
        }

        if let Some(page) = page_map.get_mut(&second) {
            page.is_after.push(first);
        }
        else {
            page_map.insert(second, PageRule {
                page: second,
                is_before: vec![],
                is_after: vec![first]
            });
        }
    }

    println!("Rules: {:?}", page_map.iter().map(|(_, v)| v.is_after.len()).collect::<Vec<usize>>());

    page_map
}

fn parse_pages(input: &str) -> Vec<Vec<i32>> {
    input.lines().map(|line| line.split(",").map(|n| n.parse::<i32>().unwrap()).collect()).collect()
}

fn get_correctly_ordered_updates(rules: HashMap<i32, PageRule>, updates: &[Vec<i32>]) -> Vec<Vec<i32>> {
    let mut correct_pages = Vec::new();

    for pages in updates {
        let mut should_add = true;
        for i in 0..pages.len() {
            let rule = &rules[&pages[i]];
            
            let pages_before    = &pages[..i];
            let pages_after     = &pages[i..];
            let mut relevant_before = rule.is_after.iter().filter(|n| pages.contains(n));
            let mut relevant_after  = rule.is_before.iter().filter(|n| pages.contains(n));
            if !relevant_before.all(|n| pages_before.contains(n)) || !relevant_after.all(|n| pages_after.contains(n)) {
                should_add = false;
                break;
            }
        }

        if should_add {
            correct_pages.push(pages.clone());
        }
    }

    correct_pages
}


fn get_incorrectly_ordered_updates(rules: HashMap<i32, PageRule>, updates: &[Vec<i32>]) -> Vec<Vec<i32>> {
    let mut incorrect_pages = Vec::new();

    for pages in updates {
        let limited_rules = pages.iter()
                                 .map(|n| (*n, rules[n].clone()))
                                 .map(|(n, mut r)| {
                                     r.is_before = r.is_before.iter().filter(|x| pages.contains(x)).map(|x| *x).collect();
                                     r.is_after = r.is_after.iter().filter(|x| pages.contains(x)).map(|x| *x).collect();
                                     
                                     (n, r)
                                 }).collect::<HashMap<i32, PageRule>>();

        for i in 0..pages.len() {
            let rule = &limited_rules[&pages[i]];

            let pages_before    = &pages[..i];
            let pages_after     = &pages[i..];
            let mut relevant_before = rule.is_after.iter().filter(|n| pages.contains(n));
            let mut relevant_after  = rule.is_before.iter().filter(|n| pages.contains(n));
            if !relevant_before.all(|n| pages_before.contains(n)) || !relevant_after.all(|n| pages_after.contains(n)) {
                incorrect_pages.push(pages.clone());
                break;
            }
        }
    }

    incorrect_pages
}
