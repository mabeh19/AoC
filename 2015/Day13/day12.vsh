#!/usr/bin/env -S v

import math

type Name = string
type Prefs = map[Name]Person

struct Person {
mut:
    neighbours map[Name]int
}


fn arrange(prefs Prefs, person Name, mut arrangement []string) {
    arrangement << person
    mut happiest := ""
    mut happiest_num := int(math.min_i32)
    for neighbour, happiness in prefs[person].neighbours {
        if !(neighbour in arrangement) && mutual_happiness(prefs, person, neighbour) > happiest_num {
            happiest = neighbour
            happiest_num = happiness
        }
    }

    if happiest == "" {
        return
    }

    arrange(prefs, happiest, mut arrangement)
}

fn mutual_happiness(prefs Prefs, p1 Name, p2 Name) int {
    return prefs[p1].neighbours[p2] + prefs[p2].neighbours[p1]
}

fn sum_happiness(prefs Prefs, arrangement []string) int {
    mut sum := 0
    for i, name in arrangement {
        prev := if i == 0 { arrangement.len } else { i } - 1
        next := if i == arrangement.len - 1 { 0 } else { i + 1 }
        sum +=  //mutual_happiness(prefs, arrangement[prev], name) + 
                mutual_happiness(prefs, arrangement[next], name)
    }

    return sum
}

fn solve(file string) ! {
    input := read_file(file)!
    lines := input.split("\n")

    mut prefs := map[Name]Person{}

    for line in lines {
        toks := line.split(" ")
        if toks.len < 10 {
            continue
        }
        name := toks[0]
        happiness := toks[3].int() * if toks[2] == "lose" { -1 } else { 1 }
        neighbour := toks[10]#[..-1]
        mut pref := prefs[name]
        pref.neighbours[neighbour] = happiness
        prefs[name] = pref
    }


    mut highest := 0
    for name, _ in prefs {
        mut arrangement := []string{}
        arrange(prefs, name, mut arrangement)
        println(arrangement)
        sum := sum_happiness(prefs, arrangement)
        highest = if sum > highest { sum } else { highest }
    }

    println(highest)
}

args := args_before("")

if args.len > 1 && args[1] == "test" {
    solve("test.txt")!
} else {
    solve("input.txt")!
}
