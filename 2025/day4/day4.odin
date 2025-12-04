package day4

import "core:fmt"
import "core:strings"
import "core:strconv"
import "core:time"

import aoc "../../"

input : string : #load("input.txt")
test_input : string : #load("test1.txt")

ParsedInput :: []string

TEST1_EXPECTED :: 13
TEST2_EXPECTED :: 43

PAPER :: '@'

parse :: proc(s: string) -> ParsedInput
{
    return aoc.lines(strings.clone(s))
}

main :: proc()
{
    fmt.println("2025 Day 4 - Printing Department")

    input_parsed := parse(input)

    t1 := time.now()

    p1 := part1(input_parsed)
    p2 := part2(input_parsed)

    t2 := time.now()

    fmt.println("Time:", time.diff(t1, t2))
    fmt.println("Part 1:", p1)
    fmt.println("Part 2:", p2)
}

get_adjacent_paper :: proc(m: []string, point: [2]int) -> int
{
    num_neighbours := 0
    
    adjacent := [][2]int {
        {1, 0},
        {0, 1},
        {-1, 0},
        {0, -1},
        {1, 1},
        {-1, 1},
        {1, -1},
        {-1, -1}
    }

    for dir in adjacent {
        n := point + dir

        // bounds check
        if  n.y < 0 || n.y >= len(m) ||
            n.x < 0 || n.x >= len(m[0]) {
            continue
        }

        if m[n.y][n.x] == PAPER {
            num_neighbours += 1
        }
    }

    return num_neighbours
}

part1 :: proc(inp: ParsedInput) -> int
{
    LIMIT :: 4

    accessable := 0

    for line, y in inp {
        for p, x in line {
            if p == PAPER { 
                if get_adjacent_paper(inp, {x, y}) < LIMIT {
                    accessable += 1
                }
            }
        }
    }

    return accessable
}

part2 :: proc(inp: ParsedInput) -> int
{
    LIMIT :: 4

    accessable := 0
    updated := true

    for updated {
        updated = false
        for &line, y in inp {
            lc := transmute([]u8)line
            for p, x in line {
                if p == PAPER {
                    ns := get_adjacent_paper(inp, {x, y})

                    if ns < LIMIT {
                        accessable += 1
                        lc[x] = 'x'
                        updated = true
                    }
                }
            }
        }
    }

    return accessable
}

import "core:testing"

@test
test1 :: proc(t: ^testing.T)
{
    testing.expect_value(t, part1(parse(test_input)), TEST1_EXPECTED)
}

@test
test2 :: proc(t: ^testing.T)
{
    testing.expect_value(t, part2(parse(test_input)), TEST2_EXPECTED)
}
