package day5

import "core:fmt"
import "core:strings"
import "core:strconv"
import "core:time"

import aoc "../../"

input : string : #load("input.txt")
test_input : string : #load("test1.txt")

Range :: struct { low, high : int }

ParsedInput :: struct {
    fresh: []Range,
    available: []int
}

TEST1_EXPECTED :: 3
TEST2_EXPECTED :: 14


parse :: proc(s: string) -> ParsedInput
{
    chunks := aoc.chunks(s)
    ranges := chunks[0]
    avail := chunks[1]

    fresh := [dynamic]Range{}
    avail_ids := [dynamic]int{}

    for range in aoc.lines(ranges) {
        if range == "" do continue
        nums := strings.split(range, "-")
        r := Range {
            low = aoc.get_int(nums[0]),
            high = aoc.get_int(nums[1])
        }

        fixed_r, skip := remove_overlapping(fresh[:], r)

        if skip do continue
        append(&fresh, fixed_r)
    }

    for id in aoc.lines(avail) {
        if id == "" do continue
        append(&avail_ids, aoc.get_int(id))
    }

    return {
        fresh = fresh[:],
        available = avail_ids[:]
    }
}

remove_overlapping :: proc(ranges: []Range, new_range: Range) -> (r: Range, skip: bool)
{
    r = new_range
    for range in ranges {
        // is contained inside other range, skip
        if range.low <= new_range.low && range.high >= new_range.high {
            skip = true
            break
        }
        // lower bound is inside other range
        if range.low <= new_range.low &&
           range.high >= new_range.low {
            r.low = range.high + 1
        }

        // upper bound is inside other range
        if range.low <= new_range.high &&
           range.high >= new_range.high {
            r.high = range.low - 1
        }
    }

    return
}

main :: proc()
{
    fmt.println("2025 Day 5 - Cafeteria")


    t1 := time.now()
    
    input_parsed := parse(input)

    p1 := part1(input_parsed)
    p2 := part2(input_parsed)

    t2 := time.now()

    fmt.println("Time:", time.diff(t1, t2))
    fmt.println("Part 1:", p1)
    fmt.println("Part 2:", p2)
}

part1 :: proc(inp: ParsedInput) -> int
{
    fresh := 0

    for id in inp.available {
        for range in inp.fresh {
            if range.low <= id && id <= range.high {
                fresh += 1
                break
            }
        }
    }

    return fresh
}

part2 :: proc(inp: ParsedInput) -> int
{
    fresh := 0

    for range in inp.fresh {
        fresh += 1 + range.high - range.low
    }

    return fresh
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

@test
test3 :: proc(t: ^testing.T)
{
    inp := 
`14-18
6-14
12-17
6-14
18-20

1
2`
    EXPECTED :: 1 + 20 - 6
    testing.expect_value(t, part2(parse(inp)), EXPECTED)
}
