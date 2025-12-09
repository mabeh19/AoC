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

        fixed_r := adjust_overlapping(fresh[:], r)

        append(&fresh, fixed_r)
    }

    remove_equal_ranges(&fresh)

    for id in aoc.lines(avail) {
        if id == "" do continue
        append(&avail_ids, aoc.get_int(id))
    }

    return {
        fresh = fresh[:],
        available = avail_ids[:]
    }
}

inside :: proc(a, b: Range) -> bool
{
    lower_inside := false
    upper_inside := false

    if b.low <= a.low &&
       b.high >= a.low {
        lower_inside = true
    }

    // upper bound is inside other range
    if b.low <= a.high &&
       b.high >= a.high {
        upper_inside = true
    }

    return lower_inside && upper_inside
}

remove_equal_ranges :: proc(ranges: ^[dynamic]Range)
{
    i := 0
    for i < len(ranges) {
        ranges_removed := false
        r := ranges[i]

        j := 0
        for j < len(ranges) {
            if i == j {
                j += 1
                continue
            }

            range := ranges[j]

            if inside(range, r) {
                // range is inside r, remove range
                unordered_remove(ranges, j)
                ranges_removed = true
                continue
            }
            else if inside(r, range) {
                // r is inside range, remove r
                unordered_remove(ranges, i)
                ranges_removed = true
                continue
            }

            j += 1
        }

        if ranges_removed {
            i = 0
        }
        else {
            i += 1
        }
    }
}

adjust_overlapping :: proc(ranges: []Range, new_range: Range) -> Range
{
    r := new_range

    for range in ranges {
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

    return r
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


@test
test4 :: proc(t: ^testing.T)
{
    inp := 
`6-13
13-14
14-18
2-20

1
2`
    EXPECTED :: 1 + 20 - 2
    testing.expect_value(t, part2(parse(inp)), EXPECTED)
}

@test
test5 :: proc(t: ^testing.T)
{
    inp := 
`7-12
5-8
10-14

1
2`
    EXPECTED :: 1 + 14 - 5
    testing.expect_value(t, part2(parse(inp)), EXPECTED)
}
