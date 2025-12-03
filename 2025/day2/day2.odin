package day2

import "core:fmt"
import "core:strings"
import "core:strconv"
import "core:time"

import aoc "../../"

input : string : #load("input.txt")
test_input : string : #load("test1.txt")

Range :: struct { start, end : int }

parse :: proc(s: string) -> []Range
{
    idranges := strings.split(s, ",")
    defer delete(idranges)

    ranges := make([]Range, len(idranges))

    for idrange, i in idranges {
        if idrange == "" do continue
        parts := strings.split(idrange, "-")
        defer delete(parts)
        ranges[i] = {
            start = aoc.get_int(parts[0]),
            end   = aoc.get_int(parts[1])
        }
    }

    return ranges[:]
}

main :: proc()
{
    input_parsed := parse(input)

    t1 := time.now()

    p1 := part1(input_parsed)
    p2 := part2(input_parsed)

    t2 := time.now()

    fmt.println("Time:", time.diff(t1, t2))
    fmt.println("Part 1:", p1)
    fmt.println("Part 2:", p2)
}

part1 :: proc(inp: []Range) -> int
{
    buf := [100]u8{}
    sum := 0

    for range in inp {
        for id in range.start ..= range.end {
            idstr := strconv.write_int(buf[:], i64(id), 10)
            halflen := len(idstr) / 2
            if idstr[:halflen] == idstr[halflen:] {
                sum += id
            }
        }
    }

    return sum
}


part2 :: proc(inp: []Range) -> int
{
    buf := [100]u8{}
    sum := 0

    for range in inp {
        for id in range.start ..= range.end {
            idstr := strconv.write_int(buf[:], i64(id), 10)

            pattern_loop:
            for pattern_len := len(idstr) / 2; pattern_len >= 1; pattern_len -= 1 {
                if len(idstr) % pattern_len != 0 do continue

                pattern := idstr[:pattern_len]

                for i in pattern_len ..< len(idstr) {
                    if idstr[i] != pattern[i % pattern_len] {
                        continue pattern_loop
                    }
                }

                sum += id
                break
            }
        }
    }

    return sum
}

import "core:testing"

@test
test1 :: proc(t: ^testing.T)
{
    testing.expect_value(t, part1(parse(test_input)), 1227775554)
}

@test
test2 :: proc(t: ^testing.T)
{
    testing.expect_value(t, part2(parse(test_input)), 4174379265)
}
