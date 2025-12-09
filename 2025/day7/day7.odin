package day7

import "core:fmt"
import "core:strings"
import "core:strconv"
import "core:slice"
import "core:time"

import aoc "../../"

input : string : #load("input.txt")
test_input : string : #load("test1.txt")

Point :: [2]int

ParsedInput :: struct {
    start: Point,
    end: int,
    splitters: []Point
}

TEST1_EXPECTED :: 21
TEST2_EXPECTED :: 40


parse :: proc(s: string) -> ParsedInput
{
    start := Point{}
    splitters := [dynamic]Point{}
    end := 0

    for line, y in aoc.lines(s) {
        for sym, x in line {
            if sym == 'S' {
                start = {x, y}
            }
            else if sym == '^' {
                append(&splitters, Point{x,y})
            }
        }

        end = y + 1
    }

    return {start, end - 1, splitters[:]}
}

main :: proc()
{
    fmt.println("2025 Day 7 - Laboratories")

    input_parsed := parse(input)

    t1 := time.now()

    p1 := part1(input_parsed)
    p2 := part2(input_parsed)

    t2 := time.now()

    fmt.println("Time:", time.diff(t1, t2))
    fmt.println("Part 1:", p1)
    fmt.println("Part 2:", p2)
}

part1 :: proc(inp: ParsedInput) -> int
{
    image := [150][150]u8{ 0 ..< 150 = [150]u8{ 0 ..< 150 = ' '}}
    beams := make([dynamic]Point)
    append(&beams, inp.start)
    splits := 0

    for len(beams) > 0 {
        i := 0
        for i < len(beams) {
            beam := &beams[i]
            beam.y += 1

            if slice.contains(inp.splitters, beam^) {
                beam := beam^
                image[beam.y][beam.x] = '^'
                // hit splitter
                splits += 1
                unordered_remove(&beams, i)
                left := Point{beam.x - 1, beam.y}
                right := Point{beam.x + 1, beam.y}

                if image[left.y][left.x] != '|' {
                    append(&beams, left)
                }

                if image[right.y][right.x] != '|' {
                    append(&beams, right)
                }
            }
            else if image[beam.y][beam.x] == '|' {
                // coalesce with other beam
                unordered_remove(&beams, i)
            }
            else if beam.y >= inp.end {
                unordered_remove(&beams, i)
            }
            else {
                if  beam.x >= 0 && beam.x < len(image[0]) &&
                    beam.y >= 0 && beam.y < len(image) {
                    image[beam.y][beam.x] = '|'
                }
                i += 1
            }
        }
    }

    return splits
}

timeline :: proc(memo: ^map[Point]int, inp: ParsedInput, pos: Point) -> int
{
    pos := pos
    if pos in memo {
        return memo[pos]
    }

    for !slice.contains(inp.splitters, pos) &&
        pos.y < inp.end {
        pos.y += 1

        if pos in memo {
            return memo[pos]
        }
    }

    if pos.y == inp.end {
        return 1
    }

    // split beam    
    left := Point{pos.x - 1, pos.y}
    right := Point{pos.x + 1, pos.y}

    memo[left] = timeline(memo, inp, left)
    memo[right] = timeline(memo, inp, right)

    return memo[left] + memo[right]
}

part2 :: proc(inp: ParsedInput) -> int
{
    memo := map[Point]int{}

    return timeline(&memo, inp, inp.start)
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
