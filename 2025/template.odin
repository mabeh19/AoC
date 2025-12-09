package dayTODAY

import "core:fmt"
import "core:strings"
import "core:strconv"
import "core:time"

import aoc "../../"

input : string : #load("input.txt")
test_input : string : #load("test1.txt")

ParsedInput :: struct {}

TEST1_EXPECTED :: 0
TEST2_EXPECTED :: 0


parse :: proc(s: string) -> ParsedInput
{
}

main :: proc()
{
    fmt.println("2025 Day TODAY - TITLE")

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
    return 0
}

part2 :: proc(inp: ParsedInput) -> int
{
    return 0
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
