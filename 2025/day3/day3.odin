package day3

import "core:fmt"
import "core:strings"
import "core:strconv"
import "core:time"

import aoc "../../"

input : string : #load("input.txt")
test_input : string : #load("test1.txt")

TEST1_EXPECTED :: 357
TEST2_EXPECTED :: 3121910778619

parse :: proc(s: string) -> []string
{
    return aoc.lines(s)
}

main :: proc()
{
    fmt.println("2025 Day 3 - Lobby")

    input_parsed := parse(input)

    t1 := time.now()

    p1 := part1(input_parsed)
    p2 := part2(input_parsed)

    t2 := time.now()

    fmt.println("Time:", time.diff(t1, t2))
    fmt.println("Part 1:", p1)
    fmt.println("Part 2:", p2)
}

joltage :: proc($NUMS: int, banks: []string) -> int
{
    total := 0

    for bank in banks {
        current_num := 0
        scores := [NUMS]u8{}

        for i in 0 ..< len(bank) {
            for num in current_num ..< NUMS {
                if bank[i] > scores[num] {
                    scores[num] = bank[i]

                    for &s in scores[num + 1:] {
                        s = 0
                    }

                    break
                }
            }

            if i >= len(bank) - (NUMS - current_num) {
                current_num += 1
            }
        }

        total += aoc.get_int(string(scores[:]))
    }

    return total
}

part1 :: proc(inp: []string) -> int
{
    return joltage(2, inp)
}

part2 :: proc(inp: []string) -> int
{
    return joltage(12, inp)
}

import "core:testing"

@test
test_simple :: proc(t: ^testing.T)
{
    testing.expect_value(t, part1(parse("879")), 89)
}

@test
test_ex1 :: proc(t: ^testing.T)
{
    testing.expect_value(t, part1(parse("987654321111111")), 98)
}

@test
test_ex2 :: proc(t: ^testing.T)
{
    testing.expect_value(t, part1(parse("811111111111119")), 89)
}

@test
test_ex3 :: proc(t: ^testing.T)
{
    testing.expect_value(t, part1(parse("234234234234278")), 78)
}

@test
test_ex4 :: proc(t: ^testing.T)
{
    testing.expect_value(t, part1(parse("818181911112111")), 92)
}

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
test_ex21 :: proc(t: ^testing.T)
{
    testing.expect_value(t, part2(parse("987654321111111")), 987654321111)
}

@test
test_ex22 :: proc(t: ^testing.T)
{
    testing.expect_value(t, part2(parse("811111111111119")), 811111111119)
}

@test
test_ex23 :: proc(t: ^testing.T)
{
    testing.expect_value(t, part2(parse("234234234234278")), 434234234278)
}

@test
test_ex24 :: proc(t: ^testing.T)
{
    testing.expect_value(t, part2(parse("818181911112111")), 888911112111)
}
