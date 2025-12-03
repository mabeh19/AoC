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
    fmt.println("2025 Day 3 - Gift Shop")

    input_parsed := parse(input)

    t1 := time.now()

    p1 := part1(input_parsed)
    p2 := part2(input_parsed)

    t2 := time.now()

    fmt.println("Time:", time.diff(t1, t2))
    fmt.println("Part 1:", p1)
    fmt.println("Part 2:", p2)
}

part1 :: proc(inp: []string) -> int
{
    total := 0
    for bank in inp {
        highestInBank := 0
        for leftCursor in 0 ..< len(bank) {
            for rightCursor in leftCursor + 1 ..< len(bank) {
                new_num := []u8{
                    bank[leftCursor],
                    bank[rightCursor]
                }
                val := aoc.get_int(string(new_num))

                if val > highestInBank {
                    highestInBank = val
                }
            }
        }

        total += highestInBank
    }

    return total
}

part2 :: proc(inp: []string) -> int
{
    total := 0

    part2_recurse :: proc(bank: string, buf: []u8, start: int, highest: int, rem: int) -> int {
        if rem == 0 {
            val := aoc.get_int(string(buf))

            if val > highest {
                return val
            } 
            else {
                return highest
            }
        }

        visited := make(map[u8]bool)
        local_highest := highest
        for i in 0..< len(buf) - start {
            idx := len(buf) - start - i
            if bank[idx] in visited {
                continue
            }

            buf[idx] = bank[idx]

            visited[bank[idx]] = true

            val := part2_recurse(bank, buf, idx + 1, local_highest, rem - 1)

            if val > local_highest {
                local_highest = val
            }
        }

        return local_highest
    }

    buf := [12]u8{}

    for bank in inp {
        highestInBank := part2_recurse(bank, buf[:], 0, 0, len(buf))

        total += highestInBank
    }

    return total
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
