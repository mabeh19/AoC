package day6

import "core:fmt"
import "core:strings"
import "core:strconv"
import "core:time"
import "core:slice"

import aoc "../../"

input : string : #load("input.txt")
test_input : string : #load("test1.txt")

ParsedInput :: []Problem

TEST1_EXPECTED :: 4277556
TEST2_EXPECTED :: 3263827

Problem :: struct {
    nums: [4]int,
    op: enum {ADD, MUL}
}


parse :: proc(s: string) -> ParsedInput
{
    problems := make([dynamic]Problem)
    lines := aoc.lines(s)
    nums := lines[:len(lines)-2]
    ops := lines[len(lines)-2]
    

    for line, j in nums {
        i := 0
        for num in aoc.words(line) {
            if num == "" do continue

            if i < len(problems) {
                p := &problems[i]

                p.nums[j] = aoc.get_int(num)
            }

            if i >= len(problems) {
                p := Problem {}

                p.nums[j] = aoc.get_int(num)
                append(&problems, p)
            }

            i += 1
        }
    }

    i := 0
    for op in aoc.words(ops) {
        if op == "" do continue
        problems[i].op = .MUL if op == "*" else .ADD
        i += 1
    }

    return problems[:]
}

parse2 :: proc(s: string) -> ParsedInput
{
    problems := make([dynamic]Problem)
    lines := aoc.lines(s)
    lines = lines[:len(lines)-1]

    if len(lines) == 4 {
        nums := [4]int{}
        num_idx := 0
        for cursor := len(lines[0]) - 1; cursor >= 0; cursor -= 1 {
            num := []u8{lines[0][cursor], lines[1][cursor], lines[2][cursor]}

            if string(num) == "   " || cursor == 0 {
                if cursor == 0 {
                    cursor = -1
                }
                p := Problem {
                    nums = nums,
                    op = .MUL if lines[3][cursor + 1] == '*' else .ADD
                }

                append(&problems, p)

                nums = {}
                num_idx = 0
            }
            else {
                nums[num_idx] = to_num(num[:])
                num_idx += 1
            }
        }
    }
    else if len(lines) == 5 {
        nums := [4]int{}
        num_idx := 0
        for cursor := len(lines[0]) - 1; cursor >= 0; cursor -= 1 {
            num := []u8{lines[0][cursor], lines[1][cursor], lines[2][cursor], lines[3][cursor]}

            if string(num) == "    " || cursor == 0 {
                if cursor == 0 {
                    nums[num_idx] = to_num(num[:])
                    cursor = -1
                }
                p := Problem {
                    nums = nums,
                    op = .MUL if lines[4][cursor + 1] == '*' else .ADD
                }

                append(&problems, p)

                nums = {}
                num_idx = 0
            }
            else {
                nums[num_idx] = to_num(num[:])
                num_idx += 1
            }
        }
    }

    return problems[:]
}

to_num :: proc(ns: []u8) -> int
{
    num := 0

    for n in ns {
        if n == ' ' do continue
        num *= 10
        num += int(n - '0')
    }

    return num
}

main :: proc()
{
    fmt.println("2025 Day 6 - Trash Compactor")

    input_parsed := parse(input)

    t1 := time.now()

    p1 := part1(parse(input))
    p2 := part2(parse2(input))

    t2 := time.now()

    fmt.println("Time:", time.diff(t1, t2))
    fmt.println("Part 1:", p1)
    fmt.println("Part 2:", p2)
}

part1 :: proc(inp: ParsedInput) -> int
{
    sum := 0

    for problem in inp {
        total := 1 if problem.op == .MUL else 0
        
        for num in problem.nums {
            if num == 0 do continue
            switch problem.op {
            case .MUL:
                total *= num
            case .ADD:
                total += num
            }
        }

        sum += total
    }

    return sum
}

part2 :: proc(inp: ParsedInput) -> int
{
    sum := 0

    for problem in inp {
        total := 1 if problem.op == .MUL else 0
        
        for num in problem.nums {
            if num == 0 do continue
            switch problem.op {
            case .MUL:
                total *= num
            case .ADD:
                total += num
            }
        }

        sum += total
    }

    return sum
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
    testing.expect_value(t, part2(parse2(test_input)), TEST2_EXPECTED)
}

@test
test3 :: proc(t: ^testing.T)
{
    inp := 
`1   
1   
12  
2 22
+
`
    EXPECTED :: 2 + 2 + 2 + 1112
    testing.expect_value(t, part2(parse2(inp)), EXPECTED)
}
