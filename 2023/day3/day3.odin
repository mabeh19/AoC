package aoc_day

import "core:fmt"
import "core:strings"
import "core:strconv"
import "core:math"
import "core:unicode"
import "core:testing"

import aoc "../.."


Schematic :: []string

Numbers :: map[int]int

main :: proc() {
        input := #load("input.txt", string)
        sch := parse_schematic(input)
        nums := get_adjacent_numbers(sch)
        fmt.println(sum_nums(nums))
}

parse_schematic :: proc(str: string) -> Schematic {
        return aoc.lines(str)
}


is_symbol :: proc(c: rune) -> bool {
        return !unicode.is_digit(c) && c != '.'
}

get_adjacent_numbers :: proc(sch: Schematic) -> Numbers {
        Coord :: struct {
                r,c: int
        }
        visited := map[Coord]bool{}
        nums := Numbers{}
        for line, r in sch {
                for sym, c in line {
                        if !is_symbol(sym) do continue
                        for i := r-1; i <= r+1; i += 1 {
                                for j := c-1; j <= c+1; j += 1 {
                                        if i < 0 || j < 0 || i > len(sch) - 1 || j > len(sch[i]) - 1 {
                                                continue
                                        }
                                        switch sch[i][j] {
                                        case '0' ..= '9':
                                                if visited[Coord{i,j}] do continue
                                                num := get_num(sch, i, j)
                                                fmt.println("Found ", num, " at (", i, ",", j, ")")
                                                n := nums[num]
                                                n += 1
                                                nums[num] = n
                                                visited[Coord{i,j}] = true
                                        case:
                                        }
                                }
                        }
                }
        }

        return nums
}


get_num :: proc(sch: Schematic, r, c: int) -> int {
        start := c
        end := c
        locate_first_digit: 
        for ; start > 0; start -= 1 {
                switch sch[r][start-1] {
                case '0' ..= '9':
                case:
                        break locate_first_digit
                }
        }
        locate_end_digit:
        for ; end < len(sch[r]) - 1; end += 1 {
                switch sch[r][end+1] {
                case '0' ..= '9':
                case:
                        break locate_end_digit
                }
        }
        

        return aoc.get_int(sch[r][start:end+1])
}

sum_nums :: proc(nums: Numbers) -> int {
        sum := 0
        for num, count in nums {
                sum += num * count
        }
        return sum
}


@test
examples :: proc(t: ^testing.T) {
        input := #load("test.txt", string)
        sch := parse_schematic(input)
        nums := get_adjacent_numbers(sch)
        fmt.println(nums)
        fmt.println(sum_nums(nums))
}
