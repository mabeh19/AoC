package aoc_day

import "core:fmt"
import "core:strings"
import "core:strconv"
import "core:math"
import "core:unicode"
import "core:testing"

import aoc "../.."

DIGIT_AS_WORD := map[string]rune {
    "one"   = '1',
    "two"   = '2',
    "three" = '3',
    "four"  = '4',
    "five"  = '5',
    "six"   = '6',
    "seven" = '7',
    "eight" = '8',
    "nine"  = '9',
}

main :: proc() {
    input := aoc.read_input() 

    sum := 0
    for line in aoc.lines(input) {
        if len(line) < 1 do continue
        sum += aoc.get_int(recover(line))
    }

    fmt.println(sum)
}

get_all_digits :: proc(str: string) -> []rune {
    digits := [dynamic]rune{}
    for c, i in str {
        if unicode.is_digit(c) {
            append(&digits, c)
        } else {
            for word, num in DIGIT_AS_WORD {
                if i + len(word) <= len(str) {
                    if strings.compare(word, str[i:i+len(word)]) == 0 {
                        append(&digits, num)
                    }
                }
            }
        }
    }

    return digits[:]
}

recover :: proc(str: string) -> string {
    digits := get_all_digits(str)
    num1 := digits[0]
    num2 := digits[len(digits)-1]

    res := fmt.aprintf("%v%v", num1, num2)
    return res
}

@test
examples :: proc(t: ^testing.T) {
    ex1 := "1abc2"
    ex2 := "pqr3stu8vwx"
    ex3 := "a1b2c3d4e5f"
    ex4 := "treb7uchet"
    ex5 := "two1nine"
    ex6 := "eighttwothree"
    ex7 := "abcone2threexyz"
    ex8 := "xtwone3four"
    ex9 := "4nineeightseven2"
    ex10 := "zoneight234"
    ex11 := "7pqrstsixteen"

    testing.expect(t, aoc.get_int(recover(ex1)) == 12)
    testing.expect(t, aoc.get_int(recover(ex2)) == 38)
    testing.expect(t, aoc.get_int(recover(ex3)) == 15)
    testing.expect(t, aoc.get_int(recover(ex4)) == 77)
    testing.expect(t, aoc.get_int(recover(ex5)) == 29)
    testing.expect(t, aoc.get_int(recover(ex6)) == 83)
    testing.expect(t, aoc.get_int(recover(ex7)) == 13)
    testing.expect(t, aoc.get_int(recover(ex8)) == 24)
    testing.expect(t, aoc.get_int(recover(ex9)) == 42)
    testing.expect(t, aoc.get_int(recover(ex10)) == 14)
    testing.expect(t, aoc.get_int(recover(ex11)) == 76)
}
