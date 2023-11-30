package day8

import "core:fmt"
import "core:testing"
import "core:strings"

import aoc "../.."


main :: proc() {
        input := aoc.read_input()

        diff := 0

        for line in aoc.lines(input) {
                if len(line) < 1 do continue
                diff += len(encode_string(line)) - count_code_chars(line)//diff_in_size(line)
        }

        fmt.println(diff)
}

diff_in_size :: proc(str: string) -> int {
        return count_code_chars(str) - count_mem_chars(str)
}

count_code_chars :: proc(str: string) -> int {
        return len(str)
}

count_mem_chars :: proc(str: string) -> int {
        count := 0
        for i := 0; i < len(str); i += 1 {
                if str[i] == '\\' {
                        if str[i + 1] == 'x' {
                                i += 3
                        } else {
                                i += 1
                        }
                }
                count += 1
        }

        return count - 2
}


encode_string :: proc(str: string) -> string {
        builder := strings.builder_make()
        strings.write_rune(&builder, '"')
        for i := 0; i < len(str); i += 1 {
                if str[i] == '\\' || str[i] == '"' {
                        strings.write_rune(&builder, '\\')
                }
                strings.write_rune(&builder, rune(str[i]))
        }
        strings.write_rune(&builder, '"')

        return strings.to_string(builder)
}


@test
examples :: proc(t: ^testing.T) {
        ex1 := `""`
        ex2 := `"abc"`
        ex3 := `"aaa\"aaa"`
        ex4 := `"\x27"`

        testing.expect(t, count_code_chars(ex1) == 2)
        testing.expect(t, count_mem_chars(ex1) == 0)
        testing.expect(t, count_code_chars(ex2) == 5)
        testing.expect(t, count_mem_chars(ex2) == 3)
        testing.expect(t, count_code_chars(ex3) == 10)
        testing.expect(t, count_mem_chars(ex3) == 7)
        testing.expect(t, count_code_chars(ex4) == 6)
        testing.expect(t, count_mem_chars(ex4) == 1)
        testing.expect(t, diff_in_size(ex1) == 2)
        testing.expect(t, diff_in_size(ex2) == 2)
        testing.expect(t, diff_in_size(ex3) == 3)
        testing.expect(t, diff_in_size(ex4) == 5)

        testing.expect(t, len(encode_string(ex1)) == 6)
        testing.expect(t, len(encode_string(ex2)) == 9)
        testing.expect(t, len(encode_string(ex3)) == 16)
        testing.expect(t, len(encode_string(ex4)) == 11)
}
