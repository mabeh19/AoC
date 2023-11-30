package day10

import "core:fmt"
import "core:testing"
import "core:strings"
import aoc "../.."

main :: proc() {
        input := "1321131112"
        for _ in 0 ..< 50 {
                input = generate_sequence(input)
        }
        fmt.println(len(input))
}

generate_sequence :: proc(seq: string) -> string {
        builder := strings.builder_make()
        for i := 0; i < len(seq); i += 1 {
                count := 0
                start := seq[i]
                for c in seq[i:] {
                        if c != rune(start) {
                                break
                        }
                        count += 1
                }
                strings.write_int(&builder, count)
                strings.write_rune(&builder, rune(start))
                i += count - 1
        }

        return strings.to_string(builder)
}

@test
examples :: proc(t: ^testing.T) {
        ex1 := generate_sequence("1")
        ex2 := generate_sequence(ex1)
        ex3 := generate_sequence(ex2)
        ex4 := generate_sequence(ex3)
        ex5 := generate_sequence(ex4)

        fmt.println(ex1)
        fmt.println(ex2)
        fmt.println(ex3)
        fmt.println(ex4)
        fmt.println(ex5)

        testing.expect(t, ex1 == "11")
        testing.expect(t, ex2 == "21")
        testing.expect(t, ex3 == "1211")
        testing.expect(t, ex4 == "111221")
        testing.expect(t, ex5 == "312211")
}
