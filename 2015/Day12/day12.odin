package day12

import "core:fmt"
import "core:testing"
import "core:strings"
import "core:encoding/json"
import aoc "../.."



main :: proc() {
        input := aoc.read_input()
        
        sum := 0
        val, _ := json.parse(transmute([]u8)input)
        for {
                sum += parse_value(val)
        }

        fmt.println(sum)
}


sum_of_nums :: proc(str: string) -> int {
        ptr := str
        prev_ptr := ""
        sum := 0
        num := 0
        for {
                num, ptr = aoc.next_number(ptr)
                if ptr == prev_ptr {
                        break
                }
                sum += num
                prev_ptr = ptr
        }

        return sum
}

parse_value :: proc(val: json.Value) -> int {
        switch v in val {
        case Null:
                return 0
        case f64:
                return int(v)
        case i64:
                return v
        case bool:
                return 0
        case string:
                return 0
        }
}


@test
examples :: proc(t: ^testing.T) {
        ex1 := "[1,2,3]"
        ex2 := `{"a":{"b":4},"c":-1}`
        ex3 := `{"a":[-1,1]}`
        ex4 := `[-1,{"a":1}]`
        ex5 := `[]`
        ex6 := `{}`
        ex7 := `{"a":2,"b":4}`
        testing.expect(t, sum_of_nums(ex1) == 6)
        testing.expect(t, sum_of_nums(ex2) == 3)
        testing.expect(t, sum_of_nums(ex3) == 0)
        testing.expect(t, sum_of_nums(ex4) == 0)
        testing.expect(t, sum_of_nums(ex5) == 0)
        testing.expect(t, sum_of_nums(ex6) == 0)
        testing.expect(t, sum_of_nums(ex7) == 6)
}

