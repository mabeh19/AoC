package aoc

import "core:os"
import "core:strings"
import "core:strconv"
import "core:unicode"


foreign {
        strtoll :: proc(cstring, ^cstring, int) -> int ---
}


lines :: proc(str: string) -> []string {
        return strings.split(str, "\n")
}


words :: proc(str: string) -> []string {
        return strings.split(str, " ")
}

get_int :: proc(str: string) -> int {
        return strconv.atoi(str)
}

get_f32 :: proc(str: string) -> f32 {
        return f32(strconv.atof(str))
}

copy_map :: proc(m: map[$T1]$T2) -> map[T1]T2 {
        new_map := make(map[T1]T2)

        for key, val in m {
                new_map[key] = val
        }

        return new_map
}

get_nth_int :: proc(str: string, n: int) -> int { 
        ptr := str
        prev_ptr := ""
        num := 0
        for _ in 0 ..< n {
                num, ptr = next_number(ptr)
                if ptr == prev_ptr {
                        break
                }
                prev_ptr = ptr
        }
        return num
}

next_number :: proc(str: string) -> (number: int, rest_of_string: string) {
        nptr : cstring
        i := 0
        for i = 0; i < len(str); i += 1 {
                if unicode.is_digit(rune(str[i])) || str[i] == '-' {
                        break
                }
        }
        intptr := strings.clone_to_cstring(str[i:])
        number = strtoll(intptr, &nptr, 10)
        
        return number, string(nptr)
}

