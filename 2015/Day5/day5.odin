package day

import "core:fmt"
import "core:os"
import "core:strings"
import "core:testing"


main :: proc() {
        f, _ := os.open("input.txt")
        defer os.close(f)
        f_inp, _ := os.read_entire_file(f)
        names := string(f_inp)
        nice_strings := 0

        for name in strings.split(names, "\n") {
                if len(name) < 1 do continue
                if is_nice(name) {
                        nice_strings += 1
                }
        }

        fmt.println(nice_strings)
}

is_nice :: proc(name: string) -> bool {
        return contains_pair(name) && contains_gapped_repeat(name)
}

contains_pair :: proc(name: string) -> bool {
        for _, i in name[0:len(name)-1] {
                if strings.count(name, name[i:i+2]) > 1 {
                        return true
                }
        }
        return false
}

contains_gapped_repeat :: proc(name: string) -> bool {
        for _, i in name[0:len(name)-2] {
                if name[i] == name[i+2] {
                        return true
                }
        }

        return false
}

@test
examples :: proc(t: ^testing.T) {
        ex1 := "qjhvhtzxzqqjkmpb"
        ex2 := "xxyxx"
        ex3 := "uurcxstgmygtbstg"
        ex4 := "ieodomkazucvgmuy"
        testing.expect(t, is_nice(ex1) == true)
        testing.expect(t, is_nice(ex2) == true)
        testing.expect(t, is_nice(ex3) == false)
        testing.expect(t, is_nice(ex4) == false)
}
