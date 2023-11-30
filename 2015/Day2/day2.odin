package day2

import "core:fmt"
import "core:strings"
import "core:strconv"
import "core:os"
import "core:testing"

Present :: [3]int

main :: proc() {
        total_area := 0
        total_ribbon := 0
        f, _ := os.open("input.txt")
        defer os.close(f)
        f_inp, _ := os.read_entire_file(f)
        input := string(f_inp)
        for line in strings.split(input, "\n") {
                if len(line) < 3 {
                        continue
                }
                fmt.println(line)
                total_area += surface_area(dim_to_present(line))
                total_ribbon += ribbon_required(dim_to_present(line))
        }


        fmt.println(total_area)
        fmt.println(total_ribbon)
}

dim_to_present :: proc(dim: string) -> Present {
        dims := strings.split(dim, "x")
        return Present {
                int(strconv.atof(dims[0])),
                int(strconv.atof(dims[1])),
                int(strconv.atof(dims[2])),
        }
}

surface_area :: proc(present: Present) -> int {
        return 2 * present.x * present.y + 2 * present.y * present.z + 2 * present.z * present.x + smallest_area(present)
}

volume :: proc(present: Present) -> int {
        return present.x * present.y * present.z
}

smallest_side :: proc(present: Present) -> (int, int) {
        smallest := min(min(present.x, present.y), present.z)
        next_smallest := 1
        min_xy := min(present.x, present.y)
        min_xz := min(present.x, present.z)
        min_yz := min(present.y, present.z)

        switch smallest {
        case present.x: next_smallest = min_yz
        case present.y: next_smallest = min_xz
        case present.z: next_smallest = min_xy
        }
        
        return smallest, next_smallest
}

smallest_area :: proc(present: Present) -> int {
        smallest, next_smallest := smallest_side(present)
        return smallest * next_smallest
}

smallest_perimeter :: proc(present: Present) -> int {
        smallest, next_smallest := smallest_side(present)
        return 2 * smallest + 2 * next_smallest
}

ribbon_required :: proc(present: Present) -> int {
        return smallest_perimeter(present) + volume(present)
}



@test
examples :: proc(t: ^testing.T) {
        ex1 := dim_to_present("2x3x4")
        ex2 := dim_to_present("1x1x10")

        testing.expect(t, surface_area(ex1) == 58)
        testing.expect(t, surface_area(ex2) == 43)
        testing.expect(t, ribbon_required(ex1) == 34)
        testing.expect(t, ribbon_required(ex2) == 14)
}
