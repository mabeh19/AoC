package aoc

import "core:os"
import "core:thread"
import "core:fmt"
import dll "core:dynlib"


Solution :: struct {
    solve1: proc(sol: ^Solution, s: string) -> string,
    solve2: proc(sol: ^Solution, s: string) -> string,
}


main :: proc() 
{
    threads := [dynamic]^thread.Thread{}

    for i in 1 ..= 24 {
        day := new(Solution)
        defer free(day)

        dir := fmt.aprintf("day%v", i)
        defer delete(dir)

        dll_path := fmt.aprintf("%v/day%v.dll", dir, i)
        defer delete(dll_path)

        test_path := fmt.aprintf("%v/test.txt", dir)
        defer delete(test_path)

        input_path := fmt.aprintf("%v/input.txt", dir)
        defer delete(input_path)

        count, ok := dll.initialize_symbols(day, dll_path)

        if !ok {
            continue
        }
        
        test_data, _ := os.read_entire_file(test_path)
        input_data, _ := os.read_entire_file(input_path)

        t := thread.create_and_start_with_poly_data3(day, transmute(string)test_data, transmute(string)input_data, proc(day: ^Solution, test_data: string, input_data: string) {
            res := day->solve1(test_data)
            fmt.println(res)
            res = day->solve2(test_data)
            fmt.println(res)
            res = day->solve1(input_data)
            fmt.println(res)
            res = day->solve2(input_data)
            fmt.println(res)
        })
        append(&threads, t)
    }

    thread.join_multiple(..threads[:])
}
