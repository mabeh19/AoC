package day6


import "core:os"
import "core:fmt"
import "core:strings"
import "core:strconv"
import "core:testing"

GRID_SIZE :: 1_000

Grid :: [GRID_SIZE][GRID_SIZE]i8

Action :: enum {
        TurnOn,
        TurnOff,
        Toggle,
}

Instruction :: struct {
        action: Action,
        start_coord: [2]int,
        end_coord: [2]int,
}

main :: proc() {
        f, _ := os.open("input.txt")
        f_inp, _ := os.read_entire_file(f)
        input := string(f_inp)
        grid := new(Grid)
        for instr in strings.split(input, "\n") {
                if len(instr) < 1 do continue
                inst := parse_instr(instr)
                perform_instr(grid, inst)
        }

        fmt.println(count_lights(grid))
}


parse_instr :: proc(instr: string) -> Instruction {
        instruct := Instruction{}
        toks := strings.split(instr, " ")
        tok_idx := 0
        if toks[0] == "turn" {
                if toks[1] == "off" {
                        instruct.action = .TurnOff
                } else {
                        instruct.action = .TurnOn
                }
                tok_idx = 2
        } else {
                instruct.action = .Toggle
                tok_idx = 1
        }
        start_coords := strings.split(toks[tok_idx], ",")
        end_coords := strings.split(toks[tok_idx + 2], ",")
        instruct.start_coord.x = strconv.atoi(start_coords[0])
        instruct.start_coord.y = strconv.atoi(start_coords[1])
        instruct.end_coord.x   = strconv.atoi(end_coords[0])
        instruct.end_coord.y   = strconv.atoi(end_coords[1])

        return instruct
}

perform_instr :: proc(grid: ^Grid, instr: Instruction) {
        for i in instr.start_coord.x ..= instr.end_coord.x {
                for j in instr.start_coord.y ..= instr.end_coord.y {
                        switch instr.action {
                        case .TurnOn:
                                grid[i][j] += 1
                        case .TurnOff:
                                grid[i][j] = max(grid[i][j] - 1, 0)
                        case .Toggle:
                                grid[i][j] += 2
                        }
                }
        }
}


count_lights :: proc(grid: ^Grid) -> int {
        count := 0
        for row in grid {
                for c in row {
                        count += int(c)
                }
        }

        return count
}


@test
examples :: proc(t: ^testing.T) {
        ex1 := parse_instr("turn on 0,0 through 999,999")
        ex2 := parse_instr("toggle 0,0 through 999,0")
        ex3 := parse_instr("turn on 499,499 through 500,500")
        ex4 := parse_instr("turn on 0,0 through 0,0")
        ex5 := parse_instr("turn off 0,0 through 999,999")

        grid1 := new(Grid)
        grid2 := new(Grid)
        grid3 := new(Grid)
        grid4 := new(Grid)
        grid5 := new(Grid)
        defer { 
                free(grid1) 
                free(grid2) 
                free(grid3) 
                free(grid4)
                free(grid5)
        }
        perform_instr(grid1, ex1)
        perform_instr(grid2, ex2)
        perform_instr(grid3, ex3)
        perform_instr(grid4, ex4)
        perform_instr(grid5, ex5)
        testing.expect(t, count_lights(grid1) == GRID_SIZE * GRID_SIZE)
        testing.expect(t, count_lights(grid2) == 2000)
        testing.expect(t, count_lights(grid3) == 4)
        testing.expect(t, count_lights(grid4) == 1)
        cl := count_lights(grid5)
        fmt.println(cl)
        testing.expect(t, cl == 0)
}

