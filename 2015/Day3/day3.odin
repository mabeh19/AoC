package day3

import "core:fmt"
import "core:os"


Coords :: [2]int


main :: proc() {
        history := make(map[Coords]int)
        defer delete(history)

        toggle := true
        santa_pos := Coords{0, 0}
        robo_pos := Coords{0, 0}
        f, _ := os.open("input.txt")
        defer os.close(f)
        f_inp, _ := os.read_entire_file(f)
        input := string(f_inp)

        for c in input {
                if toggle {
                        count := history[santa_pos]
                        history[santa_pos] = count + 1
                        move(&santa_pos, c)
                } else {
                        count := history[robo_pos]
                        history[robo_pos] = count + 1
                        move(&robo_pos, c)
                }
                toggle = !toggle
        }

        fmt.println(len(history))
}


move :: proc(pos: ^Coords, m: rune) {
        switch m {
        case '^':
                pos.y += 1
        case 'v':
                pos.y -= 1
        case '>':
                pos.x += 1
        case '<':
                pos.x -= 1
        }
}

