package day1

import "core:fmt"
import "core:strconv"

import aoc "../../"


test1 : string : #load("test1.txt")
input : string : #load("input.txt")

main :: proc()
{
    password(test1)
    password(input)
}

password :: proc(inp : string)
{
    MAX :: 100
    START :: 50
    num := START
    zeroes := 0

    for line in aoc.lines(inp) {
        if line == "" do continue

        amount := aoc.get_int(line[1:])

        for i in 0 ..< amount {
            switch line[0]
            {
            case 'L':
                num -= 1
            case 'R':
                num += 1
            }
            
            if num == MAX {
                num = 0
            }
            
            if num == -1 {
                num = 99
            }

            if num == 0 {
                zeroes += 1
            }

        }
    }

    fmt.println("Zeroes: ", zeroes)
}
