package main

import "core:fmt"
import "core:os"

File :: distinct int
Empty :: distinct struct {}

FileBlock :: union {
    File,
    Empty
}

main :: proc ()
{
    file, _ := os.read_entire_file("test.txt")
    defer delete(file)

    if file[len(file)-1] == '\n' {
        file = file[:len(file)-1]
    }

    fs := parse(file)
    defer delete(fs)

    insert_idx := 0
    take_idx := len(fs) - 1

    for insert_idx < take_idx {
        switch f in fs[insert_idx] {
        case File:
            insert_idx += 1
        case Empty:
            swap(fs, insert_idx, take_idx)
            take_idx -= 1
            
            skip_empty: for {
                switch t in fs[take_idx] {
                case Empty:
                    take_idx -= 1
                case File:
                    break skip_empty
                }
            }
        }
    }

    chk := checksum(fs)

    fmt.println(chk)
}

parse :: proc(s: []u8) -> []FileBlock
{
    isEmpty := false
    fs := [dynamic]FileBlock{}
    id : File = 0

    for c in s {
        n := c - '0'

        for i in 0..< n {

            if isEmpty {
                append(&fs, Empty{})
            }
            else {
                append(&fs, id)
            } 
        }

        if isEmpty {
            isEmpty = false
        }
        else {
            isEmpty = true
            id += 1
        }
    }

    return fs[:]
}

checksum :: proc(fs: []FileBlock) -> int
{
    total := 0

    for i in 0..< len(fs) {
        switch n in fs[i] {
        case Empty:
        case File:
            total += i * int(n)
        }
    }

    return total
}

swap :: proc(s: []$T, a: int, b: int)
{
    tmp := s[a]
    s[a] = s[b]
    s[b] = tmp
}
