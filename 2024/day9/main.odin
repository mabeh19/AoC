package main

import "core:fmt"
import "core:os"
import "core:mem/virtual"
import "core:strings"

Block :: struct {
    size: int,

    block_type: union {File, Empty}
}

File :: struct {
    id: int
}
Empty :: distinct struct {}

main :: proc ()
{
    arena := virtual.Arena{}
    _ = virtual.arena_init_growing(&arena)
    context.allocator = virtual.arena_allocator(&arena)
    file, _ := os.read_entire_file("input.txt")

    if file[len(file)-1] == '\n' {
        file = file[:len(file)-1]
    }

    fs := parse(file)

    take_idx := len(fs) - 1

    for take_idx > 0 {

        if bb, bb_ok := fs[take_idx].block_type.(File); bb_ok {
            for start_index in 0..< take_idx {
                front := &fs[start_index]
                back  := &fs[take_idx]
                if b, ok := front.block_type.(Empty); ok {
                    if front.size >= back.size {
                        front.size -= back.size
                        if front.size == 0 {
                            front^ = back^
                            back.block_type = Empty{}
                        }
                        else {
                            // after this call, neither pointers are valid anymore
                            back_cpy := back^
                            inject_at(&fs, start_index, back_cpy)
                            take_idx += 1

                            back  = &fs[take_idx]
                            back.block_type = Empty{}
                        }
                        break
                    }
                }
            }
        }

        take_idx -= 1
    }

    chk := checksum(fs[:])

    fmt.println(chk)

    virtual.arena_free_all(&arena)
}

print :: proc(fs: []Block)
{
    b := strings.Builder{}
    strings.builder_init(&b)

    for f in fs {
        for i in 0..< f.size {
            switch fb in f.block_type {
            case Empty:
                fmt.sbprint(&b, ".")
            case File:
                fmt.sbprintf(&b, "%v", fb.id)
            }
        }
    }

    fmt.println(strings.to_string(b))
}

parse :: proc(s: []u8) -> [dynamic]Block
{
    isEmpty := false
    fs := [dynamic]Block{}
    id := 0

    for c in s {
        n := int(c - '0')

        if isEmpty {
            append(&fs, Block { n, Empty{} })
        }
        else {
            append(&fs, Block { n, File{ id } })
        }

        if isEmpty {
            isEmpty = false
        }
        else {
            isEmpty = true
            id += 1
        }
    }

    return fs
}

checksum :: proc(fs: []Block) -> int
{
    total := 0
    idx := 0

    for i in 0..< len(fs) {
        switch n in fs[i].block_type {
        case Empty:
            idx += fs[i].size
        case File:
            for j in 0..< fs[i].size {
                total += idx * int(n.id)
                idx += 1
            }
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
