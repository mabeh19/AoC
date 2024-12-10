package main

import "core:fmt"
import "core:os"
import "core:mem/virtual"
import "core:strings"
import "core:slice"

P2 :: true

when P2 
{
    SUMMIT      :: 9
    TRAILHEAD   :: 0
    MAX_STEP    :: 1

    State :: struct {
        tmap: map[Point]int,
        trailheads: []Trailhead,
        summits: []Point
    }

    Trailhead :: struct {
        using position: Point,
        rating: int,
    }

    Point :: [2]int

    main :: proc ()
    {
        arena := virtual.Arena{}
        _ = virtual.arena_init_growing(&arena)
        context.allocator = virtual.arena_allocator(&arena)
        file := #load("input.txt", string)

        state := parse(file)

        for &trailhead in state.trailheads {
            trailhead.rating = find_all_trails(state, trailhead)
        }

        fmt.println(sum_scores(state.trailheads[:]))

        virtual.arena_free_all(&arena)
    }


    parse :: proc(s: string) -> State 
    {
        tmap := map[Point]int{}
        theads := [dynamic]Trailhead{}
        summits := [dynamic]Point{}

        for l, y in strings.split_lines(s) {
            for c, x in l {
                n := int(c - '0')

                p := Point{x,y}

                tmap[p] = n

                switch n {
                case 0:
                    append(&theads, Trailhead{p, 0})
                case 9:
                    append(&summits, p)
                case:
                    // do nothing
                }
            }
        }

        return State { tmap, theads[:], summits[:] }
    }

    find_all_trails :: proc(s: State, t: Trailhead) -> int
    {
        tmap := s.tmap

        unique_trails := find_new_summit(tmap, t.position)

        return len(unique_trails)
    }

    find_new_summit :: proc(tmap: map[Point]int, start: Point) -> [dynamic][]Point
    {
        unique_trails := [dynamic][]Point{}
        trail := [dynamic]Point{}
        search(tmap, start, &unique_trails, &trail)
        return unique_trails
    }

    search :: proc(tmap: map[Point]int, point: Point, unique_trails: ^[dynamic][]Point, current_trail: ^[dynamic]Point)
    {
        append(current_trail, point)
        defer pop(current_trail)

        if tmap[point] == SUMMIT {
            append(unique_trails, current_trail[:])
            return
        }

        for eligible_step in eligible_steps(tmap, point, current_trail[:]) {
            if slice.contains(current_trail[:], eligible_step) {
                continue
            }

            search(tmap, eligible_step, unique_trails, current_trail)
        }
    }

    eligible_steps :: proc(tmap: map[Point]int, point: Point, visited_points: []Point) -> []Point
    {
        steps := make([]Point, 4)

        dirs := []Point{
            Point{-1,  0},  // left
            Point{ 1,  0},  // right
            Point{ 0, -1},  // up
            Point{ 0,  1},  // down
        }

        idx := 0

        p_val := tmap[point]

        for dir in dirs {
            step := point + dir

            if step not_in tmap {
                continue
            }

            if tmap[step] - p_val != 1 {
                continue
            }

            if !slice.contains(visited_points, step) {
                steps[idx] = step
                idx += 1
            }
        }

        return steps[:idx]
    }

    sum_scores :: proc(ts: []Trailhead) -> (total: int)
    {
        for t in ts {
            total += t.rating
        }

        return total
    }
}
