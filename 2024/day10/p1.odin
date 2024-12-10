package main

import "core:fmt"
import "core:os"
import "core:mem/virtual"
import "core:strings"

P1 :: false

when P1 
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
        score: int
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
            trailhead.score = find_all_trails(state, trailhead)
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

        reached_summits := map[Point]bool{}

        for {
            if summit, summit_reached := find_new_summit(tmap, t.position, reached_summits); summit_reached {
                reached_summits[summit] = true
            }
            else {
                break
            }
        }

        return len(reached_summits)
    }

    find_new_summit :: proc(tmap: map[Point]int, start: Point, visited_summits: map[Point]bool) -> (summit: Point, summit_reached: bool)
    {
        visited := map[Point]bool{}
        return search(tmap, start, visited_summits, &visited)
    }

    search :: proc(tmap: map[Point]int, point: Point, visited_summits: map[Point]bool, visited_points: ^map[Point]bool) -> (summit: Point, summit_reached: bool)
    {
        visited_points[point] = true
        if tmap[point] == SUMMIT {
            return point, true
        }

        for eligible_step in eligible_steps(tmap, point, visited_points^) {
            if eligible_step in visited_summits {
                continue
            }
            if summit, summit_reached = search(tmap, eligible_step, visited_summits, visited_points); summit_reached {
                return
            }
        }

        return {}, false
    }

    eligible_steps :: proc(tmap: map[Point]int, point: Point, visited_points: map[Point]bool) -> []Point
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

            if step not_in visited_points {
                steps[idx] = step
                idx += 1
            }
        }

        return steps[:idx]
    }

    sum_scores :: proc(ts: []Trailhead) -> (total: int)
    {
        for t in ts {
            total += t.score
        }

        return total
    }
}
