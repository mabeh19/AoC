package day9

import "core:testing"
import "core:fmt"

import aoc "../.."


Distance :: struct {
        loc1, loc2: string,
        len: u16,
}

Location :: struct {
        neighbours: map[string]u16,
        visited: bool
}

Route :: [dynamic]string


Map :: map[string]Location

main :: proc() {
        input := aoc.read_input()
        m := parse_map(input)
        m_longest := parse_map(input)
        defer delete(m)
        defer delete(m_longest)
        dist := find_shortest_route(m)
        fmt.println("Shortest:", dist)
        fmt.println("Furthest:", find_longest_route(m_longest))
}

parse_map :: proc(input: string) -> Map {
        m := make(Map)
        for line in aoc.lines(input) {
                if len(line) < 1 do continue
                toks := aoc.words(line)
                loc1 := toks[0]
                loc2 := toks[2]
                dist := u16(aoc.get_int(toks[4]))
                start := m[loc1]
                end := m[loc2]
                start.neighbours[loc2] = dist
                end.neighbours[loc1] = dist

                m[loc1] = start
                m[loc2] = end
        }

        return m
}

find_shortest_route :: proc(m: Map) -> u16 {
        shortest_route := max(u16)
        for loc in m { 
                m_copy := aoc.copy_map(m)
                defer delete(m_copy)
                route := make(Route)
                defer delete(route)
                travel_to_closest(&m_copy, loc, &route)
                fmt.println(route)
                if len(route) == len(m) {
                        dist := calc_distance(m_copy, route)
                        shortest_route = min(shortest_route, dist)
                }
        }
        return shortest_route
}

find_longest_route :: proc(m: Map) -> u16 {
        longest_route := min(u16)
        for loc in m { 
                m_copy := aoc.copy_map(m)
                defer delete(m_copy)
                route := make(Route)
                defer delete(route)
                travel_to_furthest(&m_copy, loc, &route)
                fmt.println(route)
                if len(route) == len(m) {
                        dist := calc_distance(m_copy, route)
                        longest_route = max(longest_route, dist)
                }
        }
        return longest_route
}

travel_to_closest :: proc(m: ^Map, location: string, route: ^Route) {
        entry := m[location]
        entry.visited = true
        m[location] = entry

        closest := "UNKNOWN"
        closest_dist := max(u16)
        
        append(route, location)

        for loc, dist in entry.neighbours {
                neighbour := m[loc]
                if !neighbour.visited && dist < closest_dist {
                        closest = loc
                        closest_dist = dist
                }
        }

        if closest == "UNKNOWN" { 
                return
        }


        travel_to_closest(m, closest, route)
}

travel_to_furthest :: proc(m: ^Map, location: string, route: ^Route) {
        entry := m[location]
        entry.visited = true
        m[location] = entry

        furthest := "UNKNOWN"
        furthest_dist := min(u16)
        
        append(route, location)

        for loc, dist in entry.neighbours {
                neighbour := m[loc]
                if !neighbour.visited && dist > furthest_dist {
                        furthest = loc
                        furthest_dist = dist
                }
        }

        if furthest == "UNKNOWN" { 
                return
        }


        travel_to_furthest(m, furthest, route)
}

calc_distance :: proc(m: Map, route: Route) -> u16 {
        dist := u16(0)
        for _,i in route[0:len(route)-1] {
                loc1 := route[i]
                loc2 := route[i+1]
                start := m[loc1]
                end := m[loc2]
                dist += start.neighbours[loc2]
        }

        return dist
}



@test
examples :: proc(t: ^testing.T) {
ex1_inp := 
`London to Dublin = 464
London to Belfast = 518
Dublin to Belfast = 141
`
        //ex1 := parse_map(ex1_inp)
        ex2 := parse_map(ex1_inp)
        //dist := find_shortest_route(ex1)
        dist2 := find_longest_route(ex2)
        //testing.expect(t, dist == 605)
        testing.expect(t, dist2 == 982)
}

