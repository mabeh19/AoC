package aoc_day

import "core:fmt"
import "core:strings"
import "core:strconv"
import "core:math"
import "core:unicode"
import "core:sort"
import "core:slice"
import "core:testing"

import aoc "../.."

MARKERS :: [?]string{
    "seed-to-soil map:",
    "soil-to-fertilizer map:",
    "fertilizer-to-water map:",
    "water-to-light map:",
    "light-to-temperature map:",
    "temperature-to-humidity map:",
    "humidity-to-location map:",
}

Entry :: struct {
    dest_start:     int,
    source_start:   int,
    range:          int,
}

main :: proc() {
    input := #load("input.txt", string);
    fmt.println(solve1(input))
    fmt.println(solve2(input))
}

solve1 :: proc(str: string) -> int {
    Map :: [dynamic]Entry

    Almanac :: struct {
        seeds:          []int,
        seed2soil:      Map,
        soil2fert:      Map,
        fert2water:     Map,
        water2light:    Map,
        light2temp:     Map,
        temp2humidity:  Map,
        humidity2loc:   Map,
    }
    SEEDS_PREFIX            :: "seeds: "
    SEED2SOIL_MARKER        :: "seed-to-soil map:"
    SOIL2FERT_MARKER        :: "soil-to-fertilizer map:"
    FERT2WATER_MARKER       :: "fertilizer-to-water map:"
    WATER2LIGHT_MARKER      :: "water-to-light map:"
    LIGHT2TEMP_MARKER       :: "light-to-temperature map:"
    TEMP2HUM_MARKER         :: "temperature-to-humidity map:"
    HUM2LOC_MARKER          :: "humidity-to-location map:"
    get_map_entry :: proc(m: Map, source: int) -> int {
        for entry in m {
            if source >= entry.source_start && source < entry.source_start + entry.range {
                return entry.dest_start + (source - entry.source_start)
            }
        }
        return source
    }
    almanac := Almanac{}

    cur_map : ^Map
    lines := aoc.lines(str)
    defer delete(lines)
    for line in lines {
        if len(line) < 1 do continue
        if strings.has_prefix(line, SEEDS_PREFIX) {
            out, was_alloc := strings.remove(line, SEEDS_PREFIX, 1)
            defer if was_alloc { delete(out) }
            almanac.seeds = aoc.words2int(out) 
        }
            else {
                switch line {
                case SEED2SOIL_MARKER:
                    cur_map = &almanac.seed2soil
                case SOIL2FERT_MARKER:
                    cur_map = &almanac.soil2fert
                case FERT2WATER_MARKER:
                    cur_map = &almanac.fert2water
                case WATER2LIGHT_MARKER:
                    cur_map = &almanac.water2light
                case LIGHT2TEMP_MARKER:
                    cur_map = &almanac.light2temp
                case TEMP2HUM_MARKER:
                    cur_map = &almanac.temp2humidity
                case HUM2LOC_MARKER:
                    cur_map = &almanac.humidity2loc
                case:
                    entry := build_entry(line)
                    append(cur_map, entry)
                }
            }
    }

    lowest_loc := max(int)
    for seed in almanac.seeds {
        soil := get_map_entry(almanac.seed2soil, seed)
        fert := get_map_entry(almanac.soil2fert, soil)
        water := get_map_entry(almanac.fert2water, fert)
        light := get_map_entry(almanac.water2light, water)
        temp := get_map_entry(almanac.light2temp, light)
        hum := get_map_entry(almanac.temp2humidity, temp)
        loc := get_map_entry(almanac.humidity2loc, hum)
        lowest_loc = min(lowest_loc, loc)
    }

    return lowest_loc
}

solve2 :: proc(str: string) -> int {
    Map :: [dynamic]Entry
    Range :: struct { start, end: int }
    SEEDS_PREFIX            :: "seeds: "
    get_map_entry :: proc(m: Map, source: int) -> int {
        for entry in m {
            if source >= entry.source_start && source < entry.source_start + entry.range {
                return entry.dest_start + (source - entry.source_start)
            }
        }
        return source
    }
    get_rev_map_entry :: proc(m: ^Map, dest: int) -> int {
        for entry in m {
            if dest >= entry.dest_start && dest < entry.dest_start + entry.range {
                return entry.source_start + (dest - entry.dest_start)
            }
        }
        return dest
    }
    overlap :: proc(a,b: Range) -> Range {
        if a.start >= b.start && a.start <= b.end {
            return {a.start, b.end}
        }
        else if b.start >= a.start && b.start <= a.end {
            return {b.start, a.end}
        }
        return {}
    }

    m := [dynamic]^Map{}
    defer delete(m)
    cur_map : ^Map
    seeds := [dynamic]Range{}
    defer delete(seeds)
    lines := aoc.lines(str)
    defer delete(lines)
    parse_lines:
    for line in lines {
        if len(line) < 1 do continue
        if strings.has_prefix(line, SEEDS_PREFIX) {
            out, was_alloc := strings.remove(line, SEEDS_PREFIX, 1)
            defer if was_alloc { delete(out) }
            s := aoc.words2int(out) 
            for i := 0; i < len(s) - 1; i += 2 {
                start := s[i]
                len := s[i+1]
                append(&seeds, Range{start, start + len})
            }
        }
        else {
            for marker in MARKERS {
                if line != marker do continue 
                if cur_map != nil do append(&m, cur_map)
                cur_map = new(Map) 
                continue parse_lines
            }
            if cur_map != nil {
                entry := build_entry(line)
                append(cur_map, entry)
            }
        }
    }

    lows := seeds
    for mp in m {
        res := [dynamic]Range{}
        defer delete(res)
        for e in mp {
            r := Range{e.source_start, e.source_start + e.range}
            for seedrange in lows {
                inter := overlap(seedrange, r)
                if inter == {} do continue
                if !slice.contains(res[:], inter) {
                    fmt.printf("(%v, %v)\n", inter.start, inter.end)
                    append(&res, inter) //Range{a, b})
                }
            }
        }
        lows = res
    }
    slice.sort_by(lows[:], proc(a, b: Range) -> bool { return a.start < b.start })
    ans := lows[0].start
    seed := ans
    seed2 := 7873084
    #reverse for mp in m {
        seed = get_rev_map_entry(mp, seed)
    }
    #reverse for mp in m {
        seed2 = get_rev_map_entry(mp, seed2)
    }
    fmt.println(seed)
    fmt.println("New: ", seed)
    fmt.println("Old: ", seed2)
    return ans
}

build_entry :: proc(line: string) -> Entry {
    toks := aoc.words(line)
    defer delete(toks)
    return Entry {
        dest_start      = aoc.get_int(toks[0]),
        source_start    = aoc.get_int(toks[1]),
        range           = aoc.get_int(toks[2]),
    }
}

@test
examples :: proc(t: ^testing.T) {
    input := #load("test.txt", string)
    fmt.println(solve1(input))
    fmt.println(solve2(input))
}
