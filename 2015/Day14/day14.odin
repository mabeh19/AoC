package day14

import "core:fmt"
import "core:testing"
import aoc "../.."

Action :: enum {
        Sprint,
        Rest
}

Racer :: struct {
        name: string,
        distance_covered: int,
        speed: int,
        sprint_period: int,
        rest_period: int,
        current_action: Action,
        action_remainder: int,
        points: int,
}

Race :: struct {
        racers: map[string]Racer,
        t: int
}

main :: proc() {
        input := aoc.read_input()

        race := parse_file(input)
        winner, dist := do_race(&race, 2503)
        fmt.println(winner, ": ", dist)
}

parse_file :: proc(file: string) -> Race {
        race := Race{}
        for line in aoc.lines(file) {
                if len(line) < 1 do continue
                toks := aoc.words(line)
                race.racers[toks[0]] = Racer {
                        name = toks[0],
                        distance_covered = 0,
                        speed = aoc.get_int(toks[1]),
                        sprint_period = aoc.get_int(toks[2]),
                        rest_period = aoc.get_int(toks[3]),
                        current_action = .Sprint,
                        action_remainder = aoc.get_int(toks[2]),
                        points = 0,
                }
        }

        return race
}

do_race :: proc(race: ^Race, time: int) -> (winner: string, distance: int) {
        for t in 0 ..< time {
                for name, _ in race.racers {
                        racer := race.racers[name]
                        do_action(&racer)
                        race.racers[name] = racer
                }
                winners := get_winners(race)

                for &w in winners {
                        w.points += 1
                        race.racers[w.name] = w
                }
                race.t += 1
        }

        for name, racer in race.racers {
                if racer.points > distance {
                        winner = name
                        distance = racer.points
                }
        }

        return
}

get_winners :: proc(race: ^Race) -> ([]Racer) {
        winners := [dynamic]Racer{}
        distance := 0
        for name, racer in race.racers {
                if racer.distance_covered > distance {
                        clear(&winners)
                        distance = racer.distance_covered
                } 
                if racer.distance_covered == distance {
                        append(&winners, racer)
                }
        }

        return winners[:]
}

do_action :: proc(racer: ^Racer) {
        switch racer.current_action {
        case .Sprint:
                racer.distance_covered += racer.speed
        case .Rest:
        }

        racer.action_remainder -= 1

        if racer.action_remainder == 0 {
                switch_action(racer)
        }
}

switch_action :: proc(racer: ^Racer) {
        switch racer.current_action {
        case .Sprint:
                racer.current_action = .Rest
                racer.action_remainder = racer.rest_period
        case .Rest:
                racer.current_action = .Sprint
                racer.action_remainder = racer.sprint_period
        }
}

@test
examples :: proc(t: ^testing.T) {
        input := aoc.read_test()
        
        race := parse_file(input)
        winner, dist := do_race(&race, 1000)
        fmt.println(winner, ": ", dist)
        testing.expect(t, winner == "Comet")
        testing.expect(t, dist == 1120)
}

