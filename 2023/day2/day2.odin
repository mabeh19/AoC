package aoc_day

import "core:fmt"
import "core:strings"
import "core:strconv"
import "core:math"
import "core:testing"

import aoc "../.."

Result :: map[string]int

Game :: struct {
        id: int,
        results: [dynamic]Result,
}


main :: proc() {
        input := #load("input.txt", string)
        restrict := Result {
                "red" = 12,
                "green" = 13,
                "blue" = 14,
        }
        games := parse_games(input)
        solution := solve(games, restrict)
        fmt.println(sum_ids(solution))
        fmt.println(solve2(games))
}

parse_games :: proc(str: string) -> []Game {
        games := [dynamic]Game{}
        for line in aoc.lines(str) {
                if len(line) < 1 do continue
                append(&games, parse_game(line))
        }

        return games[:]
}

parse_game :: proc(str: string) -> Game {
        game := Game{}
        game_info := strings.split(str, ": ")
        results := strings.split(game_info[1], "; ")
        game.id = aoc.get_int(aoc.words(game_info[0])[1])
        for result in results {
                subset := strings.split(result, ", ")
                r := Result{}
                for cube in subset {
                        cube_res := aoc.words(cube)
                        r[cube_res[1]] = aoc.get_int(cube_res[0])
                }
                append(&game.results, r)
        }

        return game
}

get_cubes_required :: proc(game: Game) -> Result {
        min_required := Result{}
        for result in game.results {
                for color, num in result {
                        cube := min_required[color]
                        min_required[color] = max(num, cube)
                }
        }

        return min_required
}

is_valid :: proc(result: Result, restriction: Result) -> bool {
        ok := true
        for color, num in result {
                ok = ok && (num <= restriction[color])
        }
        return ok
}

power :: proc(set: Result) -> int {
        pow := 1
        for color, num in set {
                pow *= num
        }
        return pow
}

solve :: proc(games: []Game, restriction: Result) -> []Game {
        valid_games := [dynamic]Game{}
        for game in games {
                req := get_cubes_required(game)
                if is_valid(req, restriction) {
                        append(&valid_games, game)
                }
        }

        return valid_games[:]
}

solve2 :: proc(games: []Game) -> int {
        total_powers := 0
        for game in games {
                set := get_cubes_required(game)
                total_powers += power(set)
        }
        return total_powers
}

sum_ids :: proc(games: []Game) -> int {
        sum := 0
        for game in games {
                sum += game.id
        }
        return sum
}


@test
examples :: proc(t: ^testing.T) {
        input := #load("test.txt", string)
        restrict := Result {
                "red" = 12,
                "green" = 13,
                "blue" = 14,
        }
        game := parse_games(input)
        testing.expect(t, sum_ids(solve(game, restrict)) == 8)
        testing.expect(t, solve2(game) == 2286)
}
