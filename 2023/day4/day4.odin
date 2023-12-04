package aoc_day

import "core:fmt"
import "core:strings"
import "core:strconv"
import "core:math"
import "core:unicode"
import "core:time"
import "core:testing"

import aoc "../.."

Card :: struct {
        id:                     int,
        winning_numbers:        []int,
        own_numbers:            []int,
        count:                  int,
}

Game :: [210]Card


main :: proc() {
        input := #load("input.txt", string)
        s := time.now()
        game := parse_game(input)
        ans := solve1(game)
        e := time.now()
        free(game)
        fmt.println("P1:", ans, "Time:", time.diff(s, e))
        s = time.now()
        game = parse_game(input)
        ans = solve2(game)
        e = time.now()
        free(game)
        fmt.println("P2:", ans, "Time:", time.diff(s, e)) 
}

parse_game :: proc(str: string) -> ^Game {
        game := new(Game)
        for line in aoc.lines(str) {
                if line == "" do continue
                card_info := strings.split(line, ": ")
                id_txt := aoc.words(card_info[0])
                id := aoc.get_int(id_txt[len(id_txt)-1])
                numbers := strings.split(card_info[1], " | ")
                winning_nums := convert_nums(aoc.words(numbers[0]))
                own_nums := convert_nums(aoc.words(numbers[1]))
                card := Card {
                        id              = id,
                        winning_numbers = winning_nums,
                        own_numbers     = own_nums,
                        count           = 1,
                }
                game[id] = card
        }
        return game
}

solve1 :: proc(game: ^Game) -> int {
        total := 0
        for card, id in game {
                num := get_points(card.winning_numbers, card.own_numbers)
                total += num
        }
        return total
}

solve2 :: proc(game: ^Game) -> int {
        total := 0
        for card, id in game {
                points := get_points_linear(card.winning_numbers, card.own_numbers)
                for n in 1 ..= points {
                        c := game[id + n]
                        c.count += card.count
                        game[id + n] = c
                }
                total += card.count
        }
        return total
}


convert_nums :: proc(nums: []string) -> []int {
        as_int := [dynamic]int{}
        for num in nums {
                if num == "" do continue
                append(&as_int, aoc.get_int(num))
        }
        return as_int[:]
}

get_points :: proc(winning_nums, own_nums: []int) -> int {
        points := 0
        for wn in winning_nums {
                for on in own_nums {
                        if on != wn do continue
                        points = 1 if points == 0 else points * 2
                }
        }
        return points
}

get_points_linear :: proc(winning_nums, own_nums: []int) -> int {
        points := 0
        for wn in winning_nums {
                for on in own_nums {
                        if on != wn do continue
                        points += 1
                }
        }
        return points
}

@test
examples :: proc(t: ^testing.T) {
        input := #load("test.txt", string)
        game := parse_game(input)
        defer free(game)
        fmt.println(solve1(game))
        fmt.println(solve2(game))
}
