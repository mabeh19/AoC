#!/usr/bin/env -S v
import time


const filecontent := r'package aoc_day

import "core:fmt"
import "core:strings"
import "core:strconv"
import "core:math"
import "core:unicode"
import "core:testing"

import aoc "../.."

main :: proc() {
    input := #load("input.txt", string)
}

@test
examples :: proc(t: ^testing.T) {
    input := #load("test.txt", string)
}
'

now := time.now()
if !exists("${now.year}") {
    mkdir("${now.year}")!
}

if !exists("${now.year}/day${now.day}") {
    mkdir("${now.year}/day${now.day}")!
    mut f := open_append("${now.year}/day${now.day}/day${now.day}.odin")!
    f.write_string(filecontent)!
    f.close()
}
