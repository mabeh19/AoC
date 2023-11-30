package day4

import "core:fmt"
import "core:thread"
import "core:crypto/legacy/md5"


foreign {
        memcpy :: proc(dest, src: rawptr, len: u32) -> int ---
}


main :: proc() {
        algo := new(md5.Context)
        defer free(algo)
        md5.init(algo)

        limit : u128 = (1 << (128 - 20)) - 1
        lowest := max(u128)
        lowest_num := 0
        key := "pqrstuv"

        for num in 0 ..< max(u32) {
                data := fmt.aprintf("%v%v", key, num)
                out := transmute(u128)md5.hash(data)
                as_str := fmt.aprintf("%032x", out)

                if as_str[0:5] == "00000" {
                        lowest = out
                        lowest_num = int(num)
                        break
                }
        }

        fmt.printf("%v%v => %032x\n", key, lowest_num, lowest)
}
