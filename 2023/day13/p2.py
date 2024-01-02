

def part2(data):
        maps = ("\n".join(data)).split("\n\n")
        return sum(find_mirror(_map, diff=1) for _map in maps)

def find_mirror(_map, diff=0):
    _map_h = _map.split("\n")
    _map_v = ["".join(c) for c in zip(*_map_h)]

    for pattern, weight in ((_map_h, 100), (_map_v, 1)):
        for i in range(1, len(pattern)):
            a, b = pattern[:i], pattern[i:]
            a = "".join(a[::-1])
            b = "".join(b)
            if sum(x != y for x, y in zip(a, b)) == diff:
                return i * weight

    return -1

def get_puzzle_input():
    return [line.strip() for line in open("input.txt", "r").readlines()]

if __name__ == "__main__":
    print(part2(get_puzzle_input()))
