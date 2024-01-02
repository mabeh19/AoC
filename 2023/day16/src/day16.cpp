#include <fstream>
#include <sstream>
#include <vector>
#include <string>
#include <fmt/core.h>
#include <fmt/ranges.h>

typedef long long ll;

struct Tile {
    char type;
    bool energized;
    bool visited[4];
};

struct Contraption {
    std::vector<std::vector<Tile>> grid;
};

enum Direction {
    Up,
    Down,
    Left,
    Right
};

Contraption parse(std::stringstream &ss)
{
    Contraption ret;
    while (ss.good()) {
        std::string s;
        getline(ss, s);
        if (s.length() <= 1) continue;
        std::vector<Tile> row;
        for (auto c : s) {
            row.push_back(Tile {c, false});
        }
        ret.grid.push_back(row);
    }
    return ret;
}

void print(const Contraption &c)
{
    for (auto &r : c.grid) {
        for (auto &c : r) 
            if (c.energized)
                fmt::print("#");
            else
                fmt::print("{}", c.type); 

        fmt::println("");
    }
}

void shoot_beam(Contraption &c, ll row, ll col, Direction d)
{
    if ((row >= c.grid.size() || row < 0)       ||
        (col >= c.grid[row].size() || col < 0)) {
        return;
    }
    std::fflush(stdout);
    if (c.grid[row][col].visited[d]) {
        return;
    }
    c.grid[row][col].energized = true;
    c.grid[row][col].visited[d] = true;
    switch (c.grid[row][col].type) {
        case '/':
            switch (d) {
                case Up:
                    shoot_beam(c, row, col + 1, Right);
                    break;
                case Down:
                    shoot_beam(c, row, col - 1, Left);
                    break;
                case Left:
                    shoot_beam(c, row + 1, col, Down);
                    break;
                case Right:
                    shoot_beam(c, row - 1, col, Up);
                    break;
                default:
                    break;
            }
            break;
        case '\\':
            switch (d) {
                case Up:
                    shoot_beam(c, row, col - 1, Left);
                    break;
                case Down:
                    shoot_beam(c, row, col + 1, Right);
                    break;
                case Left:
                    shoot_beam(c, row - 1, col, Up);
                    break;
                case Right:
                    shoot_beam(c, row + 1, col, Down);
                    break;
                default:
                    break;
            }
            break;
        case '|':
            switch (d) {
                case Up:
                    shoot_beam(c, row - 1, col, Up);
                    break;
                case Down:
                    shoot_beam(c, row + 1, col, Down);
                    break;
                case Left:
                case Right:
                    shoot_beam(c, row - 1, col, Up);
                    shoot_beam(c, row + 1, col, Down);
                    break;
                default:
                    break;
            }
            break;
        case '-':
            switch (d) {
                case Up:
                case Down:
                    shoot_beam(c, row, col - 1, Left);
                    shoot_beam(c, row, col + 1, Right);
                    break;
                case Left:
                    shoot_beam(c, row, col - 1, Left);
                    break;
                case Right:
                    shoot_beam(c, row, col + 1, Right);
                    break;
                default:
                    break;
            }
            break;
        case '.':
        default:
            switch (d) {
                case Up:
                    shoot_beam(c, row - 1, col, Up);
                    break;
                case Down:
                    shoot_beam(c, row + 1, col, Down);
                    break;
                case Left:
                    shoot_beam(c, row, col - 1, Left);
                    break;
                case Right:
                    shoot_beam(c, row, col + 1, Right);
                    break;
                default:
                    break;
            }
            break;
    }
}

ll count(const Contraption &c)
{
    ll energized = 0;
    for (auto &r : c.grid) 
        for (auto &c : r)
            energized += c.energized;
    return energized;
}

void clear(Contraption &c)
{
    for (auto &r : c.grid)
        for (auto &c : r) {
            memset(c.visited, 0, sizeof c.visited);
            c.energized = false;
        }
}

ll solve1(Contraption &c)
{
    shoot_beam(c, 0, 0, Right);
    return count(c);
}

ll solve2(Contraption &c)
{
    ll energized = 0;
    for (int i = 0; i < c.grid.size(); i++) {
        shoot_beam(c, i, 0, Right);
        energized = std::max(energized, count(c));
        clear(c);
    } 
    for (int i = 0; i < c.grid.size(); i++) {
        shoot_beam(c, i, c.grid[0].size() - 1, Left);
        energized = std::max(energized, count(c));
        clear(c);
    }
    for (int i = 0; i < c.grid[0].size(); i++) {
        shoot_beam(c, 0, i, Down);
        energized = std::max(energized, count(c));
        clear(c);
    }
    for (int i = 0; i < c.grid[0].size(); i++) {
        shoot_beam(c, c.grid.size() - 1, i, Up);
        energized = std::max(energized, count(c));
        clear(c);
    }
    return energized;
}

void run(const char *file)
{
    std::fstream fs(file);
    std::stringstream ss;
    ss << fs.rdbuf();
    auto cont = parse(ss);
    auto ans1 = solve1(cont);
    clear(cont);
    auto ans2 = solve2(cont);
    fmt::println("{}", ans1);
    fmt::println("{}", ans2);

}

int main()
{
    run("../test.txt");
    run("../input.txt");
}
