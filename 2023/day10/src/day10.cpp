#include <sstream>
#include <fstream>
#include <vector>
#include <fmt/core.h>
#include <fmt/ranges.h>

typedef long long ll;
typedef std::vector<std::string> Maze;

struct Coord {
    ll x, y;
};


std::tuple<Maze, Coord> parse_maze(std::stringstream &ss)
{
    Maze maze;
    Coord s_coord;

    while (ss.good()) {
        std::string s;
        getline(ss, s);
        if (s.length() < 1) continue;
        maze.push_back(s);
        ll idx;
        if ((idx = s.find("S")) != s.npos) s_coord = (Coord) { idx, (ll)maze.size()-1 };
    }

    return std::tuple(maze, s_coord);
}

std::tuple<Coord, Coord> connected_pipes(const Maze &m, const Coord &c)
{
    char type = m[c.y][c.x];
    switch (type) {
        case 'F':
        case 'J':
        case '|':
        case 'S':
        case 'L':
        case '-':
        case '.':
        default:
            return std::tuple<Coord, Coord>();
    }
}

ll solve1(const Maze &m, const Coord &start)
{
    std::vector<Coord> loop;

    ll step = 0;
    

    return 0;   
}

int main()
{
    std::fstream fs("../test.txt");
    std::stringstream ss;
    ss << fs.rdbuf();

    auto [maze, start] = parse_maze(ss);

    fmt::println("Start: ({},{})", start.x, start.y);
    for (auto &l : maze) {
        fmt::println("{}", l);
    }

    auto ans1 = solve1(maze, start);
    fmt::println("{}", ans1);
}

