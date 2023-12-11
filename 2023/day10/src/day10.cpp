#include <sstream>
#include <fstream>
#include <vector>
#include <unordered_set>
#include <fmt/core.h>
#include <fmt/ranges.h>

typedef long long ll;
typedef unsigned long long ull;
typedef std::vector<std::string> Maze;

struct Coord {
    ll x, y;

    Coord() : x{0}, y{0} {}
    Coord(const ll &_x, const ll &_y) : x{_x}, y{_y} {}
    Coord(const Coord &other) : x{other.x}, y{other.y} {}

    Coord operator+(const Coord &rhs) const
    {
        return Coord { x + rhs.x, y + rhs.y };
    }

    bool operator==(const Coord &rhs) const
    {
        return x == rhs.x && y == rhs.y;
    }
    
    bool operator!=(const Coord &rhs) const
    {
        return !(*this == rhs);
    }

    bool operator<(const Coord &rhs) const
    {
        return x < rhs.x || (x == rhs.x && y < rhs.y);
    }
};

template<>
struct fmt::formatter<Coord> {
    template<typename ParseContext>
    constexpr auto parse(ParseContext& ctx)
    {
        return ctx.begin();
    }

    template<typename FormatContext>
    auto format(const Coord &c, FormatContext &ctx)
    {
        return fmt::format_to(ctx.out(), "({},{})", c.x, c.y);
    }
};

namespace std {
template<>
struct hash<Coord> {
    std::size_t operator()(const Coord &c) const noexcept {
        return ((ull)c.x << 32) | (ull)c.y;
    }
};
}

const auto epos = Coord {-1, -1};


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

std::tuple<Coord, Coord> connected_pipes(const Maze &m, Coord c)
{
    char type = m[c.y][c.x];
    switch (type) {
        case 'F':
            return std::tuple(c + Coord {0, 1}, c + Coord {1, 0});
        case 'J':
            return std::tuple(c + Coord {-1, 0}, c + Coord {0, -1});
        case '|':
            return std::tuple(c + Coord {0, 1}, c + Coord {0, -1});
        case 'S': {
            std::vector<Coord> v;
            auto neighbours = {
                Coord {1,0},
                Coord {-1,0},
                Coord {0,1},
                Coord {0,-1}
            };
            for (auto n : neighbours) {
                if (n.x == -1 || n.y == -1) continue;
                auto [con1, con2] = connected_pipes(m, c + n);
                if (con1 != epos && con1 == c) {
                    v.push_back(c+n);
                }
                else if (con2 != epos && con2 == c) {
                    v.push_back(c+n);
                }
            }
            return std::tuple(v[0], v[1]);
        }
        case 'L':
            return std::tuple(c + Coord {0, -1}, c + Coord {1, 0});
        case '-':
            return std::tuple(c + Coord {1, 0}, c + Coord {-1, 0});
        case '7':
            return std::tuple(c + Coord {-1, 0}, c + Coord {0, 1});
        case '.':
        default:
            return std::tuple(epos, epos);
    }
}

ll solve1(const Maze &m, const Coord &start)
{
    std::vector<Coord> loop;
    std::unordered_set<Coord> visited;

    ll step = 0;

    auto c = start;
    do {
        step++;
        visited.insert(c);
        auto [n1, n2] = connected_pipes(m, c);
        if (visited.count(n1) == 0) {
            c = n1;
        }
        else if (visited.count(n2) == 0) {
            c = n2;
        }
        else {
            fmt::println("Ended loop at: {}", c);
            break;
        }
    } while (c != start);

    return step / 2;
}

int main()
{
    std::fstream fs("../input.txt");
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

