#include <sstream>
#include <fstream>
#include <vector>
#include <map>
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

    Coord operator-(const Coord &rhs) const
    {
        return Coord { x - rhs.x, y - rhs.y };
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
    
Coord& operator+=(Coord& lhs, const Coord &rhs)
{
    lhs.x += rhs.x;
    lhs.y += rhs.y;
    return lhs;
}

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

const Coord epos    = {-1, -1};
const Coord LEFT    = {-1, 0};
const Coord RIGHT   = {1, 0};
const Coord UP      = {0, -1};
const Coord DOWN    = {0, 1};


ll is_vertical(char c)
{
    switch (c)
    {
        case 'F':
        case 'J':
        case '|':
        case 'L':
        case '7':
            return 1;
        case 'S': 
        case '-':
        case '.':
        default:
            return 0;
    }
}

ll num_crosses(const Maze &m, std::map<Coord, bool> loop, Coord c)
{
    auto c_cpy = c;
    auto count = 0;

    if (loop.count(c)) return 0;

    auto line = m[c.y].substr(0, c.x);

    while (c.x >= 0) {
        static char last_v_char = '\0';
        bool isvertical = is_vertical(line[c.x]);
        if (isvertical) {
            switch (line[c.x]) {
                case '7':
                    isvertical = last_v_char != 'L';
                    break;
                case 'J':
                    isvertical = last_v_char != 'F';
                    break;
                default:
                    last_v_char = line[c.x];
                    break;
            }
        }
        count += loop.count(c) && isvertical;
        c += LEFT;
    }

    return count;
}

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

std::tuple<Coord, Coord> connected_pipes(Maze &m, Coord c)
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
                LEFT,
                RIGHT,
                UP,
                DOWN
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
            auto first = v[0] - c;
            auto second = v[1] - c;
            char new_S;
            for (int i = 0; i < 2; i++) {
                if (first == LEFT) {
                    if (second == UP) {
                        new_S = 'J';
                        break;
                    } 
                    else if (second == DOWN) {
                        new_S = '7';
                        break;
                    }
                    else if (second == RIGHT) {
                        new_S = '-';
                        break;
                    }
                }
                else if (first == UP) {
                    if (second == DOWN) {
                        new_S = '|';
                        break;
                    }
                    else if (second == RIGHT) {
                        new_S = 'L';
                        break;
                    }
                }
                else if (first == RIGHT) {
                    if (second == DOWN) {
                        new_S = 'F';
                        break;
                    }
                }
                std::swap(first, second);
            }
            m[c.y][c.x] = new_S;
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

ll solve1(Maze &m, const Coord &start)
{
    std::vector<Coord> loop;
    std::map<Coord, bool> visited;

    ll step = 0;

    auto c = start;
    do {
        step++;
        visited[c] = true;
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

ll solve2(Maze &m, const Coord &start)
{
    std::vector<Coord> loop;
    std::map<Coord, bool> visited;

    ll step = 0;

    //
    // Construct loop
    //
    auto c = start;
    do {
        step++;
        visited[c] = true;
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

    auto points = 0;
    for (int i = 0; i < m.size(); i++) {
        for (int j = 0; j < m[i].length(); j++) {
            auto e = Coord {j, i};
            // shoot out a line to see how many lines in the loop it crosses
            // if the amount is odd, it is inside
            // if it is even, it is outside
            points += num_crosses(m, visited, e) % 2;
        }
    }

    return points;
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

    auto ans2 = solve2(maze, start);
    fmt::println("{}", ans2);
}

