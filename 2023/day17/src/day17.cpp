#include <fstream>
#include <sstream>
#include <vector>
#include <string>
#include <stack>
#include <fmt/core.h>
#include <fmt/ranges.h>

typedef long long ll;
typedef std::vector<std::string> Grid;

struct Point {
    ll x, y;

    Point operator+(const Point &rhs) const
    {
        return Point {x + rhs.x, y + rhs.y};
    }

    bool operator==(const Point &rhs) const
    {
        return x == rhs.x && y == rhs.y;
    }
};

enum Direction {
    Up, Down, Left, Right
};

Grid parse(std::stringstream &ss)
{
    Grid g;
    while (ss.good()) {
        std::string s;
        getline(ss, s);
        if (s.length() < 1) continue;
        g.push_back(s);
    }
    return g;
}

std::vector<std::tuple<Point, Direction>> get_next_pos(const Grid &g, Point pos, Direction d, std::vector<std::tuple<Point, Direction>> path)
{
static const std::tuple<Point, Direction> delta_pos[4][3] = {
    {{{1,0}, Right},    {{0,-1}, Up},       {{-1,0}, Left}},
    {{{1,0}, Right},    {{0,1},  Down},     {{-1,0}, Left}},
    {{{0,1}, Down},     {{-1,0}, Left},     {{0,-1}, Up}},
    {{{0,1}, Down},     {{1,0},  Right},    {{0,-1}, Up}},
};
    std::vector<std::tuple<Point, Direction>> ps;
    bool skip_straight = [&] {
        bool ok = true;
        for (auto p = path.end() - 3; p != path.end(); p++) {
            auto [_, dir] = *p;
            ok = ok && d == dir;
        }
        return ok;
    }();
    for (ll i = 0; i < 3; i++) {
        if (i == 1 && skip_straight) continue;
        auto [p, nd] = delta_pos[d][i];
        auto dp = pos + p;
        if (dp.x < 0 || dp.x >= g[0].length()) continue;
        if (dp.y < 0 || dp.y >= g.size()) continue;
        ps.push_back({dp, nd});
    }
    return ps;
}

bool visited(Point pos, const std::vector<std::tuple<Point, Direction>> &path)
{
    bool v = false;
    for (auto [p, d] : path)
        v = v || pos == p;
    return v;
}

void search(const Grid &g, Point pos, Direction d, std::vector<std::tuple<Point, Direction>> &path)
{
    path.push_back(std::tuple<Point, Direction>(pos, d));
    auto end = Point {(ll)g[0].size() - 1, (ll)g.size() - 1};
    if (pos == end) return;
    auto choices = get_next_pos(g, pos, d, path);
    auto [best, best_d] = [&] {
        ll lowest = INT64_MAX;
        Point lp;
        Direction bd;
        for (auto [cp, cd] : choices) {
            if (visited(cp, path)) continue;
            ll val = g[cp.y][cp.x] - '0';
            if (val < lowest) {
                lp = cp;
                bd = cd;
                lowest = val;
            }
        }
        return std::tuple<Point, Direction>({lp, bd});
    }();
    fmt::println("Best: [{},{}], heading {}", best.x, best.y, (ll)best_d);
}

ll solve1(const Grid &g)
{
    std::vector<std::tuple<Point, Direction>> path;
    search(g, Point {0, 0}, Right, path);
    return [&] {
        ll sum = 0;
        for (auto p = path.begin() + 1; p != path.end(); p++) {
            auto [pt, d] = *p;
            sum += g[pt.y][pt.x] - '0';
        }
        return sum;
    }();
}

void run(const char *file)
{
    std::fstream fs(file);
    std::stringstream ss;
    ss << fs.rdbuf();
    auto grid = parse(ss);
    auto ans1 = solve1(grid);
    //auto ans2 = solve2(grid);
    fmt::println("ans1 = {}", ans1);
    //fmt::println("ans2 = {}", ans2);
}

int main()
{
    run("../test.txt");
}
