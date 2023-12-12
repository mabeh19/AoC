#include <fstream>
#include <sstream>
#include <vector>
#include <fmt/core.h>
#include <fmt/ranges.h>


typedef unsigned long long ull;
typedef long long ll;

struct Expansion {
    enum Type {
        Col,
        Row
    } type;
    ll idx;
};

template<>
struct fmt::formatter<Expansion> {
    template<typename ParseContext>
    constexpr auto parse(ParseContext& ctx)
    {
        return ctx.begin();
    }

    template<typename FormatContext>
    auto format(const Expansion &c, FormatContext &ctx)
    {
        return fmt::format_to(ctx.out(), "{}@{}", c.type == Expansion::Col ? "Col" : "Row", c.idx);
    }
};

struct Point {
    ll x, y;

    bool operator==(const Point &rhs) const
    {
        return x == rhs.x && y == rhs.y;
    }
};


template<>
struct fmt::formatter<Point> {
    template<typename ParseContext>
    constexpr auto parse(ParseContext& ctx)
    {
        return ctx.begin();
    }

    template<typename FormatContext>
    auto format(const Point &c, FormatContext &ctx)
    {
        return fmt::format_to(ctx.out(), "({},{})", c.x, c.y);
    }
};

bool is_empty_row(const std::string &s)
{
    bool empty = true;
    for (auto c : s) 
        empty = empty && c == '.';
    return empty;
}

bool is_empty_col(const std::vector<std::string> &grid, int col)
{
    bool empty = true;
    for (int i = 0; i < grid.size(); i++)
        empty = empty && grid[i][col] == '.';
    return empty;
}

std::vector<Point> get_points(const std::string &s, int row)
{
    std::vector<Point> pv;
    for (int i = 0; i < s.length(); i++) 
         if (s[i] == '#') pv.push_back(Point{i, row});
    return pv;
}

std::tuple<std::vector<std::string>, std::vector<Expansion>, std::vector<Point>> parse_galaxy(std::stringstream &ss)
{
    std::vector<std::string> galaxy;
    std::vector<Expansion> expansions;
    std::vector<Point> points;

    while (ss.good()) {
        std::string s;
        getline(ss, s);
        if (s.length() < 1) continue;

        std::string es(s.length(), '.');
        if (is_empty_row(s))
            expansions.push_back(Expansion { Expansion::Row, (ll)galaxy.size() });
        galaxy.push_back(s);
    }

    for (ll i = 0; i < galaxy[0].length(); i++) {
        if (is_empty_col(galaxy, i)) {
            expansions.push_back(Expansion { Expansion::Col, i });
        }
    }

    for (int i = 0; i < galaxy.size(); i++) 
        for (auto p : get_points(galaxy[i], i))
            points.push_back(p);
    

    return {galaxy, expansions, points};
}

ll solve1(const std::vector<std::string> &galaxy, const std::vector<Expansion> &expansions, const std::vector<Point> &points)
{
    ll sum = 0;
    std::vector<std::tuple<Point, Point>> visited;
    auto is_in = [&](Point p1, Point p2) { for (auto [v1, v2] : visited) if ((p1 == v1 && p2 == v2) || (p2 == v1 && p1 == v2)) return true; return false; };
    auto add_expansion = [&](Point p1, Point p2) {
        ll total = 0;
        for (auto &e : expansions) {
            bool isbetween = false;
            switch (e.type) {
                case Expansion::Col:
                    isbetween = ((e.idx > p1.x) && (e.idx < p2.x)) || ((e.idx > p2.x) && (e.idx < p1.x));
                    break;
                case Expansion::Row:
                    isbetween = ((e.idx > p1.y) && (e.idx < p2.y)) || ((e.idx > p2.y) && (e.idx < p1.y));
                    break;
            }
            if (isbetween)
                total += 1;
        }
        return total;
    };
    auto dist = [&](Point p1, Point p2) {
        return std::abs(p1.x - p2.x) + std::abs(p1.y - p2.y) + add_expansion(p1, p2);
    };
    for (int i = 0; i < points.size(); i++) {
        auto p1 = points[i];
        for (int j = 0; j < points.size(); j++) {
            if (j == i) continue;
            auto p2 = points[j];
            if (is_in(p1, p2)) continue;
            sum += dist(p1, p2);
        }
    }
    return sum;
}

ll solve2(const std::vector<std::string> &galaxy, const std::vector<Expansion> &expansions, const std::vector<Point> &points)
{
    ll sum = 0;
    std::vector<std::tuple<Point, Point>> visited;
    auto is_in = [&](Point p1, Point p2) { for (auto [v1, v2] : visited) if ((p1 == v1 && p2 == v2) || (p2 == v1 && p1 == v2)) return true; return false; };
    auto add_expansion = [&](Point p1, Point p2) {
        ll total = 0;
        for (auto &e : expansions) {
            bool isbetween = false;
            switch (e.type) {
                case Expansion::Col:
                    isbetween = ((e.idx > p1.x) && (e.idx < p2.x)) || ((e.idx > p2.x) && (e.idx < p1.x));
                    break;
                case Expansion::Row:
                    isbetween = ((e.idx > p1.y) && (e.idx < p2.y)) || ((e.idx > p2.y) && (e.idx < p1.y));
                    break;
            }
            if (isbetween)
                total += 1e6 - 1;
        }
        return total;
    };
    auto dist = [&](Point p1, Point p2) {
        return std::abs(p1.x - p2.x) + std::abs(p1.y - p2.y) + add_expansion(p1, p2);
    };
    for (int i = 0; i < points.size(); i++) {
        auto p1 = points[i];
        for (int j = 0; j < points.size(); j++) {
            if (j == i) continue;
            auto p2 = points[j];
            if (is_in(p1, p2)) continue;
            sum += dist(p1, p2);
        }
    }
    return sum;
}


int main()
{
    std::fstream fs("../input.txt");
    std::stringstream ss;
    ss << fs.rdbuf();
    auto [galaxy, expansions, points] = parse_galaxy(ss);

    fmt::print("Expansions : [");
    for (auto e : expansions)
         fmt::print("{}, ", e);
    fmt::println("]");

    auto ans1 = solve1(galaxy, expansions, points);
    fmt::println("Ans1 = {}", ans1);

    auto ans2 = solve2(galaxy, expansions, points);
    fmt::println("Ans2 = {}", ans2);
}

