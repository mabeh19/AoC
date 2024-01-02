#include <fstream>
#include <sstream>
#include <vector>
#include <string>
#include <fmt/core.h>
#include <fmt/ranges.h>

typedef long long ll;

struct SubRecord {
    std::string     block;
    std::vector<ll> gears;
};

struct Line {
    std::vector<SubRecord> subrecs;
};

std::vector<Line> parse(std::stringstream &ss)
{
    std::vector<Line> vs;
    while (ss.good()) {
        std::string record;
        std::string rest;
        getline(ss, record, ' ');   // get first part of line
        getline(ss, rest);          // get second part
        Line l;
        std::vector<ll> gears;
        std::stringstream sub_ss(rest);
        while (sub_ss.good()) {
            std::string g;
            getline(sub_ss, g, ',');
            gears.push_back(std::strtol(g.c_str(), NULL, 10));
        }

        std::string block;
        std::vector<ll> sr_gs;
        ll known_spots = 0;
        for (auto c : record) {
            switch (c) {
                case '?':
                    block.push_back(c);
                    break;
                case '#':
                    known_spots++;
                    if (0) {
                        l.subrecs.push_back(SubRecord { block, sr_gs });
                        block.clear();
                    }
                    break;
            }
        }
    }
    return vs;
}

ll limit_n(const SubRecord &sr)
{
    ll max = sr.gears.size() == 1 ? sr.block.length() : sr.block.length() - [&] {
        ll t = 0;
        for (auto g = sr.gears.begin() + 1; g != sr.gears.end(); g++)
            t += *g + 1;
        return t;
    }();
    ll preceding = 0, proceding = 0;
    ll t = 0;
    bool parsing = false;

    for (auto c : sr.block) {
        switch (c) {
            case '#':
                parsing = true;
                t++;
                if (t + preceding + proceding == sr.gears[0])
                    parsing = false;
                break;
            case '?':
                if (!parsing && t == 0)
                    preceding++;
                else {
                    proceding++;
                    parsing = false;
                }
                break;
            default:
                break;
        }
    }

    ll range = sr.gears[0] - t;
    ll ret = !t ? max : std::max(0LL, std::min(max, std::min(preceding, range) + std::min(proceding, range) + t));
    fmt::println("Max: {}, range: {}, preceding: {}, proceding: {}, t: {} => ret == {}", max, range, preceding, proceding, t, ret);
    return ret;
}

ll solve_subrec(const SubRecord &sr)
{
    ll N = limit_n(sr);
    if (N == 0) return 0;
    if (sr.gears.size() == 1) {
        return N - sr.gears[0] + 1;
    }

    auto g = sr.gears[0];
    ll perms_sub = INT32_MAX;
    ll total = 0;
    for (ll i = 0; perms_sub > 0; i++) {
        auto new_sr = SubRecord {
            .block = sr.block.substr(g + 1 + i),
            .gears = std::vector<ll>(sr.gears.begin() + 1, sr.gears.end())
        };
        perms_sub = solve_subrec(new_sr);
        total += perms_sub;
    }
    return total;
}

ll get_possible_solutions(const Line &s)
{
    ll total = 0;
    for (auto &sr : s.subrecs) {
        total += solve_subrec(sr);
    }
    return total;
}

ll solve1(const std::vector<Line> &vs)
{
    ll total = 0;
    for (auto &l : vs) {
        total += get_possible_solutions(l);
    }
    return total;
}


int main()
{
    #define SUBREC(str, ...) {.subrecs = std::vector<SubRecord>({SubRecord{str, std::vector<ll>({__VA_ARGS__})}})}
    std::fstream fs("../test.txt");
    std::stringstream ss;
    ss << fs.rdbuf();

    std::vector<Line> lines = {
        // SUBREC("????",      1),
        // SUBREC("???????",   2, 1),
        // SUBREC("????????",  2, 2, 1),
        // SUBREC("???",       1, 1),
        // SUBREC("#??",       2),
        // SUBREC("#?#",       3),
        // SUBREC("#?#??",     4),
        // SUBREC("?##??",     4),
        // SUBREC("?#?#?#?#?#?#?#?", 1, 3, 1, 6),
        SUBREC("#??#????",  2, 2, 1),
    };

    auto ans1 = solve1(lines);
    fmt::println("{}", ans1);
}
