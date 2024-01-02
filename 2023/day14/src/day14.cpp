#include <sstream>
#include <fstream>
#include <vector>
#include <string>
#include <unordered_map>
#include <fmt/core.h>
#include <fmt/ranges.h>

typedef long long ll;

struct Platform {
    std::vector<std::string> grid;

    bool operator==(const Platform &rhs) const
    {
        return grid == rhs.grid;
    }
};

template <>
struct std::hash<Platform> {
    std::size_t operator()(const Platform &k) const 
    {
        std::size_t seed = k.grid.size();
        for(auto x : k.grid) {
            std::size_t sh = std::hash<std::string>()(x);
            x = ((sh >> 16) ^ sh) * 0x45d9f3b;
            x = ((sh >> 16) ^ sh) * 0x45d9f3b;
            x = (sh >> 16) ^ sh;
            seed ^= sh + 0x9e3779b9 + (seed << 6) + (seed >> 2);
        }
        return seed;
    }
};

enum RockType {
    Rounded = 'O',
    Square  = '#',
    None    = '.'
};


void tilt_up(Platform &p)
{
    auto is_empty = [&](ll i, ll j) {
        return p.grid[i-1][j] == None;
    };
    auto move_up = [&](ll i, ll j) {
        p.grid[i-1][j] = p.grid[i][j];
        p.grid[i][j] = None;
    };
    for (ll i = 1; i < p.grid.size(); i++) {
        for (ll j = 0; j < p.grid[i].length(); j++) {
            if (p.grid[i][j] == Rounded) {
                ll o = 0;
                while ((i - o) > 0 && is_empty(i - o, j)) {
                    move_up(i - o, j);
                    o++;
                }
            }
        }
    }
}

void tilt_right(Platform &p)
{
    auto is_empty = [&](ll i, ll j) {
        return p.grid[i][j + 1] == None;
    };
    auto move_right = [&](ll i, ll j) {
        p.grid[i][j + 1] = p.grid[i][j];
        p.grid[i][j] = None;
    };
    for (ll i = 0; i < p.grid.size(); i++) {
        for (ll j = p.grid[i].length() - 2; j >= 0; j--) {
            if (p.grid[i][j] == Rounded) {
                ll o = 0;
                while ((j + o) < (p.grid[i].length() - 1) && is_empty(i, j + o)) {
                    move_right(i, j + o);
                    o++;
                }
            }
        }
    }
}

void tilt_down(Platform &p)
{
    auto is_empty = [&](ll i, ll j) {
        return p.grid[i + 1][j] == None;
    };
    auto move_down = [&](ll i, ll j) {
        p.grid[i + 1][j] = p.grid[i][j];
        p.grid[i][j] = None;
    };
    for (ll i = p.grid.size()-2; i >= 0; i--) {
        for (ll j = 0; j < p.grid[i].length(); j++) {
            if (p.grid[i][j] == Rounded) {
                ll o = 0;
                while ((i + o) < (p.grid.size() - 1) && is_empty(i + o, j)) {
                    move_down(i + o, j);
                    o++;
                }
            }
        }
    }
}

void tilt_left(Platform &p)
{
    auto is_empty = [&](ll i, ll j) {
        return p.grid[i][j - 1] == None;
    };
    auto move_left = [&](ll i, ll j) {
        p.grid[i][j - 1] = p.grid[i][j];
        p.grid[i][j] = None;
    };
    for (ll i = 0; i < p.grid.size(); i++) {
        for (ll j = 1; j < p.grid[i].length(); j++) {
            if (p.grid[i][j] == Rounded) {
                ll o = 0;
                while ((j - o) > 0 && is_empty(i, j - o)) {
                    move_left(i, j - o);
                    o++;
                }
            }
        }
    }
}

void spin(Platform &p)
{
    tilt_up(p);
    tilt_left(p);
    tilt_down(p);
    tilt_right(p);
}

Platform parse(std::stringstream &ss)
{
    Platform pf;
    while (ss.good()) {
        std::string s;
        getline(ss, s);
        if (s.length() == 0) continue;
        pf.grid.push_back(s);
    }
    return pf;
}

ll total_load(const Platform &pf)
{
    ll total = 0;
    const ll max = pf.grid.size();
    for (ll i = 0; i < pf.grid.size(); i++) {
        for (const auto &r : pf.grid[i]) {
            if (r == Rounded)
                total += max - i;
        }
    }
    return total;
}

void run(const char *file)
{
    ll cycles = 1e9;
    std::fstream fs(file);
    std::stringstream ss;
    ss << fs.rdbuf();
    auto pf = parse(ss);
    auto pf2 = pf;
    tilt_up(pf);
    fmt::println("Total load: {}", total_load(pf));

    ll load = 0;
    bool loopFound = false;
    Platform loopStart;
    ll looplength = 0;
    std::unordered_map<Platform, ll> visited; 
    for (ll i = 0; i < cycles; i++) {
        spin(pf2);
        if (!visited.count(pf2)) 
            visited[pf2] = 0;
        else if (visited[pf2]) {
            visited[pf2] += 1;
            if (loopFound && loopStart == pf2) {
                ll rem = cycles - i - 1;
                ll in_loop = rem % looplength;
                for (ll _ = 0; _ < in_loop; _++) {
                    //fmt::println("loop load: {}", total_load(pf2));
                    spin(pf2);
                }
                load = total_load(pf2);
                break;
            }
            else if (!loopFound) {
                loopFound = true;
                loopStart = pf2;
                fmt::println("Loop found");
            } 
            looplength++;
            continue;
        }
        visited[pf2] += 1;
    }

    fmt::println("Total load: {}", load);
}

int main()
{
    run("../test.txt");
    run("../input.txt");
}
