#include <iostream>
#include <sstream>
#include <cstdio>
#include <fstream>
#include <map>
#include <set>
#include <vector>

#include <fmt/core.h>
#include <fmt/ranges.h>


typedef std::string Pos;

struct Next {
    Pos left, right;
};

struct Day8 {
    std::string ins;
    std::map<Pos, Next> m;
};

Day8 parse(std::stringstream &ss)
{
    Day8 d;

    ss >> d.ins;

    while (ss.good()) {
        char cb[4] = {}, lb[4] = {}, rb[4] = {};
        std::string inter;
        getline(ss, inter);
        if (inter.length() < 2) continue;
        sscanf(inter.c_str(), "%3c = (%3c, %3c)", cb, lb, rb);

        auto n = Next{};
        d.m[std::string(cb)] = Next { std::string(lb), std::string(rb) };
    }

    return d;
}



int solve1(Day8 &d)
{
    return 0;
}


int main()
{
    const char *tst = "test.txt";
    std::fstream fs(tst);
    std::stringstream ss;
    ss << fs.rdbuf();

    auto m = parse(ss);

    auto ans1 = solve1(m);

    fmt::println("{}", ans1);
}
