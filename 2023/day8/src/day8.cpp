#include <iostream>
#include <sstream>
#include <fstream>
#include <cstdio>
#include <vector>
#include <set>
#include <map>
#include <numeric>
#include <fmt/core.h>
#include <fmt/ranges.h>


struct Next {
    std::string left, right;
};


int solve1(std::stringstream &ss)
{
    std::map<std::string, Next> m;
    int steps = 0;
    std::string instr;
    std::string line;
    getline(ss, instr);
    getline(ss, line);  // discard empty line

    while (ss.good()) {
        char    cb[4] = {}, 
                lb[4] = {}, 
                rb[4] = {};
        getline(ss, line);
        
        if (line.length() < 1) continue;

        sscanf(line.c_str(), "%3c = (%3c, %3c)", cb, lb, rb);
        
        std::string cur(cb), left(lb), right(rb);
        m[cur] = Next {left, right};
    }

    int cur_instr = 0;
    std::string current("AAA");
    while (current != "ZZZ") {
        steps++;
        switch (instr[cur_instr]) {
            case 'R':
                current = m[current].right;
                break;
            case 'L':
                current = m[current].left;
                break;
        }
        cur_instr += 1;

        if (cur_instr == instr.length()) {
            cur_instr = 0;
        }
    }

    return steps;
}

long long solve2(std::stringstream &ss)
{
    std::map<std::string, Next> m;
    std::string instr;
    std::string line;
    getline(ss, instr);
    getline(ss, line);  // discard empty line

    while (ss.good()) {
        char    cb[4] = {}, 
                lb[4] = {}, 
                rb[4] = {};
        getline(ss, line);
        
        if (line.length() < 1) continue;

        sscanf(line.c_str(), "%3c = (%3c, %3c)", cb, lb, rb);
        
        std::string cur(cb), left(lb), right(rb);
        m[cur] = Next {left, right};
    }

    std::vector<std::string> nodes;
    std::vector<long long> steps_each;
    for (auto &kv : m) {
        if (kv.first[2] == 'A') nodes.push_back(kv.first);
    }

    fmt::println("Running with input: {}", nodes);

    for (auto &n : nodes) {
        long long cur_instr = 0;
        long long steps = 0;
        while (n[2] != 'Z') {
            steps++;
            switch (instr[cur_instr++]) {
                case 'R':
                    n = m[n].right;
                    break;
                case 'L':
                    n = m[n].left;
                    break;
            }

            if (cur_instr == instr.length()) {
                cur_instr = 0;
            }
        }
        steps_each.push_back(steps);
    }

    long long lcm = 1;
    for (auto &s : steps_each) {
        lcm = lcm * s / std::gcd(lcm, s);
    }

    return lcm;
}


int main(void)
{
    std::fstream fs("../input.txt");
    std::stringstream ss;
    ss << fs.rdbuf();

    auto ans1 = solve2(ss);
    fmt::println("Steps: {}", ans1);
}
