#include <iostream>
#include <sstream>
#include <fstream>
#include <vector>
#include <fmt/core.h>
#include <fmt/ranges.h>


typedef long long ll;


std::vector<ll> get_diffs(std::vector<ll> v)
{
    std::vector<ll> diffs;
    bool b = true;
    ll last = 0;
    for (auto &n : v) {
        if (b) {
            b = !b;
            last = n;
            continue;
        }

        diffs.push_back(n - last);
        last = n;
    }

    return diffs;
}


ll parseline(const std::string &line)
{
    std::stringstream ss(line);
    std::vector<ll> v;
    while (ss.good()) {
        std::string tok;
        getline(ss, tok, ' ');
        v.push_back(std::stol(tok));
    }
    
    auto diffs = get_diffs(v);
    auto all_zeroes = [](std::vector<ll> v) {
        bool ret = true;
        for (auto &e : v) ret = ret && e == 0;
        return ret;
    };

    ll sum = v[v.size()-1];
    while (!all_zeroes(diffs)) {
        sum += diffs[diffs.size()-1];
        diffs = get_diffs(diffs);
    }

    return sum;
}

ll parse_prev(const std::string &line)
{
    std::stringstream ss(line);
    std::vector<ll> v;
    while (ss.good()) {
        std::string tok;
        getline(ss, tok, ' ');
        v.push_back(std::stol(tok));
    }
    
    auto diffs = get_diffs(v);
    auto all_zeroes = [](std::vector<ll> v) {
        bool ret = true;
        for (auto &e : v) ret = ret && e == 0;
        return ret;
    };

    ll sum = v[0];
    ll sign = -1;
    while (!all_zeroes(diffs)) {
        sum += sign * diffs[0];
        diffs = get_diffs(diffs);
        sign *= -1;
    }

    return sum;
}

ll solve1(std::stringstream &ss)
{
    ll sum = 0;

    while (ss.good()) {
        std::string line;
        getline(ss, line);
        if (line.length() < 1) continue;
        sum += parseline(line);
    }
    return sum;
}

ll solve2(std::stringstream &ss)
{
    ll sum = 0;

    while (ss.good()) {
        std::string line;
        getline(ss, line);
        if (line.length() < 1) continue;
        sum += parse_prev(line);
    }
    return sum;
}


int main()
{
    std::fstream fs("../input.txt");
    std::stringstream ss;
    ss << fs.rdbuf();
    auto ans1 = solve2(ss);
    fmt::println("{}", ans1);
}
