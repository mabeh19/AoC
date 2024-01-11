#include <iostream>
#include <fstream>
#include <sstream>
#include <vector>
#include <unordered_map>
#include <string>
#include <functional>
#include <fmt/core.h>
#include <fmt/ranges.h>

struct Workflow;
struct Part;
typedef std::function<bool (Part)> Condition;
typedef std::unordered_map<std::string, Workflow> Workflows;
typedef std::vector<Part> Parts;

struct Rule {
    Condition condition;
    int compval;
    char compType;
    char compChar;
    std::string destination;
};

struct Workflow {
    std::string name;
    std::vector<Rule> rules;
};

struct Part {
    long long x, m, a, s;
};

struct Range {
    long long low, high;

    bool operator==(const Range &rhs) const 
    {
        return low == rhs.low && high == rhs.high;
    }
};

struct PartRange {
    Range x, m, a, s;
};

std::ostream& operator<<(std::ostream& os, const Part &p)
{
    os << "{x = " << p.x << ", m = " << p.m << ", a = " << p.a << ", s = " << p.s << "}";
    return os;
}

bool handle_part(const Part &p, const Workflows &ws)
{
    auto wf = ws.at("in");

    for (;;) {

        for (auto &r : wf.rules) {
            if (r.condition(p)) {
                std::string dest = r.destination;
                if (dest == "A") {
                    return true;
                }
                else if (dest == "R") {
                    return false;
                }
                wf = ws.at(dest);
                break;
            }
        }
    }

    return false;
}

std::tuple<std::vector<Workflow>, std::vector<Rule>> getRulesForDestination(const Workflows &wfs, const std::string &dest)
{
    std::vector<Workflow> ass_wfs;
    std::vector<Rule> rules;
    for (auto &[k, v] : wfs) {
        for (auto &r : v.rules) {
            if (r.destination == dest) {
                ass_wfs.push_back(v);
                rules.push_back(r);
            }
        }
    }

    return {ass_wfs, rules};
}

Range getValidRange(const Rule &r, Range range)
{
    Range valid = range;
    switch (r.compType) {
    case '>':
        valid.low = std::max(r.compval + 1LL, range.low);
        break;
    case '<':
        valid.high = std::min(r.compval - 1LL, range.high);
        break;
    }

    if (valid.high <= valid.low) {
        valid = {0LL, 0LL};
    }

    return valid;
}

std::vector<PartRange> getValidRanges(const std::vector<Rule> &rules, const PartRange &prange)
{
    std::vector<PartRange> pranges;
    for (auto &r : rules) {
        PartRange nr = prange;
        switch (r.compChar) {
        case 'x':
            nr.x = getValidRange(r, prange.x);
            break;
        case 'm':
            nr.m = getValidRange(r, prange.m);
            break;
        case 'a':
            nr.a = getValidRange(r, prange.a);
            break;
        case 's':
            nr.s = getValidRange(r, prange.s);
            break;
        }
        pranges.push_back(nr);
    }

    return pranges;
}

bool performComp(int pval, int val, char comp)
{
    //std::cout << "Performing comp " << pval << " " << comp << " " << val << std::endl;
    switch (comp) {
    case '>':
        return pval > val;
    case '<':
        return pval < val;
    }
    return false;
}

Rule parseRule(const std::string &s)
{
    //std::cout << "Parsing rule " << s << std::endl;

    if (s.find(':') == s.npos) {
        return Rule {
            .condition = [](Part p) { return true; },
            .destination = s
        };
    }

    char partCategory = s[0];
    char comp = s[1];
    int val = strtol(s.c_str() + 2, NULL, 10);
    std::string dest = s.substr(s.find(":") + 1);

    return (Rule) {
        .condition = [=](Part p) {
            int pval = [&]() {
                switch (partCategory) {
                case 'a':
                    return p.a;
                case 'x':
                    return p.x;
                case 'm':
                    return p.m;
                case 's':
                    return p.s;
                }
                return 0LL;
            }();

            return performComp(pval, val, comp);
        },
        .compval = val,
        .compType = comp,
        .compChar = partCategory,
        .destination = dest
    };
}

Workflow parseWorkflow(const std::string &s)
{
    //std::cout << "Parsing Workflow " << s << std::endl;
    Workflow wf;
    size_t namelen = s.find("{");
    wf.name = s.substr(0, namelen);
    std::string rules = s.substr(namelen + 1, s.length() - namelen - 2);
    std::stringstream ss(rules);

    while (ss.good()) {
        std::string rule;
        std::getline(ss, rule, ',');
        Rule r = parseRule(rule);
        wf.rules.push_back(r);
    }

    return wf;
}

Part parsePart(const std::string &s)
{
    std::stringstream ss(s.substr(1));
    std::string xstr;
    std::string mstr;
    std::string astr;
    std::string sstr;

    getline(ss, xstr, ',');
    getline(ss, mstr, ',');
    getline(ss, astr, ',');
    getline(ss, sstr, ',');

    return Part {
        .x = (int)strtol(xstr.c_str() + 2, NULL, 10),
        .m = (int)strtol(mstr.c_str() + 2, NULL, 10),
        .a = (int)strtol(astr.c_str() + 2, NULL, 10),
        .s = (int)strtol(sstr.c_str() + 2, NULL, 10)
    };
}


std::tuple<Workflows, Parts> parseFile(std::stringstream &ss)
{
    Workflows ws;
    Parts parts;
    bool parsingWfs = true;

    while (ss.good()) {
        std::string line;
        getline(ss, line);
        if (line.length() == 0) {
            parsingWfs = false;
            continue;
        }
        if (parsingWfs) {
            auto wf = parseWorkflow(line);
            ws[wf.name] = wf;
        }
        else {
            auto part = parsePart(line);
            parts.push_back(part);
        }
    }
    return {ws, parts};
}


void solve1(const char *file)
{
    std::fstream fs(file);
    std::stringstream ss;
    ss << fs.rdbuf();

    auto [wfs, pts] = parseFile(ss);
    Parts accepted;
    Parts rejected;
    
    for (auto &p : pts) {
        if (handle_part(p, wfs)) {
            accepted.push_back(p);
        }
        else {
            rejected.push_back(p);
        }
    }

    long long total = 0;
    for (auto &p : accepted) {
        std::cout << "accepted: " << p << std::endl;
        total += p.x + p.m + p.a + p.s;
    }

    std::cout << "Total: " << total << std::endl;
}

std::vector<PartRange> findRanges(const Workflows &wfs, std::vector<PartRange> ranges, std::string cd)
{
    auto [awfs, rs] = getRulesForDestination(wfs, cd);
    std::vector<PartRange> newRanges;

    if (awfs.size() == 0)
        return ranges;

    for (auto &pr : ranges) {
        auto sr = getValidRanges(rs, pr);
        newRanges.insert(newRanges.end(), sr.begin(), sr.end());
    }

    std::vector<PartRange> nnr;
    for (auto &wf : awfs) {
        auto sr = findRanges(wfs, newRanges, wf.name);
        nnr.insert(nnr.end(), sr.begin(), sr.end());
    }

    return nnr;
}
    
auto zero = Range {0, 0};

void pruneOverlaps(std::vector<PartRange> &pr)
{
    auto get_overlap = [](const Range &p1, const Range &p2) {
        if (p1 == zero || p2 == zero) return zero;

        if (p1.low >= p2.low && p1.low <= p2.high) {
            return Range { p1.low, std::min(p2.high, p1.high) };
        }
        else if (p2.low >= p1.low && p2.low <= p1.high) {
            return Range { p2.low, std::min(p2.high, p1.high) };
        }

        return zero;
    };
    for (size_t i = 0; i < pr.size(); i++) {
        for (size_t j = 0; j < pr.size(); j++) {
            if (i == j) continue;
            auto *pr1 = &pr[i];
            auto *pr2 = &pr[j];
            
            auto xo = get_overlap(pr1->x, pr2->x);
            auto mo = get_overlap(pr1->m, pr2->m);
            auto ao = get_overlap(pr1->a, pr2->a);
            auto so = get_overlap(pr1->s, pr2->s);

            if (xo == zero ||
                mo == zero ||
                ao == zero ||
                so == zero) {

                continue;
            }

            if (xo.low == pr1->x.low) {
                if (xo.high == pr1->x.high)
                    pr1->x = zero;
                else
                    pr1->x.low = xo.high;
            }
            else if (xo.low == pr2->x.low) {
                if (xo.high == pr2->x.high)
                    pr2->x = zero;
                else
                    pr1->x.high = xo.low;
            }

            if (mo.low == pr1->m.low) {
                if (mo.high == pr1->m.high)
                    pr1->m = zero;
                else
                    pr1->m.low = mo.high;
            }
            else if (mo.low == pr2->m.low) {
                if (mo.high == pr2->m.high)
                    pr2->m = zero;
                else
                    pr1->m.high = mo.low;
            }

            if (ao.low == pr1->a.low) {
                if (ao.high == pr1->a.high)
                    pr1->a = zero;
                else
                    pr1->a.low = ao.high;
            }
            else if (ao.low == pr2->a.low) {
                if (ao.high == pr2->a.high)
                    pr2->a = zero;
                else
                    pr1->a.high = ao.low;
            }

            if (so.low == pr1->s.low) {
                if (so.high == pr1->s.high)
                    pr1->s = zero;
                else
                    pr1->s.low = so.high;
            }
            else if (so.low == pr2->s.low) {
                if (so.high == pr2->s.high)
                    pr2->s = zero;
                else
                    pr1->s.high = so.low;
            }
        }
    }
}

void solve2(const char *file)
{
    std::fstream fs(file);
    std::stringstream ss;
    ss << fs.rdbuf();

    auto [wfs, pts] = parseFile(ss);
    std::vector<PartRange> cr{PartRange { {1, 4000}, {1, 4000}, {1, 4000}, {1, 4000} }};

    auto ans = findRanges(wfs, cr, "A");

    pruneOverlaps(ans);

    long long tot = 0;
    for (auto &pr : ans) {
        tot += (pr.x.high - pr.x.low) * (pr.m.high - pr.m.low) * (pr.a.high - pr.a.low) * (pr.s.high - pr.s.low);
    }

    std::cout << "Total: " << tot << std::endl;
}


int main()
{
    solve1("../test.txt");
    solve2("../test.txt");
    //solve1("../input.txt");
}



