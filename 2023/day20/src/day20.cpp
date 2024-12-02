#include <iostream>
#include <fstream>
#include <sstream>
#include <unordered_map>
#include <vector>
#include <string>
#include <fmt/core.h>
#include <fmt/ranges.h>

class Module;
typedef std::unordered_map<std::string, Module*> Modules;

class Module {


public:
    std::vector<std::string> dms;
    Modules* modules;
    std::string name;
    bool pulse;
    size_t highs = 0;
    size_t lows = 0;

    Module () {}
    ~Module () {}

    virtual void SendSignal(const std::string &input, bool pulse) {}
    virtual void Reset() {}

    virtual std::vector<std::string> Propagate()
    {
        std::vector<std::string> vs;
        for (auto &s : dms) {
            if (pulse)
                highs++;
            else
                lows++;
            //modules->at(s)->SendSignal(name, pulse);
            vs.push_back(s);
        }
        return vs;
    }
};


class FlipFlop : public Module {


public:
    bool lastInput = false;

    FlipFlop(Modules* m, std::string n, std::vector<std::string> ds)
    {
        dms = ds;
        modules = m;
        name = n;
        pulse = false; 
    }

    ~FlipFlop() {}

    virtual void SendSignal(const std::string &input, bool pulse) override
    {
        lastInput = pulse;
        if (!pulse)
            this->pulse ^= 1;
    }

    virtual void Reset() override
    {
        pulse = false;
        highs = 0;
        lows = 0;
        lastInput = false;
    }

    virtual std::vector<std::string> Propagate() override
    {
        std::vector<std::string> vs;
        if (!lastInput) {
            for (auto &s : dms) {
                if (pulse)
                    highs++;
                else
                    lows++;
                //modules->at(s)->SendSignal(name, pulse);
                vs.push_back(s);
            }
        }
        return vs;
    }
};

class Conjunction : public Module {

public:
    std::unordered_map<std::string, bool> inputs;

    Conjunction(Modules* m, std::string n, std::vector<std::string> ds, const std::vector<std::string> &inp) 
    { 
        dms = ds;
        modules = m;
        name = n;
        for (auto &i : inp) {
            inputs[i] = false;
        }
    }

    ~Conjunction() {}

    virtual void SendSignal(const std::string &input, bool pulse) override
    {
        bool out = true;
        inputs[input] = pulse;

        for (auto [k,v] : inputs) {
            out = out && v;
        }

        this->pulse = !out;
    }

    virtual void Reset() override
    {
        highs = 0;
        lows = 0;
        for (auto [k,v] : inputs) {
            inputs[k] = false;
        }
    }
};


class Broadcaster : public Module {

public:

    Broadcaster(Modules* m, std::string n, std::vector<std::string> ds) 
    { 
        dms = ds;
        modules = m;
        name = n;
        pulse = false; 
    }

    ~Broadcaster() {}

    virtual void SendSignal(const std::string &input, bool pulse) override
    {
        this->pulse = pulse;
    }

    virtual void Reset() override
    {
        /* empty */
        highs = 0;
        lows = 0;
    }
};


std::vector<std::string> sendSignals(Modules* mods, std::vector<std::string> ms)
{
    std::vector<std::string> new_ms;
    for (auto &s : ms) {
        auto m = mods->at(s);
        auto propped_to = m->Propagate();
        new_ms.insert(new_ms.end(), propped_to.begin(), propped_to.end());
    }

    for (auto &s : ms) {
        auto m = mods->at(s);
        for (auto &d : m->dms) {
            m->SendSignal(d, m->pulse);
        }
    }

    return new_ms;
}


size_t prodPulses(const Modules* mods, size_t presses)
{
    size_t highs = 0;
    size_t lows = 0;

    for (auto [k,v] : *mods) {
        highs += v->highs;
        lows += v->lows;
    }

    std::cout << "highs: " << highs << std::endl;
    std::cout << "lows: " << lows << std::endl;
    return highs * (presses + lows);
}


void runSystem(Modules* mods, size_t iters)
{
    for (size_t i = 0; i < iters; i++) {
        auto next = std::vector<std::string>{"broadcaster"};
        while (next.size()) {
            next = sendSignals(mods, next);
        }
    }

    std::cout << "Total: " << prodPulses(mods, iters) << std::endl;
}

void resetSystem(Modules* mods)
{
    for (auto [k,v] : *mods) {
        v->Reset();
    }
}


Modules* parseFile(std::stringstream& ss)
{
    Modules* ms = new Modules;

    while (ss.good()) {
        std::string s;
        getline(ss, s);

        if (s.length() == 0) continue;

        char type = s[0];
        std::string name = s.substr(0, s.find(" -> "));
        std::string dest = s.substr(name.length() + 4);

        std::cout << "Name: " << name << " Dests: " << dest << std::endl;

        std::vector<std::string> dests;
        std::stringstream dss(dest);

        while (dss.good()) {
            std::string s;
            getline(dss, s, ',');
            if (s[0] == ' ') s = s.substr(1);
            dests.push_back(s);
        }

        switch (type) {
        case '%':
            ms->insert({name.substr(1), new FlipFlop(ms, name.substr(1), dests)});
            break;
        case '&':
            ms->insert({name.substr(1), new Conjunction(ms, name.substr(1), dests, {})});
            break;
        case 'b':
            ms->insert({name, new Broadcaster(ms, name, dests)});
            break;
        }
    }

    /*
     * Run one iter
     * to fill out conjunctions...
     */
    runSystem(ms, 1);
    resetSystem(ms);    // ..and then reset system

    return ms;
}


void solve1(const char *path)
{
    std::fstream fs(path);
    std::stringstream ss;
    ss << fs.rdbuf();
    auto ms = parseFile(ss);

    runSystem(ms, 1000);
}


int main()
{
    solve1("../test.txt");
}
