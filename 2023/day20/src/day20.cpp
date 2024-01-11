#include <iostream>
#include <fstream>
#include <sstream>
#include <unordered_map>
#include <vector>
#include <string>

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

    virtual void Send_Signal(const std::string &input, bool pulse) {}
    virtual void Reset() {}

    virtual std::vector<std::string> Propagate()
    {
        std::vector<std::string> vs;
        for (auto &s : dms) {
            if (pulse)
                highs++;
            else
                lows++;
            modules->at(s)->Send_Signal(name, pulse);
            vs.push_back(s);
        }
        return vs;
    }
};


class FlipFlop : public Module {


public:
    bool lastInput;

    FlipFlop(Modules* m, std::string n, std::vector<std::string> ds)
    {
        dms = ds;
        modules = m;
        name = n;
        pulse = false; 
    }

    ~FlipFlop() {}

    virtual void Send_Signal(const std::string &input, bool pulse) override
    {
        lastInput = pulse;
        if (!pulse)
            this->pulse ^= 1;
    }

    virtual void Reset() override
    {
        pulse = false;
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
                modules->at(s)->Send_Signal(name, pulse);
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

    virtual void Send_Signal(const std::string &input, bool pulse) override
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

    virtual void Send_Signal(const std::string &input, bool pulse) override
    {
        this->pulse = pulse;
    }

    virtual void Reset() override
    {
        /* empty */
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

    return highs * (presses + lows);
}


void runSystem(Modules* mods, size_t iters)
{
    auto next = std::vector<std::string>{"broadcaster"};
    for (size_t i = 0; i < iters; i++) {
        while (next.size()) {
            next = sendSignals(mods, next);
        }
    }

    std::cout << "Total: " <<  prodPulses(mods, iters) << std::endl;
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

        std::string name = s.substr(0, s.find(" -> "));
        std::string dest = s.substr(s.find(" -> ") + 4);

        std::vector<std::string> dests;
        std::stringstream dss(dest);

        while (dss.good()) {
            std::string s;
            getline(ss, s, ',');
            dests.push_back(s);
        }

        if (name == "broadcaster") {
            ms->insert({name, new Broadcaster(ms, name, dests)});
            continue;
        }

        switch (name[0]) {
        case '%':
            ms->insert({name, new FlipFlop(ms, name, dests)});
            break;
        case '&':
            ms->insert({name, new Conjunction(ms, name, dests, {})});
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

    runSystem(ms, 1);
}


int main()
{
    solve1("../test.txt");
}
