package day7

import "core:os"
import "core:fmt"
import "core:strings"
import "core:strconv"
import "core:unicode"
import "core:testing"


Circuit :: map[Id]Wire

GateType :: enum {
        AND,
        OR,
        LSHIFT,
        RSHIFT,
        NOT,
}

Signal :: union {
        u16,
        Input,
        Gate,
}

Id :: string

Input :: union {
        Id,
        u16
}

Wire :: struct {
        id: Id,
        sig: Signal,
}


Gate :: struct {
        type: GateType,
        inputs: [2]Input,
        output: Id,
        ctx: int
}

NC :: ""

main :: proc() {
        f, _ := os.open("input.txt")
        defer os.close(f)
        f_inp, _ := os.read_entire_file(f)
        input := string(f_inp)

        circuit := parse_circuit(input)

        fmt.println(sim_wire(&circuit, "a"))
}

get_gate_type :: proc(str: string) -> GateType {
        switch str {
        case "RSHIFT":
                return .RSHIFT
        case "LSHIFT":
                return .LSHIFT
        case "AND":
                return .AND
        case "NOT":
                return .NOT
        case "OR":
                return .OR
        }

        panic("Unknown gate")
}

to_input :: proc(str: string) -> Input {
        return unicode.is_digit(rune(str[0])) ? u16(strconv.atoi(str)) : Id(str)
}

parse_circuit :: proc(input: string) -> Circuit {
        circuit := make(Circuit)

        for line in strings.split(input, "\n") {
                if len(line) < 1 do continue
                parse_line(&circuit, line)
        }

        return circuit
}

parse_line :: proc(circuit: ^Circuit, line: string) {
        toks := strings.split(line, " ")

        // Tokens are in one of the following formats:
        //      - Signal to wire (e.g. 123 -> x)        $NUM -> $WIRE
        //      - Gated signal (e.g. x AND y -> d)      {$WIRE/$NUM} $GATE $WIRE -> $WIRE
        //      - Shifted signal (e.g. x LSHIFT 2 -> f) $WIRE $SHIFT $NUM -> $WIRE
        //      - Negated signal (e.g. NOT x -> h)      NOT $WIRE -> $WIRE
        //      - Aliased signal (e.g. x -> a)          $WIRE -> $WIRE
        if toks[0] == "NOT" {
                // negated signal
                circuit[toks[3]] = Wire { 
                        id = toks[3],
                        sig = Gate { 
                                type = .NOT,
                                inputs = { toks[1], NC },
                                output = toks[3],
                                ctx = 0,
                        },
                }
        } else if toks[1] == "->" {
                // signal to wire
                circuit[toks[2]] = Wire {
                        id = toks[2],
                        sig = to_input(toks[0]),
                }
        } else if unicode.is_digit(rune(toks[2][0])) {
                // Shifted signal
                circuit[toks[4]] = Wire {
                        id = toks[4],
                        sig = Gate {
                                type = get_gate_type(toks[1]),
                                inputs = { toks[0], NC },
                                output = toks[4],
                                ctx = strconv.atoi(toks[2]),
                        },
                }
        } else {
                // Gated signal
                id := toks[4]
                circuit[id] = Wire {
                        id = id,
                        sig = Gate {
                                type = get_gate_type(toks[1]),
                                inputs = { 
                                        to_input(toks[0]), 
                                        toks[2],
                                },
                                output = id,
                                ctx = 0,
                        },
                }
        }
}

sim_wire :: proc(circuit: ^Circuit, wire_id: Id) -> u16 {
        wire := circuit[wire_id]
        switch sig in wire.sig {
        case u16:
                return sig
        case Gate:
                fmt.println("Simming gated", wire_id)
                wire.sig = sim_gate(circuit, sig)
                circuit[wire_id] = wire
                return wire.sig.(u16)
        case Input:
                fmt.println("Simming input", wire_id)
                switch input in sig {
                case Id:
                        wire.sig = sim_wire(circuit, input)
                        circuit[wire_id] = wire
                        return wire.sig.(u16)
                case u16:
                        return input
                }
        }

        panic("Error occurred while simulating wire")
}

sim_input :: proc(circuit: ^Circuit, inp: Input) -> u16 {
        switch input in inp {
        case Id:
                return sim_wire(circuit, input)
        case u16:
                return input
        }

        panic("Error occurred while simulating input")
}

sim_gate :: proc(circuit: ^Circuit, gate: Gate) -> u16 {
        switch gate.type {
        case .NOT:
                return max(u16) - sim_input(circuit, gate.inputs[0])
        case .AND:
                return sim_input(circuit, gate.inputs[0]) & sim_input(circuit, gate.inputs[1])
        case .OR:
                return sim_input(circuit, gate.inputs[0]) | sim_input(circuit, gate.inputs[1])
        case .LSHIFT:
                return sim_input(circuit, gate.inputs[0]) << u16(gate.ctx)
        case .RSHIFT:
                return sim_input(circuit, gate.inputs[0]) >> u16(gate.ctx)
        }

        panic("Error occurred while simulating gate")
}


@test
examples :: proc(t: ^testing.T) {
        ex1 := 
`
123 -> x
456 -> y
x AND y -> d
x OR y -> e
x LSHIFT 2 -> f
y RSHIFT 2 -> g
NOT x -> h
NOT y -> i
`
        circuit := parse_circuit(ex1)
        testing.expect(t, sim_wire(&circuit, "d") == 72)
        testing.expect(t, sim_wire(&circuit, "e") == 507)
        testing.expect(t, sim_wire(&circuit, "f") == 492)
        testing.expect(t, sim_wire(&circuit, "g") == 114)
        testing.expect(t, sim_wire(&circuit, "h") == 65412)
        testing.expect(t, sim_wire(&circuit, "i") == 65079)
        testing.expect(t, sim_wire(&circuit, "x") == 123)
        testing.expect(t, sim_wire(&circuit, "y") == 456)
}
