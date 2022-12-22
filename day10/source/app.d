import std.stdio : File, writeln, write;
import std.algorithm : map;
import std.sumtype : SumType, match;
import std.array : split, array;
import std.conv : to;
import std.typecons : Nullable;
import std.range : empty, popFront;

struct Noop {};
struct Addx {
  int count;
};
alias Instruction = SumType!(Noop, Addx);

// How much do you wait before executing an instruction
int instruction_cycles(Instruction instruction) {
  return instruction.match!(
    (Noop _) => 0,
    (Addx addx) => 2
  );
}

string format(Instruction instruction) {
  return instruction.match!(
    (Noop _) => "Noop",
    (Addx addx) => "Addx " ~ addx.count.to!string
  );
}

class State {
  this(Instruction[] instructions) {
    this.instructions = instructions;
    assign();
  }

  bool run() {
    if (done) return false;

    if (--remaining_cycles <= 0) {
      perform();
      if (!assign())
        done = true;
      // fallthrough
    }
    cycle++;
    return true;
  }

  string dump() {
    return "Cycle #" ~ cycle.to!string ~
      ", x: " ~ x.to!string ~
      ", rem: " ~ remaining_cycles.to!string ~
      ", next: " ~ format(next) ~
      ", instrs: " ~ instructions.length.to!string;
  }

  int public_cycle() { return 1 + cycle; }

  int value() { return x; }

private:
  void perform() {
    next.match!(
      (Noop _) => 0,
      (Addx addx) => x += addx.count
    );
  }

  bool assign() {
    if (instructions.empty)
      return false;
    next = instructions[0];
    instructions.popFront();
    remaining_cycles = instruction_cycles(next);
    return true;
  }

  Instruction next;
  Instruction[] instructions;
  int remaining_cycles = 0;
  int x = 1;
  int cycle = 0;
  bool done = false;
};

Instruction parse(char[] linechars)
{
  string line = linechars.idup;
  if (line == "noop")
    return Instruction(Noop());
  immutable parts = line.split(" ");
  if (parts[0] == "addx")
    return Instruction(Addx(parts[1].to!int));
  throw new Exception("Wrong line: " ~ line);
}

void step1(Instruction[] instructions) {
  auto state = new State(instructions);
  int strength = 0;
  do {
    writeln(state.dump());
    int cycle = state.public_cycle();
    if (cycle >= 20 && (cycle - 20) % 40 == 0) {
      auto signal = cycle * state.value();
      strength += signal;
      writeln("Signal #" ~ cycle.to!string ~ ": " ~ signal.to!string ~ ", strength now " ~ strength.to!string);
    }
  } while (state.run());
  writeln("STRENGTH: " ~ strength.to!string);
  writeln(state.dump());
}

void prn(char[] screen, int total, int cut) {
  for (auto i = 0; i < total; ++i) {
    if (i % cut == 0) write('\n');
    write(screen[i]);
  }
  writeln('\n');
}

void step2(Instruction[] instructions) {
  auto state = new State(instructions);
  auto screen = new char[240];
  auto cut = 40;
  screen[] = ' ';
  auto cursor = 0;
  do {
    write(state.dump());
    auto value = state.value();
    write(" | value = " ~ value.to!string ~ ", cursor = " ~ cursor.to!string);
    auto c = cursor % cut;
    auto draw = value - 1 == c || value == c || value + 1 == c ? '#' : '.';
    screen[cursor++ % 240] = draw;
    prn(screen, 240, cut);
  } while (state.run());
}

void main(string[] argv)
{
  auto file = argv[1];
  step1(File(file).byLine().map!parse.array);
  step2(File(file).byLine().map!parse.array);
}
