from system/io import readLines
from std/strutils import join, split, parseInt, replace
from std/sequtils import mapIt, toSeq, map, filterIt, foldl, insert, delete
from itertools import chunked
from utils import splitAtDrop, transpose

type # XXX `distinct`?
  StackItem = char
  Stack = seq[StackItem]
  Instruction = object
    count, frm, to: int

proc parseHeaderLine(line: string): Stack =
  line.toSeq.chunked(4).toSeq.mapIt(it[1])

proc cleanStack(stack: Stack): Stack =
  return stack.filterIt(it != ' ')

proc parseStacks(xs: seq[string]): seq[Stack] =
  return xs.map(parseHeaderLine).transpose.map(cleanStack)

proc parseInstructionLine(line: string): Instruction =
  # Clear the string so we can split on space
  let clean = line.replace("move ", "").replace("from ").replace("to ")
  let parts = clean.split(" ")
  let count = parseInt(parts[0])
  let frm = parseInt(parts[1])
  let to = parseInt(parts[2])
  return Instruction(count: count, frm: frm - 1, to: to - 1)

proc parseInstructions(xs: seq[string]): seq[Instruction] =
  return xs.map(parseInstructionLine)

proc perform(state: seq[Stack], inst: Instruction): seq[Stack] =
  var stacks = state
  let
    rng = 0..<inst.count
    els = stacks[inst.frm][rng]
  stacks[inst.frm].delete(rng)
  stacks[inst.to].insert(els, 0)
  return stacks

proc runInstructions(state: seq[Stack], insts: seq[Instruction]): seq[Stack] =
  return insts.foldl(perform(a, b), state)

proc stackTops(stacks: seq[Stack]): seq[StackItem] =
  return stacks.mapIt(if len(it) > 0: it[0] else: ' ')

proc formatTops(tops: seq[StackItem]): string =
  return tops.join("")

# Tests
let
  parsedHeaderLine = parseHeaderLine("[C]         [S] [H]                ")
  parsedStacks = parseStacks(@["[F]         [C]", "[B] [W]     [W]"])
  parsedStacks2 = parseStacks(@["[F]            ", "[B] [W]     [W]"])
  testStack = @[@['A', 'B', 'C'], @['1', '2'], @['I'], @[]]
  testTops = stackTops(testStack)
  testInst1 = Instruction(count: 3, frm: 0, to: 1)
  testInst2 = Instruction(count: 5, frm: 1, to: 3)
  performed = runInstructions(testStack, @[testInst1, testInst2])

assert parsedHeaderLine == @['C', ' ', ' ', 'S', 'H', ' ', ' ', ' ', ' ']
assert parsedStacks == @[@['F', 'B'], @['W'], @[], @['C', 'W']]
assert parsedStacks2 == @[@['F', 'B'], @['W'], @[], @['W']]
assert cleanStack(@['F', 'B', ' ']) == @['F', 'B']
assert testTops == @['A', '1', 'I', ' ']
assert formatTops(@['F', 'B']) == "FB"
assert performed == @[@[], @[], @['I'], @['A', 'B', 'C', '1', '2']]

# Main
let
  # TODO read argv[1]
  data = readLines("data.txt", 513) # ???????
  (head, ins) = splitAtDrop(data, proc (x: string): bool = x != "")
  stacks = parseStacks(head)
  insts = parseInstructions(ins)
  endState = runInstructions(stacks, insts)
  tops = stackTops(endState)
  formattedTops = formatTops(tops)
echo("### FORMATTED TOPS")
echo(formattedTops)
