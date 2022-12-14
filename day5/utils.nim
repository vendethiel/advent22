from std/sequtils import toSeq, mapIt, map, foldl
from sugar import collect
from itertools import takeWhile, dropWhile

# Split at + drop the match
proc splitAtDrop*[T](xs: seq[T], p: proc(a: T): bool): (seq[T], seq[T]) =
  let
    taken = toSeq(takeWhile(xs, p))
    start = len(taken) + 1
    rest = (if len(xs) <= start: @[] else: xs[start .. ^1])
  (taken, rest)

proc transpose*[T](xs: seq[seq[T]]): seq[seq[T]] =
  let longest = xs.mapIt(len(it)).foldl(max(a, b), 0)
  return collect:
    for i in 0 ..< longest:
      collect:
        for j in 0 ..< len(xs):
          if i < len(xs[j]):
            xs[j][i]

when isMainModule:
  let
    data = @["a", "b", "", "c", "d"]
    (xs, ys) = splitAtDrop(data, proc (x: string): bool = x != "")
  assert xs == @["a", "b"]
  assert ys == @["c", "d"]

  assert transpose[string](@[]) == @[]
  assert transpose(@[@[1, 2], @[3, 4]]) == @[@[1, 3], @[2, 4]]
  assert transpose(@[@[1, 2, 5], @[3, 4]]) == @[@[1, 3], @[2, 4], @[5]]
