enum Dir <R U L D>;

class Pos {
  has Int $.x; # vertical, left-to-right
  has Int $.y; # horizontal, top-to-bottom

  method offset(Pos $other --> Pos) {
    Pos.new(x => $!x + $other.x, y => $!y + $other.y);
  }

  method inverse(--> Pos) {
    Pos.new(x => -1 * $!x, y => -1 * $!y);
  }

  method WHICH() {
    "$!x;$!y"
  }
}

multi sub pos-offset(R) { Pos.new(x => 1, y => 0) }
multi sub pos-offset(L) { Pos.new(x => -1, y => 0) }
multi sub pos-offset(U) { Pos.new(x => 0, y => -1) }
multi sub pos-offset(D) { Pos.new(x => 0, y => 1) }

class Move {
  has Dir $.dir;
  has Int $.count;
}

class Board {
  has Pos $.head;
  has Pos $.tail;
  has Int $.width;
  has Int $.height;
  has Set $!tail-positions;

  method move(Dir $dir) {
    my $pos-offset = pos-offset($dir);
    $!head.=offset($pos-offset);
    unless self!tail-within-range() {
      # If we're going straight in some direction...
      if $!head eqv $!tail.offset($pos-offset).offset($pos-offset) {
        $!tail.=offset($pos-offset);
      } else {
        say "ruh roh!";
        # Snap the tail back
        $!tail = $!head.offset($pos-offset.inverse);
      }
    }

    $!tail-positions (|)= $!tail;
  }

  method count-visited-positions(--> Int) {
    +$!tail-positions
  }

  method !tail-within-range(--> Bool) {
    for -1..1 X -1..1 -> ($x, $y) {
      return True if $!tail.offset(Pos.new(:$x, :$y)) eqv $!head;
    }
    return False
  }

  method draw(--> Str) {
    for 0..^$!height -> $y {
      for 0..^$!width -> $x {
        my $cur = Pos.new(:$x, :$y);
        my $is-head = $!head eqv $cur;
        my $is-tail = $!tail eqv $cur;
        print do given ($is-head * 2 + $is-tail) {
          when 3 { "_" }
          when 2 { "H" }
          when 1 { "T" }
          when 0 { $!tail-positions{$cur}:exists ?? "#" !! "." }
        }
      }
      say "";
    }
  }
}

sub process(Board $board, Move @move) {
  for @move -> $move {
    say "# $move.dir() $move.count()";
    for 1..$move.count {
      say "Step $_ of $move.count()";
      $board.move($move.dir);
      say $board.draw;
    }
  }
}

sub parse(Str $file where *.IO.e --> Array[Move]) {
  my @lines = lines slurp $file;
  Array[Move].new(@lines.map({
    my ($dir, $count) = .split(" ");
    Move.new(dir => Dir::{$dir}, count => +$count);
  }));
}

my $width = 6;
my $height = 5;
my Board $board.=new(
  head => Pos.new(x => 0, y => $height - 1),
  tail => Pos.new(x => 0, y => $height - 1),
  :$width,
  :$height
);
my Move @move = parse(@*ARGS[0]);
process($board, @move);
say $board.count-visited-positions;
