enum Dir <R U L D>;

class Pos {
  has Int $.x; # vertical, left-to-right
  has Int $.y; # horizontal, top-to-bottom

  multi method offset(Pos $other --> Pos) {
    Pos.new(x => $!x + $other.x, y => $!y + $other.y);
  }

  multi method offset(:$x = 0, :$y = 0) {
    Pos.new(x => $!x + $x, y => $!y + $y);
  }

  method inverse(--> Pos) {
    Pos.new(x => -1 * $!x, y => -1 * $!y);
  }

  method WHICH() {
    "$!x;$!y"
  }
  method Str() { self.WHICH() }

  method clone() {
    Pos.new(:$!x, :$!y);
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
  has Pos @.tail;
  has Int $.width;
  has Int $.height;
  has Set $!tail-positions;

  submethod TWEAK() {
    @!tail = $!head.clone xx 9
  }

  method move(Dir $dir) {
    my $pos-offset = pos-offset($dir);
    $!head.=offset($pos-offset);
    update-tail($!head, @!tail[0], $pos-offset);
    update-tail(@!tail[0], @!tail[1], $pos-offset);
    update-tail(@!tail[1], @!tail[2], $pos-offset);
    update-tail(@!tail[2], @!tail[3], $pos-offset);
    update-tail(@!tail[3], @!tail[4], $pos-offset);
    update-tail(@!tail[4], @!tail[5], $pos-offset);
    update-tail(@!tail[5], @!tail[6], $pos-offset);
    update-tail(@!tail[6], @!tail[7], $pos-offset);
    update-tail(@!tail[7], @!tail[8], $pos-offset);
    $!tail-positions (|)= @!tail[*-1];
  }

  method count-visited-positions(--> Int) {
    +$!tail-positions
  }

  sub update-tail($head, $tail is rw, $pos-offset) {
    unless tail-within-range(:$head, :$tail) {
      for Dir::.values -> $dir {
        my $offset = pos-offset($dir);
        if $head eqv $tail.offset($offset).offset($offset) {
          $tail.=offset($offset);
          return;
        }
      }
      # Move diagonally
      my $snap-x = $head.x > $tail.x ?? 1 !! -1;
      my $snap-y = $head.y > $tail.y ?? 1 !! -1;
      $tail.=offset(Pos.new(x => $snap-x, y => $snap-y));
    }
  }

  sub tail-within-range(Pos :$head!, Pos :$tail! --> Bool) {
    for -1..1 X -1..1 -> ($x, $y) {
      return True if $tail.offset(Pos.new(:$x, :$y)) eqv $head;
    }
    return False
  }

  method draw(--> Str) {
    for 0..^$!height -> $y {
      for 0..^$!width -> $x {
        my $cur = Pos.new(:$x, :$y);
        if $!head eqv $cur {
          print "H";
        } else {
          with @!tail.first($cur, :k) -> $i {
            print $i + 1; # index starts at 0
          } elsif $!tail-positions{$cur}:exists {
            print "#";
          } else {
            print ".";
          }
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

my $width = 26;
my $height = 21;
my Board $board.=new(
  head => Pos.new(x => 11, y => 15),
  :$width,
  :$height
);
my Move @move = parse(@*ARGS[0]);
process($board, @move);
say $board.count-visited-positions;
