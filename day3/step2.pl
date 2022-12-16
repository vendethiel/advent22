use 5.30.3;
use feature qw(signatures);
no warnings qw(experimental::signatures);
use strict;
use warnings;

my $total;
my %weight;
my $i;
for ('a'..'z', 'A'..'Z') {
  $weight{$_} = ++$i;
}

my @lines = <<>>;
while (@lines) {
  my $fst = pop @lines;
  chomp($fst);
  last if "" eq $fst;
  my $snd = pop @lines;
  chomp($snd);
  my $trd = pop @lines;
  chomp($trd);

  my %has;
  $has{$_} = 1 for split '', $fst;
  # Need an if here, looks ugly :(
  for (split '', $snd) {
    $has{$_} = 2 if ($has{$_} // 0) == 1;
  }
  for (split '', $trd) {
    $has{$_} = 3 if ($has{$_} // 0) == 2;
  }

  foreach (keys %has) {
    if ($has{$_} == 3) {
      $total += $weight{$_} 
    }
  }
}
say $total;
