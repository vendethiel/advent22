use 5.30.3;
use feature qw(signatures);
no warnings qw(experimental::signatures);

sub cut_in_half($string) {
  my $len = length($string) / 2;
  (substr($string, 0, $len), substr($string, $len))
}

my $total;
my %weight;
my $i;
for ('a'..'z', 'A'..'Z') {
  $weight{$_} = ++$i;
}
for (<<>>) {
  chomp;
  my ($lfg, $rgt) = cut_in_half($_);

  my %has;
  # However many times it appears in $lft, mark it once
  # (2 times in lft doesn't count as being in both parts).
  $has{$_} = 1 for split '', $lfg;
  # Multiply by two, so if it wasn't in $lft it won't be increased.
  $has{$_} *= 2 for split '', $rgt;

  foreach (keys %has) {
    if ($has{$_} >= 2) {
      $total += $weight{$_} 
    }
  }
}
say $total;
