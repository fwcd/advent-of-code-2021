#!/usr/bin/perl
use warnings;
use strict;
use List::Util qw(sum min);

open(FH, '<', 'resources/input.txt') or die $!;

my $large = 10000;
my @lava_map = ();

while (<FH>) {
  my @line = split /\s*/, $_;
  push @lava_map, \@line;
}

my $width = scalar @{ $lava_map[0] };
my $height = @lava_map;

sub neighbors {
  my $y = $_[0];
  my $x = $_[1];
  map {
    my $dy = $_;
    map {
      my $dx = $_;
      my $nx = $x + $dx;
      my $ny = $y + $dy;
      if ((($dx == 0) ^ ($dy == 0)) && $nx >= 0 && $nx < $width && $ny >= 0 && $ny < $height) {
        $lava_map[$ny][$nx];
      } else {
        @{ [] };
      }
    } (-1..1);
  } (-1..1);
}

my @valleys = map {
  my $y = $_;
  map {
    my $x = $_;
    my @ns = neighbors $y, $x;
    my $value = $lava_map[$y][$x];

    if ($value < min @ns) {
      \@{ [$y, $x] };
    } else {
      @{ [] };
    }
  } (0..($width - 1));
} (0..($height - 1));

my $part1 = sum map { $lava_map[@$_[0]][@$_[1]] + 1; } @valleys;
print "Part 1: $part1\n";

close(FH);
