#!/usr/bin/perl
use warnings;
use strict;
use List::Util qw/sum product min/;

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
        \@{ [$ny, $nx] };
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
    my @ns = map { $lava_map[@$_[0]][@$_[1]] } neighbors $y, $x;
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

my %visited = ();

sub bfs {
  my $count = 0;
  my @queue = (\@_);
  while (@queue) {
    my $ref = shift @queue;
    my $y = @$ref[0];
    my $x = @$ref[1];
    my $p = "($y, $x)";
    if (!defined $visited{$p} && $lava_map[$y][$x] != 9) {
      $visited{$p} = 1;
      $count += 1;
      for (neighbors $y, $x) {
        push @queue, \@$_;
      }
    }
  }
  $count;
}

my @basins = ();

for (@valleys) {
  my $y = @$_[0];
  my $x = @$_[1];
  my $size = bfs $y, $x;
  push @basins, $size;
}

my @sorted_basins = sort { $b <=> $a } @basins;
my $part2 = product @sorted_basins[0..2];
print "Part 2: $part2\n";

close(FH);
