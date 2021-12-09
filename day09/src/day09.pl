#!/usr/bin/perl
use warnings;
use strict;

open(FH, '<', 'resources/demo.txt') or die $!;

while (<FH>) {
  print $_;
}

close(FH);
