#!/usr/bin/env perl

use strict;
use warnings;

my $la = `uptime`;
$la =~ s/^.*e: (([0-9.]+).*)$/$1/;
chomp $la;

if    ($2 < 1.5) { print           "$la\n"; }      # normal
elsif ($2 < 4.0) { print "^fg(green)$la^fg()\n"; } # important
else             { print   "^fg(red)$la^fg()\n"; } # critical
