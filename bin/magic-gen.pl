#!/usr/bin/env perl

use strict;
use warnings;
use List::Util qw(shuffle);

my $value = '';
my $mnemonics = '';

open(my $words, '<', '/usr/share/dict/words') or die;
foreach (shuffle <$words>) {
    next unless /^[a-foilsz]+$/i;

    chomp;
    tr/A-Z/a-z/;  # convert to lower case

    $mnemonics = $_;
    tr/oilsz/0115z/;
    print length . " $mnemonics $_\n";
}
close $words;
