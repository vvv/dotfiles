#!/usr/bin/env perl

use strict;
use warnings;
use List::Util qw(shuffle);

my $auto_p = 0;
if (defined $ARGV[0]) {
    die "Usage: $0 [--auto]\n" unless
	$#ARGV == 0 and ($ARGV[0] eq '-a' or $ARGV[0] eq '--auto');
    $auto_p = 1;
}

my $value = '';
my $mnemonics = '';

open(my $words, '<', '/usr/share/dict/words') or die;
foreach (shuffle <$words>) {
    next unless $_ =~ /^[a-foils]+$/i;
    chomp;
    tr/A-Z/a-z/;  # convert to lower case

    unless ($auto_p) {
	$mnemonics = $_;
	tr/oils/0115/;
	print length($_) . " $mnemonics $_\n";
	next;
    }

    $value .= $_;
    $mnemonics .= " $_";
    if (length $value >= 16) {
	$value =~ tr/oils/0115/;
	print substr($value, 0, 16)
	    . substr($mnemonics, 0, length($mnemonics) + 16 - length($value))
	    . "\n";
	$value = $mnemonics = '';
    }
}
close $words;
