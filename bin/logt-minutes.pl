#!/usr/bin/perl

use strict;
use warnings;

use Time::Local;

my %logt = ();
my $stopped = 0;
my $prev_time = 0;

while (<>) {
    die "Line $.: Invalid input\n" unless
	#           1       2      3      4      5      6       7
	(/^(?:\w+ )?(\d{4})-(\d\d)-(\d\d) (\d\d):(\d\d):(\d\d)> (\S+)(?:\s+--.*)?$/);

    my $t = timelocal($6, $5, $4, $3, $2, $1);
    die "Line $.: Timestamp is out of order\n"
	if $prev_time != 0 && $t >= $prev_time;
    die "Impossible happened" if $t == 0;

    if ($7 eq '.') {
	die "Line $.: Two stops in row\n" if $stopped;
	$stopped = 1;
    } else {
	die "Line $.: \"Stop\" (.) expected\n" if $prev_time == 0;

	$logt{$7} += $prev_time - $t;
	$stopped = 0;
    }

    $prev_time = $t;
}

die "LogT must not end with \"stop\" (.)\n" if ($stopped);

sub show_time($$) {
    my $s = shift;
    print "$s\t";
    $s = shift;

    my $h = int($s / 3600);
    $s -= $h * 3600;

    my $m = int($s / 60);
    $s -= $m * 60;

    printf "%s\t%02u:%02u:%02u\n", ($h * 60 + $m, $h, $m, $s);
}

my $total = 0;

foreach (keys %logt) {
    show_time($_, $logt{$_});
    $total += $logt{$_};
}

show_time('--TOTAL', $total);
