#!/usr/bin/perl

use strict;
use warnings;

use Time::Local;

my %logt = ();
my $stopped = 0;
my $prev_time = 0;
my $accum_time = 0;

while (<>) {
    $_ =~ s/--.*$//;  # strip of comments
    $_ =~ s/^\s+//;   # strip of leading whitespace
    $_ =~ s/\s+$//;   # strip of trailing whitespace
    next unless ($_); # skip empty lines

    die "Line $.: Invalid input\n" unless
	#           1       2      3      4      5      6       7     8
	(/^(?:\w+ )?(\d{4})-(\d\d)-(\d\d) (\d\d):(\d\d):(\d\d)> ([io])( .*)?$/);

    my $t = timelocal($6, $5, $4, $3, int($2) - 1, $1);
    die "Line $.: Timestamp is out of order\n"
	if $prev_time != 0 && $t >= $prev_time;
    die "Impossible happened" if $t == 0;

    if ($7 eq 'o') {
	die "Line $.: Two stops in row\n" if $stopped;
	$stopped = 1;
    } else {
	die "Line $.: \"Stop\" (o) expected\n" if $prev_time == 0;
	$stopped = 0;

	if ($8) {
	    my $task = $8;
	    $task =~ s/^\s*(.+)$/$1/; # strip of leading whitespace
	    $logt{$task} += $accum_time + $prev_time - $t;
	    $accum_time = 0;
	} else {
	    $accum_time += $prev_time - $t;
	}
    }

    $prev_time = $t;
}

die "LogT must not end with \"stop\" (o)\n" if ($stopped);
die "Task name is missing at the end of LogT\n" if ($accum_time != 0);

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
