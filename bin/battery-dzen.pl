#!/usr/bin/perl

use strict;
use warnings;

sub openR ($) { ### open file for reading
    my ($name) = @_;
    open my $f, '<', $name or die "Can't read file: $!";
    return $f;
}

sub line2num ($) { ### read line, get a number from it
    my ($f) = @_;
    $_ = <$f>;
    return (/\d+/ and $& or '');
}

sub battery_params() { ### read battery parameters
    my $f = openR '/proc/acpi/battery/BAT1/state';
    scalar <$f>; scalar <$f>; # skip 2 lines

    $_ = <$f>;
    my $ch_state = (/\S+$/ and $& or ''); # `charging state'
    exit 0 if $ch_state eq 'charged';

    my $rate = line2num $f;    # `present rate'
    my $rem_cap = line2num $f; # `remaining capacity'
    close $f;

    $f = openR '/proc/acpi/battery/BAT1/info';
    scalar <$f>;
    my $des_cap = line2num $f; # `design capacity'
    close $f;

    return ($ch_state, $rate, $rem_cap, $des_cap);
}

my ($ch_state, $rate, $rem_cap, $des_cap) = &battery_params;

my $report = '?';
if ($des_cap && $rem_cap) { $report = int(100 * $rem_cap / $des_cap); }
$report .= '% (';

unless ($rate) {
    $report .= '???)';
    print "|| $ch_state $report \n";
    exit 0;
}

my ($minutes, $color) = (0.0, '');

if ($ch_state eq 'charging') {
    $minutes = 60 * ($des_cap - $rem_cap) / $rate;
    $ch_state = '+';
} else { # discharging
    $minutes = 60 * $rem_cap / $rate;
    $report .= '~';
    if ($minutes < 20) { $color = 'red'; }
    elsif ($minutes < 40) { $color = 'green'; }
}

if ($minutes < 60) {
    $report .= int($minutes) . "')";
} else {
    $report .= sprintf '%d:%02d)', (int($minutes / 60), int($minutes % 60));
}

if ($color) { $report = "^fg($color)" . $report . "^fg()"; }

print "|| $ch_state $report \n";
