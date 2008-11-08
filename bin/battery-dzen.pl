#!/usr/bin/env perl

use strict;
use warnings;

sub numval ($) {
    my ($f) = @_;
    $_ = <$f>; s/\D+//g; return $_;
}

open my $f, '<', '/proc/acpi/battery/BAT1/state'
    or die "Can't read file: $!";

scalar <$f>; scalar <$f>;

$_ = <$f>; /\S+$/; my $charging_state = $&;
exit 0 if $charging_state eq 'charged';

my $present_rate = numval $f;
my $remaining_capacity = numval $f;
close $f;

print "|| $charging_state: ";

if ($charging_state eq 'discharging') {
    my $ml = $remaining_capacity * 60 / $present_rate; # minutes left
    if ($ml < 20) { print '^fg(red)' . int($ml) . '^fg()'; }
    elsif ($ml < 40) { print '^fg(green)' . int($ml) . '^fg()'; }
    elsif ($ml < 60) { print int($ml); }
    else { printf "%d:%02d", (int($ml / 60), int($ml % 60)); }
}

if ($charging_state eq 'charging') {
    open my $f, '<', '/proc/acpi/battery/BAT1/info'
	or die "Can't read file: $!";
    scalar <$f>;
    my $design_capacity = numval $f;
    close $f;

    print int(100 * $remaining_capacity / $design_capacity) . '%';
}

print " \n";
