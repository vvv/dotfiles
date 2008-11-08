#!/usr/bin/env perl

use strict;
use warnings;

open my $state, '<', '/proc/acpi/battery/BAT1/state'
    or die "Can't read file: $!";

scalar <$state>; scalar <$state>;

$_ = <$state>; /\S+$/; my $charging_state = $&;
exit 0 if $charging_state eq 'charged';

$_ = <$state>; s/\D+//g; my $present_rate = $_; # XXX function
$_ = <$state>; s/\D+//g; my $remaining_capacity = $_;
close $state;

print "|| $charging_state: ";

if ($charging_state eq 'discharging') {
    my $ml = $remaining_capacity * 60 / $present_rate; # minutes left
    if ($ml < 20) { print '^fg(red)' . int($ml) . '^fg()'; }
    elsif ($ml < 40) { print '^fg(green)' . int($ml) . '^fg()'; }
    elsif ($ml < 60) { print int($ml); }
    else { printf "%d:%02d", (int($ml / 60), int($ml % 60)); }
}

if ($charging_state eq 'charging') {
    open my $info, '<', '/proc/acpi/battery/BAT1/info'
	or die "Can't read file: $!";
    scalar <$info>;
    $_ = <$info>; s/\D+//g; my $design_capacity = $_;
    close $info;

    print int(100 * $remaining_capacity / $design_capacity) . '%';
}

print " \n";
