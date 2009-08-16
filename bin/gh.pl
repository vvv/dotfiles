#!/usr/bin/env perl

## Getting songs from http://gh.ffshrine.org/soundtracks/3574:
##
## for n in `seq 19`; do \
##   wget -O - http://gh.ffshrine.org/song/3574/$n | gh.pl; \
## done | wget -i -

use strict;
use warnings;
use URI::Escape qw(uri_unescape);

my (@sloc, $addr, @decodings);

while (<>) {
    next unless $_ =~ q/^\s*(?:<script>)?var data = "([^"]+)";.*$/;
    @sloc = split("\n", uri_unescape $1);
    last;
}

sub getAddr($) {
    my ($ln) = @_;
    die "Galbadia strikes back!\n$ln\n"
        unless $ln =~ /^var\ addr\ =\ (?:
                                       "(.+)";         # 0. ready to use `addr'
                                       |new\ Array\(\) # 1. to be gathered
                                      )$/x;
    return (defined $1 ? $1 : "");
}

unless ($addr = getAddr(shift @sloc)) {
    $addr .= $1 while (shift(@sloc) =~ /^addr\[\d+\] = "(.*)";$/);
}

foreach (@sloc) {
    next unless m|^.+Address\.replace\(/(.+?)/g?, "(.+?)"\);$|;
    push @decodings, [$1,$2];
}

$addr =~ s/$_->[0]/$_->[1]/g foreach (@decodings);
print "$addr\n";
