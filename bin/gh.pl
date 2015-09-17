#!/usr/bin/env perl
## ---------------------------------------------------------------------
## Get a link to audio file from Galbadia Hotel page.
##
## Galbadia Hotel <http://gh.ffshrine.org/> obfuscates its HTML
## sources for some reason, and mouse-clicking is not fun.
##
## Example:
##   $ wget "$(wget -qO - http://gh.ffshrine.org/song/3574/9 | gh.pl)"
##
## Homepage: http://http://github.com/vvv/dotfiles/blob/master/bin/
##
## See also:
##   gh  -  download the whole album from Galbadia Hotel
## ---------------------------------------------------------------------
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
        # by default ^ and $ will not match at \n inside string, but only at the
        # beginning and ending of a multi-line string, if more "grep" alike behavior
        # is expected, one might use 'm' modifier for match operator: '//m'
        unless $ln =~ /^var\ addr\ =\ (?:
                                       "(.+)";         # 0. ready to use `addr'
                                       |new\ Array\(\) # 1. to be gathered
                                      )$/x;
    return $1 // '';
}

unless ($addr = getAddr(shift @sloc)) {
    $addr .= $1 while (shift(@sloc) =~ /^addr\[\d+\] = "(.*)";$/);
}

foreach (@sloc) {
    push @decodings, [$1,$2]
        if  m|^.+Address\.replace\(/(.+?)/g?, "(.+?)"\);$|;
}

$addr =~ s/$_->[0]/$_->[1]/g foreach (@decodings);
print "$addr\n";
