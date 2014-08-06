#!/usr/bin/awk -f

function flush() {
    if (n > 0) {
	if (n > 1)
	    printf("<%u times> ", n);
	print mem
	mem = ""
	n = 0
    }
}

BEGIN { mem = n = 0 }

$0 == mem { ++n; next }

{
    flush()
    mem = $0
    n = 1
}

END { flush() }
