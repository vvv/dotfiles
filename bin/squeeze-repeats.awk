#!/usr/bin/awk -f

$0 == mem {
    ++ndup;
    next;
}

{
    if (ndup > 1)
	print "last message repeated", ndup+1, "times";
    else if (ndup == 1)
	print mem;

    print;

    mem = $0;
    ndup = 0;
}
