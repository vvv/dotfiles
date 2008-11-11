#!/usr/bin/awk -f

{
    printf("----- %d -----\n", NR);
    for (i=1; i <= NF; i++)
	printf("%d  %s\n", i, $i);
}
