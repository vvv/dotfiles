#if 0
set -e
gcc -Wall -W -Werror -o ${0%.c} $0 $*
exec ${0%.c} "$@"
#else

#include <stdio.h>

int main(void)
{
	printf("Hello, world!\n");
	return 0;
}

#endif
