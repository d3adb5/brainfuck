#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

/*
 * This program will generate random sequences of Brainfuck ops/symbols without
 * caring for whether it outputs infinite loops, unmatching braces, or anything
 * that'd cause trouble for a Brainfuck interpreter or render the program
 * "invalid".
 *
 * This is just for fun, please don't be mean.
 *
 * It's quite boring to see a bunch of short AND infinite loops go through the
 * interpreter, so a friend gave me an idea: to set a minimum of operations
 * inside the loops. That's what the variable `int cnt;` is for.
 *
 * I made it start at 0 because I feared random values could interfere with the
 * first few characters, but that might not be the case and it's a useless
 * assignment. Meh.
 *
 * I then added `int lcnt;` so that every loop would be closed.
 */

int
main(void)
{
	const char *c = "+-,.[]<>";
	int length, cnt = 0, lcnt = 0;
	char op;

	srand(time(NULL));

	length = 100 + (rand() % 200);

	for (; length; --length)
	{
		op = c[rand() % strlen(c)];

		while (op == ']' && cnt - length < 8)
			op = c[rand() % strlen(c)];

		if (op == '[')
		{
			cnt = length;
			++lcnt;
		}

		putchar(op);
	}

	for (; lcnt; --lcnt)
		putchat(']');

	return EXIT_SUCCESS;
}
