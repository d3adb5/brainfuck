#include <stdio.h>
#include <stdlib.h>

#include "bfops.h"

FILE *bffp;
int looplock;

static void
brainfuck(int op)
{
	if (looplock) {
		if      (op == '[') ++looplock;
		else if (op == ']') --looplock;

		return;
	}

	switch (op) {
		case '+': bfops_inc(); break;
		case '-': bfops_dec(); break;
		case '>': bfops_next(); break;
		case '<': bfops_prev(); break;
		case '[': bfops_startloop(); break;
		case ']': bfops_closeloop(); break;
		case '.': bfops_print(); break;
		case ',': bfops_read(); break;
	}
}

int
main(int argc, char **argv)
{
	int op;

	if (argc < 2) {
		puts("You need to provide a filename.");
		exit(1);
	}

	if ((bffp = fopen(argv[1], "r")) == NULL) {
		perror("Failed to open file");
		exit(1);
	}

	bfops_init();

	while ((op = fgetc(bffp)) != EOF)
		brainfuck(op);

	return 0;
}
