#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "bfops.h"

static char *celllist;
static char *currentcell;
static int  cellcount;

static long *seekstack;
static int  seekcount;
static int  seektop;

void
bfops_init(void)
{
	/* TODO: alloc error checking */
	celllist = calloc(10, 1);
	currentcell = celllist;
	cellcount = 10;

	seekstack = malloc(5);
	seekcount = 5;
	seektop = 0;
}

void
bfops_inc(void)
{
	++(*currentcell);
}

void
bfops_dec(void)
{
	--(*currentcell);
}

void
bfops_next(void)
{
	if (++currentcell - celllist >= cellcount) {
		/* TODO: alloc error checking */
		celllist = realloc(celllist, cellcount + 5);
		currentcell = celllist + cellcount;
		cellcount += 5;

		memset(currentcell, 0, 5);
	}
}

void
bfops_prev(void)
{
	if (--currentcell < celllist) {
		fprintf(stderr, "Attempt to go to cell -1.\n");
		exit(1);
	}
}

void
bfops_print(void)
{
	putchar(*currentcell);
}

void
bfops_read(void)
{
	*currentcell = getchar();
}

void
bfops_startloop(void)
{
	if (*currentcell == 0) {
		looplock = 1;
		return;
	}

	seekstack[seektop++] = ftell(bffp);

	if (seektop >= seekcount) {
		/* TODO: alloc error check */
		seekstack = realloc(seekstack, seekcount + 5);
		seekcount += 5;
	}
}

void
bfops_closeloop(void)
{
	if (*currentcell == 0) {
		--seektop;
		return;
	}

	fseek(bffp, seekstack[seektop - 1], SEEK_SET);
}
