#!/usr/bin/env python3

import sys
import time
import argparse

parser = argparse.ArgumentParser()
parser.add_argument("file", help = "Path to a brainfuck script/program.")

parser.add_argument("-s", "--sleep", default = 0.03, type = float,
	help = "Sleep (in seconds) between each loop iteration.")
parser.add_argument("-q", "--quiet", action = "store_true",
	help = "Don't prettify output (or sleep).")
parser.add_argument("--hide-cells", action = "store_true",
	help = "Hide cells from output. Necessary if a program uses a lot of cells.")

args = parser.parse_args()

out_str  = ""            	# output string
ins_tape = ""            	# instructions tape
lop_tape = []            	# loop indexes tape
dat_tape = bytearray([0])	# data tape
dat_indx = 0             	# data index (will be used as pointer)
ins_indx = 0             	# instruction index

filename = args.file
p_sleep  = args.sleep
o_quiet  = args.quiet
o_cells  = args.hide_cells

# increment/decrement current cell value
def inc_cell(_):

	try:
		dat_tape[dat_indx] += 1
	except ValueError:
		dat_tape[dat_indx] = 0

	return _ + 1

def dec_cell(_):

	try:
		dat_tape[dat_indx] -= 1
	except ValueError:
		dat_tape[dat_indx] = 255

	return _ + 1

# increment/decrement cell pointer
def inc_pntr(_):

	global dat_indx

	dat_indx += 1

	while len(dat_tape) <= dat_indx:
		dat_tape.append(0)

	return _ + 1

def dec_pntr(_):

	global dat_indx

	if dat_indx != 0:
		dat_indx -= 1
	else:
		dat_tape.insert(0, 0)

	return _ + 1

# start/end loop
def start_loop(indx):

	if dat_tape[dat_indx] != 0:
		lop_tape.append(indx + 1)

		return indx + 1
	else:
		indx = close_loop(indx)

		return indx

def close_loop(indx):

	while ins_tape[indx] != ']':
		indx += 1

		if ins_tape[indx] == '[':
			indx = close_loop(indx)

	return indx + 1

def end_loop(indx):

	try:
		if dat_tape[dat_indx] != 0:
			return lop_tape[-1]
		else:
			lop_tape.remove(lop_tape[-1])

			return indx + 1
	except IndexError:
		return indx + 1 # ignore unmatched closing loop

# character io
def read_char(_):

	dat_tape[dat_indx] = ord(sys.stdin.read(1)[0])

	return _ + 1

def print_char(_):

	global out_str

	if o_quiet:
		print(chr(dat_tape[dat_indx]), end="")
	else:
		out_str += chr(dat_tape[dat_indx])

	return _ + 1

# function dictionary with all of brainfuck's symbols
fun_dict = {
	'+': inc_cell,
	'-': dec_cell,
	'>': inc_pntr,
	'<': dec_pntr,
	'[': start_loop,
	']': end_loop,
	',': read_char,
	'.': print_char
}

# getting characters from a string
def ins_prep(indx, offset):

	prep_str = ins_tape[indx - offset:indx]
	ch_offset = offset

	while not prep_str:
		ch_offset -= 1
		prep_str = ins_tape[indx - ch_offset:indx]

	while len(prep_str) != offset:
		prep_str = " " + prep_str

	return prep_str

# read the first argument, try to open it as a file
with open(filename, "r") as inf:
	ins_tape = ''.join([c for c in inf.read() if c in fun_dict.keys()])

# let's go through the instructions, now
while ins_indx < len(ins_tape):

	op_c = ins_tape[ins_indx]
	old_indx = ins_indx

	# using our function dict
	ins_indx = fun_dict[op_c](ins_indx)

	if o_quiet:
		continue

	print("\033[1;1H", end="")

	print(" Program:", ins_prep(ins_indx, 35), "\033[31m" +
		(ins_tape[ins_indx] if ins_indx < len(ins_tape) else "HALT") + "\033[0m",
		ins_tape[ins_indx+1:ins_indx+36].ljust(32))

	if not o_cells:
		print("   Cells:", ' '.join(["{:3d}".format(_) for _ in dat_tape]))
		print(" Pointer:", "    " * dat_indx, " ^", " " * 4)

	print("  Output:", out_str.replace("\n", "\n        : "))

	# rewind/forward the tape if we're caught by a loop
	if abs(ins_indx - old_indx) != 1 and ins_indx < len(ins_tape):

		while old_indx != ins_indx:

			old_indx += 1 if ins_indx > old_indx else -1

			print("\033[1;1H", end="")

			print(" Program:", ins_prep(old_indx, 35), "\033[34m" +
				ins_tape[old_indx] + "\033[0m",
				ins_tape[old_indx+1:old_indx+36].ljust(35))

			print("\033[%d;1H" % (4 + len([_ for _ in out_str if _ == '\n'])), end="")

			time.sleep(p_sleep)
	else:
		time.sleep(p_sleep)
