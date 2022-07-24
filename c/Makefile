SRC = bfc.c bfops.c
OBJ = $(subst .c,.o,$(SRC))

.PHONY = all clean

%.o: %.c
	$(CC) $(CFLAGS) -c $^ -o $@

bf: $(OBJ)
	$(CC) $(CFLAGS) $^ -o $@

all: bf

clean:
	$(RM) *.o bf
