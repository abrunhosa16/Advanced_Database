CC = gcc
CFLAGS = -Wall -I/usr/include/postgresql
LDFLAGS = -lpq
SRC = main.c db_utils.c database/tetrominoes.c database/puzzles.c database/solutions.c
OBJ = $(SRC:.c=.o)
TARGET = main

all: $(TARGET)

$(TARGET): $(OBJ)
	$(CC) -o $@ $^ $(LDFLAGS)

%.o: %.c
	$(CC) $(CFLAGS) -c $< -o $@

clean:
	rm -f $(OBJ) $(TARGET)
