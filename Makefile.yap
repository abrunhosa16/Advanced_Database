CC = gcc
CFLAGS = -fPIC -Wall -I../yap-6.3/include -I../yap-6.3/H -I/usr/include/postgresql
LDFLAGS = -shared -lpq -L../yap-6.3 -lYap

TARGET = yap2pgsql.so
SOURCES = yap2pgsql.c

all: $(TARGET)

$(TARGET): $(SOURCES)
	$(CC) $(CFLAGS) -o $@ $^ $(LDFLAGS)

clean:
	rm -f $(TARGET) 