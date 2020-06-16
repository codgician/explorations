# Makefile for naive-chatroom


CC = gcc
CFLAGS = -g -Wall
LDFLAGS = -lpthread

all: server client

csapp.o: csapp.c csapp.h
	$(CC) $(CFLAGS) -c csapp.c

server.o: server.c csapp.h
	$(CC) $(CFLAGS) -c server.c

client.o: client.c csapp.h
	$(CC) $(CFLAGS) -c client.c

server: server.o csapp.o
	$(CC) $(CFLAGS) server.o csapp.o -o server $(LDFLAGS)

client: client.o csapp.o
	$(CC) $(CFLAGS) client.o csapp.o -o client $(LDFLAGS)

clean:
	rm -f *~ *.o server client *.tar *.zip *.gzip *.bzip *.gz

