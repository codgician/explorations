# Makefile for naive-chatroom-udp


CC = gcc
CFLAGS = -g -Wall
LDFLAGS = -lpthread

all: server client

csapp.o: csapp.c csapp.h
	$(CC) $(CFLAGS) -c csapp.c

csappudp.o: csappudp.c csappudp.h
	$(CC) $(CFLAGS) -c csappudp.c

sbuf.o: sbuf.c sbuf.h
	$(CC) $(CFLAGS) -c sbuf.c

server.o: server.c csapp.h
	$(CC) $(CFLAGS) -c server.c

client.o: client.c csapp.h
	$(CC) $(CFLAGS) -c client.c

server: server.o sbuf.o csappudp.o csapp.o
	$(CC) $(CFLAGS) server.o sbuf.o csappudp.o csapp.o -o server $(LDFLAGS)

client: client.o csappudp.o csapp.o
	$(CC) $(CFLAGS) client.o csappudp.o csapp.o -o client $(LDFLAGS)

clean:
	rm -f *~ *.o server client *.tar *.zip *.gzip *.bzip *.gz

