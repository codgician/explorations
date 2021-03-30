#ifndef __CSAPPUDP_H__
#define __CSAPPUDP_H__

#include "csapp.h"

int open_clientfd_udp(char *hostname, char *port, struct sockaddr *addr, socklen_t *addrlen);
int Open_clientfd_udp(char *hostname, char *port, struct sockaddr *addr, socklen_t *addrlen);
int open_listenfd_udp(char *port);
int Open_listenfd_udp(char *port);
ssize_t Sendto(int fd, const void *buf, size_t n, int flags, struct sockaddr *addr, socklen_t addrlen);
ssize_t Recvfrom(int fd, void *buf, size_t n, int flags, struct sockaddr *addr, socklen_t *addrlen);

#endif