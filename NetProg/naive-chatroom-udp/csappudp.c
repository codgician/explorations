#include "csappudp.h"

int open_clientfd_udp(char *hostname, char *port, struct sockaddr *addr, socklen_t *addrlen) {
    int clientfd, rc;
    struct addrinfo hints, *listp, *p;

    /* Get a list of potential server addresses */
    memset(&hints, 0, sizeof(struct addrinfo));
    hints.ai_family = AF_UNSPEC;
    hints.ai_socktype = SOCK_DGRAM;
    hints.ai_protocol = IPPROTO_UDP;
    hints.ai_flags =  AI_PASSIVE;

    if ((rc = getaddrinfo(hostname, port, &hints, &listp)) != 0) {
        fprintf(stderr, "getaddrinfo failed (%s:%s): %s\n", hostname, port, gai_strerror(rc));
        return -2;
    }
  
    /* Walk the list for one that we can successfully connect to */
    for (p = listp; p; p = p->ai_next) {
        /* Create a socket descriptor */
        if ((clientfd = socket(p->ai_family, p->ai_socktype, p->ai_protocol)) < 0) 
            continue; /* Socket failed, try the next */
        *addrlen = p->ai_addrlen;
        memcpy(addr, p->ai_addr, p->ai_addrlen);
        break;
    } 

    /* Clean up */
    freeaddrinfo(listp);
    if (!p) /* All connects failed */
        return -1;
    else    /* The last connect succeeded */
        return clientfd;
}

/* Wrapper for open_clientfd_udp() */
int Open_clientfd_udp(char *hostname, char *port, struct sockaddr *addr, socklen_t *addrlen)  {
    int rc;

    if ((rc = open_clientfd_udp(hostname, port, addr, addrlen)) < 0) {
        // unix_error("Open_clientfd error");
    }
    return rc;
}

int open_listenfd_udp(char *port) {
    struct addrinfo hints, *listp, *p;
    int listenfd, rc, optval=1;

    /* Get a list of potential server addresses */
    memset(&hints, 0, sizeof(struct addrinfo));
    hints.ai_family = AF_UNSPEC;
    hints.ai_socktype = SOCK_DGRAM;
    hints.ai_protocol = IPPROTO_UDP;
    hints.ai_flags =  AI_PASSIVE;

    if ((rc = getaddrinfo(NULL, port, &hints, &listp)) != 0) {
        fprintf(stderr, "getaddrinfo failed (port %s): %s\n", port, gai_strerror(rc));
        return -2;
    }

    /* Walk the list for one that we can bind to */
    for (p = listp; p; p = p->ai_next) {
        /* Create a socket descriptor */
        if ((listenfd = socket(p->ai_family, p->ai_socktype, p->ai_protocol)) < 0) 
            continue;  /* Socket failed, try the next */

        /* Eliminates "Address already in use" error from bind */
        setsockopt(listenfd, SOL_SOCKET, SO_REUSEADDR,  
                   (const void *)&optval , sizeof(int));

        /* Bind the descriptor to the address */
        if (bind(listenfd, p->ai_addr, p->ai_addrlen) == 0)
            break; /* Success */
        if (close(listenfd) < 0) { /* Bind failed, try the next */
            fprintf(stderr, "open_listenfd_udp close failed: %s\n", strerror(errno));
            return -1;
        }
    }

    /* Clean up */
    freeaddrinfo(listp);
    if (!p) /* No address worked */
        return -1;
        
    return listenfd;
}

/* Wrapper for open_listenfd_udp() */
int Open_listenfd_udp(char *port)  {
    int rc;
    if ((rc = open_listenfd_udp(port)) < 0)
	    unix_error("open_listenfd_udp error");
    return rc;
}

/* Wrapper for sendto() */
ssize_t Sendto(int fd, const void *buf, size_t n, int flags, struct sockaddr *addr, socklen_t addrlen) {
    ssize_t rc;
    if ((rc = sendto(fd, buf, n, flags, addr, addrlen)) < 0)
        unix_error("sendto error");
    return rc;
}

/* Wrapper for Recvfrom() */
ssize_t Recvfrom(int fd, void *buf, size_t n, int flags, struct sockaddr *addr, socklen_t *addrlen) {
    ssize_t rc;
    if ((rc = recvfrom(fd, buf, n, flags, addr, addrlen)) < 0)
        unix_error("recvfrom error");
    return rc;
}
