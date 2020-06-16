#include <assert.h>
#include <stdbool.h>

#include "csapp.h"

/* Length */
#define FD_MAX_NUM              65535
#define USERNAME_MAX_LENGTH     32
#define MAX_USER_NUM            128

/* Error code */
#define OK 0
#define ERR_USERNAME_EMPTY      -1
#define ERR_USERNAME_TOO_LONG   -2

/* Data structure holding active descriptors */
typedef struct _Fds {
    bool bt[FD_MAX_NUM]; 
    int usercnt, readcnt;
    sem_t mutex, w;
} Fds;

Fds fds;        /* Active descriptors */

void *thread(void *vargp);
void work(int fd);
void broadcast_msg(int src, char *username, char *msg);
void set_fd(int fd, bool st);
int check_username(char *username);
int read_usercnt();

int main(int argc, char **argv) {
    Signal(SIGPIPE, SIG_IGN);

    /* Set up a server */
    int listenfd;
    socklen_t clientlen;
    struct sockaddr_storage clientaddr;
    char client_hostname[MAXLINE], client_port[MAXLINE];
    pthread_t tid;

    if (argc != 2) {
        fprintf(stderr, "usage: %s <port>\n", argv[0]);
        exit(0);
    }

    /* Initialize fds */
    fds.readcnt = 0, fds.usercnt = 0;
    memset(fds.bt, false, sizeof(fds.bt));
    Sem_init(&fds.mutex, 0, 1);
    Sem_init(&fds.w, 0, 1);

    listenfd = Open_listenfd(argv[1]);
    while (1) {
	    clientlen = sizeof(struct sockaddr_storage); 
	    int *connfdp = (int *)Malloc(sizeof(int));
        *connfdp = Accept(listenfd, (SA *)&clientaddr, &clientlen);
        Getnameinfo((SA *) &clientaddr, clientlen, client_hostname, MAXLINE, 
                    client_port, MAXLINE, 0);
        printf("Connected to (%s, %s) fd = %d\n", client_hostname, client_port, *connfdp);
        if (read_usercnt() >= MAX_USER_NUM) {
            Rio_writen(*connfdp, "#Server: Server is full...\n", 
                        strlen("#Server: Server is full...\n"));
        } else {
            Pthread_create(&tid, NULL, thread, connfdp);    /* Create new thread */
        }
    }

    return 0;
}

/* Server thread */
void *thread(void *vargp) {
    int fd = *((int *)vargp);
    Pthread_detach(pthread_self());
    Free(vargp);
    set_fd(fd, true);
    work(fd);
    set_fd(fd, false);
    Close(fd);
    return NULL;
}

/* Work! */
void work(int fd) {
    char buf[MAXLINE], username[USERNAME_MAX_LENGTH + 1];
    rio_t rio;

    /* Retrieve username */
    int err;
    Rio_readinitb(&rio, fd);
    if (Rio_readlineb(&rio, buf, MAXLINE) < 0)
        return;
    while ((err = check_username(buf)) != OK) {
        if (err == ERR_USERNAME_TOO_LONG) {
            Rio_writen(fd, "#Server: Username too long.\n", 
                strlen("#Server: Username too long.\n"));
        } else if (err == ERR_USERNAME_EMPTY) {
            Rio_writen(fd, "#Server: Username should not be empty.\n", 
                strlen("#Server: Username should not be empty.\n"));
        }
        if (Rio_readlineb(&rio, buf, MAXLINE) <= 0)
            return;
    }
    strncpy(username, buf, USERNAME_MAX_LENGTH);
    int len = strlen(username);
    for (int i = 0; i < len; i++) {
        if (username[i] == '\r') {
            username[i] = '\0';
            break;
        }
    }

    /* Display welcome message */
    sprintf(buf, "#Server: Welcome to naive chat room, %s!\r\n", username);
    Rio_writen(fd, buf, strlen(buf));
    sprintf(buf, "#Server: %s has just joined the room.\r\n", username);
    broadcast_msg(-1, NULL, buf);

    /* Retrieve message */
    if (Rio_readlineb(&rio, buf, MAXLINE) <= 0)
        return;
    while (strncmp(buf, "\r\n", MAXLINE)) {
        broadcast_msg(fd, username, buf);
        if (Rio_readlineb(&rio, buf, MAXLINE) <= 0)
            break;
    }

    sprintf(buf, "#Server: %s has just left the room.\r\n", username);
    broadcast_msg(-1, NULL, buf);
}

/* Check if username is valid */
int check_username(char *username) {
    if (strncmp(username, "\r\n", MAXLINE) == 0)
        return ERR_USERNAME_EMPTY;
    if (strlen(username) >= USERNAME_MAX_LENGTH)
        return ERR_USERNAME_TOO_LONG;
    return OK;
}

/* Broadcast msg: Reader */
void broadcast_msg(int src, char *username, char *msg) {
    P(&fds.mutex);              /* Lock read */
    fds.readcnt++;
    if (fds.readcnt == 1) {     /* First in */
        P(&fds.w);              /* Lock write */
    }
    V(&fds.mutex);              /* Unlock read */

    /* Reading happens: broadcast message */
    int len = strlen(msg);
    char usernamebuf[MAXLINE] = "\0";
    if (username != NULL)
        sprintf(usernamebuf, "[%s]: ", username);
    int unlen = strlen(usernamebuf);
    for (int i = 0; i < FD_MAX_NUM; i++) {
        if (fds.bt[i] && i != src) {
            Rio_writen(i, usernamebuf, unlen);
            Rio_writen(i, msg, len);
        }
    }

    P(&fds.mutex);
    fds.readcnt--;
    if (fds.readcnt == 0) {
        V(&fds.w);              /* Unlock write */ 
    }
    V(&fds.mutex);              /* Unlock read */
} 

/* Read active user count */
int read_usercnt() {
     P(&fds.mutex);              /* Lock read */
    fds.readcnt++;
    if (fds.readcnt == 1) {     /* First in */
        P(&fds.w);              /* Lock write */
    }
    V(&fds.mutex);              /* Unlock read */

    /* Reading happens: broadcast message */
    int ret = fds.readcnt;

    P(&fds.mutex);
    fds.readcnt--;
    if (fds.readcnt == 0) {
        V(&fds.w);              /* Unlock write */ 
    }
    V(&fds.mutex);              /* Unlock read */

    return ret;
}

/* Set active status of a descriptor: Writer */
void set_fd(int fd, bool st) {
    assert(fd >= 0 && fd <= FD_MAX_NUM);
    
    P(&fds.w);      /* Lock write */

    /* Writing happens */
    fds.bt[fd] = st;

    V(&fds.w);      /* Unlock write */
}
