#include <assert.h>
#include <stdbool.h>

#include "csapp.h"

/* Length */
#define USERNAME_MAX_LENGTH     32

/* Error code */
#define OK 0
#define ERR_USERNAME_EMPTY      -1
#define ERR_USERNAME_TOO_LONG   -2

int check_username(char *username);
void *reciever_thread(void *vargp);

int main(int argc, char **argv)  {
    int clientfd;
    char *host, *port, buf[MAXLINE], username[MAXLINE];
    rio_t rio;
    pthread_t tid;

    if (argc != 3) {
	    fprintf(stderr, "usage: %s <host> <port>\n", argv[0]);
	    exit(0);
    }
    host = argv[1];
    port = argv[2];

    clientfd = Open_clientfd(host, port);
    Rio_readinitb(&rio, clientfd);

    /* Retrieve username */
    printf("Please enter your username: ");
    Fgets(buf, MAXLINE, stdin);
    int err;
    while ((err = check_username(buf)) != OK) {
        if (err == ERR_USERNAME_TOO_LONG) {
            printf("#Server: Username too long.\n");
        } else if (err == ERR_USERNAME_EMPTY) {
            printf("#Server: Username should not be empty.\n");
        }
        printf("Please enter your username again: ");
        Fgets(buf, MAXLINE, stdin);
    }

    sscanf(buf, "%[^\r\n]", username);
    Rio_writen(clientfd, username, strlen(username));       /* Send username to server */
    Rio_writen(clientfd, "\r\n", strlen("\r\n"));

    Pthread_create(&tid, NULL, reciever_thread, &rio); /* Create reciever */

    /* Sender */
    while (Fgets(buf, MAXLINE, stdin) != NULL) {
        if (strncasecmp(buf, "/quit", strlen("/quit")) == 0)
            break;
        Rio_writen(clientfd, buf, strlen(buf));
    }

    Pthread_cancel(tid);                            /* Cancel reciever */
    Rio_writen(clientfd, "\r\n", strlen("\r\n"));   /* Close connection */

    Close(clientfd);
    exit(0);
}

/* Reciever thread */
void *reciever_thread(void *vargp) {
    Pthread_detach(pthread_self());
    rio_t *rp = (rio_t *)vargp;

    char buf[MAXLINE];

    while (1) {
        Rio_readlineb(rp, buf, MAXLINE);
        Fputs(buf, stdout);
    }

    return NULL;
}

/* Check if username is valid */
int check_username(char *username) {
    if (strncmp(username, "\r\n", MAXLINE) == 0)
        return ERR_USERNAME_EMPTY;
    if (strlen(username) >= USERNAME_MAX_LENGTH)
        return ERR_USERNAME_TOO_LONG;
    return OK;
}
