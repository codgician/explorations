#include <assert.h>
#include <stdbool.h>

#include "csapp.h"
#include "csappudp.h"

/* Length */
#define USERNAME_MAX_LENGTH     32
#define INST_MAX_LENGTH         32
#define BUF_SIZE (MAXLINE + INST_MAX_LENGTH + USERNAME_MAX_LENGTH)

/* Error code */
#define OK 0
#define ERR_USERNAME_EMPTY      -1
#define ERR_USERNAME_TOO_LONG   -2

/* Data structure holding all params needed for reciever */
typedef struct _RemoteInfo {
    int fd;
    struct sockaddr_storage addr;
    socklen_t addrlen;
} RemoteInfo;

RemoteInfo rinfo;  /* Info of remote server */
int global_uid;     /* uid */
sem_t mutex;        /* mutex for uid */

void *reciever_thread(void *vargp);
int check_username(char *username);
void sigintHandler(int sig_num);
void logout();

int main(int argc, char **argv)  {
    int clientfd;
    char *host, *port, buf[BUF_SIZE], buf_s[MAXLINE], username[MAXLINE];
    pthread_t tid;

    if (argc != 3) {
	    fprintf(stderr, "usage: %s <host> <port>\n", argv[0]);
	    exit(0);
    }
    host = argv[1];
    port = argv[2];
    Sem_init(&mutex, 0, 0);
    
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

    /* Set up socket */
    struct sockaddr_storage addr;
    socklen_t addrlen = sizeof(addr);
    clientfd = Open_clientfd_udp(host, port, (struct sockaddr *)&addr, &addrlen);

    /* Create reciever thread */
    rinfo.fd = clientfd;
    rinfo.addr = addr;
    rinfo.addrlen = addrlen;
    Pthread_create(&tid, NULL, reciever_thread, NULL);

    /* Attempt login */
    sscanf(buf, "%[^\r\n]", username);
    sprintf(buf, "LOGIN %s\r\n", username);
    Sendto(clientfd, buf, strlen(buf), 0, (struct sockaddr *)&addr, addrlen);

    /* Sender */
    P(&mutex);
    while (Fgets(buf_s, MAXLINE, stdin) != NULL) {
        if (strncasecmp(buf_s, "/quit", strlen("/quit")) == 0) {
            logout();
            break;
        }
        sprintf(buf, "MSG %d %s", global_uid, buf_s);
        Sendto(clientfd, buf, strlen(buf), 0, (struct sockaddr *)&addr, addrlen);
    }

    Pthread_cancel(tid);
    Close(clientfd);
    exit(0);
}

/* Reciever thread */
void *reciever_thread(void *vargp) {
    Pthread_detach(pthread_self());
    int fd = rinfo.fd;
    struct sockaddr_storage addr = rinfo.addr;
    socklen_t addrlen = rinfo.addrlen;

    char buf[BUF_SIZE];
    ssize_t len;
    while (1) {
        len = Recvfrom(fd, buf, sizeof(buf), 0, (struct sockaddr *)&addr, &addrlen);
        buf[len] = '\0';

        if (strncasecmp(buf, "LOGIN ", strlen("LOGIN ")) == 0) {
            int uid;
            if (sscanf(buf, "LOGIN %d", &uid) == 1) {
                // printf("uid = %d\n", uid);
                global_uid = uid;
                V(&mutex);  /* Mark uid as available */
            }
        } else if (strncasecmp(buf, "MSG ", strlen("MSG ")) == 0) {
            printf("%s", buf + 4);
        } else if (strncasecmp(buf, "ERROR ", strlen("ERROR ")) == 0) {
            fprintf(stderr, "[ERROR] %s", buf + 6);
        } else {
            fprintf(stderr, "[ERROR] Unrecognized response: %s", buf);
        }
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

/* Handle SIGINT signal */
void sigintHandler(int sig_num)  { 
    Signal(SIGINT, sigintHandler);
    fflush(stdout);
    logout();
    exit(0);
} 

/* Send logout instruction to server */
void logout() {
    char buf[BUF_SIZE];
    sprintf(buf, "LOGOUT %d\r\n", global_uid);
    Sendto(rinfo.fd, buf, strlen(buf), 0, (struct sockaddr *)&rinfo.addr, rinfo.addrlen);
}