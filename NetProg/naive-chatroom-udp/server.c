#include <assert.h>
#include <stdbool.h>

#include "csapp.h"
#include "csappudp.h"
#include "sbuf.h"

/* Concurrency */
#define SBUF_SIZE               32
#define WORKER_NUM              8

/* Length */
#define UID_MAX_NUM             65535
#define USERNAME_MAX_LENGTH     32
#define INST_MAX_LENGTH         32
#define BUF_SIZE                (MAXLINE + USERNAME_MAX_LENGTH + INST_MAX_LENGTH + 1)

/* Error code */
#define OK 0
#define ERR_USERNAME_EMPTY      -1
#define ERR_USERNAME_TOO_LONG   -2

/* Data structure that holds free descriptors */
typedef struct _UidPool {
    int pool[UID_MAX_NUM];
    int rear;
    sem_t mutex;
} UidPool;

/* Data structure user info */
typedef struct _UserInfo {
    char username[MAXLINE];
    struct sockaddr addr;
    socklen_t addrlen;
} UserInfo;

/* Data structure holding active users */
typedef struct _Users {
    UserInfo *arr[UID_MAX_NUM];
    int usercnt, readcnt;
    sem_t mutex, w;
} Users;

UidPool uidpool;                        /* Free descriptors pool */
Users users;                            /* Active descriptors */
sbuf_t sbuf;                            /* Message queue */

void *broadcast_thread(void *vargp);
void broadcast_msg(int listenfd, int src, char *username, char *msg);
int check_username(char *username);
int read_usercnt();
bool check_uid(int uid);
char *get_username(int uid);
void reg_user(int uid, UserInfo *uinfo);
void unreg_user(int uid);
void insert_uid();
int remove_uid();

int main(int argc, char **argv) {
    Signal(SIGPIPE, SIG_IGN);
    if (argc != 2) {
        fprintf(stderr, "usage: %s <port>\n", argv[0]);
        exit(0);
    }

    /* Initialize descriptor pool */
    for (int i = 0; i < UID_MAX_NUM; i++) {
        uidpool.pool[i] = i;
    }
    uidpool.rear = UID_MAX_NUM;
    Sem_init(&uidpool.mutex, 0, 1);

    /* Initialize users */
    users.readcnt = 0, users.usercnt = 0;
    for (int i = 0; i < UID_MAX_NUM; i++) {
        users.arr[i] = NULL;
    }
    Sem_init(&users.mutex, 0, 1);
    Sem_init(&users.w, 0, 1);
    
    /* Set up server */
    struct sockaddr_storage addr;
    socklen_t addrlen = sizeof(addr);
    int listenfd = Open_listenfd_udp(argv[1]);

    /* Create broadcast threads */
    pthread_t tid;
    sbuf_init(&sbuf, SBUF_SIZE);
    for (int i = 0; i < WORKER_NUM; i++) {
        Pthread_create(&tid, NULL, broadcast_thread, &listenfd);
    }

    char buf[BUF_SIZE];
    ssize_t len;
    while (1) {
        len = Recvfrom(listenfd, buf, sizeof(buf), 0, (struct sockaddr *)&addr, &addrlen);
        buf[len] = '\0';

        // printf("[LOG] recv = %s", buf);

        if (strncasecmp(buf, "LOGIN ", strlen("LOGIN ")) == 0) {        /* Request login */
            int uid = remove_uid();
            if (uid < 0) {   /* Server is full */
                Sendto(listenfd, "ERROR Server is full\r\n", 
                        strlen("ERROR Server is full\r\n"), 
                        0, (struct sockaddr *)&addr, addrlen);
            } else {
                /* Parse username */
                UserInfo *uinfo = (UserInfo *)Malloc(sizeof(UserInfo));
                if (sscanf(buf + strlen("LOGIN "), "%[^\r\n]", uinfo -> username) == 1) {
                    int err = check_username(uinfo -> username);
                    if (err == OK) {
                        /* Register user info */
                        uinfo -> addr = *(struct sockaddr *)&addr;
                        uinfo -> addrlen = addrlen;
                        reg_user(uid, uinfo);
                        /* Send descriptor back */
                        sprintf(buf, "LOGIN %d\r\n", uid);
                        Sendto(listenfd, buf, strlen(buf), 0, (struct sockaddr *)&addr, addrlen);
                        /* Broadcast welcome message */
                        Msg *msg = (Msg *)Malloc(sizeof(Msg));
                        msg -> username = NULL;
                        sprintf(msg -> buf, "Welcome to naive chatroom, %s!", uinfo -> username);
                        sbuf_insert(&sbuf, msg);
                    } else {
                        if (err == ERR_USERNAME_TOO_LONG) {
                            Sendto(listenfd, "ERROR Username too long\r\n", 
                                    strlen("ERROR Username too long\r\n"), 
                                    0, (struct sockaddr *)&addr, addrlen);
                        } else if (err == ERR_USERNAME_EMPTY) {
                            Sendto(listenfd, "ERROR Username should not be empty\r\n", 
                                    strlen("ERROR Username should not be empty\r\n"), 
                                    0, (struct sockaddr *)&addr, addrlen);
                        } else {
                            assert(false);
                        }
                        Free(uinfo);
                        insert_uid(uid);
                    }
                } else {
                    Sendto(listenfd, "ERROR Failed to parse username\r\n", 
                                    strlen("ERROR Failed to parse username\r\n"), 
                                    0, (struct sockaddr *)&addr, addrlen);
                    Free(uinfo);
                    insert_uid(uid);
                }
            }
        } else if (strncasecmp(buf, "MSG ", strlen("MSG")) == 0) {      /* Send message */
            Msg *msg = (Msg *)Malloc(sizeof(Msg));
            if (sscanf(buf, "MSG %d %[^\r\n]", &msg -> uid, msg -> buf) == 2) {
                if (check_uid(msg -> uid)) {
                    Free(msg);
                } else {
                    msg -> username = get_username(msg -> uid);
                    sbuf_insert(&sbuf, msg);
                }
            }
        } else if (strncmp(buf, "LOGOFF ", sizeof("LOGOFF ")) == 0) {   /* Request logoff */
            int uid;
            if (sscanf(buf, "LOGOFF %d", &uid) == 1) {
                unreg_user(uid);
                remove_uid(uid);
            }
        } else {                                                        /* Invalid request */
            Sendto(listenfd, "ERROR Invalid request\r\n", 
                    strlen("ERROR Invalid request\r\n"), 
                    0, (struct sockaddr *)&addr, addrlen);
        }
    }

    return 0;
}

/* Broadcast thread */
void *broadcast_thread(void *vargp) {
    int listenfd = *(int *)vargp;
    Pthread_detach(pthread_self());
    while (1) {
        Msg *msg = sbuf_remove(&sbuf);
        broadcast_msg(listenfd, msg -> uid, msg -> username, msg -> buf);
        if (msg -> username != NULL)
            Free(msg -> username);
        Free(msg);
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

/* Broadcast msg: Reader */
void broadcast_msg(int listenfd, int src, char *username, char *msg) {
    P(&users.mutex);              /* Lock read */
    users.readcnt++;
    if (users.readcnt == 1) {     /* First in */
        P(&users.w);              /* Lock write */
    }
    V(&users.mutex);              /* Unlock read */

    /* Reading happens: broadcast message */
    char buf[BUF_SIZE];
    if (username == NULL) {
        sprintf(buf, "MSG #Server: %s\r\n", msg);
    } else {
        sprintf(buf, "MSG [%s]: %s\r\n", username, msg);
    }

    int len = strlen(buf);
    for (int i = 0; i < UID_MAX_NUM; i++) {
        if (users.arr[i] != NULL && i != src) {
            Sendto(listenfd, buf, len, 0, &users.arr[i] -> addr, users.arr[i] -> addrlen);
        }
    }

    P(&users.mutex);
    users.readcnt--;
    if (users.readcnt == 0) {
        V(&users.w);              /* Unlock write */ 
    }
    V(&users.mutex);              /* Unlock read */
}

/* Register new user */
void reg_user(int uid, UserInfo *uinfo) {
    assert(uinfo != NULL);

    P(&users.w);

    /* Writing happens */
    users.arr[uid] = uinfo;

    V(&users.w);
}

/* Unregister new user */
void unreg_user(int uid) {
    P(&users.w);

    /* Writing happens */
    assert(users.arr[uid] != NULL);
    free(users.arr[uid]);
    users.arr[uid] = NULL;

    V(&users.w);
}

/* Retrieve username */
char *get_username(int uid) {
    P(&users.mutex);              /* Lock read */
    users.readcnt++;
    if (users.readcnt == 1) {     /* First in */
        P(&users.w);              /* Lock write */
    }
    V(&users.mutex);              /* Unlock read */

    /* Reading happens: read username */
    char *ret = (char *)Malloc((strlen(users.arr[uid] -> username) + 1) * sizeof(char));
    strncpy(ret, users.arr[uid] -> username, strlen(users.arr[uid] -> username));

    P(&users.mutex);
    users.readcnt--;
    if (users.readcnt == 0) {
        V(&users.w);              /* Unlock write */ 
    }
    V(&users.mutex);              /* Unlock read */

    return ret;
}

/* Check if uid is available */
bool check_uid(int uid) {
    if (uid < 0 || uid >= UID_MAX_NUM)
        return false;

    P(&users.mutex);              /* Lock read */
    users.readcnt++;
    if (users.readcnt == 1) {     /* First in */
        P(&users.w);              /* Lock write */
    }
    V(&users.mutex);              /* Unlock read */

    /* Reading happens: broadcast message */
    bool ret = (users.arr[uid] == NULL);

    P(&users.mutex);
    users.readcnt--;
    if (users.readcnt == 0) {
        V(&users.w);              /* Unlock write */ 
    }
    V(&users.mutex);              /* Unlock read */

    return ret;
}

/* Insert descriptor to pool */
void insert_uid(int uid) {
    P(&uidpool.mutex);   /* Lock pool */

    assert(uidpool.rear < UID_MAX_NUM);
    uidpool.pool[uidpool.rear++] = uid;

    V(&uidpool.mutex);   /* Unlock pool */
}

/* Remove descriptor from pool */
int remove_uid() {
    P(&uidpool.mutex);   /* Lock pool */    

    int ret = -1;
    if (uidpool.rear > 0) {
        ret = uidpool.pool[--uidpool.rear];
    }

    V(&uidpool.mutex);   /* Unlock pool */
    return ret;
}