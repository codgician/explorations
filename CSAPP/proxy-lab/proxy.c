#include <stdio.h>
#include <assert.h>
#include <stdbool.h>
#include <limits.h>

#include "csapp.h"
#include "sbuf.h"

/* Recommended max cache and object sizes */
#define MAX_CACHE_SIZE 1049000
#define MAX_OBJECT_SIZE 102400
#define CACHE_SIZE (MAX_CACHE_SIZE / sizeof(CacheObject) + 1)

/* Length */
#define METHOD_MAX_LEN      32
#define VERSION_MAX_LEN     32
#define HOST_MAX_LEN        128
#define PORT_MAX_LEN        8
#define PATH_MAX_LEN        128
#define HEADER_MAX_NUM      128
#define HEADER_MAX_LEN      128

/* Concurrency */
#define SBUF_SIZE           32
#define WORKER_NUM          8

/* Data structure for requests */
typedef struct _Request {
    char method[METHOD_MAX_LEN];
    char version[VERSION_MAX_LEN];
    char host[HOST_MAX_LEN];
    char port[PORT_MAX_LEN];
    char path[MAXLINE];
    int headercnt;
    char headers[HEADER_MAX_NUM][2][HEADER_MAX_LEN];
} Request;

/* Data structure for cache */
typedef struct _CacheObject {
    int timest;
    sem_t mutex;
    Request key;
    char val[MAX_OBJECT_SIZE];
} CacheObject;

typedef struct _Cache {
    CacheObject data[CACHE_SIZE];
    int itemcnt, readcnt, timest;
    sem_t mutex, w;
} Cache;

void work(int fd);
void *thread(void *vargp);
void clienterror(int fd, char *cause, char *errnum, 
		 char *shortmsg, char *longmsg);
int parse_request(int fd, rio_t *rp, Request *req);
void parse_headers(rio_t *rp, Request *req);
int send_to_remote(int fd, Request *req, bool *cacheable);
bool check_request_equal(const Request *key, const Request *value);

Cache *cache_init();
char *cache_read(const Request *key);
void cache_insert(const Request *key, const char *val);

sbuf_t sbuf;    /* Shared buffer of connected descriptors */
Cache *cachep;  /* Shared cache */

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

    /* Initialize worker threads */
    sbuf_init(&sbuf, SBUF_SIZE);
    for (int i = 0; i < WORKER_NUM; i++) {
        Pthread_create(&tid, NULL, thread, NULL);
    }

    cachep = cache_init();

    listenfd = Open_listenfd(argv[1]);
    while (1) {
	    clientlen = sizeof(struct sockaddr_storage); 
	    int connfd = Accept(listenfd, (SA *)&clientaddr, &clientlen);
        sbuf_insert(&sbuf, connfd);
        Getnameinfo((SA *) &clientaddr, clientlen, client_hostname, MAXLINE, 
                    client_port, MAXLINE, 0);
        // printf("Connected to (%s, %s)\n", client_hostname, client_port);
    }

    Free(cachep);
    return 0;
}

/* Proxy thread */
void *thread(void *vargp) {
    Pthread_detach(pthread_self());
    while (1) {
        int fd = sbuf_remove(&sbuf);
        work(fd);
        Close(fd);
    }
    return NULL;
}

/* Work! */
void work(int fd) {
    char buf[MAXLINE];
    rio_t rio;
    Request req;
    Rio_readinitb(&rio, fd);
    
    parse_request(fd, &rio, &req);  /* Parse request line */
    parse_headers(&rio, &req);      /* Parse headers */

    if (strcasecmp(req.method, "GET")) {    /* Check method */
        clienterror(fd, req.method, "501", "Not Implemented",
                "Method not implemented");
        return;
    }
    
    char *data = cache_read(&req);
    if (data == NULL) { /* Cache miss */
        bool cacheable = true;
        int connfd = send_to_remote(fd, &req, &cacheable); /* Send request to remote */
        if (connfd < 0) {
            return;
        }
        data = (char *)Malloc(MAX_OBJECT_SIZE * sizeof(char));
        char *data_head = data;
        int charcnt = 0;
        ssize_t n;
        Rio_readinitb(&rio, connfd);
        while ((n = Rio_readlineb(&rio, buf, MAXLINE))) {
            if (cacheable) {
                if (n < 0 || data_head + n >= data + MAX_OBJECT_SIZE) {
                    cacheable = false;
                } else {
                    strncpy(data_head, buf, n);
                    data_head += n; charcnt += n;
                }
            }

            if (n >= 0)
                Rio_writen(fd, buf, n);
        }
        Close(connfd);
        if (cacheable) {
            cache_insert(&req, data);
        }
    } else {            /* Cache hit */
        Rio_writen(fd, data, strlen(data));
    }
    Free(data);
}

/* Parse request */
int parse_request(int fd, rio_t *rp, Request *req) {
    char buf[MAXLINE], method[MAXLINE], uri[MAXLINE], version[MAXLINE];

    /* Parse request line */
    if (Rio_readlineb(rp, buf, MAXLINE) <= 0) {
        return 0;
    }

    sscanf(buf, "%s %s %s", method, uri, version); 

    strncpy(req -> method, method, METHOD_MAX_LEN);
    strncpy(req -> version, version, VERSION_MAX_LEN);

    /* Parse URI */
    char prefix[MAXLINE], host[MAXLINE], port[MAXLINE], path[MAXLINE];
    strncpy(port, "80", 3);
    path[0] = '\0';
    sscanf(uri, "%[^:/]://%[^/]/%255[^\r\n]", prefix, buf, path);
    sscanf(buf, "%[^:]:%s", host, port);

    if (strlen(host) < 3) {
        printf("uri = %s, host = %s, path = %s\n", uri, host, path);
    }

    strncpy(req -> host, host, HOST_MAX_LEN);
    strncpy(req -> port, port, PORT_MAX_LEN);
    strncpy(req -> path, path, PATH_MAX_LEN);

    return 0;
}

/* Parse headers */
#define PATCH_NUM 2
void parse_headers(rio_t *rp, Request *req) {
    char buf[MAXLINE];

    /* Parse headers */
    int num = 0;
    Rio_readlineb(rp, buf, MAXLINE);
    while (strcmp(buf, "\r\n")) { 
        assert(num < HEADER_MAX_NUM);
        sscanf(buf, "%[^:]: %[^\r\n]", req -> headers[num][0], req -> headers[num][1]);
        num++;
        strncpy(buf, "\r\n", MAXLINE);
        Rio_readlineb(rp, buf, MAXLINE);
    }

    /* Patch headers as required */
    char * const PATCHES[PATCH_NUM][2] = {
        {"Connection", "close"},
        {"Proxy-Connection", "close"}
    };

    bool fixed[PATCH_NUM];
    memset(fixed, false, sizeof(fixed));
    for (int i = 0; i < num; i++) {
        for (int j = 0; j < PATCH_NUM; j++) {
            if (strncmp(req -> headers[i][0], PATCHES[j][0], HEADER_MAX_LEN) == 0) {
                strncpy(req -> headers[i][1], PATCHES[j][1], HEADER_MAX_LEN);
                fixed[j] = true;
            }
        }
    }

    for (int i = 0; i < PATCH_NUM; i++) {
        if (!fixed[i]) {
            assert(num < HEADER_MAX_NUM);
            strncpy(req -> headers[num][0], PATCHES[i][0], HEADER_MAX_LEN);
            strncpy(req -> headers[num][1], PATCHES[i][1], HEADER_MAX_LEN);
            num++;
        }
    }

    req -> headercnt = num;
}


/* Deliver error page */
void clienterror(int fd, char *cause, char *errnum, 
		 char *shortmsg, char *longmsg) {
    char buf[MAXLINE];

    /* Print the HTTP response headers */
    sprintf(buf, "HTTP/1.0 %s %s\r\n", errnum, shortmsg);
    Rio_writen(fd, buf, strlen(buf));
    sprintf(buf, "Content-type: text/html\r\n\r\n");
    Rio_writen(fd, buf, strlen(buf));

    /* Print the HTTP response body */
    sprintf(buf, "<html><title>Opoos</title>");
    Rio_writen(fd, buf, strlen(buf));
    sprintf(buf, "<body bgcolor=""ffffff"">\r\n");
    Rio_writen(fd, buf, strlen(buf));
    sprintf(buf, "%s: %s\r\n", errnum, shortmsg);
    Rio_writen(fd, buf, strlen(buf));
    sprintf(buf, "<p>%s: %s\r\n", longmsg, cause);
    Rio_writen(fd, buf, strlen(buf));
    sprintf(buf, "<hr><em>codgician's naive proxy</em>\r\n");
    Rio_writen(fd, buf, strlen(buf));
}

/* Redirect request to remote server */
int send_to_remote(int fd, Request *req, bool* cacheable) {
    int clientfd;
    char buf[MAXLINE], *buf_head = buf;
    rio_t rio;

    clientfd = Open_clientfd(req -> host, req -> port);
    if (clientfd < 0) {
        clienterror(fd, req -> host, "500", "Internal Server Error",
                "DNS lookup failed");
        return clientfd;
    }
    Rio_readinitb(&rio, clientfd);
    sprintf(buf_head, "%s /%s HTTP/1.0\r\n", req -> method, req -> path);
    buf_head += strlen(buf_head);

    // Send headers
    for (int i = 0; i < req -> headercnt; i++) {
        sprintf(buf_head, "%s: %s\r\n", req -> headers[i][0], req -> headers[i][1]);
        buf_head += strlen(buf_head);
    }

    sprintf(buf_head, "\r\n");
    Rio_writen(clientfd, buf, MAXLINE);
    *cacheable = errno != EPIPE && errno != ECONNRESET;
    return clientfd;
}

/* Initialize cache */
Cache *cache_init() {
    Cache *cachep = (Cache *)Malloc(sizeof(Cache));
    cachep -> itemcnt = 0;
    cachep -> readcnt = 0;
    cachep -> timest = 0;
    Sem_init(&cachep -> mutex, 0, 1);
    Sem_init(&cachep -> w, 0, 1);
    return cachep; 
}

/* Check if Requests are equal */
bool check_request_equal(const Request * fst, const Request * snd) {
    if (strncmp(fst -> method, snd -> method, METHOD_MAX_LEN) != 0)
        return false;
    if (strncmp(fst -> host, snd -> host, HOST_MAX_LEN) != 0)
        return false;
    if (strncmp(fst -> path, snd -> path, PATH_MAX_LEN) != 0)
        return false;
    if (fst -> headercnt != snd -> headercnt)
        return false;

    for (int i = 0; i < fst -> headercnt; i++) { 
        if (strncmp(fst -> headers[i][0], snd -> headers[i][0], HEADER_MAX_LEN) != 0)
            return false;
        if (strncmp(fst -> headers[i][1], snd -> headers[i][1], HEADER_MAX_LEN) != 0)
            return false;
    }
    return true;
}

/* Read from cache: Reader */
char *cache_read(const Request *key) {
    P(&cachep -> mutex);    /* Lock read */

    cachep -> readcnt++;
    if (cachep -> readcnt == 1) {     /* First in */
        P(&cachep -> w);    /* Lock write */
    }
    V(&cachep -> mutex);    /* Unlock cache */

    /* Reading happens */
    CacheObject * objp = NULL;
    for (int i = 0; i < cachep -> itemcnt && objp == NULL; i++) {
        if (check_request_equal(&cachep -> data[i].key, key)) {
            objp = cachep -> data + i;
            break;
        }
    }

    char *retp = NULL;
    P(&cachep -> mutex);    /* Lock read */
    if (objp != NULL) {
        P(&objp -> mutex);  /* Lock cache Object */
        retp = (char *)Malloc(sizeof(char) * MAX_OBJECT_SIZE);
        strncpy(retp, objp -> val, MAX_OBJECT_SIZE);
        objp -> timest = cachep -> timest++;
        V(&objp -> mutex);  /* Unlock cache object */
    }

    cachep -> readcnt--;
    if (cachep -> readcnt == 0) { 
        V(&cachep -> w);    /* Unlock write */
    }
    V(&cachep -> mutex);    /* Unlock read */

    return retp;
}

/* Inserting objects into cache: Writer */
void cache_insert(const Request *key, const char *val) {
    P(&cachep -> w);    /* Lock write */

    /* Eviction */
    if (cachep -> itemcnt == CACHE_SIZE) {
        int curtimest = INT_MAX, id = -1;
        for (int i = 0; i < cachep -> itemcnt; i++) {
            if (curtimest > cachep -> data[i].timest) {
                curtimest = cachep -> data[i].timest;
                id = i;
            }
        }

        assert(id != -1 && id < cachep -> itemcnt);
        if (id != cachep -> itemcnt) {
            cachep -> data[id] = cachep -> data[cachep -> itemcnt - 1];
        }
        cachep -> itemcnt--;
        assert(cachep -> itemcnt >= 0);
    }

    /* Writing happens */
    memcpy(&cachep -> data[cachep -> itemcnt].key, key, sizeof(Request));
    strncpy(cachep -> data[cachep -> itemcnt].val, val, MAX_OBJECT_SIZE);
    Sem_init(&cachep -> data[cachep -> itemcnt].mutex, 0, 1);
    cachep -> data[cachep -> itemcnt].timest = cachep -> timest++;
    cachep -> itemcnt++;

    V(&cachep -> w);    /* Unlock write */
}
