#include "rx_data_queue.h"

QueueNode qn;

void initQueueNode(void) {
    qn.headPt = 0;
    qn.tailPt = 0;
    qn.len = 0;
}

uint8_t queIsEmpty(void) {
    return qn.len == 0;
}

uint8_t queIsFull(void) {
    return qn.len == QUEUE_SIZE;
}

uint8_t quePush(uint8_t ch) {
    if (queIsFull())
        return 0;
    qn.que[qn.tailPt] = ch;
    qn.tailPt = qn.tailPt == QUEUE_SIZE - 1 ? 0 : qn.tailPt + 1;
    qn.len++;
    return 1;
}

uint8_t queFront(uint8_t *ret) {
    if (queIsEmpty())
        return 0;
    *ret = qn.que[qn.headPt];
    return 1;
}

uint8_t quePop(void) {
    if (queIsEmpty())
        return 0;
    qn.headPt = qn.headPt == QUEUE_SIZE - 1 ? 0 : qn.headPt + 1;
    qn.len--;
    return 1;
}

uint16_t queSize(void) {
    return qn.len;
}
