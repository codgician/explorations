#ifndef __ESP_DATA_QUEUE_H_
#define __ESP_DATA_QUEUE_H_

#include "stm32f10x.h"

#include <string.h>
#include <stdio.h>
#include <stdlib.h>

#define QUEUE_SIZE 2048

typedef struct Node {
    uint16_t headPt, tailPt;
    uint16_t len;
    uint8_t que[QUEUE_SIZE];
} QueueNode;

void initQueueNode(void);
uint8_t queIsEmpty(void);
uint8_t queIsFull(void);
uint8_t quePush(uint8_t);
uint8_t quePop(void);
uint8_t queFront(uint8_t*);
uint16_t queSize(void);

#endif
