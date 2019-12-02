#include "bsp_led.h"

#include "bsp_tim.h"

void initLED(void) {
    GPIO_InitTypeDef GPIO_InitTStruct;
    RCC_APB2PeriphClockCmd(LED1_GPIO_CLK | LED2_GPIO_CLK, ENABLE);
    GPIO_InitTStruct.GPIO_Pin = LED1_GPIO_PIN;
    GPIO_InitTStruct.GPIO_Mode = GPIO_Mode_Out_PP;
    GPIO_InitTStruct.GPIO_Speed = GPIO_Speed_10MHz;
    GPIO_Init(LED1_GPIO_PORT, &GPIO_InitTStruct);
    GPIO_SetBits(LED1_GPIO_PORT, LED1_GPIO_PIN);
    GPIO_InitTStruct.GPIO_Pin = LED2_GPIO_PIN;
    GPIO_Init(LED2_GPIO_PORT, &GPIO_InitTStruct);
    GPIO_SetBits(LED2_GPIO_PORT, LED2_GPIO_PIN);
}

void setLED(uint8_t state) {
    if (state & 1) {
        GPIO_ResetBits(LED1_GPIO_PORT, LED1_GPIO_PIN);
    } else {
        GPIO_SetBits(LED1_GPIO_PORT, LED1_GPIO_PIN);
    }
    
    if ((state >> 1) & 1) {
        GPIO_ResetBits(LED2_GPIO_PORT, LED2_GPIO_PIN);
    } else {
        GPIO_SetBits(LED2_GPIO_PORT, LED2_GPIO_PIN);
    }
}

void toggleLED(uint8_t state) {
    if (state & 1) {
        digitalToggle(LED1_GPIO_PORT, LED1_GPIO_PIN);
    }
    
    if ((state >> 1) & 1) {
        digitalToggle(LED2_GPIO_PORT, LED2_GPIO_PIN);
    }
}

void triggerLEDCheck(void) {
    if (getTime(1) >= 1000) {
        toggleLED(1);
        setTime(1, 0);
    }
}
