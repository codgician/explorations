#ifndef __BSP_LED_H
#define __BSP_LED_H

#include "stm32f10x.h"

#define  LED1_GPIO_CLK         RCC_APB2Periph_GPIOC
#define  LED1_GPIO_PORT        GPIOC
#define  LED1_GPIO_PIN         GPIO_Pin_2
#define  LED2_GPIO_CLK         RCC_APB2Periph_GPIOC
#define  LED2_GPIO_PORT        GPIOC
#define  LED2_GPIO_PIN         GPIO_Pin_3

#define digitalToggle(p, i)     { p -> ODR ^= i; }

void initLED(void);
void setLED(uint8_t);
void toggleLED(uint8_t);
void triggerLEDCheck(void);

#endif
