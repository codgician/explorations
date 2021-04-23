#ifndef __BSP_TIM_H
#define __BSP_TIM_H

#include "stm32f10x.h"

#define            MODBUS_TIM                   TIM6
#define            MODBUS_TIM_APBxClock_FUN     RCC_APB1PeriphClockCmd
#define            MODBUS_TIM_CLK               RCC_APB1Periph_TIM6
#define            MODBUS_TIM_Period            1000-1
#define            MODBUS_TIM_Prescaler         71
#define            MODBUS_TIM_IRQ               TIM6_IRQn
#define            MODBUS_TIM_IRQHandler        TIM6_IRQHandler

#define            LED_TIM                      TIM7
#define            LED_TIM_APBxClock_FUN        RCC_APB1PeriphClockCmd
#define            LED_TIM_CLK                  RCC_APB1Periph_TIM7
#define            LED_TIM_Period               1000-1
#define            LED_TIM_Prescaler            71
#define            LED_TIM_IRQ                  TIM7_IRQn
#define            LED_TIM_IRQHandler           TIM7_IRQHandler

void initTim(void);
void incTime(uint8_t);
void setTime(uint8_t, uint32_t);
uint32_t getTime(uint8_t);

#endif
