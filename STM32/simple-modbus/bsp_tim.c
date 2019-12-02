#include "bsp_tim.h"

volatile uint32_t timeCounter[2];

void initTim(void) {
    TIM_TimeBaseInitTypeDef  TIM_TimeBaseStructure;
    MODBUS_TIM_APBxClock_FUN(MODBUS_TIM_CLK, ENABLE);
    TIM_TimeBaseStructure.TIM_Period = MODBUS_TIM_Period;	
    TIM_TimeBaseStructure.TIM_Prescaler = MODBUS_TIM_Prescaler;
    TIM_TimeBaseInit(MODBUS_TIM, &TIM_TimeBaseStructure);
    TIM_ClearFlag(MODBUS_TIM, TIM_FLAG_Update);
    TIM_ITConfig(MODBUS_TIM,TIM_IT_Update,ENABLE);
    TIM_Cmd(MODBUS_TIM, ENABLE);	
    
    NVIC_InitTypeDef NVIC_InitStructure; 
    NVIC_PriorityGroupConfig(NVIC_PriorityGroup_0);		
    NVIC_InitStructure.NVIC_IRQChannel = MODBUS_TIM_IRQ ;	
    NVIC_InitStructure.NVIC_IRQChannelPreemptionPriority = 0;	 
    NVIC_InitStructure.NVIC_IRQChannelSubPriority = 3;	
    NVIC_InitStructure.NVIC_IRQChannelCmd = ENABLE;
    NVIC_Init(&NVIC_InitStructure);
    
    MODBUS_TIM_APBxClock_FUN(LED_TIM_CLK, ENABLE);
    TIM_TimeBaseStructure.TIM_Period = LED_TIM_Period;	
    TIM_TimeBaseStructure.TIM_Prescaler = LED_TIM_Prescaler;
    TIM_TimeBaseInit(LED_TIM, &TIM_TimeBaseStructure);
    TIM_ClearFlag(LED_TIM, TIM_FLAG_Update);
    TIM_ITConfig(LED_TIM,TIM_IT_Update,ENABLE);
    TIM_Cmd(LED_TIM, ENABLE);	
    
    NVIC_PriorityGroupConfig(NVIC_PriorityGroup_1);		
    NVIC_InitStructure.NVIC_IRQChannel = LED_TIM_IRQ ;	
    NVIC_InitStructure.NVIC_IRQChannelPreemptionPriority = 0;	 
    NVIC_InitStructure.NVIC_IRQChannelSubPriority = 3;	
    NVIC_InitStructure.NVIC_IRQChannelCmd = ENABLE;
    NVIC_Init(&NVIC_InitStructure);
    
    timeCounter[0] = 0;
    timeCounter[1] = 0;
}

void incTime(uint8_t id) {
    timeCounter[id]++;
}

void setTime(uint8_t id, uint32_t t) {
    timeCounter[id] = t;
}

uint32_t getTime(uint8_t id) {
    return timeCounter[id];
}
