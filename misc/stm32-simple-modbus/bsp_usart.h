#ifndef __BSP_USART_H
#define __BSP_USART_H

#include <stdio.h>
#include "stm32f10x.h"

// Modbus: USART-1
#define  MODBUS_USARTx                   USART1
#define  MODBUS_USART_CLK                RCC_APB2Periph_USART1
#define  MODBUS_USART_APBxClkCmd         RCC_APB2PeriphClockCmd
#define  MODBUS_USART_BAUDRATE           9600

#define  MODBUS_USART_GPIO_CLK           (RCC_APB2Periph_GPIOA)
#define  MODBUS_USART_GPIO_APBxClkCmd    RCC_APB2PeriphClockCmd
    
#define  MODBUS_USART_TX_GPIO_PORT       GPIOA   
#define  MODBUS_USART_TX_GPIO_PIN        GPIO_Pin_9
#define  MODBUS_USART_RX_GPIO_PORT       GPIOA
#define  MODBUS_USART_RX_GPIO_PIN        GPIO_Pin_10

#define  MODBUS_USART_IRQ                USART1_IRQn
#define  MODBUS_USART_IRQHandler         USART1_IRQHandler

void initUSART(void);
void usartSendByte(USART_TypeDef * pUSARTx, uint8_t ch);
void usartSendStr(USART_TypeDef * pUSARTx, char *str);

#endif

