#include "bsp_usart.h"

void initUSART(void) {    
    GPIO_InitTypeDef GPIO_InitStructure;
    USART_InitTypeDef USART_InitStructure;

    MODBUS_USART_GPIO_APBxClkCmd(MODBUS_USART_GPIO_CLK, ENABLE);
    MODBUS_USART_APBxClkCmd(MODBUS_USART_CLK, ENABLE);

    GPIO_InitStructure.GPIO_Pin = MODBUS_USART_TX_GPIO_PIN;
    GPIO_InitStructure.GPIO_Mode = GPIO_Mode_AF_PP;
    GPIO_InitStructure.GPIO_Speed = GPIO_Speed_50MHz;
    GPIO_Init(MODBUS_USART_TX_GPIO_PORT, &GPIO_InitStructure);

    GPIO_InitStructure.GPIO_Pin = MODBUS_USART_RX_GPIO_PIN;
    GPIO_InitStructure.GPIO_Mode = GPIO_Mode_IN_FLOATING;
    GPIO_Init(MODBUS_USART_RX_GPIO_PORT, &GPIO_InitStructure);
    
    USART_InitStructure.USART_BaudRate = MODBUS_USART_BAUDRATE;
    USART_InitStructure.USART_WordLength = USART_WordLength_8b;
    USART_InitStructure.USART_StopBits = USART_StopBits_1;
    USART_InitStructure.USART_Parity = USART_Parity_No ;
    USART_InitStructure.USART_HardwareFlowControl = USART_HardwareFlowControl_None;
    USART_InitStructure.USART_Mode = USART_Mode_Rx | USART_Mode_Tx;
    USART_Init(MODBUS_USARTx, &USART_InitStructure);
    
    NVIC_InitTypeDef NVIC_InitStructure;
  
    NVIC_PriorityGroupConfig(NVIC_PriorityGroup_2);
  
    NVIC_InitStructure.NVIC_IRQChannel = MODBUS_USART_IRQ;
    NVIC_InitStructure.NVIC_IRQChannelPreemptionPriority = 1;
    NVIC_InitStructure.NVIC_IRQChannelSubPriority = 1;
    NVIC_InitStructure.NVIC_IRQChannelCmd = ENABLE;
    NVIC_Init(&NVIC_InitStructure);
    
    USART_ITConfig(MODBUS_USARTx, USART_IT_RXNE, ENABLE);	
    USART_ITConfig (MODBUS_USARTx, USART_IT_IDLE, ENABLE ); 

    USART_Cmd(MODBUS_USARTx, ENABLE);	    
}

void usartSendByte(USART_TypeDef * pUSARTx, uint8_t ch) {
    USART_SendData(pUSARTx,ch);
    while (USART_GetFlagStatus(pUSARTx, USART_FLAG_TXE) == RESET);	
}

void usartSendStr(USART_TypeDef * pUSARTx, char *str) {
    unsigned int k = 0;
    while (1) {
        usartSendByte(pUSARTx, str[k]);
        if (str[k++] == '\0')
            break;
    }
    while (USART_GetFlagStatus(pUSARTx, USART_FLAG_TC) == RESET);
}

// Redirect printf to serial port
int fputc(int ch, FILE *f) {
    USART_SendData(MODBUS_USARTx, (uint8_t) ch);
    while (USART_GetFlagStatus(MODBUS_USARTx, USART_FLAG_TXE) == RESET);		
    return (ch);
}

// Redirect scanf to serial port
int fgetc(FILE *f) {
    while (USART_GetFlagStatus(MODBUS_USARTx, USART_FLAG_RXNE) == RESET);
    return (int)USART_ReceiveData(MODBUS_USARTx);
}
