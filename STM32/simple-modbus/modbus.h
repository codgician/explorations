#ifndef __MODBUS_H
#define __MODBUS_H

#include "stm32f10x.h"

#include "bsp_led.h"
#include "bsp_tim.h"
#include "bsp_usart.h"

#include "rx_data_queue.h"

#define BUF_SIZE 256
#define REG_SIZE 256

#define TIMEOUT (3500.0 / (MODBUS_USART_BAUDRATE / 10))

void initRegs(void);
void triggerModbusCheck(void);

#endif
