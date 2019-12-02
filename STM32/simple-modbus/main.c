#include "stm32f10x.h"
#include "stm32f10x_it.h"

#include "bsp_led.h"
#include "bsp_tim.h"
#include "bsp_usart.h"

#include "modbus.h"

#include "rx_data_queue.h"

int main(void) {
    // Initialize hardware
    initLED(); 
    initUSART();
    initTim();
    initQueueNode();
    initRegs();
    
    // printf("Finished initalization!\n");
    while (1) {}
}
