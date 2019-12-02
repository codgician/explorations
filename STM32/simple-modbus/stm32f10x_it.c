/**
  ******************************************************************************
  * @file    Project/STM32F10x_StdPeriph_Template/stm32f10x_it.c 
  * @author  MCD Application Team
  * @version V3.5.0
  * @date    08-April-2011
  * @brief   Main Interrupt Service Routines.
  *          This file provides template for all exceptions handler and 
  *          peripherals interrupt service routine.
  ******************************************************************************
  * @attention
  *
  * THE PRESENT FIRMWARE WHICH IS FOR GUIDANCE ONLY AIMS AT PROVIDING CUSTOMERS
  * WITH CODING INFORMATION REGARDING THEIR PRODUCTS IN ORDER FOR THEM TO SAVE
  * TIME. AS A RESULT, STMICROELECTRONICS SHALL NOT BE HELD LIABLE FOR ANY
  * DIRECT, INDIRECT OR CONSEQUENTI
  
  AL DAMAGES WITH RESPECT TO ANY CLAIMS ARISING
  * FROM THE CONTENT OF SUCH FIRMWARE AND/OR THE USE MADE BY CUSTOMERS OF THE
  * CODING INFORMATION CONTAINED HEREIN IN CONNECTION WITH THEIR PRODUCTS.
  *
  * <h2><center>&copy; COPYRIGHT 2011 STMicroelectronics</center></h2>
  ******************************************************************************
  */

/* Includes ------------------------------------------------------------------*/

#include "stm32f10x_it.h"

#include "bsp_led.h"
#include "bsp_tim.h"
#include "bsp_usart.h"

#include "rx_data_queue.h"
#include "modbus.h"

#include "stdio.h"

/** @addtogroup STM32F10x_StdPeriph_Template
  * @{
  */

/* Private typedef -----------------------------------------------------------*/
/* Private define ------------------------------------------------------------*/
/* Private macro -------------------------------------------------------------*/
/* Private variables ---------------------------------------------------------*/
/* Private function prototypes -----------------------------------------------*/
/* Private functions ---------------------------------------------------------*/

/******************************************************************************/
/*            Cortex-M3 Processor Exceptions Handlers                         */
/******************************************************************************/

/**
  * @brief  This function handles NMI exception.
  * @param  None
  * @retval None
  */
void NMI_Handler(void)
{
}

/**
  * @brief  This function handles Hard Fault exception.
  * @param  None
  * @retval None
  */
void HardFault_Handler(void)
{
  /* Go to infinite loop when Hard Fault exception occurs */
  while (1) {}
}

/**
  * @brief  This function handles Memory Manage exception.
  * @param  None
  * @retval None
  */
void MemManage_Handler(void)
{
  /* Go to infinite loop when Memory Manage exception occurs */
  while (1) {}
}

/**
  * @brief  This function handles Bus Fault exception.
  * @param  None
  * @retval None
  */
void BusFault_Handler(void) {
  /* Go to infinite loop when Bus Fault exception occurs */
  while (1) {}
}

/**
  * @brief  This function handles Usage Fault exception.
  * @param  None
  * @retval None
  */
void UsageFault_Handler(void) {
  /* Go to infinite loop when Usage Fault exception occurs */
  while (1) {}
}

/**
  * @brief  This function handles SVCall exception.
  * @param  None
  * @retval None
  */

/*
void SVC_Handler(void)
{
}
*/

/**
  * @brief  This function handles Debug Monitor exception.
  * @param  None
  * @retval None
  */
void DebugMon_Handler(void)
{
}

/**
  * @brief  This function handles PendSVC exception.
  * @param  None
  * @retval None
  */
/*
void PendSV_Handler(void)
{
}
*/

/**
  * @brief  This function handles SysTick Handler.
  * @param  None
  * @retval None
  */
/*
void SysTick_Handler(void)
{
}
*/

void MODBUS_USART_IRQHandler(void) {
    uint8_t ch; 
  if (USART_GetITStatus(MODBUS_USARTx, USART_IT_RXNE) != RESET) {
        ch = USART_ReceiveData(MODBUS_USARTx);
        quePush(ch); setTime(0, 0);
    }
    USART_ClearFlag(MODBUS_USARTx, USART_FLAG_TC);
}

void MODBUS_TIM_IRQHandler(void) {
    if (TIM_GetITStatus(MODBUS_TIM, TIM_IT_Update) != RESET)
        incTime(0);
    triggerModbusCheck();
    TIM_ClearITPendingBit(MODBUS_TIM, TIM_FLAG_Update);
}

void LED_TIM_IRQHandler(void) {
    if (TIM_GetITStatus(LED_TIM, TIM_IT_Update) != RESET)
        incTime(1);
    triggerLEDCheck();
    TIM_ClearITPendingBit(LED_TIM, TIM_FLAG_Update);
}

/**
  * @brief  This function handles PPP interrupt request.
  * @param  None
  * @retval None
  */
/*void PPP_IRQHandler(void)
{
}*/

/**
  * @}
  */ 


/******************* (C) COPYRIGHT 2011 STMicroelectronics *****END OF FILE****/
