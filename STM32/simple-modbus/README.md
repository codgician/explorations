# Simple Modbus

A naive implementation of Modbus protocol.

Communication is done via USART, timeout is implemented using Basic TIM. Tested with Modbus Poll.

## Board

- Board: `WildFire STM32F103-MINI`
- Chip: `STM32F103RC`

## Available function codes

- `03`: Read Multiple Holding Registers
- `06`: Write Single Holding Register
- `10`: Writing Multiple Holding Registers
