#include "modbus.h"

#include <string.h>

const uint8_t myAddr = 0x01;    // Address of current device

uint8_t buff[BUF_SIZE], ret[BUF_SIZE];
uint16_t regs[REG_SIZE], retPt, buffPt;

void initRegs() {
    memset(regs, 0, sizeof(regs));
}

uint8_t highByte(uint16_t n) { return n >> 8; }
uint8_t lowByte(uint16_t n) { return n & (((uint16_t)1 << 8) - 1); }
uint16_t combineHalfBytes(uint8_t high, uint8_t low) { return (uint16_t)high << 8 | low; }

uint16_t CRC16(uint8_t *data, uint16_t len) {
    uint16_t crc = 0xffff, polyn = 0xa001;
    for (uint16_t i = 0; i < len; i++) {
        crc ^= data[i] ;
        for (uint8_t j = 0; j < 8; j++) {
            if ((crc & 0x0001) != 0) {
                crc >>= 1;
                crc ^= polyn;
            } else {
                crc >>= 1;
            }
        }
    }
    return crc;
}

int crcCheck(uint16_t len) {
    return CRC16(buff, len - 2) == combineHalfBytes(buff[len - 1], buff[len - 2]);
}

void modbusReadRegs(uint16_t base, uint16_t num) {
    ret[retPt++] = num << 1;
    for (uint16_t i = 0; i < num; i++) {
        ret[retPt++] = highByte(regs[base + i]);
        ret[retPt++] = lowByte(regs[base + i]);
    }
}

void modbusWriteReg(uint16_t addr, uint16_t data) {
    regs[addr] = data;
    ret[retPt++] = highByte(addr);
    ret[retPt++] = lowByte(addr);
    ret[retPt++] = highByte(data);
    ret[retPt++] = lowByte(data);
}

int8_t arrIndexOf(uint8_t * arr, uint16_t pos, uint16_t len) {
    if (pos >= len)
        return 0;
    return arr[pos];
}

void modbusWriteRegs(uint16_t base, uint16_t num, uint8_t * data, uint16_t len) {
    for (uint16_t i = 0; i < num; i++)
        regs[base + i] = combineHalfBytes(arrIndexOf(data, i << 1, len), arrIndexOf(data, i << 1 | 1, len));
    ret[retPt++] = highByte(base);
    ret[retPt++] = lowByte(base);
    ret[retPt++] = highByte(num);
    ret[retPt++] = lowByte(num);
}

void reply(void) {
    uint16_t crc = CRC16(ret, retPt);
    
    for (uint16_t i = 0; i < retPt; i++)
        usartSendByte(MODBUS_USARTx, ret[i]);
    usartSendByte(MODBUS_USARTx, lowByte(crc));
    usartSendByte(MODBUS_USARTx, highByte(crc));
}

/* Decode */
void modbusDecode(uint16_t len) {
    // printf("Enter decode\n");
    uint8_t addr = buff[0];
    
    if (addr != myAddr) {
        //printf("Not targetting me!\n");
        return;
    }
    
    uint16_t base = combineHalfBytes(buff[2], buff[3]);
    uint16_t num = combineHalfBytes(buff[4], buff[5]);
    
    retPt = 0;
    ret[retPt++] = addr;
    ret[retPt++] = buff[1];
    
    if (!crcCheck(len)) {
        // printf("CRC checksum failed!\n");
        ret[retPt++] = 0x07;
        reply();
        return;
    }
    
    switch (buff[1]) {
        case 0x03: { // Read successive registers
            modbusReadRegs(base, num);
            break;
        }
        case 0x06: { // Preset single register
            modbusWriteReg(base, num);
            break;
        }
        case 0x10: {  // Preset successive registers
            uint8_t dataLen = buff[6];
            uint8_t * head = buff + 7;
            modbusWriteRegs(base, num, head, dataLen);
            break;
        }
        default: {    // Unrecognized feature code
            ret[retPt++] = 0x01;
            break;
        }
    }
    
    reply();
}

void triggerModbusCheck(void) {
    if (!queIsEmpty() && getTime(0) >= TIMEOUT) {
        buffPt = queSize();
        for (uint16_t i = 0; i < buffPt; i++) {
            queFront(&buff[i]);
            quePop();
        }
        modbusDecode(buffPt);
    }
}
