/************************************************************************************************
 * Project: USB AVR-ISP
 * Author: Christian Ulrich
 * Contact: christian at ullihome dot de
 *
 * Creation Date: 2007-03-22
 * Copyright: (c) 2007 by Christian Ulrich
 * License: GPLv2 for private use
 *	        commercial use prohibited 
 ***********************************************************************************************/

#include <stdint.h>
#include <stdio.h>
#include <avr/pgmspace.h>
#include <avr/wdt.h>
#include <avr/eeprom.h>
#include "jtagiceprotocol.h"
#include "usb_uart.h"
#include "isp.h"
#include "timer.h"
#include "led.h"
#include "main.h"
#include "crc16.h"

#include <util/delay.h>

#define BUFFER_SIZE     320
#define RX_TIMEOUT      200 /* timeout in milliseconds */

#define JTAGICE_TXMSG_START 8

static uint8_t        Buffer[BUFFER_SIZE];
static uint32_t       rxPos;
static uint32_t  	  rxLen;
static uint32_t  	  txLen;

uint8_t    		JTAGICE_Status;
uint32_t   		JTAGICE_Address;

void JTAGICE_init()
{
  JTAGICE_Status = PGM_STATUS_IDLE;
}

void JTAGICE_save()
{
}
  

void JTAGICE_sendmessage()
{
  uint8_t *p = Buffer;
  uint16_t sum;
  uint16_t len;
  *p++ = JTAGICE_STX;
  *p++ = Buffer[1];  /* sequence number */
  *p++ = Buffer[2];  /* sequence number */
  *p++ = txLen;
  *p++ = txLen >> 8;
  *p++ = txLen >> 16;
  *p++ = txLen >> 24;
  *p++ = JTAGICE_TOKEN;

  len = txLen+9;
  txLen = len--;
  sum = calculateCRC16(Buffer,txLen);
  p = Buffer;
  while (txLen--)
    UART_putc(*p++);
  UART_putc(sum>>8);
  UART_putc(sum&0xFF);
}

void JTAGICE_processmessage()
{
  txLen = 2;

  switch (Buffer[JTAGICE_TXMSG_START])
    {
	  case JTAGICE_CMD_SIGN_ON:
	    {
          static PROGMEM char JTAGICE_initstring[] = {11, 'J', 'T', 'A', 'G', 'I', 'C', 'E', 'm', 'k', 'I','I', 0};
          uint8_t *p = &Buffer[JTAGICE_TXMSG_START + 2];
          strcpy_P((char*)p, JTAGICE_initstring);
          Buffer[JTAGICE_TXMSG_START + 1] = JTAGICE_STATUS_CMD_OK;
          txLen = 11;
		  break; 
        }
      case JTAGICE_CMD_SET_PARAMETER:
	    {
/*
          JTAGICE_Param.bytes[Buffer[JTAGICE_TXMSG_START + 1] & 0x1f] = Buffer[JTAGICE_TXMSG_START + 2];
		  if (Buffer[JTAGICE_TXMSG_START + 1] == JTAGICE_PARAM_SCK_DURATION)
		    {
              switch (Buffer[JTAGICE_TXMSG_START + 2])
                {
                case 0xfe:
				case 0x4c:
				case 0x3:ISP_Speed = 1;break;
                case 0x2:ISP_Speed = 2;break;
                case 0x1:ISP_Speed = 4;break;
                case 0x0:ISP_Speed = 6;
                }
            } 
*/
          Buffer[JTAGICE_TXMSG_START + 1] = JTAGICE_STATUS_CMD_OK;
		  break;
        }
      case JTAGICE_CMD_GET_PARAMETER:
	    {
/*
		  if (Buffer[JTAGICE_TXMSG_START + 1] == JTAGICE_PARAM_SCK_DURATION)
		    {
              switch (ISP_Speed)
                {
                case 1:JTAGICE_Param.s.sckDuration = 0xfe;break;
                case 2:
				case 3:JTAGICE_Param.s.sckDuration = 0x2;break;
                case 4:
				case 5:JTAGICE_Param.s.sckDuration = 0x1;break;
                case 6:
				case 7:JTAGICE_Param.s.sckDuration = 0x0;
                }
            } 
          else if (Buffer[JTAGICE_TXMSG_START + 1] == JTAGICE_PARAM_VTARGET)
		    {
#if defined(USBASP_COMPATIBLE)||defined(CCCB_COMPATIBLE)
  		  	  JTAGICE_Param.s.vTarget = 50;
#else
			  JTAGICE_Param.s.vTarget = ISP_getsupplyvoltage();
#endif
			}  
          Buffer[JTAGICE_TXMSG_START + 2] = JTAGICE_Param.bytes[Buffer[JTAGICE_TXMSG_START + 1] & 0x1f];
*/
          Buffer[JTAGICE_TXMSG_START + 1] = JTAGICE_STATUS_CMD_OK;
          txLen = 3;
		  break;
        } 
      default:
	    {
          Buffer[JTAGICE_TXMSG_START + 1] = JTAGICE_STATUS_CMD_FAILED;
		}
	   
	}
  JTAGICE_sendmessage();
}

void JTAGICE_byterecived(uint8_t data)
{
  if(rxPos == 0)
    { 
      if (data == JTAGICE_STX)
        Buffer[rxPos++] = data;
    }
  else
    {
      if(rxPos < BUFFER_SIZE)
	    {
          Buffer[rxPos++] = data;
          if (rxPos == 7)
		    {
			  rxLen = Buffer[6];
			  rxLen <<= 8;
			  rxLen += Buffer[5];
			  rxLen <<= 8;
			  rxLen += Buffer[4];
			  rxLen <<= 8;
			  rxLen += Buffer[3];
              rxLen += 9;
              if(rxLen > BUFFER_SIZE) //wrong length
			    {
                rxPos = 0;
				}
            }
		  else if (rxPos == 8)
		    {
              if(data != JTAGICE_TOKEN)
			    {
                rxPos = 0;
				}
            }
		  else if (rxPos > 7 && rxPos == rxLen)
		    { 
              uint16_t sum = calculateCRC16(Buffer,rxPos);
			  if(sum == (uint16_t)Buffer[rxPos]) //chksum ok
			    { 
                  JTAGICE_processmessage();
                }
			  else //chksum error
 			    {
//                  Buffer[JTAGICE_TXMSG_START] = JTAGICE_ANSWER_CKSUM_ERROR;
//                  Buffer[JTAGICE_TXMSG_START + 1] = JTAGICE_ANSWER_CKSUM_ERROR;
//			      txLen = 3;
				  txLen = rxLen;
                  JTAGICE_sendmessage();
                }
            }
        }
	  else
	    { 
          rxPos = 0;
        }
    }
}

