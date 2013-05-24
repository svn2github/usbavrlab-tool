/************************************************************************************************
 * Project: USB AVR-Lab
 * Author: Christian Ulrich
 * Contact: christian at ullihome dot de
 *
 * Creation Date: 2007-09-24
 * Copyright: (c) 2007 by Christian Ulrich
 * License: GPLv2 for private use
 *	        commercial use prohibited 
 *			based on i2c-tiny-usb by Till Harbaum	
 *
 * Changes:
 ***********************************************************************************************/

#include <stdio.h>
#include <ctype.h>
#include <string.h>

#include <avr/io.h>
#include <avr/interrupt.h>
#include <avr/pgmspace.h>
#include <avr/wdt.h>

#include <util/delay.h>
#include "led.h"
#include "main.h"
#include "i2c.h"

#define FUNC_START_BOOTLOADER		30
#define FUNC_GET_TYPE				0xFE

#ifndef USBASP_COMPATIBLE
led_t leds[] =  {{4,LED_OFF,LED_OFF},
                 {3,LED_OFF,LED_OFF},
                 {5,LED_OFF,LED_OFF}}; 
#else
led_t leds[] =  {{0,LED_OFF,LED_OFF},
                 {1,LED_OFF,LED_OFF},
                 {3,LED_OFF,LED_OFF}}; 
#endif
const uint8_t led_count = sizeof(leds)/sizeof(led_t);

#ifndef USBTINY
// use avrusb library
#include "usbdrv.h"
#include "oddebug.h"
#else
// use usbtiny library 
#include "usb.h"
#include "usbtiny.h"
typedef byte_t uchar;
#endif

#define CMD_ECHO       0
#define CMD_GET_FUNC   1
#define CMD_SET_DELAY  2
#define CMD_GET_STATUS 3

#define CMD_I2C_IO     4
#define CMD_I2C_BEGIN  1  // flag fo I2C_IO
#define CMD_I2C_END    2  // flag fo I2C_IO

/* linux kernel flags */
#define I2C_M_TEN		0x10	/* we have a ten bit chip address */
#define I2C_M_RD		0x01
#define I2C_M_NOSTART		0x4000
#define I2C_M_REV_DIR_ADDR	0x2000
#define I2C_M_IGNORE_NAK	0x1000
#define I2C_M_NO_RD_ACK		0x0800

/* To determine what functionality is present */
#define I2C_FUNC_I2C			0x00000001
#define I2C_FUNC_10BIT_ADDR		0x00000002
#define I2C_FUNC_PROTOCOL_MANGLING	0x00000004 /* I2C_M_{REV_DIR_ADDR,NOSTART,..} */
#define I2C_FUNC_SMBUS_HWPEC_CALC	0x00000008 /* SMBus 2.0 */
#define I2C_FUNC_SMBUS_READ_WORD_DATA_PEC  0x00000800 /* SMBus 2.0 */ 
#define I2C_FUNC_SMBUS_WRITE_WORD_DATA_PEC 0x00001000 /* SMBus 2.0 */ 
#define I2C_FUNC_SMBUS_PROC_CALL_PEC	0x00002000 /* SMBus 2.0 */
#define I2C_FUNC_SMBUS_BLOCK_PROC_CALL_PEC 0x00004000 /* SMBus 2.0 */
#define I2C_FUNC_SMBUS_BLOCK_PROC_CALL	0x00008000 /* SMBus 2.0 */
#define I2C_FUNC_SMBUS_QUICK		0x00010000 
#define I2C_FUNC_SMBUS_READ_BYTE	0x00020000 
#define I2C_FUNC_SMBUS_WRITE_BYTE	0x00040000 
#define I2C_FUNC_SMBUS_READ_BYTE_DATA	0x00080000 
#define I2C_FUNC_SMBUS_WRITE_BYTE_DATA	0x00100000 
#define I2C_FUNC_SMBUS_READ_WORD_DATA	0x00200000 
#define I2C_FUNC_SMBUS_WRITE_WORD_DATA	0x00400000 
#define I2C_FUNC_SMBUS_PROC_CALL	0x00800000 
#define I2C_FUNC_SMBUS_READ_BLOCK_DATA	0x01000000 
#define I2C_FUNC_SMBUS_WRITE_BLOCK_DATA 0x02000000 
#define I2C_FUNC_SMBUS_READ_I2C_BLOCK	0x04000000 /* I2C-like block xfer  */
#define I2C_FUNC_SMBUS_WRITE_I2C_BLOCK	0x08000000 /* w/ 1-byte reg. addr. */
#define I2C_FUNC_SMBUS_READ_I2C_BLOCK_2	 0x10000000 /* I2C-like block xfer  */
#define I2C_FUNC_SMBUS_WRITE_I2C_BLOCK_2 0x20000000 /* w/ 2-byte reg. addr. */
#define I2C_FUNC_SMBUS_READ_BLOCK_DATA_PEC  0x40000000 /* SMBus 2.0 */
#define I2C_FUNC_SMBUS_WRITE_BLOCK_DATA_PEC 0x80000000 /* SMBus 2.0 */

#define I2C_FUNC_SMBUS_BYTE I2C_FUNC_SMBUS_READ_BYTE | \
                            I2C_FUNC_SMBUS_WRITE_BYTE
#define I2C_FUNC_SMBUS_BYTE_DATA I2C_FUNC_SMBUS_READ_BYTE_DATA | \
                                 I2C_FUNC_SMBUS_WRITE_BYTE_DATA
#define I2C_FUNC_SMBUS_WORD_DATA I2C_FUNC_SMBUS_READ_WORD_DATA | \
                                 I2C_FUNC_SMBUS_WRITE_WORD_DATA
#define I2C_FUNC_SMBUS_BLOCK_DATA I2C_FUNC_SMBUS_READ_BLOCK_DATA | \
                                  I2C_FUNC_SMBUS_WRITE_BLOCK_DATA
#define I2C_FUNC_SMBUS_I2C_BLOCK I2C_FUNC_SMBUS_READ_I2C_BLOCK | \
                                  I2C_FUNC_SMBUS_WRITE_I2C_BLOCK

#define I2C_FUNC_SMBUS_EMUL I2C_FUNC_SMBUS_QUICK | \
                            I2C_FUNC_SMBUS_BYTE | \
                            I2C_FUNC_SMBUS_BYTE_DATA | \
                            I2C_FUNC_SMBUS_WORD_DATA | \
                            I2C_FUNC_SMBUS_PROC_CALL | \
                            I2C_FUNC_SMBUS_WRITE_BLOCK_DATA | \
                            I2C_FUNC_SMBUS_WRITE_BLOCK_DATA_PEC | \
                            I2C_FUNC_SMBUS_I2C_BLOCK

/* the currently support capability is quite limited */
const unsigned long func PROGMEM = I2C_FUNC_I2C | I2C_FUNC_SMBUS_EMUL;

/* ------------------------------------------------------------------------- */
static unsigned short expected = 0;
static unsigned char saved_cmd;

struct i2c_cmd {
  unsigned char type;
  unsigned char cmd;
  unsigned short flags;
  unsigned short addr;
  unsigned short len;  
};

#define STATUS_IDLE          0
#define STATUS_ADDRESS_ACK   1
#define STATUS_ADDRESS_NAK   2
#define STATUS_NAK   		 4

static uchar status = STATUS_IDLE;

static uchar i2c_do(struct i2c_cmd *cmd) {
  uchar addr;

  /* normal 7bit address */
  addr = ( cmd->addr << 1 );
  if (cmd->flags & I2C_M_RD )
    addr |= 1;

  if(cmd->cmd & CMD_I2C_BEGIN) 
    i2c_start();
  else 
    i2c_repstart();    

  // send DEVICE address
  if(!i2c_write_byte(addr)) {

    status = STATUS_ADDRESS_NAK;
    expected = 0;
    i2c_stop();
  } else {  
    status = STATUS_ADDRESS_ACK;
    expected = cmd->len;
    saved_cmd = cmd->cmd;

    /* check if transfer is already done (or failed) */
    if((cmd->cmd & CMD_I2C_END) && !expected) 
      i2c_stop();
  }

  /* more data to be expected? */
#ifndef USBTINY
  return(cmd->len?0xff:0x00);
#else
  return(((cmd->flags & I2C_M_RD) && cmd->len)?0xff:0x00);
#endif
}

#ifndef USBTINY
uchar	usbFunctionSetup(uchar data[8]) {
  static uchar replyBuf[4];
  usbMsgPtr = replyBuf;
#else
extern	byte_t	usb_setup ( byte_t data[8] )
{
  byte_t *replyBuf = data;
#endif

  switch(data[1]) {

  case FUNC_GET_TYPE:
    replyBuf[0] = 6;
    return 1;
    break;
  case FUNC_START_BOOTLOADER:
    cli();
	wdt_enable(WDTO_15MS);
    while(1);
	break;
  case CMD_ECHO: // echo (for transfer reliability testing)
    replyBuf[0] = data[2];
    replyBuf[1] = data[3];
    return 2;
    break;

  case CMD_GET_FUNC:
    memcpy_P(replyBuf, &func, sizeof(func));
    return sizeof(func);
    break;

  case CMD_SET_DELAY:
    /* The delay function used delays 4 system ticks per cycle. */
    /* This gives 1/3us at 12Mhz per cycle. The delay function is */
    /* called twice per clock edge and thus four times per full cycle. */ 
    /* Thus it is called one time per edge with the full delay */ 
    /* value and one time with the half one. Resulting in */
    /* 2 * n * 1/3 + 2 * 1/2 n * 1/3 = n us. */
    clock_delay = *(unsigned short*)(data+2);
    if(!clock_delay) clock_delay = 1;
    clock_delay2 = clock_delay/2;
    if(!clock_delay2) clock_delay2 = 1;

    break;

  case CMD_I2C_IO:
  case CMD_I2C_IO + CMD_I2C_BEGIN:
  case CMD_I2C_IO                 + CMD_I2C_END:
  case CMD_I2C_IO + CMD_I2C_BEGIN + CMD_I2C_END:
    // these are only allowed as class transfers

    leds[LED_RED].counter = 10; 
    leds[LED_RED].frequency = LED_FLASH;
    return i2c_do((struct i2c_cmd*)data);
    break;

  case CMD_GET_STATUS:
    replyBuf[0] = status;
    return 1;
    break;

  default:
    // must not happen ...
    break;
  }

  leds[LED_BLUE].counter = 10; 
  leds[LED_BLUE].frequency = LED_FLASH_NEG;
  return 0;  // reply len
}


/*---------------------------------------------------------------------------*/
/* usbFunctionRead                                                           */
/*---------------------------------------------------------------------------*/

#ifndef USBTINY
uchar usbFunctionRead( uchar *data, uchar len )
#else
extern	byte_t	usb_in ( byte_t* data, byte_t len )
#endif
{
  uchar i;

  if(status == STATUS_ADDRESS_ACK) {
    if(len > expected) {
      len = expected;
    }

    // consume bytes
    for(i=0;i<len;i++) {
      expected--;
      *data = i2c_read_byte(expected == 0);
      data++;
    }

    // end transfer on last byte
    if((saved_cmd & CMD_I2C_END) && !expected) 
      i2c_stop();

  } else {
    memset(data, 0, len);
  }
  return len;
}

/*---------------------------------------------------------------------------*/
/* usbFunctionWrite                                                          */
/*---------------------------------------------------------------------------*/

#ifndef USBTINY
uchar usbFunctionWrite( uchar *data, uchar len )
#else
extern	void	usb_out ( byte_t* data, byte_t len )
#endif
{
  uchar i, err=0;

  if(status == STATUS_ADDRESS_ACK) {
    if(len > expected) {
      len = expected;
    }

    // consume bytes
    for(i=0;i<len;i++) 
	  {
        expected--;
        if(!i2c_write_byte(*data++))
		  {
	        err = 1;
          } 
      }

    // end transfer on last byte
    if((saved_cmd & CMD_I2C_END) && !expected) 
      i2c_stop();

    if(err) {
      status += STATUS_NAK;

    }

  } else {
    memset(data, 0, len);
  }

#ifndef USBTINY
  return len;
#endif
}

/* ------------------------------------------------------------------------- */

int	main(void) 
{
  extern uchar usbNewDeviceAddr;
  uint8_t i;
  PORTC |= (1<<PC2);

//Reconnect USB
  usbDeviceDisconnect();  /* enforce re-enumeration, do this while interrupts are disabled! */
  i = 0;
  while(--i)
     _delay_ms(2);
  usbDeviceConnect();
  usbInit();
  sei();
  leds[LED_RED].frequency = LED_ON;
  LED_init();
  for (i=0;i<3;i++)
    TIMER_delay(250);
  leds[LED_RED].frequency = LED_OFF;
  LED_poll();
//  wdt_enable(WDTO_60MS);
  i2c_init();

  for(;;) 
    {
      if(usbNewDeviceAddr)
	    {
          leds[LED_BLUE].frequency = LED_ON;
		}
      wdt_reset();
      LED_poll();
      usbPoll();
    }
  return 0;
}

