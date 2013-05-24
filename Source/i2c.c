#include "i2c.h"
#include <util/delay.h>

unsigned short clock_delay = 10;
unsigned short clock_delay2 = 10/2;

#define HDEL	_delay_loop_2(clock_delay)
#define QDEL	_delay_loop_2(clock_delay2)

#define SDA_HI	I2C_PORT |= (1<<I2C_SDA)
#define SDA_LO	I2C_PORT &= ~(1<<I2C_SDA)

#define SDA_OUT	I2C_DDR |= (1<<I2C_SDA)
#define SDA_IN	I2C_DDR &= ~(1<<I2C_SDA)

#define SCL_HI	I2C_PORT |= (1<<I2C_SCL)
#define SCL_LO	I2C_PORT &= ~(1<<I2C_SCL)

#define SCL_TOGGLE { HDEL; SCL_HI; HDEL; SCL_LO; HDEL;}
#define START_CONDITION 	{         HDEL; SDA_LO; HDEL; SCL_LO; HDEL;}
#define STOP_CONDITION		{ SDA_LO; HDEL; SCL_HI; HDEL; SDA_HI; HDEL; }


void i2c_start(void)
{
  START_CONDITION;
}

void i2c_stop(void)
{
  STOP_CONDITION;
}

void i2c_repstart(void)
{
  SDA_HI;
  SCL_HI;
  QDEL;
  START_CONDITION;
}

void i2c_init(void)
{
  SDA_HI;
  SDA_OUT;
  I2C_DDR |= (1<<I2C_SCL);
  SCL_HI;
}

unsigned char i2c_write_byte(unsigned char byte)
{
  unsigned char i;
  for (i=8;i>0;i--)
    {
	  if (byte & (1<<(i-1)))
	    SDA_HI;
      else
	    SDA_LO;
	  SCL_TOGGLE;	 
	}
  SDA_HI;
  SDA_IN;
  SCL_HI;
  HDEL;
  byte = (I2C_PIN & (1<<I2C_SDA));
  HDEL;
  SCL_LO;
  HDEL;
  SDA_OUT;
  return (byte == 0);
}

unsigned char i2c_read_byte(unsigned char nak)
{
  HDEL;
  HDEL;
  unsigned char i, byte = 0;
  SDA_HI;
  SDA_IN;
  for (i=0;i<8;i++)
    {
	  HDEL;
	  SCL_HI;
	  byte <<= 1;
	  byte |= (I2C_PIN & (1<<I2C_SDA)) >> I2C_SDA;
	  HDEL;
	  SCL_LO;
	}
  HDEL;
  SDA_OUT;
  if (nak)
    SDA_HI;
  else
    SDA_LO;
  SCL_TOGGLE;
  SDA_HI;
  HDEL;
  return byte;
}
