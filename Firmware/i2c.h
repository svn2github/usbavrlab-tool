#include <util/delay.h>
#include <avr/io.h>

#define I2C_PORT PORTB
#define I2C_PIN  PINB
#define I2C_DDR	 DDRB
#define I2C_SDA	 PB3
#define I2C_SCL  PB2
 
extern unsigned short clock_delay;
extern unsigned short clock_delay2;

void i2c_start(void);
void i2c_stop(void);
void i2c_repstart(void);
void i2c_init(void);
unsigned char i2c_write_byte(unsigned char b);
unsigned char i2c_read_byte(unsigned char nak);
