/************************************************************************************************
 * Project: USB AVR-ISP
 * Author: Christian Ulrich
 * Contact: christian at ullihome dot de
 *
 * Creation Date: 2007-03-22
 * Copyright: (c) 2007 by Christian Ulrich
 * License: GPLv2
 ***********************************************************************************************/

#ifndef __jtagiceprotocol_h_included__
#define __jtagiceprotocol_h_included__

#include <stdint.h>

#define  JTAGICE_STX	27
#define  JTAGICE_TOKEN	14

#define JTAGICE_CMD_SIGN_ON                         0x01
#define JTAGICE_CMD_SET_PARAMETER                   0x02
#define JTAGICE_CMD_GET_PARAMETER                   0x03



/* Success */
#define JTAGICE_STATUS_CMD_OK                       0x00

/* Warnings */
#define JTAGICE_STATUS_CMD_TOUT                     0x80
#define JTAGICE_STATUS_RDY_BSY_TOUT                 0x81
#define JTAGICE_STATUS_SET_PARAM_MISSING            0x82

/* Errors */
#define JTAGICE_STATUS_CMD_FAILED                   0xC0
#define JTAGICE_STATUS_CKSUM_ERROR                  0xC1
#define JTAGICE_STATUS_CMD_UNKNOWN                  0xC9
#define JTAGICE_ANSWER_CKSUM_ERROR                  0xB0

void JTAGICE_byterecived(uint8_t data);
void JTAGICE_init();

#define PGM_STATUS_PROGRAMMING 1
#define PGM_STATUS_IDLE        2    

#endif /* __jtagiceprotocol_h_included__ */
