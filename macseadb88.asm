;;; 80 characters wide please ;;;;;;;;;;;;;;;;;;;;;;;;;; 8-space tabs please ;;;


;
;;;
;;;;;  Macintosh SE ADB Controller for PIC16F88
;;;
;


;;; Connections ;;;

;;;                                                              ;;;
;                               .--------.                         ;
;    ADB Drive <---        RA2 -|01 \/ 18|- RA1        <--- ST1    ;
;    ADB Sense --->        RA3 -|02    17|- RA0        <--- ST0    ;
;    ADB Sense --->        RA4 -|03    16|- RA7/CLKIN  <---        ;
;              --->  !MCLR/RA5 -|04    15|- RA6        --->        ;
;                       Ground -|05    14|- Supply                 ;
;              --->        RB0 -|06    13|- RB7        ----        ;
;              --->        RB1 -|07    12|- RB6        ----        ;
;         SCLK <---        RB2 -|08    11|- RB5        ----        ;
;          DIO <-->        RB3 -|09    10|- RB4        ---> INT    ;
;                               '--------'                         ;
;;;                                                              ;;;

;Pin connections to VIA
;PIC     Dir  VIA  Name  Description
;17/RA0  <--  PB4  ST0   State Input 0
;18/RA1  <--  PB5  ST1   State Input 1
;10/RB4  -->  PB3  INT   Interrupt
;09/RB3  <->  CB2  DIO   Data In/Out
;08/RB2  -->  CB1  SCLK  Shift Clock


;;; Assembler Directives ;;;

	list		P=PIC16F88, F=INHX32, ST=OFF, MM=OFF, R=DEC, X=ON
	#include	P16F88.inc
	__config	_CONFIG1, _INTRC_IO & _WDT_OFF & _PWRTE_ON & _MCLR_ON & _BOREN_OFF & _LVP_OFF & _CPD_OFF & _WRT_OFF & _DEBUG_OFF & _CCP1_RB0 & _CP_OFF
			;_INTRC_IO   internal RC osc, port I/O on RA7-RA6
			;_WDT_OFF    watchdog timer off
			;_PWRTE_ON   power-on timer on
			;_MCLR_ON    !MCLR function on !MCLR pin
			;_BOREN_OFF  brownout reset off
			;_LVP_OFF    low-voltage programming off
			;_CPD_OFF    EEPROM protection off
			;_WRT_OFF    write-protection off
			;_DEBUG_OFF  in-circuit debugging off
			;_CCP1_RB0   CCP1 function on RB0
			;_CP_OFF     code protection off
	__config	_CONFIG2, _FCMEN_OFF & _IESO_OFF
			;_FCMEN_OFF  fail-safe clock monitor off
			;_IESO_OFF   internal-external switchover off


;;; Macros ;;;

DNOP	macro			;Double-NOP
	goto	$+1
	endm

RLSADB	macro			;Release ADB
	movlw	0xFB
	movwf	PORTA
	endm

PULLADB	macro			;Pull ADB low
	movlw	0xFF
	movwf	PORTA
	endm


;;; Constants ;;;

CDOWNAI	equ	0x73	;Value that CDOWNA is initialized to
CDOWNBI	equ	0x54	;Value that CDOWNB is initialized to

INT_PIN	equ	RB4	;INT pin
DIO_PIN	equ	RB3	;DIO pin
SCK_PIN	equ	RB2	;SCLK pin

			;FLAGS:
F_ERROR	equ	5	;Error
F_SRQ	equ	4	;SRQ
F_LCMD1	equ	3	;Last ADB command
F_LCMD0	equ	2	; "
F_LST1	equ	1	;Last ST1-0
F_LST0	equ	0	; "

			;FLAGS2:
F2_CMDL	equ	3	;Command byte loaded for transmission
F2_UNK2	equ	2	;TODO ?
F2_UNK1	equ	1	;TODO ?
F2_UNK0	equ	0	;TODO ?


;;; Variable Storage ;;;

	cblock	0x28
	
	READA	;State of the ST1-0 last time they were read in the main loop
	LASTA	;State of READA last time... TODO ??
	COMMAND	;Command byte to transmit
	XMITLEN	;Number of bytes to transmit over ADB
	FLENGTH	;ADB frame length
	VIADATA	;Data received from/to be sent to VIA
	GPR0E	;(not used)
	FLAGS	;Flags
	DELAY	;Delay counter
	COUNT	;General-purpose counter
	GPR12	;(not used)
	LATCHB	;Intended state of PORTB pins
	GPR14
	CDOWNB	;Countdown B TODO
	CDOWNA	;Countdown A TODO
	FLAGS2	;More flags
	
	endc

	cblock	0xF8
	
	BUFFER0
	BUFFER1
	BUFFER2
	BUFFER3
	BUFFER4
	BUFFER5
	BUFFER6
	BUFFER7
	
	endc


;;; Notes ;;;

;RB0 and 1 are btfss'd each once... but not connected to anything?


;;; Initialization ;;;

	org	0x0

Init
	clrf	STATUS		;TODO is necessary?

	banksel	OSCCON		;Internal RC oscillator to 2 MHz, match PIC1654S
	movlw	B'01010000'	; which divided ~4 MHz oscillator by 8, thus
	movwf	OSCCON		; instruction clock is 500 kHz (1 cycle = 2 us)

	banksel	ANSEL		;Digital I/O on all pins, analog disabled
	clrf	ANSEL

	clrf	STATUS		;All directly-accessed registers are in bank 0

	clrf	PCLATH		;TODO is necessary?

	RLSADB			;Release ADB
	clrf	PORTB		;When PORTB pins are driven, drive them low
	movlw	0xFB		;Writes to PORTA drive RA2 and no other pins
	tris	PORTA		; "
	movlw	0xFF		;No pins on PORTB driven at first
	tris	PORTB		; "

	clrf	READA		;Clear/init GPRs
	clrf	COMMAND		; "
	clrf	XMITLEN		; "
	clrf	FLENGTH		; "
	clrf	VIADATA		; "
	clrf	FLAGS		; "
	clrf	DELAY		; "
	clrf	COUNT		; "
	clrf	GPR14		; "
	clrf	CDOWNB		; "
	clrf	CDOWNA		; "
	clrf	FLAGS2		; "
	movlw	0x03		;Act like last read of state pins was S3 (null
	movwf	LASTA		; state)
	movlw	0xFF		;Initialize PORTB latches to all high
	movwf	LATCHB		; "
	;fall through


;;; Mainline ;;;

Main
	movf	PORTA,W		;Read state pins; if they are the same as last
	xorwf	LASTA,W		; time, decrement the countdowns TODO
	andlw	0x03		; "
	btfsc	STATUS,Z	; "
	goto	DecCountdowns	; "
	movf	PORTA,W		;Read state pins and save their value for later
	andlw	0x03		; use
	movwf	READA		; "
	movlw	CDOWNAI		;State pins have changed, so reinitialize the
	movwf	CDOWNA		; countdowns
	movlw	CDOWNBI		; "
	movwf	CDOWNB		; "
	movf	LATCHB,W	;Deassert INT and let DIO and SCLK float
	iorlw	0x1C		; "
	movwf	LATCHB		; "
	movf	FLAGS,W		;Load state pins into bits 1 and 0 of FLAGS
	andlw	0xFC		; "
	iorwf	READA,W		; "
	movwf	FLAGS		; "
	andlw	0x0f			; "
	addwf	PCL,F			;Jump table based on low 4 bits of FLAGS:
	goto	lb_060			;0000 = receive command or throwaway byte from VIA?
	goto	SendByteAndError	;0001 = send byte (and error status?) from data buffer to VIA?
	goto	SendByteAndSrq		;0010 = send byte (and SRQ status?) from data buffer to VIA?
	goto	UpdatePortB		;0011
	goto	lb_060			;0100 = receive command or throwaway byte from VIA?
	goto	SendByteAndError	;0101 = send byte (and error status?) from data buffer to VIA?
	goto	SendByteAndSrq		;0110 = send byte (and SRQ status?) from data buffer to VIA?
	goto	UpdatePortB		;0111
	goto	SendAdbCommand		;1000 = send command and data in buffer over ADB
	goto	ReceiveFromVia		;1001 = receive byte from VIA into data buffer
	goto	ReceiveFromVia		;1010 = receive byte from VIA into data buffer
	goto	SendAdbCommand		;1011 = send command and data in buffer over ADB
	goto	lb_060			;1100 = receive command or throwaway byte from VIA?
	goto	lb_053			;1101 = send command and receive data into buffer over ADB?
	goto	SendByteAndSrq		;1110 = send byte (and SRQ status?) from data buffer to VIA?
	btfss	FLAGS2,F2_CMDL		;1111
	goto	UpdatePortB
	movf	COMMAND,W
	movwf	GPR14
	bsf	FLAGS2,F2_UNK0

UpdatePortB
	movf	LATCHB,W	;Update PORTB with the latched values
	tris	PORTB		; "
	;fall through

UpdateLastA
	RLSADB			;Release ADB
	movf	READA,W		;Update LASTA with the most recent read of PORTA
	movwf	LASTA		; "
	goto	Main		;Return to main

DecCountdowns
	decfsz	CDOWNA,F	;Decrement CDOWNA, skip ahead if it's nonzero
	goto	DecCou1		; "
	movlw	CDOWNAI		;Reset CDOWNA
	movwf	CDOWNA		; "
	decfsz	CDOWNB,F	;Decrement CDOWNB, skip ahead if it's nonzero
	goto	DecCou0		; "
	movlw	CDOWNBI		;Reset CDOWNB
	movwf	CDOWNB		; "
	call	ViaSend		;Re-send the last byte we sent to the VIA? TODO
	goto	Main		; and return to main
DecCou0	btfsc	CDOWNB,0	;Every other time CDOWNA hits zero, proceed;
	goto	DecCou1		; else skip to the same place as if it didn't
	movf	READA,W		;If we're in any state except for the S3/NULL
	xorlw	0x03		; state, return to main
	btfss	STATUS,Z	; "
	goto	Main		; "
	btfsc	FLAGS2,F2_UNK1
	goto	DecCou2
	btfss	FLAGS2,F2_UNK0
	goto	Main
	movf	FLAGS,W		;Clear the error and SRQ flags
	andlw	0x0F		; "
	movwf	FLAGS		; "
	movf	GPR14,W
	movwf	COMMAND
	goto	lb_063
DecCou1	movf	READA,W		;If we're in any state except for the S3/NULL
	xorlw	0x03		; state, return to main
	btfss	STATUS,Z	; "
	goto	Main		; "
	btfss	FLAGS2,F2_UNK1
	goto	Main
DecCou2	btfsc	FLAGS2,F2_UNK2
	goto	Main
	movf	GPR14,W
	movwf	VIADATA
	call	ViaSend
	bsf	FLAGS2,F2_UNK2
	goto	Main


;;; Subprograms ;;;

;Clock the byte in VIADATA out, MSB first, to the VIA over SCLK/DIO.
ViaSend
	movlw	0x08		;Initialize count to send 8 bits
	movwf	COUNT		; "
	movf	LATCHB,W	;Lower SCLK
ViaSnd0	andlw	~(1 << SCK_PIN)	; "
	tris	PORTB		; "
	iorlw	1 << DIO_PIN	;Copy MSB of VIADATA into DIO pin
	btfss	VIADATA,7	; "
	andlw	~(1 << DIO_PIN)	; "
	tris	PORTB		; "
	rlf	VIADATA,F	;Rotate next bit into position
	iorlw	1 << SCK_PIN	;Raise SCLK
	tris	PORTB		; "
	decfsz	COUNT,F		;Decrement count and loop to send next bit
	goto	ViaSnd0		; "
	rlf	VIADATA,F	;Rotate VIADATA to its original position
	iorlw	0x0C		;Raise both SCLK and DIO
	tris	PORTB		; "
	retlw	0x00		;Return with 0 in W

;Clock a byte from the VIA over SCLK/DIO, MSB first, into VIADATA.
ViaReceive
	movlw	0x08		;Initialize count to receive 8 bits
	movwf	COUNT		; "
	movf	LATCHB,W	;Lower SCLK
ViaRec0	andlw	~(1 << SCK_PIN)	; "
	tris	PORTB		; "
	bcf	STATUS,C	;Clear carry bit to start, this is what will be
	nop			; rotated into VIADATA
	iorlw	0x0C		;Raise SCLK and DIO (the latter so it can be
	tris	PORTB		; driven by VIA)
	btfsc	PORTB,DIO_PIN	;If DIO is high, set carry
	bsf	STATUS,C	; "
	rlf	VIADATA,F	;Rotate carry into VIADATA
	decfsz	COUNT,F		;Decrement count and loop to receive next bit
	goto	ViaRec0		; "
	retlw	0x00		;Return with 0 in W

;Clear the data buffer to 0xFF.
ClearBuffer
	movlw	0xF8		;Start FSR at start of data buffer
	movwf	FSR		; "
	movlw	0xFF		;Load 0xFF into W for loading into INDF
ClearB0	movwf	INDF		;Load 0xFF into next register
	incfsz	FSR,F		;Increment FSR and loop until it reaches the end
	goto	ClearB0		; of the data buffer
	DNOP			;Double NOP for some reason (TODO why?)
	retlw	0x00		;Return with 0 in W

;Delay 6 + 3*W cycles (including call/return).
Delay6Plus3W
	nop			;Delay one cycle
	;fall through

;Delay 5 + 3*W cycles (including call/return).
Delay5Plus3W
	nop			;Delay one cycle
	;fall through

;Delay 4 + 3*W cycles (including call/return).
Delay4Plus3W
	movwf	DELAY		;Delay 3*W cycles
	decfsz	DELAY,F		; "
	goto	$-1		; "
	;fall through

;Delay 4 cycles (including call/return).
Delay4
	retlw	0x00		;Return with 0 in W

;Send start bit, followed by XMITLEN bytes starting from FSR, over ADB.
AdbSendStart
	PULLADB			;Pull ADB low to send 1 start bit
	movlw	0x03		;Delay 15 cycles
	call	Delay5Plus3W	; "
	RLSADB			;Release ADB
	movlw	0x07		;Delay 26 cycles
	call	Delay4Plus3W	; "
	;fall through

;Send XMITLEN bytes starting from FSR over ADB.
AdbSend
	movlw	0x08		;Initialize counter to send 8 bits
	movwf	COUNT		; "
AdbSnd0	PULLADB			;Pull ADB low to send data bit
	movlw	0x02		;Delay 11 cycles
	call	Delay4Plus3W	; "
	rlf	INDF,F		;Rotate next bit to send into position
	btfss	STATUS,C	;If it's zero, delay longer before releasing and
	goto	AdbSnd4		; shorter after
	RLSADB			;Release ADB
	movlw	0x02		;Delay 13 cycles
	call	Delay6Plus3W	; "
AdbSnd1	RLSADB			;Release ADB
	decfsz	COUNT,F		;Decrement bit counter and loop again if there
	goto	AdbSnd5		; are more bits to send
	decfsz	XMITLEN,F	;Decrement byte counter and continue if there
	goto	AdbSnd2		; are more bytes to send, skip ahead if not
	goto	AdbSnd3		; "
AdbSnd2	rlf	INDF,F		;Rotate INDF so it ends up where it started
	bsf	COUNT,3		;Set bit counter to 8 again
	incf	FSR,F		;Increment FSR to next byte to send
	call	Delay4		;Delay 4 cycles
	goto	AdbSnd0		;Loop to send first bit of byte
AdbSnd3	movlw	0x01		;Delay 8 cycles
	call	Delay4Plus3W	; "
	PULLADB			;Pull ADB low to send 0 stop bit
	movlw	0x08		;Delay 29 cycles
	call	Delay4Plus3W	; "
	RLSADB			;Release ADB
	rlf	INDF,F		;Rotate INDF so it ends up where it started
	retlw	0x00		;Return with 0 in W
AdbSnd4	movlw	0x02		;Delay 11 cycles
	call	Delay4Plus3W	; "
	goto	AdbSnd1		;Return to release ADB after sending 0 bit
AdbSnd5	movlw	0x01		;Delay 9 cycles
	call	Delay5Plus3W	; "
	goto	AdbSnd0		;Return to send next bit

;Receive a bit over the ADB pin - wait for the pin to go high and then low
; again, return 1 or 0 in W accordingly.  If the pin stays high, fall through to
; end the transmission; note that in this case the stack is not popped, but this
; doesn't matter as the PIC16F88's stack is circular and it does not reset on
; overflow.
AdbRecvBit
	btfsc	PORTA,RA3	;07 cycles, 14 us
	goto	ARB14us		;08 cycles, 16 us
	btfsc	PORTA,RA3	;09 cycles, 18 us
	goto	ARB18us		;10 cycles, 20 us
	btfsc	PORTA,RA3	;11 cycles, 22 us
	goto	ARB22us		;12 cycles, 24 us
	btfsc	PORTA,RA3	;13 cycles, 26 us
	goto	ARB26us		;14 cycles, 28 us
	btfsc	PORTA,RA3	;15 cycles, 30 us
	goto	ARB30us		;16 cycles, 32 us
	btfsc	PORTA,RA3	;17 cycles, 34 us
	goto	ARB34us		;18 cycles, 36 us
	btfsc	PORTA,RA3	;19 cycles, 38 us
	goto	ARB38us		;20 cycles, 40 us
	btfsc	PORTA,RA3	;21 cycles, 42 us
	goto	ARB42us		;22 cycles, 44 us
	btfsc	PORTA,RA3	;23 cycles, 46 us
	goto	ARB46us		;24 cycles, 48 us
	btfsc	PORTA,RA3	;25 cycles, 50 us
	goto	ARB50us		;26 cycles, 52 us
	btfsc	PORTA,RA3	;27 cycles, 54 us
	goto	ARB54us		;28 cycles, 56 us
	btfsc	PORTA,RA3	;29 cycles, 58 us
	goto	ARB58us		;30 cycles, 60 us
	btfsc	PORTA,RA3	;31 cycles, 62 us
	goto	ARB62us		;32 cycles, 64 us
	btfsc	PORTA,RA3	;33 cycles, 66 us
	goto	ARB66us		;34 cycles, 68 us
	btfsc	PORTA,RA3	;35 cycles, 70 us
	goto	ARB70us		;36 cycles, 72 us
	btfsc	PORTA,RA3	;37 cycles, 74 us
	goto	ARB74us		;38 cycles, 76 us
	btfsc	PORTA,RA3	;39 cycles, 78 us
	goto	ARB78us		;40 cycles, 80 us
	btfsc	PORTA,RA3	;41 cycles, 82 us
	goto	ARB82us		;42 cycles, 84 us
	btfsc	PORTA,RA3	;43 cycles, 86 us
	goto	ARB86us		;44 cycles, 88 us
	btfsc	PORTA,RA3	;45 cycles, 90 us
	goto	ARB90us		;46 cycles, 92 us
	bsf	FLAGS,F_ERROR	;ADB was not released in time, set error flag
	retlw	0x00		; and return
ARB90us	btfss	PORTA,RA3	;096 --- --- --- --- --- --- --- --- --- --- ---
	retlw	0x00		;--- --- --- --- --- --- --- --- (94% - 94% = 0)
ARB86us	btfss	PORTA,RA3	;100 092 --- --- --- --- --- --- --- --- --- ---
	retlw	0x00		;--- --- --- --- --- --- --- --- (90% - 93% = 0)
ARB82us	btfss	PORTA,RA3	;104 096 088 --- --- --- --- --- --- --- --- ---
	retlw	0x00		;--- --- --- --- --- --- --- --- (87% - 93% = 0)
ARB78us	btfss	PORTA,RA3	;108 100 092 084 --- --- --- --- --- --- --- ---
	retlw	0x00		;--- --- --- --- --- --- --- --- (83% - 93% = 0)
ARB74us	btfss	PORTA,RA3	;112 104 096 088 080 --- --- --- --- --- --- ---
	retlw	0x00		;--- --- --- --- --- --- --- --- (80% - 92% = 0)
ARB70us	btfss	PORTA,RA3	;116 108 100 092 084 076 --- --- --- --- --- ---
	retlw	0x00		;--- --- --- --- --- --- --- --- (78% - 92% = 0)
ARB66us	btfss	PORTA,RA3	;120 112 104 096 088 080 072 --- --- --- --- ---
	retlw	0x00		;--- --- --- --- --- --- --- --- (75% - 92% = 0)
ARB62us	btfss	PORTA,RA3	;124 116 108 100 092 084 076 068 --- --- --- ---
	retlw	0x00		;--- --- --- --- --- --- --- --- (73% - 91% = 0)
ARB58us	btfss	PORTA,RA3	;128 120 112 104 096 088 080 072 064 --- --- ---
	retlw	0x00		;--- --- --- --- --- --- --- --- (70% - 91% = 0)
ARB54us	btfss	PORTA,RA3	;132 124 116 108 100 092 084 076 068 060 --- ---
	retlw	0x00		;--- --- --- --- --- --- --- --- (68% - 90% = 0)
ARB50us	btfss	PORTA,RA3	;136 128 120 112 104 096 088 080 072 064 056 ---
	retlw	0x00		;--- --- --- --- --- --- --- --- (66% - 89% = 0)
ARB46us	btfss	PORTA,RA3	;140 132 124 116 108 100 092 084 076 068 060 052
	retlw	0x00		;--- --- --- --- --- --- --- --- (64% - 88% = 0)
ARB42us	btfss	PORTA,RA3	;144 136 128 120 112 104 096 088 080 072 064 056
	retlw	0x00		;048 --- --- --- --- --- --- --- (62% - 88% = 0)
ARB38us	btfss	PORTA,RA3	;148 140 132 124 116 108 100 092 084 076 068 060
	retlw	0x00		;052 044 --- --- --- --- --- --- (61% - 86% = 0)
ARB34us	btfss	PORTA,RA3	;152 144 136 128 120 112 104 096 088 080 072 064
	retlw	0x00		;056 048 040 --- --- --- --- --- (59% - 85% = 0)
ARB30us	btfss	PORTA,RA3	;156 148 140 132 124 116 108 100 092 084 076 068
	retlw	0x00		;060 052 044 036 --- --- --- --- (58% - 83% = 0)
ARB26us	btfss	PORTA,RA3	;160 152 144 136 128 120 112 104 096 088 080 072
	retlw	0x00		;064 056 048 040 032 --- --- --- (56% - 81% = 0)
ARB22us	btfss	PORTA,RA3	;164 156 148 140 132 124 116 108 100 092 084 076
	retlw	0x00		;068 060 052 044 036 028 --- --- (55% - 79% = 0)
ARB18us	btfss	PORTA,RA3	;168 160 152 144 136 128 120 112 104 096 088 080
	retlw	0x00		;072 064 056 048 040 032 024 --- (54% - 75% = 0)
ARB14us	btfss	PORTA,RA3	;172 164 156 148 140 132 124 116 108 100 092 084
	retlw	0x00		;076 068 060 052 044 036 028 020 (52% - 70% = 0)
	btfss	PORTA,RA3	;176 168 160 152 144 136 128 120 112 104 096 088
	retlw	0x00		;080 072 064 056 048 040 032 024 (51% - 58% = 0)
	btfss	PORTA,RA3	;180 172 164 156 148 140 132 124 116 108 100 092
	retlw	0x00		;084 076 068 060 052 044 036 028 (50% - 50% = 0)
	btfss	PORTA,RA3	;184 176 168 160 152 144 136 128 120 112 104 096
	retlw	0x01		;088 080 072 064 056 048 040 032 (44% - 49% = 1)
	btfss	PORTA,RA3	;188 180 172 164 156 148 140 132 124 116 108 100
	retlw	0x01		;092 084 076 068 060 052 044 036 (39% - 48% = 1)
	btfss	PORTA,RA3	;192 184 176 168 160 152 144 136 128 120 112 104
	retlw	0x01		;096 088 080 072 064 056 048 040 (35% - 47% = 1)
	btfss	PORTA,RA3	;196 188 180 172 164 156 148 140 132 124 116 108
	retlw	0x01		;100 092 084 076 068 060 052 044 (32% - 46% = 1)
	btfss	PORTA,RA3	;200 192 184 176 168 160 152 144 136 128 120 112
	retlw	0x01		;104 096 088 080 072 064 056 048 (29% - 45% = 1)
	btfss	PORTA,RA3	;204 196 188 180 172 164 156 148 140 132 124 116
	retlw	0x01		;108 100 092 084 076 068 060 052 (27% - 44% = 1)
	btfss	PORTA,RA3	;208 200 192 184 176 168 160 152 144 136 128 120
	retlw	0x01		;112 104 096 088 080 072 064 056 (25% - 43% = 1)
	btfss	PORTA,RA3	;212 204 196 188 180 172 164 156 148 140 132 124
	retlw	0x01		;116 108 100 092 084 076 068 060 (23% - 42% = 1)
	btfss	PORTA,RA3	;216 208 200 192 184 176 168 160 152 144 136 128
	retlw	0x01		;120 112 104 096 088 080 072 064 (22% - 42% = 1)
	btfss	PORTA,RA3	;220 212 204 196 188 180 172 164 156 148 140 132
	retlw	0x01		;124 116 108 100 092 084 076 068 (21% - 41% = 1)
	btfss	PORTA,RA3	;224 216 208 200 192 184 176 168 160 152 144 136
	retlw	0x01		;128 120 112 104 096 088 080 072 (19% - 40% = 1)
	btfss	PORTA,RA3	;228 220 212 204 196 188 180 172 164 156 148 140
	retlw	0x01		;132 124 116 108 100 092 084 076 (18% - 39% = 1)
	btfss	PORTA,RA3	;232 224 216 208 200 192 184 176 168 160 152 144
	retlw	0x01		;136 128 120 112 104 096 088 080 (18% - 39% = 1)
	btfss	PORTA,RA3	;236 228 220 212 204 196 188 180 172 164 156 148
	retlw	0x01		;140 132 124 116 108 100 092 084 (17% - 38% = 1)
	;fall through		;If line stayed high longer than a bit cell...


;;; Mainline ;;;

AdbRecvDone
	movf	FSR,W		;Receive complete, ignore the last (stop) bit,
	andlw	0x0F		; derive length from FSR; XOR gives a length of
	xorlw	0x08		; 8 if FSR wrapped (as with an 8-byte frame)
AdbRDn0	movwf	FLENGTH		;Move length of received frame into FLENGTH
	xorlw	0x01		;If length was 1 (ADB frames must be 2 to 8
	btfsc	STATUS,Z	; bytes), set the error flag
	bsf	FLAGS,F_ERROR	; "
	movlw	0xF8		;Point FSR to the beginning of the data buffer
	movwf	FSR		; again
	btfsc	FLAGS,F_LST1
	goto	lb_071
	goto	SendByteAndError

ReceiveFromVia
	call	ViaReceive	;Receive byte from VIA
	movf	VIADATA,W	;Place received byte into data buffer, increment
	movwf	INDF		; pointer and frame length
	incf	FSR,F		; "
	incf	FLENGTH,F	; "
	movlw	0xFF		;Clear VIA buffer to 0xFF
	movwf	VIADATA		; "
	goto	UpdateLastA	;Return to main

lb_053
	btfss	FLAGS2,F2_CMDL
	goto	SendByteAndError
	movf	COMMAND,W
	movwf	GPR14
	bsf	FLAGS2,F2_UNK0
	goto	lb_063

SendByteAndError
	btfsc	FLAGS,F_ERROR	;We're in S1, so assert the INT pin if the error
	bcf	LATCHB,INT_PIN	; flag is up
	bcf	FLAGS,F_ERROR	;Clear error flag
	movlw	1 << F2_UNK0		;TODO only place where FLAGS2 flags are reset
	movwf	FLAGS2
	goto	SBASrq0		;Continue further down with sending next byte

SendByteAndSrq
	btfsc	FLAGS,F_SRQ	;We're in S2, so assert the INT pin if the SRQ
	bcf	LATCHB,INT_PIN	; flag is up
	bcf	FLAGS,F_SRQ	;Clear SRQ flag
SBASrq0	movf	FLENGTH,F	;If Mac is trying to read past the end of the
	btfsc	STATUS,Z	; data buffer, assert the INT pin; otherwise,
	bcf	LATCHB,INT_PIN	; decrement FLENGTH  TODO ??
	btfss	STATUS,Z	; "
	decf	FLENGTH,F	; "
	movf	INDF,W		;Send the next byte from the buffer to the VIA
	movwf	VIADATA		; and increment the pointer
	call	ViaSend		; "
	incf	FSR,F		; "
	goto	UpdateLastA	;Return to main

SendAdbCommand
	bcf	FLAGS2,F2_CMDL	;Clear command-loaded flag, we're about to send
	PULLADB			;Pull ADB low
	nop			;Delay 360 cycles
	movlw	0x76		; "
	call	Delay4Plus3W	; "
	movlw	COMMAND		;Point FSR to COMMAND, which contains command
	movwf	FSR		; byte to transmit
	movlw	0x01		;Set XMITLEN to send one byte
	movwf	XMITLEN		; "
	RLSADB			;Release ADB
	movlw	0x06		;Delay 24 cycles
	call	Delay4Plus3W	; "
	nop			; "
	call	AdbSend		;Send the command byte
	movlw	0x1A		;Repeat this loop 26 times
	movwf	DELAY		; "
SAdCmd0	btfsc	PORTA,RA3	;If the ADB pin has been released, exit the loop
	goto	SAdCmd1		; "
	bsf	FLAGS,F_SRQ	;ADB pin is still low, a device is requesting
	decfsz	DELAY,F		; service; set the SRQ flag and loop while we
	goto	SAdCmd0		; wait for it to release the ADB pin
SAdCmd1	movlw	0x11		;Delay 85 cycles if SRQ flag was set, 58 cycles
	btfsc	FLAGS,F_SRQ	; if not
	movlw	0x1A		; "
	call	Delay4Plus3W	; "
	movlw	0xF8		;Point FSR to the start of the data buffer
	movwf	FSR		; "
	movf	FLENGTH,W	;Send FLENGTH bytes over ADB
	movwf	XMITLEN		; "
	call	AdbSendStart	; "
	call	ClearBuffer	;Clear the data buffer just sent
	btfss	FLAGS,F_LST0
	goto	lb_060
	clrf	FLAGS
	movlw	0xF8		;Point FSR to the start of the data buffer
	movwf	FSR		; "
	goto	UpdatePortB	;Return to main

lb_060
	btfss	FLAGS2,F2_UNK1
	goto	lb_061
	bcf	LATCHB,INT_PIN		;Assert INT pin
	call	ViaReceive
	bsf	FLAGS2,F2_UNK2
	goto	UpdateLastA
lb_061	call	ViaReceive		;Read a byte from the VIA
	clrf	FLENGTH			;Clear ADB frame length
	movf	VIADATA,W		;Copy the byte from the VIA to COMMAND
	movwf	COMMAND			; "
	andlw	0x0c			;Move the two bits that determine the ADB command to FLAGS, zero all others
	movwf	FLAGS			; "
	movlw	0xff			;Clear VIADATA to 0xFF
	movwf	VIADATA			; "
	bsf	FLAGS2,F2_CMDL		;Set command-byte-loaded flag
	btfss	FLAGS,F_LCMD1		;If the command doesn't send or receive a payload (it's not a listen or a talk),
	goto	lb_062			; skip ahead to send it immediately
	movlw	0xf8			;If the command has a payload, point FSR to beginning of data buffer and return to
	movwf	FSR			; main loop
	goto	UpdateLastA		; "
lb_062	bsf	FLAGS,F_ERROR		;Set error flag (TODO why? sending a flush or sendreset command isn't an error)

lb_063
	bcf	FLAGS2,F2_CMDL		;Clear command-byte-loaded flag since we're about to send it
	PULLADB				;Pull ADB low
	call	ClearBuffer		;Clear data buffer
	movlw	0x68			;Delay 317 cycles
	call	Delay4Plus3W		; "
	movf	COMMAND,W		;If low four bits of command byte are 0x0, i.e. a SendReset command, continue to
	andlw	0x0f			; send a reset pulse and reset the firmware, else skip ahead to send the command
	btfss	STATUS,Z		; "
	goto	lb_064			; "
	PULLADB				;Pull ADB low to send reset pulse
	movlw	0xff			;Delay 1460 cycles
	call	Delay4Plus3W		; "
	movlw	0xe4			; "
	call	Delay5Plus3W		; "
	RLSADB				;Release ADB
	goto	lb_072			;Reset firmware
lb_064	RLSADB				;Release ADB
	movlw	0x05			;Delay 20 cycles
	call	Delay4Plus3W		; "
	movlw	COMMAND			;Point FSR to COMMAND, which contains command byte, and send it over ADB
	movwf	FSR			; "
	movlw	0x01			; "
	movwf	XMITLEN			; "
	call	AdbSend			; "
	movlw	0x1a			;Repeat this loop 26 times
	movwf	DELAY			; "
lb_065	btfsc	PORTA,RA3		;If the ADB pin has been released, exit the loop
	goto	lb_066			; "
	bsf	FLAGS,F_SRQ		;Set the SRQ flag
	decfsz	DELAY,F			;Decrement loop counter, loop again while we wait for the device requesting service
	goto	lb_065			; to release the ADB pin
lb_066	btfss	FLAGS,F_LCMD1		;If the command sent was a flush, skip ahead
	goto	lb_070			; "
	bcf	STATUS,C		;Clear carry bit so we don't rotate a one in when we shift in the byte from ADB
	movlw	0xf8			;Point FSR to the data buffer
	movwf	FSR			; "
	movlw	0x1a			;Repeat this loop 26 times
	movwf	COUNT			; "
lb_067	btfss	PORTA,RA3		;If the device has pulled the ADB pin low, skip ahead to start receiving
	goto	lb_068			; "
	decfsz	COUNT,F			;Decrement loop counter, loop again while we wait for the device to start sending
	goto	lb_067			; "
	bsf	FLAGS,F_ERROR		;Device never started sending, so set error flag, and jump into receive-complete
	movlw	0x02			; handler pretending we just received two bytes
	goto	AdbRDn0			; "
lb_068	call	AdbRecvBit		;Throw away the start bit
	DNOP				;
lb_069	clrf	INDF			;Clear the buffer byte for the byte we're about to receive
	call	AdbRecvBit		;Receive MSB, shift it left into the buffer byte
	iorwf	INDF,F			; "
	rlf	INDF,F			; "
	call	AdbRecvBit		;Receive bit 6, shift it left into the buffer byte
	iorwf	INDF,F			; "
	rlf	INDF,F			; "
	call	AdbRecvBit		;Receive bit 5, shift it left into the buffer byte
	iorwf	INDF,F			; "
	rlf	INDF,F			; "
	call	AdbRecvBit		;Receive bit 4, shift it left into the buffer byte
	iorwf	INDF,F			; "
	rlf	INDF,F			; "
	call	AdbRecvBit		;Receive bit 3, shift it left into the buffer byte
	iorwf	INDF,F			; "
	rlf	INDF,F			; "
	call	AdbRecvBit		;Receive bit 2, shift it left into the buffer byte
	iorwf	INDF,F			; "
	rlf	INDF,F			; "
	call	AdbRecvBit		;Receive bit 1, shift it left into the buffer byte
	iorwf	INDF,F			; "
	rlf	INDF,F			; "
	call	AdbRecvBit		;Receive LSB into the buffer byte
	iorwf	INDF,F			; "
	incfsz	FSR,F			;Increment the FSR; if we just received an eighth byte, jump into receive-complete
	goto	lb_069			; handler, otherwise loop for the next byte (if the transmission ends with less
	goto	AdbRecvDone		; than eight bytes, AdbRecvBit will fall through into receive-complete)
lb_070	movlw	0x02			;The command sent was a flush, so pretend we received two bytes (TODO ??) and
	movwf	FLENGTH			; point FSR to beginning of data buffer
	movlw	0xf8			; "
	movwf	FSR			; "

lb_071
	btfsc	FLAGS,F_SRQ
	bsf	FLAGS2,F2_UNK1
	btfss	FLAGS,F_ERROR
	bsf	FLAGS2,F2_UNK1
	btfss	FLAGS2,F2_UNK1
	goto	UpdateLastA
	btfss	PORTB,RB0
	goto	UpdateLastA
	btfss	PORTB,RB1
	goto	UpdateLastA
	bcf	LATCHB,INT_PIN		;Assert INT pin
	goto	UpdatePortB

lb_072
	goto	Init


;;; End of Program ;;;

	end
