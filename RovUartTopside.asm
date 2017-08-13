;Convert analog signal from joystick to PWM values to be sent to 6 thrusters
;via UART

    list		p=16f1937	;list directive to define processor
    #include	<p16f1937.inc>		; processor specific variable definitions
	
    errorlevel -302	;no "register not in bank 0" warnings
    errorlevel -312     ;no  "page or bank selection not needed" messages
	
    #define BANK0  (h'000')
    #define BANK1  (h'080')
    #define BANK2  (h'100')
    #define BANK3  (h'180')

    __CONFIG _CONFIG1,    _MCLRE_OFF & _CP_OFF & _CPD_OFF & _BOREN_OFF & _WDTE_OFF & _PWRTE_ON & _FOSC_XT & _FCMEN_OFF & _IESO_OFF

;Context saving variables:
CONTEXT	UDATA_SHR
userMillis	RES	1
w_copy		RES     1	;variable used for context saving (work reg)
status_copy	RES     1	;variable used for context saving (status reg)
pclath_copy	RES     1	;variable used for context saving (pclath copy)
adcCounter	RES	1	;counter to be increented till value in
				;ADRESH is reached
ADRESHc		RES	1	;copy of ADRESH
compCounter	RES	1	;counter to be incremented once every 6 servo
				;steps to give full range of motion
motorTemp	RES	1
ADRESH0		RES	1	;copy of value from pin AN0
ADRESH1		RES	1	;copy of value from pin AN1
ADRESH2		RES	1	;copy of value from pin AN2
AN0disp		RES	1	;displacement of ADRESHO from 127
AN1disp		RES	1	;displacement of ADRESH1 from 127
positionSpeed	RES	1	;value returned from getMotorSpeed routine
				;to be placed in forward, reverse and upDown speeds
state		RES	1	;desired directional "state" of ROV

;General Variables
GENVAR1	UDATA
transData	RES	1	;Data to be transmitted via UART
receiveData	RES	1	;Data received via UART
dly16Ctr	RES	1
forwardSpeed	RES	1	;Forward value for CCPR1L
reverseSpeed	RES	1	;Reverse value for CCPR2L
upDownSpeed	RES	1	;CCPR3L value for up/down thrusters


;**********************************************************************
    ORG		0x000	
    pagesel		start	; processor reset vector
    goto		start	; go to beginning of program
INT_VECTOR:
    ORG		0x004		; interrupt vector location
INTERRUPT:
	movwf   w_copy                      ; save off current W register contents
        movf    STATUS,w                    ; move status register into W register
        movwf   status_copy                 ; save off contents of STATUS register
        movf    PCLATH,W
        movwf   pclath_copy
	
	banksel	PORTB
	movfw	PORTB		;clear mismatch
	
	bcf	INTCON, IOCIF	;clear flag 
	banksel	IOCBF
	clrf	IOCBF
	
;stop motors
	movlw	    .8		;"stop" state
	movwf	    state
	movlw	    .95
	banksel	    forwardSpeed
	movwf	    forwardSpeed
	movwf	    reverseSpeed
	movwf	    upDownSpeed
	call	    sendThrust
stickDirection    
;3) Check AN1 (Rotation Value) 
    ;Set AN1 as analog input for AD conversion and start AD conversion
    movlw	b'00000101'
		; -00001--  CHS<0:4> (bits 2-6) = 00001 = pin AN1/PORTA, 1 as analog input
		; ------0-  stop AD conversion
		; -------1  Enable ADC
    banksel	ADCON0
    movwf	ADCON0
    call	Delay16Us
    call	Delay16Us
    banksel	ADCON0
    bsf		ADCON0, GO
waitAdcRotate
    btfsc	ADCON0, NOT_DONE
    goto	waitAdcRotate
    
    banksel	ADRESH
    movfw	ADRESH
    movwf	ADRESH1
    
;4) Check AN0 (Depth value) 
    ;Set AN0 as analog input for AD conversion and start AD conversion
    movlw	b'00000001'
		; -00000--  CHS<0:4> (bits 2-6) = 00000 = pin AN0/PORTA, 0 as analog input
		; ------0-  stop AD conversion
		; -------1  Enable ADC
    banksel	ADCON0
    movwf	ADCON0
    call	Delay16Us
    call	Delay16Us
    banksel	ADCON0
    bsf		ADCON0, GO
waitAdcDepth
    btfsc	ADCON0, NOT_DONE
    goto	waitAdcDepth
    
    banksel	ADRESH
    movfw	ADRESH
    movwf	ADRESH0
    
checkFRslopInt
    movlw	.160
    subwf	ADRESH0, w
    btfsc	STATUS, C	;(C=0 is neg #)
    goto	DispInt	;value > 160 (go to rotation section)
    ;not greater than 160, check if less than 100
    movlw	.90
    subwf	ADRESH0, w
    btfss	STATUS, C	;(C=0 is neg #)
    goto	DispInt	;value < 126 (go to rotation section)
checkLRslopInt
    movlw	.160
    subwf	ADRESH1, w
    btfsc	STATUS, C	;(C=0 is neg #)
    goto	DispInt	;value > 128 (go to rotation section)
    ;not greater than 128, check if less than 126
    movlw	.90
    subwf	ADRESH1, w
    btfss	STATUS, C	;(C=0 is neg #)
    goto	DispInt	;value < 126 (go to rotation section)
    ;Stop all thrusters (neutral joystick position)
    movlw	.8		;"stop" state
    movwf	state
    movlw	.95		;1500uS pulse width
    banksel	forwardSpeed		
    movwf	forwardSpeed
    movwf	reverseSpeed
    movwf	upDownSpeed
    goto	isrEnd

DispInt
;5) Get AN1 displacement from 127     
    call	getAn1Disp
;6) Get AN0 displacement from 127
    call	getAn0Disp
    
;subtract AN1disp from AN0disp to see which displacement is greater
    movfw	AN1disp
    subwf	AN0disp, w
    btfss	STATUS, C	;(C=0 is neg #)
    goto	rotate		;AN1 (rotate) is greater
    goto	depth		;ANO (depth) is greater
    
rotate
    ;Determine whether we need to rotate CCW or CW:
    ;(if ADRESH1 > 127 then right, if ADRESH1 <= 127 then left)
    btfss	ADRESH1, 7	;test MSB of ADRESH1 (1: > 127, 0: <= 127)
    goto	CCW
    
CW ;(Clockwise rotation):
    movlw	.5		;"clockwise-rotation" state
    movwf	state
    ;Forward PWM (normal value) to thrusters 1 and 4 (top-left and bottom-right thrusters)
    movfw	ADRESH1		;send normal PWM value from ADC conversion
    movwf	ADRESHc		;to thrusters 1 and 4 via the forward logic
    call	getMotorSpeed	;IC and through P1A of receiving device
    movfw	positionSpeed
    banksel	forwardSpeed
    movwf	forwardSpeed
    ;Reverse PWM (calculated value) to thrusters 2 and 3 (top-right and bottom-left thrusters)
    ;get displacement of ADRESH1 (already calculated above)
    movlw	.128		;128 instead of 127 to prevent overflow
    movwf	ADRESHc
    movfw	AN1disp
    subwf	ADRESHc, f	;and subtract displacement from 128 
    call	getMotorSpeed	;to get a reverse PWM value and output it to the
    movfw	positionSpeed	;reverse logic IC via P2A of receiving device
    banksel	reverseSpeed
    movwf	reverseSpeed
    goto	isrEnd
    
CCW ;(Counter-clockwise rotation):
    movlw	.6		;"counterclockwise-rotation" state
    movwf	state
    ;Reverse PWM (normal value) to thrusters 1 and 4 (top-left and bottom-right thrusters)
    movfw	ADRESH1
    movwf	ADRESHc
    call	getMotorSpeed
    movfw	positionSpeed
    banksel	reverseSpeed
    movwf	reverseSpeed
    ;Forward PWM (calculated value) to thrusters 2 and 3 (top-right and bottom-left thrusters)
    ;get displacement of ADRESH1 (already calculated above)
    movlw	.127		
    movwf	ADRESHc
    movfw	AN1disp
    addwf	ADRESHc, f	;and subtract displacement from 128 
    call	getMotorSpeed	;to get a forward PWM value and output it to the
    movfw	positionSpeed	;forward logic IC via P1A
    banksel	forwardSpeed
    movwf	forwardSpeed
    goto	isrEnd
	
depth
    movlw	.7		;"up/down" state
    movwf	state
;use value from AN0 to get speed for dive/surface thrusters:
    movfw	ADRESH0
    movwf	ADRESHc
    call	getMotorSpeed
    movfw	positionSpeed
    banksel	upDownSpeed
    movwf	upDownSpeed
    
;restore pre-ISR values to registers
isrEnd
    call	sendThrust
;test if button is still pressed:
    banksel	PORTB
    btfsc	PORTB, 0
    goto	stickDirection
    
;stop motors
    movlw	.8		;"stop" state
    movwf	state
    movlw	.95
    banksel	forwardSpeed
    movwf	forwardSpeed
    movwf	reverseSpeed
    movwf	upDownSpeed
    call	sendThrust
	
    movf	pclath_copy,W
    movwf	PCLATH
    movf	status_copy,w   ;retrieve copy of STATUS register
    movwf	STATUS          ;restore pre-isr STATUS register contents
    swapf	w_copy,f
    swapf	w_copy,w
    retfie
    
delayMillis
    movwf	userMillis	;user defined number of milliseconds
startDly
    banksel	TMR0
    clrf	TMR0
waitTmr0
    movfw	TMR0
    xorlw	.125		;125 * 8uS = 1mS
    btfss	STATUS, Z
    goto	waitTmr0
    decfsz	userMillis, f	;reached user defined milliseconds yet?
    goto	startDly
    
    retlw	0
    
Delay16Us
    clrf	    dly16Ctr	;zero out delay counter
begin
    nop			;1 uS (4Mhz clock/4 = 1uS per instruction
    banksel	    dly16Ctr
    incf	    dly16Ctr, f
    movlw	    .16		
    xorwf	    dly16Ctr, w	 ;16 uS passed?
    btfss	    STATUS, Z
    goto	    begin	;no so keep looping
    retlw	    0
    
;Get the displacement of AN0 analog input (distance from 127)
getAn0Disp
    btfss	ADRESH0, 7	;test MSB of ADRESH0 (1: > 127, 0: <= 127
    goto	A0Lowerhalf	;ADRESH0 <= 127
    ;ADRESH0 > 127:
    movlw	.127
    subwf	ADRESH0, w
    movwf	AN0disp
    retlw	0
    ;ADRESH0 <= 127:
A0Lowerhalf
    movlw	.127
    movwf	AN0disp
    movfw	ADRESH0
    subwf	AN0disp, f
    retlw	0
    
;Get the displacement of AN1 analog input (distance from 127)
getAn1Disp
    btfss	ADRESH1, 7	;test MSB of ADRESH1 (1: > 127, 0: <= 127
    goto	A1Lowerhalf	;ADRESH1 <= 127
    ;ADRESH1 > 127:
    movlw	.127
    subwf	ADRESH1, w
    movwf	AN1disp
    retlw	0
    ;ADRESH1 <= 127:
A1Lowerhalf
    movlw	.127
    movwf	AN1disp
    movfw	ADRESH1
    subwf	AN1disp, f
    retlw	0
    
;Get the speed of the thruster motor:
getMotorSpeed
    clrf	adcCounter
    clrf	compCounter
    clrf	positionSpeed
    movlw	.70		;Start with speed value at max reverse
    movwf	positionSpeed	;motorSpeed will be placed in CCPRxL
startAdding
    ;make sure adcCounter isn't < 5 away from 255
    movfw	adcCounter
    subwf	motorTemp, w		;value in temp is 255
    btfss	STATUS, C
    goto	motorEnd	;it is so exit routine
    
    movlw	.5
    addwf	adcCounter, f	;add 5 to adcCounter till value in ADRESH is reached
    movfw	adcCounter
    subwf	ADRESHc, w
    btfss	STATUS, C	;reached value of ADRESH yet? (C=0 is neg #)
    goto	motorEnd	;yes so exit routine
    incf	positionSpeed, f	;no so increment motorSpeed by one
    goto	startAdding
motorEnd
    retlw	0
    
;Send thruster data
sendThrust
    movfw	state
    movwf	transData
    call	Transmit
    movlw	.5
    call	delayMillis
    
    banksel	forwardSpeed
    movfw	forwardSpeed
    movwf	transData
    call	Transmit
    movlw	.5
    call	delayMillis
    
    banksel	reverseSpeed
    movfw	reverseSpeed
    movwf	transData
    call	Transmit
    movlw	.5
    call	delayMillis
    
    banksel	upDownSpeed
    movfw	upDownSpeed
    movwf	transData
    call	Transmit
    
    movlw	.50
    call	delayMillis
    
    retlw	0
	
;*************************UART SUBROUTINES**************************************
Transmit
    banksel	transData
    movfw	transData	
    banksel	TXREG
    movwf	TXREG		;data to be transmitted loaded into TXREG
				;and then automatically loaded into TSR
    nop
    nop
    banksel	PIR1
wait_trans
    btfss	PIR1, TXIF	;Is TX buffer full? (1=empty, 0=full)
    goto	wait_trans
    retlw	0
    
;*************************END UART SUBROUTINES**********************************
	
start:
    banksel BANK1
    ;Set PORTS to output
    movlw   b'00000011'		
    movwf   (TRISA ^ BANK1)
    movlw   b'11111111'		    
    movwf   (TRISB ^ BANK1)
    movlw   b'11111111'		;PORTC, 7 = RX pin for UART         
    movwf   (TRISC ^ BANK1)
    movlw   b'11111111'
    movwf   (TRISD ^ BANK1)	    
    movlw   b'00000000'
    movwf   (TRISE ^ BANK1)
    
    ;Configure timer
    ;With 4Mhz external crystal, FOSC is not divided by 4.
    ;Therefore each instruction is 1/4 of a microsecond (250*10^-9 sec.)
    movlw	b'11000100'	
		 ;1-------	WPUEN=0, all weak pull-ups are disabled
		 ;-1------	INTEDG=1, Interrupt on rising edge of INT pin
		 ;--0-----	TMR0CS=0, TMR0 clock source=internal instruction
			        ;	  (FOSC/4)
		 ;---0----	;TMR0SE=0, disregard
		 ;----0---	;PSA=0, prescaler assigned to TMR0 module
		 ;-----100	;PS<2:0> = 00, TMRO increments once every 32
				;instruction cycles 
				;Every instruction cycle is 250*10^-9 sec (4Mhz), 
				;therefore TMR0 increments once every 32 * 250*10^-9 sec
				;or once every 8uS
    banksel	OPTION_REG	
    movwf	OPTION_REG	
	
    ;enable interrupts
    movlw	b'11001000'
	         ;1-------	;Enable global interrupts (GIE=1)
		 ;-1------	;Enable peripheral interrupts (PEIE=1)
		 ;--0-----	;Disable TMR0 interrupts (TMROIE=0)
		 ;---0----	;Disable RBO/INT external interrupt (INTE=1)
		 ;----1---	;Enable interrupt on change for PORTB (IOCIE=0)
    movwf	INTCON
    
    ;Enable interrupt on change for PORTB, 0
    movlw	b'00000001'	;PORTB,  pin set for IOC (rising edge)
    banksel	IOCBP
    movwf	IOCBP
    
    ;Config ADC:
    movlw	b'00000011'
    banksel	ANSELA
    movwf	ANSELA
    
    movlw	b'00010000'
		;0-------  ADFM=0 (left justified. 8MSBs are in ADRESH
		;-001----  ADCS<0:2>, bits 4-6 =001. (2.0uS)
			;FOSC/8=4Mhz/8 (doubles instruction cycle time)
			;Instruction Cycle period (TCY) now equals
			;2uS (greater than 1.6uS necessary for ADC)
    banksel	ADCON1
    movwf	ADCON1
    
    ;4Mhz external crystal:
    movlw	b'00000000'
    banksel	OSCCON
    movwf	OSCCON
    
    ;5 second delay
    clrf	motorTemp
delayStart
    movlw	.250
    call	delayMillis
    movlw	.250
    call	delayMillis
    movlw	.250
    call	delayMillis
    movlw	.250
    call	delayMillis
    incf	motorTemp, f
    movlw	.5
    xorwf	motorTemp, w
    btfss	STATUS, Z
    goto	delayStart
done5sDelay
    
;CONFIGURE UART:
    ;Configure Baud rate
    movlw	b'01000000' 
    banksel	SPBRG
    movwf	SPBRG	    ;Move 'X' to baurate generator
    
    movlw	b'00000011'
    banksel	SPBRGH
    movwf	SPBRGH
    
    banksel	BAUDCON
    movlw	b'00001000'
		 ;----1---	BRG16 (16 bit baud rate generator)
    movwf	BAUDCON
    
    ;Enable Transmission:
    movlw	b'00100000'
		 ;-0------  :8-bit transmission (TX9 = 0)
		 ;--1-----  :Enable transmission (TXEN = 1)
		 ;---0----  :Asynchronous mode (SYNC = 0)
		 ;-----0--  :Low speed baud rate (BRGH = 0)
    banksel	TXSTA
    movwf	TXSTA
		 
    ;Enable Reception:
    movlw	b'10010000'
		 ;1-------  :Serial port enabled (SPEN = 1)
		 ;-0------  :8-bit reception (RX9 = 0)
		 ;---1----  :Enable receiver (CREN = 1)
		 ;----0---  :Disable address detection (ADDEN = 0)
    ;			     all bytes are received and 9th bit can be used as
    ;			     parity bit
    banksel	RCSTA
    movwf	RCSTA
    movlw	d'20'
    call	delayMillis
    banksel	transData
    clrf	transData
    
    banksel	ANSELB
    clrf	ANSELB
    clrf	ANSELD
    clrf	ANSELE
    
    ;1/4 second delay
    movlw	.250
    call	delayMillis
    movlw	.95
    banksel	forwardSpeed
    movwf	forwardSpeed
    movwf	reverseSpeed
    movwf	upDownSpeed
    
    movlw	.250
    movwf	motorTemp

mainLoop
    ;9th data bit = LSB of transmission:
    ;banksel	TXSTA
    ;bcf	TXSTA, TX9D
    ;Send Thruster data 
    call	sendThrust
    
    ;1) Check AN0 (FORWARD/REVERSE DIRECTION) 
    ;Set AN0 as analog input for AD conversion and start AD conversion
    movlw	b'00000001'
		; -00000--  CHS<0:4> (bits 2-6) = 00000 = pin AN0/PORTA, 0 as analog input
		; ------0-  stop AD conversion
		; -------1  Enable ADC
    banksel	ADCON0
    movwf	ADCON0
    call	Delay16Us
    call	Delay16Us
    banksel	ADCON0
    bsf		ADCON0, GO
waitAdc3
    btfsc	ADCON0, NOT_DONE
    goto	waitAdc3
    
    banksel	ADRESH
    movfw	ADRESH
    movwf	ADRESH0
    
    ;2) Check AN1 (LEFT/RIGHT DIRECTION) 
    ;Set AN1 as analog input for AD conversion and start AD conversion
    movlw	b'00000101'
		; -00000--  CHS<0:4> (bits 2-6) = 00001 = pin AN1/PORTA, 1 as analog input
		; ------0-  stop AD conversion
		; -------1  Enable ADC
    banksel	ADCON0
    movwf	ADCON0
    call	Delay16Us
    call	Delay16Us
    banksel	ADCON0
    bsf		ADCON0, GO
waitAdc2
    btfsc	ADCON0, NOT_DONE
    goto	waitAdc2
    
    banksel	ADRESH
    movfw	ADRESH
    movwf	ADRESH1

;Account for "slop" in fwd/rev and left/right potentiometers
;check if greater than 160    
checkFRslop
    movlw	.160
    subwf	ADRESH0, w
    btfsc	STATUS, C	;(C=0 is neg #)
    goto	Displacement	;value > 160 (go to rotation section)
    ;not greater than 160, check if less than 100
    movlw	.90
    subwf	ADRESH0, w
    btfss	STATUS, C	;(C=0 is neg #)
    goto	Displacement	;value < 126 (go to rotation section)
checkLRslop
    movlw	.160
    subwf	ADRESH1, w
    btfsc	STATUS, C	;(C=0 is neg #)
    goto	Displacement	;value > 128 (go to rotation section)
    ;not greater than 128, check if less than 126
    movlw	.90
    subwf	ADRESH1, w
    btfss	STATUS, C	;(C=0 is neg #)
    goto	Displacement	;value < 126 (go to rotation section)
    ;Stop all thrusters (neutral joystick position)
    movlw	.8		;"stop" state
    movwf	state
    movlw	.95		;1500uS pulse width
    banksel	forwardSpeed		
    movwf	forwardSpeed
    movwf	reverseSpeed
    movwf	upDownSpeed
    
    goto	mainLoop
    
Displacement
    ;Check whether AN0 or AN1 value is further away from 127
    
;1) Get AN0 displacement from 127
    call	getAn0Disp
;2) Get AN1 displacement from 127     
    call	getAn1Disp
    
zeroVsOneCont
    ;subtract AN1disp from AN0disp to see which displacement is greater
    movfw	AN1disp
    subwf	AN0disp, w
    btfss	STATUS, C	;(C=0 is neg #)
    goto	leftRight	;AN1 (lt/rt) is greater
    goto	fwdRev		;ANO (fwd/rev) is greater
    
fwdRev 
    ;Determine whether we need to go forward or reverse
    ;ADRESH0 > 127 = forward******ADRESH0 <= 127 = reverse
    btfss	ADRESH0, 7
    goto	reverse
    
forward
    movlw	.1		;"forward" state
    movwf	state
    ;Forward PWM (normal value) to thrusters 1 and 2 (top-left and top-right thrusters)
    movfw	ADRESH0
    movwf	ADRESHc
    call	getMotorSpeed
    movfw	positionSpeed
    banksel	forwardSpeed
    movwf	forwardSpeed
    ;Reverse PWM (calculated value) to thrusters 3 and 4 (bottom left/bottom right thrusters)
    ;get displacement of ADRESH0 (already calculated above)
    movlw	.128		;128 instead of 127 to prevent overflow
    movwf	ADRESHc
    movfw	AN0disp
    subwf	ADRESHc, f	;and subtract displacement from 128 
    call	getMotorSpeed	;to get a reverse PWM value and output it to the
    movfw	positionSpeed	;reverse logic IC via P2A
    banksel	reverseSpeed
    movwf	reverseSpeed
    goto        mainLoop
    
reverse
    movlw	.2		;"reverse" state
    movwf	state
    ;Reverse PWM (normal value) to thrusters 1/2 (top-left and top-right thrusters)
    movfw	ADRESH0
    movwf	ADRESHc
    call	getMotorSpeed
    movfw	positionSpeed
    banksel	reverseSpeed
    movwf	reverseSpeed
    ;Forward PWM (calculated value) to thrusters 3/4 (bottom-right and bottom-left thrusters)
    ;get displacement of ADRESH0 (already calculated above)
    movlw	.127		
    movwf	ADRESHc
    movfw	AN0disp
    addwf	ADRESHc, f	;and subtract displacement from 128 
    call	getMotorSpeed	;to get a forward PWM value and output it to the
    movfw	positionSpeed	;forward logic IC via P1A
    banksel	forwardSpeed
    movwf	forwardSpeed
    goto	mainLoop
    
leftRight
    ;Determine whether we need to go left or right:
    ;(ADRESH1 > 127 = right********ADRESH1 <= 127 = left)
    btfss	ADRESH1, 7	;test MSB of ADRESH1 (1: > 127, 0: <= 127)
    goto	traverseLeft
    
traverseRight
    movlw	.3		;"traverse right" state
    movwf	state
    ;Forward PWM (normal value) to thrusters 1 and 3 (top-left and bottom-left thrusters)
    movfw	ADRESH1		;send normal PWM value from ADC conversion
    movwf	ADRESHc		;to thrusters 1 and 4 via the forward logic
    call	getMotorSpeed	;IC and through P1A
    movfw	positionSpeed
    banksel	forwardSpeed
    movwf	forwardSpeed
    ;Reverse PWM (calculated value) to thrusters 2 and 4 (top-right and bottom-right thrusters)
    ;get displacement of ADRESH1 (already calculated above)
    movlw	.128		;128 instead of 127 to prevent overflow
    movwf	ADRESHc
    movfw	AN1disp
    subwf	ADRESHc, f	;and subtract displacement from 128 
    call	getMotorSpeed	;to get a reverse PWM value and output it to the
    movfw	positionSpeed	;reverse logic IC via P2A
    banksel	reverseSpeed
    movwf	reverseSpeed
    goto	mainLoop
    
traverseLeft
    movlw	.4		;"traverse left" state
    movwf	state
    ;Reverse PWM (normal value) to thrusters 1 and 3 (top-left and bottom-left thrusters)
    movfw	ADRESH1
    movwf	ADRESHc
    call	getMotorSpeed
    movfw	positionSpeed
    banksel	reverseSpeed
    movwf	reverseSpeed
    ;Forward PWM (calculated value) to thrusters 2 and 4 (top-right and bottom-right thrusters)
    ;get displacement of ADRESH1 (already calculated above)
    movlw	.127		
    movwf	ADRESHc
    movfw	AN1disp
    addwf	ADRESHc, f	;and subtract displacement from 128 
    call	getMotorSpeed	;to get a forward PWM value and output it to the
    movfw	positionSpeed	;forward logic IC via P1A
    banksel	forwardSpeed
    movwf	forwardSpeed
    
    goto	mainLoop
   
    END                       

































