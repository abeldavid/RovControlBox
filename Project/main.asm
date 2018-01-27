;Convert analog signal from joystick to PWM values to be sent to 6 thrusters
;via UART

    list	p=16f1937	;list directive to define processor
    #include	<p16f1937.inc>		; processor specific variable definitions
	
    errorlevel -302	;no "register not in bank 0" warnings
    errorlevel -312     ;no  "page or bank selection not needed" messages
    errorlevel -207    ;no label after column one warning
    
    extern	delayMillis	;delayMillis subroutine (external file)
    extern	ESCready	;routine to set ESC ready light
    extern	Leak		;routine to set Leak warning light
    extern	Delay16Us	;16uS delay for ADC conversions
    extern	Transmit	;UART transmit subroutine
    extern	Receive		;UART receive subroutine
    extern	sendThrust	;routine to send thruster data via UART
    extern	getAn0Disp	;displacement from "dead-center" for AN0
    extern	getAn1Disp	;displacement from "dead-center" for AN1
    extern	getMotorSpeed	;routine to determine correct speed of motor
    extern	checkSlop
    extern	slopFlag
    
    global	transData	;UART transmit variable
    global	receiveData	;UART receive variable
    global	state		;desired directional "state" of ROV
    global	forwardSpeed	;forward speed variable
    global	reverseSpeed	;reverse speed variable
    global	upDownSpeed	;depth control variable
    global	ADRESH0		;copy of ADRESH for AN0
    global	ADRESH1		;copy of ADRESH for AN1
    global	ADRESHc
    global	AN0disp
    global	AN1disp
    global	adcCounter
    global	compCounter
    global	positionSpeed
    global	motorTemp
	
    #define BANK0  (h'000')
    #define BANK1  (h'080')
    #define BANK2  (h'100')
    #define BANK3  (h'180')

    __CONFIG _CONFIG1,    _MCLRE_ON & _CP_OFF & _CPD_OFF & _BOREN_OFF & _WDTE_OFF & _PWRTE_ON & _FOSC_XT & _FCMEN_OFF & _IESO_OFF

;Variables accessible in all banks:
MULTIBANK	UDATA_SHR
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
GENVAR1	        UDATA
transData	RES	1	;Data to be transmitted via UART
receiveData	RES	1	;Data received via UART
forwardSpeed	RES	1	;Forward value for CCPR1L
reverseSpeed	RES	1	;Reverse value for CCPR2L
upDownSpeed	RES	1	;CCPR3L value for up/down thrusters


;**********************************************************************
.ResetVector code	0x000	
    pagesel		start	; processor reset vector
    goto		start	; go to beginning of program
INT_VECTOR:
.Interupt code		0x004		 ;interrupt vector location
INTERRUPT:

    movwf       w_copy           ;save off current W register contents
    movf        STATUS,w         ;move status register into W register
    movwf       status_copy      ;save off contents of STATUS register
    movf        PCLATH,W
    movwf       pclath_copy
	
    ;Determine source of interrupt
    banksel	PIR1
    btfss	PIR1, RCIF	 ;UART receive interrupt?
    goto	motors
;UART triggered interrupt on reception	
UartReceive
    call	Receive
    ;determine sensor type
    banksel	receiveData
    movlw	.1		;code for leak
    xorwf	receiveData, w
    btfss	STATUS, Z
    goto	testsubReady
    goto	LeakDetected
testsubReady
    movlw	.2		;code for ready light upon ESC initialization
    banksel	receiveData
    xorwf	receiveData, w
    btfss	STATUS, Z
    goto	isrEnd
    call	ESCready
    goto	isrEnd
LeakDetected
    banksel	PIE1
    bcf	        PIE1, RCIE	 ;disable UART receive interrupts
    pagesel	Leak
    call	Leak
    pagesel$
    goto	isrEnd
motors    
    banksel	PORTB
    movfw	PORTB		;clear mismatch
	
    bcf	        INTCON, IOCIF	;clear flag 
    banksel	IOCBF
    clrf	IOCBF
	
;stop motors
    movlw	.7		;"stop" state
    movwf	state
    movlw	.95
    banksel	forwardSpeed
    movwf	forwardSpeed
    movwf	reverseSpeed
    movwf	upDownSpeed
    pagesel	sendThrust
    call	sendThrust
    pagesel$
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
    movlw	.7		;"stop" state
    movwf	state
    movlw	.95		;1500uS pulse width
    banksel	forwardSpeed		
    movwf	forwardSpeed
    movwf	reverseSpeed
    movwf	upDownSpeed
    goto	endMotors

DispInt
;5) Get AN1 displacement from 127  
    pagesel	getAn1Disp
    call	getAn1Disp
    pagesel$
;6) Get AN0 displacement from 127
    pagesel	getAn0Disp
    call	getAn0Disp
    pagesel$
    
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
    movlw	.4		;"clockwise-rotation" state
    movwf	state
    ;Forward PWM (normal value) to thrusters 1 and 4 (top-left and bottom-right thrusters)
    movfw	ADRESH1		;send normal PWM value from ADC conversion
    movwf	ADRESHc		;to thrusters 1 and 4 via the forward logic
    pagesel	getMotorSpeed
    call	getMotorSpeed	;IC and through P1A of receiving device
    pagesel$
    movfw	positionSpeed
    banksel	forwardSpeed
    movwf	forwardSpeed
    ;Reverse PWM (calculated value) to thrusters 2 and 3 (top-right and bottom-left thrusters)
    ;get displacement of ADRESH1 (already calculated above)
    movlw	.128		;128 instead of 127 to prevent overflow
    movwf	ADRESHc
    movfw	AN1disp
    subwf	ADRESHc, f	;and subtract displacement from 128 
    pagesel	getMotorSpeed
    call	getMotorSpeed	;to get a reverse PWM value and output it to the
    pagesel$
    movfw	positionSpeed	;reverse logic IC via P2A of receiving device
    banksel	reverseSpeed
    movwf	reverseSpeed
    goto	endMotors
    
CCW ;(Counter-clockwise rotation):
    movlw	.5		;"counterclockwise-rotation" state
    movwf	state
    ;Reverse PWM (normal value) to thrusters 1 and 4 (top-left and bottom-right thrusters)
    movfw	ADRESH1
    movwf	ADRESHc
    pagesel	getMotorSpeed
    call	getMotorSpeed
    pagesel$
    movfw	positionSpeed
    banksel	reverseSpeed
    movwf	reverseSpeed
    ;Forward PWM (calculated value) to thrusters 2 and 3 (top-right and bottom-left thrusters)
    ;get displacement of ADRESH1 (already calculated above)
    movlw	.127		
    movwf	ADRESHc
    movfw	AN1disp
    addwf	ADRESHc, f	;and subtract displacement from 128 
    pagesel	getMotorSpeed
    call	getMotorSpeed	;to get a forward PWM value and output it to the
    pagesel$
    movfw	positionSpeed	;forward logic IC via P1A
    banksel	forwardSpeed
    movwf	forwardSpeed
    goto	endMotors
	
depth
    movlw	.6		;"up/down" state
    movwf	state
;use value from AN0 to get speed for dive/surface thrusters:
    movfw	ADRESH0
    movwf	ADRESHc
    pagesel	getMotorSpeed
    call	getMotorSpeed
    pagesel$
    movfw	positionSpeed
    banksel	upDownSpeed
    movwf	upDownSpeed
    
;restore pre-ISR values to registers
endMotors
    pagesel	sendThrust
    call	sendThrust
    pagesel$
;test if button is still pressed:
    banksel	PORTB
    btfsc	PORTB, 0
    goto	stickDirection
    
;stop motors
    movlw	.7		;"stop" state
    movwf	state
    movlw	.95
    banksel	forwardSpeed
    movwf	forwardSpeed
    movwf	reverseSpeed
    movwf	upDownSpeed
    pagesel	sendThrust
    call	sendThrust
    pagesel$
isrEnd	
    banksel	PIE1
    bsf	        PIE1, RCIE	 ;enable UART receive interrupts
    movf	pclath_copy,W
    movwf	PCLATH
    movf	status_copy,w   ;retrieve copy of STATUS register
    movwf	STATUS          ;restore pre-isr STATUS register contents
    swapf	w_copy,f
    swapf	w_copy,w
    retfie
   
.main    code	
start:
    banksel BANK1
    ;************************Configure PORTS************************************
    movlw   b'00000011'		
    movwf   (TRISA ^ BANK1)
    movlw   b'11111111'		    
    movwf   (TRISB ^ BANK1)
    movlw   b'11111000'		;PORTC, 7 = RX pin for UART, PORTC, 2=P1A (PWM)         
    movwf   (TRISC ^ BANK1)
    movlw   b'00000000'		;Leak and ready lights
    movwf   (TRISD ^ BANK1)	    
    movlw   b'00000000'
    movwf   (TRISE ^ BANK1)
    
    banksel	PORTD
    clrf	PORTD
    clrf	PORTC
    
    ;************************Configure timer************************************
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
	
    ;*************************Enable interrupts*********************************
    movlw	b'11001000'
	         ;1-------	;Enable global interrupts (GIE=1)
		 ;-1------	;Enable peripheral interrupts (PEIE=1)
		 ;--0-----	;Disable TMR0 interrupts (TMROIE=0)
		 ;---0----	;Disable RBO/INT external interrupt (INTE=1)
		 ;----1---	;Enable interrupt on change for PORTB (IOCIE=0)
    movwf	INTCON
    
    ;Enable interrupt on change for PORTB
    movlw	b'00000001'	;PORTB,  pin set for IOC (rising edge)
    banksel	IOCBP
    movwf	IOCBP
    
    ;************************Config ADC:****************************************
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
    
    ;***********************Configure PWM***************************************
    movlw       b'00000110'     ; configure Timer2:
                ; -----1--          turn Timer2 on (TMR2ON = 1)
                ; ------10          prescale = 16 (T2CKPS = 10)
    banksel     T2CON           ; -> TMR2 increments every 4 us
    movwf       T2CON
    movlw       .140            ; PR2 = 140
    banksel     PR2             ; -> period = 560uS
    movwf       PR2             ; -> PWM frequency = 1.8 kHz
    ;Configure CCP1, CCP2 and CCP3 to be based off of TMR2:
    banksel     CCPTMRS0
    movlw	b'11111100'
		 ;------00	;CCP1 based off of TMR2
    ;configure CCP1
    movlw       b'00001100'     ; configure CCP:
                ; 00------          single output (P1M = 00 -> CCP1 active)
                ; --00----          DC1B = 00 -> LSBs of PWM duty cycle = 00
                ; ----1100          PWM mode: all active-high (CCP1M = 1100)
    banksel     CCP1CON         ; -> single output (CCP1) mode, active-high
    movwf       CCP1CON
    banksel	CCPR1L
    clrf	CCPR1L
    ;***************************************************************************
    
    ;4Mhz external crystal:
    movlw	b'00000000'
    banksel	OSCCON
    movwf	OSCCON
    
    clrf	motorTemp
    
;*********************************CONFIGURE UART********************************
    ;Configure Baud rate
    movlw	b'01000000' 
    banksel	SPBRG
    movwf	SPBRG	    
    
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
    pagesel	delayMillis
    call	delayMillis
    pagesel$
    banksel	transData
    clrf	transData
    ;***************************************************************************
    movlw	b'00100000'
		 ;--1-----	;Enable USART receive interrupt (RCIE=1)
    banksel	PIE1
    movwf	PIE1
    
    banksel	ANSELB
    clrf	ANSELB
    clrf	ANSELD
    clrf	ANSELE
    
    
    
    ;1/4 second delay
    movlw	.250
    pagesel	delayMillis
    call	delayMillis
    pagesel$
    movlw	.95
    banksel	forwardSpeed
    movwf	forwardSpeed
    movwf	reverseSpeed
    movwf	upDownSpeed
    
    movlw	.250
    movwf	motorTemp
    banksel	receiveData
    clrf	receiveData
    clrf	transData

mainLoop
    ;9th data bit = LSB of transmission:
    ;banksel	TXSTA
    ;bcf	TXSTA, TX9D
    ;Send Thruster data 
    pagesel	sendThrust
    call	sendThrust
    pagesel$
    
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
;Check for joystick slop
    pagesel	checkSlop
    call	checkSlop
    pagesel$
    banksel	slopFlag
    btfsc	slopFlag, 0
    goto	mainLoop
Displacement
    ;Check whether AN0 or AN1 value is further away from 127
    
;1) Get AN0 displacement from 127
    pagesel	getAn0Disp
    call	getAn0Disp
    pagesel$
    
;2) Get AN1 displacement from 127     
    pagesel	getAn1Disp
    call	getAn1Disp
    pagesel$

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
    movlw	.0		;"forward" state
    movwf	state
    ;Forward PWM (normal value) to thrusters 1 and 2 (top-left and top-right thrusters)
    movfw	ADRESH0
    movwf	ADRESHc
    pagesel	getMotorSpeed
    call	getMotorSpeed
    pagesel$
    movfw	positionSpeed
    banksel	forwardSpeed
    movwf	forwardSpeed
    ;Reverse PWM (calculated value) to thrusters 3 and 4 (bottom left/bottom right thrusters)
    ;get displacement of ADRESH0 (already calculated above)
    movlw	.128		;128 instead of 127 to prevent overflow
    movwf	ADRESHc
    movfw	AN0disp
    subwf	ADRESHc, f	;and subtract displacement from 128 
    pagesel	getMotorSpeed
    call	getMotorSpeed	;to get a reverse PWM value and output it to the
    pagesel$
    movfw	positionSpeed	;reverse logic IC via P2A
    banksel	reverseSpeed
    movwf	reverseSpeed
    goto        mainLoop
    
reverse
    movlw	.1		;"reverse" state
    movwf	state
    ;Reverse PWM (normal value) to thrusters 1/2 (top-left and top-right thrusters)
    movfw	ADRESH0
    movwf	ADRESHc
    pagesel	getMotorSpeed
    call	getMotorSpeed
    pagesel$
    movfw	positionSpeed
    banksel	reverseSpeed
    movwf	reverseSpeed
    ;Forward PWM (calculated value) to thrusters 3/4 (bottom-right and bottom-left thrusters)
    ;get displacement of ADRESH0 (already calculated above)
    movlw	.127		
    movwf	ADRESHc
    movfw	AN0disp
    addwf	ADRESHc, f	;and subtract displacement from 128 
    pagesel	getMotorSpeed
    call	getMotorSpeed	;to get a forward PWM value and output it to the
    pagesel$
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
    movlw	.2		;"traverse right" state
    movwf	state
    ;Forward PWM (normal value) to thrusters 1 and 3 (top-left and bottom-left thrusters)
    movfw	ADRESH1		;send normal PWM value from ADC conversion
    movwf	ADRESHc		;to thrusters 1 and 4 via the forward logic
    pagesel	getMotorSpeed
    call	getMotorSpeed	;IC and through P1A
    pagesel$
    movfw	positionSpeed
    banksel	forwardSpeed
    movwf	forwardSpeed
    ;Reverse PWM (calculated value) to thrusters 2 and 4 (top-right and bottom-right thrusters)
    ;get displacement of ADRESH1 (already calculated above)
    movlw	.128		;128 instead of 127 to prevent overflow
    movwf	ADRESHc
    movfw	AN1disp
    subwf	ADRESHc, f	;and subtract displacement from 128 
    pagesel	getMotorSpeed
    call	getMotorSpeed	;to get a reverse PWM value and output it to the
    pagesel$
    movfw	positionSpeed	;reverse logic IC via P2A
    banksel	reverseSpeed
    movwf	reverseSpeed
    goto	mainLoop
    
traverseLeft
    movlw	.3		;"traverse left" state
    movwf	state
    ;Reverse PWM (normal value) to thrusters 1 and 3 (top-left and bottom-left thrusters)
    movfw	ADRESH1
    movwf	ADRESHc
    pagesel	getMotorSpeed
    call	getMotorSpeed
    pagesel$
    movfw	positionSpeed
    banksel	reverseSpeed
    movwf	reverseSpeed
    ;Forward PWM (calculated value) to thrusters 2 and 4 (top-right and bottom-right thrusters)
    ;get displacement of ADRESH1 (already calculated above)
    movlw	.127		
    movwf	ADRESHc
    movfw	AN1disp
    addwf	ADRESHc, f	;and subtract displacement from 128 
    pagesel	getMotorSpeed
    call	getMotorSpeed	;to get a forward PWM value and output it to the
    pagesel$
    movfw	positionSpeed	;forward logic IC via P1A
    banksel	forwardSpeed
    movwf	forwardSpeed
    
    goto	mainLoop
   
    END                       






































