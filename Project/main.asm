;Convert analog signal from joystick to PWM values to be sent to 6 thrusters
;via UART

	list	p=16f1937	   ;list directive to define processor
	#include	<p16f1937.inc>	   ;processor specific variable definitions
	#include    <mainConfig.inc>
    
	errorlevel -302	;no "register not in bank 0" warnings
	errorlevel -312     ;no  "page or bank selection not needed" messages
	errorlevel -207    ;no label after column one warning
   
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
	call	displayHeaders
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
    
    ;Check for joystick slop
	pagesel	checkSlop
	call	checkSlop
	pagesel$
	banksel	slopFlag
	btfsc	slopFlag, 0
	goto	endMotors	;slop, so exit


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
	call	peripheralInit	    ;initialize peripherals
	
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
	goto	mainLoop	;slop, so reloop
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






































