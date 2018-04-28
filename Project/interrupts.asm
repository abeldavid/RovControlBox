;Interrupt service routines
    list	p=16f1937	;list directive to define processor
    #include	<p16f1937.inc>		; processor specific variable definitions
    
    errorlevel -302	;no "register not in bank 0" warnings
    errorlevel -207    ;no label after column one warning
    
    global	motors
    global	Reception
    
    extern	forwardSpeed
    extern	forwardSpeed	;forward speed variable
    extern	reverseSpeed	;reverse speed variable
    extern	positionSpeed
    extern	upDownSpeed	;depth control variable
    extern	getMotorSpeed
    extern	sendThrust
    extern	state
    extern	ADRESH0		;copy of ADRESH for AN0
    extern	ADRESH1		;copy of ADRESH for AN1
    extern	ADRESHc
    extern	AN0disp
    extern	AN1disp
    extern	getAn0Disp	;displacement from "dead-center" for AN0
    extern	getAn1Disp	;displacement from "dead-center" for AN1
    extern	Delay16Us	;16uS delay for ADC conversions
    extern	checkSlop
    extern	slopFlag
    extern	Receive
    extern	receiveData
    extern	ESCready
    extern	Leak
    
    .intrpt code
;*******PortB interrupt on change for joystick push-button switch **************
motors    
    banksel	PORTB
    movfw	PORTB		;clear mismatch
	
    bcf	        INTCON, IOCIF	;clear flag 
    banksel	IOCBF
    clrf	IOCBF
	
;stop motors
    movlw	.7		;"stop" state
    banksel	state
    movwf	state
    movlw	.95
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
    pagesel	Delay16Us
    call	Delay16Us
    pagesel$
    pagesel	Delay16Us
    call	Delay16Us
    pagesel$
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
    pagesel$
    pagesel	Delay16Us
    call	Delay16Us
    pagesel$
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
    banksel	state
    movwf	state
    ;Forward PWM (normal value) to thrusters 1 and 4 (top-left and bottom-right thrusters)
    movfw	ADRESH1		;send normal PWM value from ADC conversion
    movwf	ADRESHc		;to thrusters 1 and 4 via the forward logic
    pagesel	getMotorSpeed
    call	getMotorSpeed	;IC and through P1A of receiving device
    pagesel$
    banksel	positionSpeed
    movfw	positionSpeed
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
    banksel	positionSpeed
    movfw	positionSpeed	;reverse logic IC via P2A of receiving device
    movwf	reverseSpeed
    goto	endMotors
    
CCW ;(Counter-clockwise rotation):
    movlw	.5		;"counterclockwise-rotation" state
    banksel	state
    movwf	state
    ;Reverse PWM (normal value) to thrusters 1 and 4 (top-left and bottom-right thrusters)
    movfw	ADRESH1
    movwf	ADRESHc
    pagesel	getMotorSpeed
    call	getMotorSpeed
    pagesel$
    banksel	positionSpeed
    movfw	positionSpeed
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
    banksel	positionSpeed
    movfw	positionSpeed	;forward logic IC via P1A
    movwf	forwardSpeed
    goto	endMotors
	
depth
    movlw	.6		;"up/down" state
    banksel	state
    movwf	state
;use value from AN0 to get speed for dive/surface thrusters:
    movfw	ADRESH0
    movwf	ADRESHc
    pagesel	getMotorSpeed
    call	getMotorSpeed
    pagesel$
    banksel	positionSpeed
    movfw	positionSpeed
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
    banksel	state
    movwf	state
    movlw	.95
    banksel	forwardSpeed
    movwf	forwardSpeed
    movwf	reverseSpeed
    movwf	upDownSpeed
    pagesel	sendThrust
    call	sendThrust
    pagesel$
    retlw	0
;*************End PortB interrupt on change for joystick PB switch**************

;****************************UART Reception Interrupt***************************
Reception
    pagesel	Receive
    call	Receive
    pagesel$
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
    retlw	0
    pagesel	ESCready
    call	ESCready
    pagesel$
    retlw	0
LeakDetected
    banksel	PIE1
    bcf	        PIE1, RCIE	 ;disable UART receive interrupts
    pagesel	Leak
    call	Leak
    pagesel$
    
    retlw	0

    END