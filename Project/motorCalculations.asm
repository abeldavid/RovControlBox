; Numerous routines used to perform calculations required to determine motor
;speed and direction
	list	p=16f1937	;list directive to define processor
	#include	<p16f1937.inc>		; processor specific variable definitions
	global      getAn0Disp
	global	getAn1Disp
	global	getMotorSpeed
	global	checkSlop
	global	slopFlag
    
	extern	ADRESH0
	extern	ADRESH1
	extern	AN0disp
	extern	AN1disp
	extern	adcCounter
	extern	compCounter
	extern	positionSpeed
	extern	motorTemp
	extern	ADRESHc
	extern	state
	extern	forwardSpeed
	extern	reverseSpeed
	extern	upDownSpeed
    
	
	errorlevel -302	;no "register not in bank 0" warnings
	errorlevel -312     ;no  "page or bank selection not needed" messages
	errorlevel -207    ;no label after column one warning
	
    GENVAR1	UDATA
    slopFlag	RES	1
    
;******Get the displacement of AN0 analog input (distance from 127)*************
.An0Displacement code
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

;*********Get the displacement of AN1 analog input (distance from 127)**********
.An1Displacement code
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
    
;***************Get the speed of the thruster motor:****************************
.getMotorSpeed code
getMotorSpeed
	clrf	adcCounter
	clrf	compCounter
	clrf	positionSpeed
	movlw	.70		;Start with speed value at max reverse
	movwf	positionSpeed	;motorSpeed will be placed in CCPRxL
startAdding
    ;make sure adcCounter isn't < 5 away from 255
	movfw	adcCounter
	subwf	motorTemp, w		;value in temp is 250
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
;**************Account for "slop" in fwd/rev and lt/rt potentiometers***********   
.checkSlop code
checkSlop
	banksel	slopFlag
	clrf	slopFlag
    ;Account for "slop" in fwd/rev and left/right potentiometers
;check if greater than 160    
checkFRslop
	movlw	.160
	subwf	ADRESH0, w
	btfsc	STATUS, C	;(C=0 is neg #)
	retlw	0		;value > 160 (go to rotation section)
    ;not greater than 160, check if less than 100
	movlw	.90
	subwf	ADRESH0, w
	btfss	STATUS, C	;(C=0 is neg #)
	retlw	0		;value < 126 (go to rotation section)
checkLRslop
	movlw	.160
	subwf	ADRESH1, w
	btfsc	STATUS, C	;(C=0 is neg #)
	retlw	0		;value > 128 (go to rotation section)
    ;not greater than 128, check if less than 126
	movlw	.90
	subwf	ADRESH1, w
	btfss	STATUS, C	;(C=0 is neg #)
	retlw	0		;value < 126 (go to rotation section)
    ;Stop all thrusters (neutral joystick position)
	banksel	slopFlag
	bsf		slopFlag, 0	;Set flag to indicate slop in joystick
	movlw	.7		;"stop" state
	movwf	state
	movlw	.95		;1500uS pulse width
	banksel	forwardSpeed		
	movwf	forwardSpeed
	movwf	reverseSpeed
	movwf	upDownSpeed
    
	retlw	0
    
	END