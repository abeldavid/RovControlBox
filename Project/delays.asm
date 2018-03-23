 ;Variable length delay subroutine (1-255 milliseconds) and 16 microsecond
 ;delay routine for ADC conversions
    
	list	p=16f1937	;list directive to define processor
	#include	<p16f1937.inc>		; processor specific variable definitions
	global      delayMillis
	global	Delay16Us
	
	errorlevel -302	;no "register not in bank 0" warnings
	errorlevel -312     ;no  "page or bank selection not needed" messages
	errorlevel -207    ;no label after column one warning
    
MULTIBANK	UDATA_SHR
userMillis	RES	1
	
GENVAR1		UDATA
dly16Ctr	RES	1
	
.delay code    
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

.usDelay code
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


	END


