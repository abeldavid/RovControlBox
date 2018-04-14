;Code for leak detector and ESC ready indicator light
    
    list	p=16f1937	;list directive to define processor
    #include	<p16f1937.inc>		; processor specific variable definitions
    global      Leak
    global	ESCready
	
    errorlevel -302	;no "register not in bank 0" warnings
    errorlevel -207    ;no label after column one warning
	
    
.indicator code 
Leak
    banksel	PORTC
    bsf		PORTC, 0
    movlw       .25              
    banksel	CCPR1L
    movwf       CCPR1L          ; -> PWM duty cycle = 18%
    retlw	0
    
ESCready
    banksel	PORTC
    bsf		PORTC, 1
    retlw	0
    
    
    END


