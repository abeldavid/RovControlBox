;Code for leak detector and ESC ready indicator light
    
    list	p=16f1937	;list directive to define processor
    #include	<p16f1937.inc>		; processor specific variable definitions
    global      Leak
    global	ESCready
	
    errorlevel -302	;no "register not in bank 0" warnings
    errorlevel -312     ;no  "page or bank selection not needed" messages
    errorlevel -207    ;no label after column one warning
	
    #define BANK0  (h'000')
    #define BANK1  (h'080')
    #define BANK2  (h'100')
    #define BANK3  (h'180')
    
.leak code 
Leak
    banksel	PORTC
    bsf		PORTC, 0
    movlw       .25              
    banksel	CCPR1L
    movwf       CCPR1L          ; -> PWM duty cycle = 18%
    retlw	0
    
.ESCready code
ESCready
    banksel	PORTC
    bsf		PORTC, 1
    retlw	0
    
    
    END


