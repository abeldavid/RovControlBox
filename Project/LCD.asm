    ;LCD Routines


    list	p=16f1937	;list directive to define processor
    #include	<p16f1937.inc>		; processor specific variable definitions
    
    errorlevel -302	;no "register not in bank 0" warnings
    errorlevel -312     ;no  "page or bank selection not needed" messages
    errorlevel -207    ;no label after column one warning
    
    extern  delayMillis
    
    global  LCDInit
    global  displayHeaders
    global  sendData
    global  sendCommand
    global  ESCinitializing
    
GENVAR		UDATA_SHR
	
;Set command mode
.lcd code
RS0
    banksel	PORTB
    bcf		PORTB, 2	;PORTB, 2/RS=0
    retlw	0
;Set character mode
RS1
    banksel	PORTB
    bsf		PORTB, 2	;PORTB, 2/RS=1
    retlw	0
;Pulse E (enable)
ePulse
    banksel	PORTB
    bsf		PORTB, 1		;take E line higH
    nop	
    nop					;hold for 3 clock cycles
    nop
    nop
    nop
    nop
    bcf		PORTB, 1		;take E line low
    retlw	0
	
;Send a command to LCD (command is already in work register)
sendCommand
    banksel	PORTD
    movwf	PORTD	    ;send command to PORTB
    call	RS0	    ;Enter command mode
    call	ePulse	    ;pulse E line
    movlw	.2
    call	delayMillis
    retlw	0
	
;Send character data to LCD (data is already in work register)
sendData
    banksel	PORTD
    movwf	PORTD	    ;send character to PORTB
    call	RS1	    ;Enter character mode
    call	ePulse	    ;pulse E line
    movlw	.2
    call	delayMillis
    retlw	0	

;***************Initialize LCD**************************************************
LCDInit
;25ms startup delay
    movlw	.50
    call	delayMillis

    movlw	b'00111000'	;user-defined "function set" command
    call	sendCommand
    
	;confirm entry mode set
    movlw	b'00000110'	;increment cursor, display shift off
    call	sendCommand
	
	;Display on, cursor on, blink on
    movlw	b'00001111'
    call	sendCommand
	
    ;LCD is now active and will display any data printed to DDRAM
    ;Clear display and reset cursor
    movlw	b'00000001'
    call	sendCommand

    retlw	0   

;***************************Display data***************************************
displayHeaders
    ;Clear display and reset cursor
    movlw	b'00000001'
    call	sendCommand
    movlw	.20
    call	delayMillis
pressure
    movlw	b'01010100'
    call	sendData
    movlw	b'01100101'
    call	sendData
    movlw	b'01101101'
    call	sendData
    movlw	b'01110000'
    call	sendData
    movlw	b'00111010'
    call	sendData
;Set display address to beginning of second line
    movlw	b'11000000'
    call	sendCommand

temp
    movlw	b'01010000'
    call	sendData
    movlw	b'01110010'
    call	sendData
    movlw	b'01100101'
    call	sendData
    movlw	b'01110011'
    call	sendData
    movlw	b'01110011'
    call	sendData
    movlw	b'00111010'
    call	sendData

    retlw	0
ESCinitializing
    movlw	b'01001001'	;I
    call	sendData
    movlw	b'01001110'	;N
    call	sendData
    movlw	b'01001001'	;I
    call	sendData
    movlw	b'01010100'	;T
    call	sendData
    movlw	b'01001001'	;I
    call	sendData
    movlw	b'01000001'	;A
    call	sendData    
    movlw	b'01001100'	;L
    call	sendData
    movlw	b'01001001'	;I
    call	sendData
    movlw	b'01011010'	;Z
    call	sendData
    movlw	b'01001001'	;I
    call	sendData
    movlw	b'01001110'	;N
    call	sendData
    movlw	b'01000111'	;G
    call	sendData
    movlw	b'0010000'	;SPACE
    call	sendData
    movlw	b'01000101'
    call	sendData
    movlw	b'01010011'
    call	sendData
    movlw	b'01000011'
    call	sendData
    movlw	b'01110011'
    call	sendData
    movlw	b'00101110'
    call	sendData
    movlw	b'00101110'
    call	sendData
    movlw	b'00101110'
    call	sendData
    
    
    
    retlw	0
    
    END