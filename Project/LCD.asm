    ;LCD Routines


    list	p=16f1937	;list directive to define processor
    #include	<p16f1937.inc>		; processor specific variable definitions
    
    errorlevel -302	;no "register not in bank 0" warnings
    errorlevel -207    ;no label after column one warning
    
    extern  delayMillis
    extern  stringIndex1
    extern  stringIndex2
    
    global  LCDInit
    global  sendData
    global  sendCommand
    global  displayHeaders
    
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
    bcf		PORTB, 1		;take E line low
    retlw	0
	
;Send a command to LCD (command is already in work register)
sendCommand
    banksel	PORTD
    movwf	PORTD	    ;send command to PORTB
    call	RS0	    ;Enter command mode
    call	ePulse	    ;pulse E line
    movlw	.2
    pagesel	delayMillis
    call	delayMillis
    pagesel$
    retlw	0
	
;Send character data to LCD (data is already in work register)
sendData
    banksel	PORTD
    movwf	PORTD	    ;send character to PORTB
    call	RS1	    ;Enter character mode
    call	ePulse	    ;pulse E line
    movlw	.2
    pagesel	delayMillis
    call	delayMillis
    pagesel$
    retlw	0	
    
getString1
    movlw	    LOW string1
    banksel	    stringIndex1
    addwf	    stringIndex1, w
    btfsc	    STATUS, C
    incf	    PCLATH, f
    movwf	    PCL
string1	dt	    "Ensure control-box ", 0
	
getString2
    movlw	    LOW string2
    banksel	    stringIndex2
    addwf	    stringIndex2, w
    btfsc	    STATUS, C
    incf	    PCLATH, f
    movwf	    PCL
string2	dt	    "turned on first !!", 0

;***************Initialize LCD**************************************************
LCDInit
    banksel	stringIndex1	    ;Clear out string index counter
    clrf	stringIndex1
    clrf	stringIndex2
;25ms startup delay
    movlw	.50
    pagesel	delayMillis
    call	delayMillis
    pagesel$

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
    call	ESCinitializing
    
    ;Display connection info message
    ;Set display address to beginning of 3rd line
    movlw	0x94
    call	sendCommand
    ;get next character from string:
s1
    call	getString1	;display first name
    ;finish if null
    iorlw	0
    btfsc	STATUS, Z	;end of string?
    goto	s2		;if end of string quit looping
	;Write character to display:
    call	sendData	;write char
    movlw	.1
    call	delayMillis	;delay
    banksel	stringIndex1
    incf	stringIndex1	;increment string index to point to next char
    goto	s1
s2
    ;Set display address to beginning of 4th line
    movlw	0xD4
    call	sendCommand
dispS2
    call	getString2	;display first name
    ;finish if null
    iorlw	0
    btfsc	STATUS, Z	;end of string?
    goto	lcdDone  	;if end of string quit looping
    ;Write character to display:
    call	sendData	;write char
    movlw	.1
    call	delayMillis	;delay
    banksel	stringIndex2
    incf	stringIndex2	;increment string index to point to next char
    goto	dispS2
lcdDone
    
    retlw	0   

;***************************Display data***************************************
displayHeaders
    ;Clear display and reset cursor
    movlw	b'00000001'
    call	sendCommand
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
    movlw	0xC0
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
   
;Display "Initializing ESC"
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