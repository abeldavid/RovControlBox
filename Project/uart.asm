; UART transmit and receive subroutines as well as sendThrust soubroutine to 
;send data packets containing thruster speed/direction data
    list	p=16f1937	;list directive to define processor
    #include	<p16f1937.inc>		; processor specific variable definitions
    global      Transmit
    global	Receive
    global	sendThrust
    extern	transData
    extern	receiveData
    extern	state
    extern	forwardSpeed
    extern	reverseSpeed
    extern	upDownSpeed
    extern	delayMillis
	
    errorlevel -302	;no "register not in bank 0" warnings
    errorlevel -312     ;no  "page or bank selection not needed" messages
    errorlevel -207    ;no label after column one warning
    
;********************Transmit UART data packets*********************************
.transmit code
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

;*******************Receive UART data packets***********************************
.receive code
 Receive
    banksel	PIR1
wait_receive
    btfss	PIR1, RCIF	;Is RX buffer full? (1=full, 0=notfull)
    goto	wait_receive	;wait until it is full
    banksel	RCSTA
    bcf		RCSTA, CREN
    banksel	RCREG
    movfw	RCREG		;Place data from RCREG into "receiveData"
    banksel	receiveData
    movwf	receiveData
    banksel	PIR1
    bcf	        PIR1, RCIF	    ;clear UART receive interrupt flag
    banksel	RCSTA
    bsf		RCSTA, CREN
    retlw	0

;**************Send thruster speed/direction data via UART**********************
.sendThrust code
;Send thruster data
sendThrust
    movfw	state
    movwf	transData
    pagesel	Transmit
    call	Transmit
    pagesel$
    movlw	.5
    pagesel	delayMillis
    call	delayMillis
    pagesel$
    
    banksel	forwardSpeed
    movfw	forwardSpeed
    movwf	transData
    pagesel	Transmit
    call	Transmit
    pagesel$
    movlw	.5
    pagesel	delayMillis
    call	delayMillis
    pagesel$
    
    banksel	reverseSpeed
    movfw	reverseSpeed
    movwf	transData
    pagesel	Transmit
    call	Transmit
    pagesel$
    movlw	.5
    pagesel	delayMillis
    call	delayMillis
    pagesel$
    
    
    banksel	upDownSpeed
    movfw	upDownSpeed
    movwf	transData
    pagesel	Transmit
    call	Transmit
    pagesel$
    movlw	.50
    pagesel	delayMillis
    call	delayMillis
    pagesel$
    
    retlw	0
    
    
    END


