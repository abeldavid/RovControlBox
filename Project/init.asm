    ;*********************INITILIZE PERIPHERALS*********************************
    
    list	    p=16f1937	   ;list directive to define processor
    #include    <p16f1937.inc>	   ;processor specific variable definitions
    
    errorlevel -302	;no "register not in bank 0" warnings
    errorlevel -207    ;no label after column one warning
    
    global	peripheralInit
    extern	delayMillis
    extern	motorTemp
    extern	transData
    extern	forwardSpeed
    extern	reverseSpeed
    extern	upDownSpeed
    extern	receiveData
    extern	slopFlag
    extern	LCDInit
    
    #define BANK0  (h'000')
    #define BANK1  (h'080')
    #define BANK2  (h'100')
    #define BANK3  (h'180')
    
.initialization code
peripheralInit
    banksel BANK1
    ;************************Configure PORTS************************************
    movlw   b'00000011'		
    movwf   (TRISA ^ BANK1)
    movlw   b'11111001'		    
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
	
    ;4Mhz external crystal:
    movlw	b'00000000'
    banksel	OSCCON
    movwf	OSCCON
    
    ;*************************Enable interrupts*********************************
    movlw	b'11000000'
	         ;1-------	;Enable global interrupts (GIE=1)
		 ;-1------	;Enable peripheral interrupts (PEIE=1)
		 ;--0-----	;Disable TMR0 interrupts (TMROIE=0)
		 ;---0----	;Disable RBO/INT external interrupt (INTE=1)
		 ;----0---	;Disable interrupt on change for PORTB (IOCIE=0)
				;PORTB IOC will be enabled after ESC init
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
    banksel	ANSELB
    clrf	ANSELB
    clrf	ANSELD
    clrf	ANSELE
    ;**************************Initialize LCD***********************************
    pagesel	LCDInit
    call	LCDInit
    pagesel$
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
    clrf	slopFlag
    
    ;***************************************************************************
    movlw	b'00100000'
		 ;--1-----	;Enable USART receive interrupt (RCIE=1)
    banksel	PIE1
    movwf	PIE1
    
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
    banksel	motorTemp
    movwf	motorTemp
    clrf	receiveData
    clrf	transData
    
    
    
    retlw	   0


    END