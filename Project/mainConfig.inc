    ;config file for main.asm
    
    errorlevel -302	;no "register not in bank 0" warnings
    errorlevel -207    ;no label after column one warning
    
    extern	delayMillis	;delayMillis subroutine (external file)
    extern	ESCready	;routine to set ESC ready light
    extern	Leak		;routine to set Leak warning light
    extern	Delay16Us	;16uS delay for ADC conversions
    extern	Transmit	;UART transmit subroutine
    extern	Receive		;UART receive subroutine
    extern	sendThrust	;routine to send thruster data via UART
    extern	getAn0Disp	;displacement from "dead-center" for AN0
    extern	getAn1Disp	;displacement from "dead-center" for AN1
    extern	getMotorSpeed	;routine to determine correct speed of motor
    extern	checkSlop
    extern	peripheralInit
    extern	motors
    extern	Reception
    
    global	transData	;UART transmit variable
    global	receiveData	;UART receive variable
    global	state		;desired directional "state" of ROV
    global	forwardSpeed	;forward speed variable
    global	reverseSpeed	;reverse speed variable
    global	upDownSpeed	;depth control variable
    global	ADRESH0		;copy of ADRESH for AN0
    global	ADRESH1		;copy of ADRESH for AN1
    global	ADRESHc
    global	AN0disp
    global	AN1disp
    global	adcCounter
    global	compCounter
    global	positionSpeed
    global	motorTemp
    global	slopFlag
    global	userMillis
    global	dly16Ctr

    #define BANK0  (h'000')
    #define BANK1  (h'080')
    #define BANK2  (h'100')
    #define BANK3  (h'180')

    __CONFIG _CONFIG1,    _MCLRE_ON & _CP_OFF & _CPD_OFF & _BOREN_OFF & _WDTE_OFF & _PWRTE_ON & _FOSC_ECH & _FCMEN_OFF & _IESO_OFF
    
    ;Variables accessible in all banks:
    MULTIBANK	    UDATA_SHR
    userMillis	    RES	1
    adcCounter	    RES	1	;counter to be increented till value in
				;ADRESH is reached
    ADRESHc	    RES	1	;copy of ADRESH
    ADRESH0	    RES	1	;copy of value from pin AN0
    ADRESH1	    RES	1	;copy of value from pin AN1
    ADRESH2	    RES	1	;copy of value from pin AN2
    AN0disp	    RES	1	;displacement of ADRESHO from 127
    AN1disp	    RES	1	;displacement of ADRESH1 from 127
    
    

    ;General Variables
    GENVAR1	    UDATA
    w_copy	    RES 1	;variable used for context saving (work reg)
    status_copy	    RES 1	;variable used for context saving (status reg)
    pclath_copy	    RES 1	;variable used for context saving (pclath copy)
    transData	    RES	1	;Data to be transmitted via UART
    receiveData	    RES	1	;Data received via UART
    positionSpeed   RES	1	;value returned from getMotorSpeed routine
				;to be placed in forward, reverse and upDown speeds
    forwardSpeed    RES	1	;Forward value for CCPR1L
    reverseSpeed    RES	1	;Reverse value for CCPR2L
    upDownSpeed	    RES	1	;CCPR3L value for up/down thrusters
    slopFlag	    RES	1
    dly16Ctr	    RES	1
    motorTemp	    RES	1
    compCounter	    RES	1	;counter to be incremented once every 6 servo
				;steps to give full range of motion
    state	    RES	1	;desired directional "state" of ROV


   