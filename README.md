This program is one of two designed to control an underwater ROV.  Specifically, this program is written for
a "control box" device which contains an analog joystick and is used to alter the direction and depth of 
the under-water ROV.  The microcontroller used is a PIC16F1937.

The ROV in use is a 6-thruster variant and is of a "vectored" thrust design.  There is a thruster at each 
corner of the vehicle to control horizontal movement as well as two more thrusters pointed up/down 
to control depth.

The analog voltage of the joystick potentiometers are continuously sampled and converted to digital values 
which will be placed in the CCPRXL registers of the receiving PIC16F1937.  The MCU and circuitry of the
receiving device takes these values and sends an appropriate PWM value to the ESCs of each thruster.

The data is transmitted in 8-bit data packets via the PIC16F1937 UART module.


