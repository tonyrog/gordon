# Flash Raspberry pi application

This application run on a raspberry pi ( any version will do )

The program will flash a file using lpc21isp when a button,
wired to pin 17 is pressed ( = 1 )

While lpc21isp is running the green button is flashing ( pin 27)
and if the flash process is succeful it will fo into a steady green.
On the other hand if the flash process failed then a the steady 
red led ( pin 7 ) will glow.

The file is currently called BootLoader.hex and should be stored on 
a memory stick called "GORDON".

