# macseadb88

Drop-in replacement for the Macintosh SE's stock ADB controller.  Rough adaptation of original PIC1654S code.

## Status

The adapted binary is known to work on the DIP packaged PIC16F87 and PIC16F88 as a drop-in replacement for the Macintosh SE's ADB controller.  With an adapter and other packages, it can work on other Macintoshes as well.

My annotation of the disassembled code is only partially complete.  Anyone who wants to pick up the effort from here is more than welcome, as I may never get to it.  :)

## Caveats

The PIC1654S executed one instruction cycle to eight clock ticks, the PIC16F88/87 execute one instruction cycle to four clock ticks, meaning that the external clock cannot be used without extensive changes to the original code.  As such, the adapted code uses the PIC16F88/87's internal oscillator at 2 MHz, which winds up being slightly faster than the PIC1654S driven at ~3.6 MHz.  If this seems to be causing trouble in your setup, see [this](https://github.com/lampmerchant/macseadb88/issues/1) issue for a potential solution.

## Download

[Click here.](https://github.com/lampmerchant/macseadb88/releases/download/20230122/macseadb88.HEX)
