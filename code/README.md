# Zilog Z80 Curiosity Board #2 - The KIO Version : Software

## The Z80 Monitor 
The MONZ80-RAM.ASM file contains a modified version of the excellent Z80 monitor called MONZ80 written by [Dave Dunfield](https://dunfield.themindfactory.com/). You can find the source code to the Z80 monitor along with monitors for other micros in the MONITORS.ZIP file on Dave's website. The monitor code needs a slight tweek to reflect the addresses of the SIO registers within the KIO chip. There's additional code to setup the KIO, handle the copying of the ROM into RAM and the disabling of the ROM.

MONZ80 can be assembled using the free online assembler at [ASM80.COM](https://asm80.com) - select the Z80 CPU.

I've not managed to locate a user manual for MONZ80 so I have included the user manual for MON85 as a guide to MONZ80.

# The Monitor and BASIC v4.7

## The Monitor (monitor.asm)

The simple monitor program is essentially Grants monitor program but modified to take into account the different addresses of the CompactFlash and SIO registers in my design. The monitor also copies itself into RAM and disables the ROM. The baud rate is also set to 57600 baud (rather than 115200 baud).

The main change is the use of SIO channel B. My Z80 KIO curiosity board doesn't have a hard wired clock into the SIO channel B TxCLKB & RxCLKB pins. Without a clock on TxCLKB, any byte in the TxB register will stay there forever and any code that waits for the Tx Buffer Empty bit to clear will hang forever.

As a result of this, the code expects a terminal connection on SIO channel A. I've added a constant called USE_SIOB which can be set to 0 to ignore SIO channel B or set to a 1 to also use SIO channel B. Setting USE_SIOB to a 1 will let the code detect a terminal on either SIO channel.

There are also some hard coded values that tell the monitor where to find BASIC. I've added a new value called BASSIZE which is the rough amount of ROM space that BASIC occupies. It is only needed when copying BASIC from ROM to RAM and I've set it to $2000.
 
## BASIC v4.7 (basic.asm)

The banner for this version of BASIC says that it is Z80 BASIC Ver 4.7b (c) 1978 by Microsoft. The assembler source has the phrase "NASCOM ROM BASIC Ver 4.7, (C) 1978 Microsoft".

I've modified this version of BASIC so that it can be assembled using ASM80. Other than that, the code is identical.

The Intel-Hex file can be programmed into the ROM at the same time as the monitor or it can be loaded into RAM using the monitor. 

# Installing CP/M

These are my notes for loading CP/M onto the board, which are almost identical to Grants instructions and include any modifications I may have done to his code.

## Step 1 - Formatting the CompactFlash drive

The formatting of the CompactFlash drive is done using the file FORM128.ASM (for 128Mb or larger CompactFlash cards). I've modified the CompactFlash registers to suit my KIO board and changed the directives so that the code can be assembled using ASM80.

Use the monitor to load the FORM128.HEX file and type G5000 to execute the code. The formatter should report back that it has created and formatted drives A to P.

## Step 2 - Installing CP/M

Installing CP/M involves 3 separate files: CP/M itself (CPM22.ASM), the CP/M BIOS (CBIOS128.ASM) and a small program to copy CP/M and the CP/M BIOS onto the CompactFlash card (PUTSYS.ASM).

* The CPM22.ASM file has been edited to change the directives so that the code can be assembled using ASM80.
* The CBIOS128.ASM file has been modified so the CompactFlash registers and SIO registers are correct for my KIO board. Directives have also been altered so that the code can be assembled using ASM80.
* The PUTSYS.ASM file has been modified so the CompactFlash registers are correct for my KIO board. Directives have also been altered so that the code can be assembled using ASM80.

Once the 3 files have been assembled, load each in turn from the monitor prompt. Then type G5000 to execute the PUTSYS program. This will copy CP/M and the CBIOS onto the CompactFlash card.

CP/M is now installed on the CompactFlash card. Reset the board and from the monitor prompt type X to boot CP/M, answering Y to the Boot CP/M prompt.

## Step 3 - Installing the downloader.

In order to transfer files to the CP/M system, a small application called DOWNLOAD.COM needs to be installed on the CP/M A: drive.

First, load CP/M off the CompactFlash card (X then Y in the monitor) and then reset the board. CP/M and the CBIOS are still in memory.

Download the file DOWNLOAD2.HEX. This file will be loaded into memory at address $4100. Then type GFFE8. This will execute a small piece of code that is part of the CBIOS. It will copy DOWNLOAD2.HEX from $4100 to $0100 and the run CP/M that is already in memory.

When the A> prompt appears, type SAVE 2 DOWNLOAD.COM. This will save 2 pages (2x 256 byte blocks, or 512 bytes) of data starting at address $0100 in memory to the file DOWNLOAD.COM on the A: drive.

At the A> prompt, you can type DIR <enter> to get a directory listing of drive A. You should see the file DOWNLOAD.COM.

## Step 4 - Transferring files to the CP/M system

You can read up on Grant Seales web page on how to use his packager program.

Before transferring the packaged files to the CP/M system, make sure that your terminal is set to use RTS/CTS handshaking as the serial routines in the CBIOS will use hardware handshaking to control the flow of data.

With CP/M running, download the file pkgCPM211Files.txt. This file is a package containing several CP/M programs: LOAD.COM, PIP.COM, STAT.COM, SUBMIT.COM, DDT.COM, DISPLAY.COM, DUMP.COM, ED.COM and ASM.COM. Most of these programs are detailed in the CP/M Operating System Manual.

You can also download the file pkgMBASIC.txt. This file is the CP/M version of BASIC-80 v5.21 by Microsoft.
