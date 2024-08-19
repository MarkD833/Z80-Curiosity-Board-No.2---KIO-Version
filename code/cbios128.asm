;==================================================================================
; Contents of this file are copyright Grant Searle
; Blocking/unblocking routines are the published version by Digital Research
; (bugfixed, as found on the web)
;
; You have permission to use this for NON COMMERCIAL USE ONLY
; If you wish to use it elsewhere, please include an acknowledgement to myself.
;
; http://searle.hostei.com/grant/index.html
;
; eMail: home.micros01@btinternet.com
;
; If the above don't work, please perform an Internet search to see if I have
; updated the web page hosting service.
;
;==================================================================================

; 2024-08-06 MJD
; - Modified CF card & SIO addresses for my Z80 KIO Curiosity project
; - Modified to assemble online using ASM80.COM
; - SIO_A & SIO_B are configured for 57600 baud
; - Additional comments as I learned what the code did
;
; Documentation references are to sections/pages/tables in the
; CP/M 2.2 Alteration Guide 1979 which can be found here (and other places too):
; http://bitsavers.informatik.uni-stuttgart.de/pdf/digitalResearch/cpm/2.2/CPM_2.2_Alteration_Guide_1979.pdf
;
ccp			EQU	0D000h		; Base of CCP.
bdos		EQU	ccp + 0806h	; Base of BDOS.
bios		EQU	ccp + 1600h	; Base of BIOS.

; Set CP/M low memory data, vector and buffer addresses.

iobyte		EQU	03h		; Intel standard I/O definition byte.
userdrv		EQU	04h		; Current user number and drive.
tpabuf		EQU	80h		; Default I/O buffer and command line storage.


SER_BUFSIZE	EQU	60
SER_FULLSIZE	EQU	50
SER_EMPTYSIZE	EQU	5

; MJD The RTS setting also sets DTR LOW + Tx 8 bits + Tx Enable
RTS_HIGH	EQU	0E8H
RTS_LOW		EQU	0EAH

; MJD SIO subsystem of the Zilog KIO starts at $88
SIOA_D		EQU	$88
SIOA_C		EQU	$89
SIOB_D		EQU	$8A
SIOB_C		EQU	$8B

int38		EQU	38H
nmi			EQU	66H

blksiz		EQU	4096		;CP/M allocation size
hstsiz		EQU	512			;host disk sector size
hstspt		EQU	32			;host disk sectors/trk
hstblk		EQU	hstsiz/128	;CP/M sects/host buff
cpmspt		EQU	hstblk * hstspt	;CP/M sectors/track
secmsk		EQU	hstblk-1	;sector mask
							;compute sector mask
;secshf		EQU	2		;log2(hstblk)

wrall		EQU	0			;write to allocated
wrdir		EQU	1			;write to directory
wrual		EQU	2			;write to unallocated

; MJD CompactFlash I/O addresses on my Z80 KIO Curiosity board start at $90
; CF registers
CF_DATA		EQU	$90			; Data (R/W)
CF_FEATURES	EQU	$91			; Features (W)
CF_ERROR	EQU	$91			; Error(R)
CF_SECCOUNT	EQU	$92			; Sector Count (R/W)
CF_SECTOR	EQU	$93			; Sector Number (R/W)
CF_CYL_LOW	EQU	$94			; Cylinder Low Byte (R/W)
CF_CYL_HI	EQU	$95			; Cylinder High Byte (R/W)
CF_HEAD		EQU	$96			; Drive / Head (R/W)
CF_STATUS	EQU	$97			; Status (R)
CF_COMMAND	EQU	$97			; Command (W)
CF_LBA0		EQU	$93			; LBA bits 07..00 (R/W)
CF_LBA1		EQU	$94			; LBA bits 15..08 (R/W)
CF_LBA2		EQU	$95			; LBA bits 23..16 (R/W)
CF_LBA3		EQU	$96			; LBA bits 27..24 (R/W)

;****************************************************************
; CompactFlash Features
CF_8BIT		EQU	1
CF_NOCACHE	EQU	082H

;****************************************************************
; CompactFlash Commands
CF_RD_SEC	EQU	020H
CF_WR_SEC	EQU	030H
CF_SET_FEAT	EQU 0EFH

LF			EQU	0AH		;line feed
FF			EQU	0CH		;form feed
CR			EQU	0DH		;carriage RETurn

;================================================================================================

		ORG	bios		; BIOS origin

;================================================================================================
; BIOS jump table.
;================================================================================================
		JP	boot		;  0 Initialize
wboote:
		JP	wboot		;  1 Warm boot
		JP	const		;  2 Console status
		JP	conin		;  3 Console input
		JP	conout		;  4 Console output
		JP	list		;  5 List output
		JP	punch		;  6 Punch output
		JP	reader		;  7 Reader input
		JP	home		;  8 Home disk
		JP	seldsk		;  9 Select disk
		JP	settrk		; 10 Select track
		JP	setsec		; 11 Select sector
		JP	setdma		; 12 Set DMA address
		JP	read		; 13 Read 128 bytes
		JP	write		; 14 Write 128 bytes
		JP	listst		; 15 List status
		JP	sectran		; 16 Sector translate

;================================================================================================
; Disk parameter headers for disk 0 to 15
;================================================================================================
dpbase:
	 	DW 0000h,0000h,0000h,0000h,dirbuf,dpb0,0000h,alv00
	 	DW 0000h,0000h,0000h,0000h,dirbuf,dpb,0000h,alv01
	 	DW 0000h,0000h,0000h,0000h,dirbuf,dpb,0000h,alv02
	 	DW 0000h,0000h,0000h,0000h,dirbuf,dpb,0000h,alv03
	 	DW 0000h,0000h,0000h,0000h,dirbuf,dpb,0000h,alv04
	 	DW 0000h,0000h,0000h,0000h,dirbuf,dpb,0000h,alv05
	 	DW 0000h,0000h,0000h,0000h,dirbuf,dpb,0000h,alv06
	 	DW 0000h,0000h,0000h,0000h,dirbuf,dpb,0000h,alv07
	 	DW 0000h,0000h,0000h,0000h,dirbuf,dpb,0000h,alv08
	 	DW 0000h,0000h,0000h,0000h,dirbuf,dpb,0000h,alv09
	 	DW 0000h,0000h,0000h,0000h,dirbuf,dpb,0000h,alv10
	 	DW 0000h,0000h,0000h,0000h,dirbuf,dpb,0000h,alv11
	 	DW 0000h,0000h,0000h,0000h,dirbuf,dpb,0000h,alv12
	 	DW 0000h,0000h,0000h,0000h,dirbuf,dpb,0000h,alv13
	 	DW 0000h,0000h,0000h,0000h,dirbuf,dpb,0000h,alv14
	 	DW 0000h,0000h,0000h,0000h,dirbuf,dpbLast,0000h,alv15

; First drive has a reserved track for CP/M
dpb0:
		DW 128 ;SPT - sectors per track
		DB 5   ;BSH - block shift factor
		DB 31  ;BLM - block mask
		DB 1   ;EXM - Extent mask
		DW 2043 ; (2047-4) DSM - Storage size (blocks - 1)
		DW 511 ;DRM - Number of directory entries - 1
		DB 240 ;AL0 - 1 bit set per directory block
		DB 0   ;AL1 -            "
		DW 0   ;CKS - DIR check vector size (DRM+1)/4 (0=fixed disk)
		DW 1   ;OFF - Reserved tracks

dpb:
		DW 128 ;SPT - sectors per track
		DB 5   ;BSH - block shift factor
		DB 31  ;BLM - block mask
		DB 1   ;EXM - Extent mask
		DW 2047 ;DSM - Storage size (blocks - 1)
		DW 511 ;DRM - Number of directory entries - 1
		DB 240 ;AL0 - 1 bit set per directory block
		DB 0   ;AL1 -            "
		DW 0   ;CKS - DIR check vector size (DRM+1)/4 (0=fixed disk)
		DW 0   ;OFF - Reserved tracks

; Last drive is smaller because CF is never full 64MB or 128MB
dpbLast:
		DW 128 ;SPT - sectors per track
		DB 5   ;BSH - block shift factor
		DB 31  ;BLM - block mask
		DB 1   ;EXM - Extent mask
		DW 511 ;DSM - Storage size (blocks - 1)  ; 511 = 2MB (for 128MB card), 1279 = 5MB (for 64MB card)
		DW 511 ;DRM - Number of directory entries - 1
		DB 240 ;AL0 - 1 bit set per directory block
		DB 0   ;AL1 -            "
		DW 0   ;CKS - DIR check vector size (DRM+1)/4 (0=fixed disk)
		DW 0   ;OFF - Reserved tracks

;================================================================================================
; Cold boot
;================================================================================================

boot:
		DI						; Disable interrupts.
		LD		SP,biosstack	; Set default stack.

; MJD The ROM is already disabled by the monitor
;		Turn off ROM

;		LD	A,$01
;		OUT ($38),A

; Initialise SIO_A
; Don't reset SIO_A again otherwise DTR will go high and re-enable the ROM.

;		LD	A,$00
;		OUT	(SIOA_C),A
;		LD	A,$18
;		OUT	(SIOA_C),A

		LD		A,$04			; Select Register 4
		OUT		(SIOA_C),A
		LD		A,$84			; x32 Clock (57600 baud) & 1 stop bit
		OUT		(SIOA_C),A

		LD		A,$01			; Select Register 1
		OUT		(SIOA_C),A
		LD		A,$18			; Enable Rx interrupts
		OUT		(SIOA_C),A
		
		LD		A,$03			; Select Register 3
		OUT		(SIOA_C),A
		LD		A,$E1			; 8-bit Rx & Enable Rx
		OUT		(SIOA_C),A		
		
		LD		A,$05			; Select Register 5
		OUT		(SIOA_C),A
		LD		A,RTS_LOW
		OUT		(SIOA_C),A

;	Initialise SIO_B
		LD		A,$00			; Select Register 0
		OUT		(SIOB_C),A
		LD		A,$18			; Reset SIO_B
		OUT		(SIOB_C),A

		LD		A,$04			; Select Register 4
		OUT		(SIOB_C),A
		LD		A,$84			; x32 Clock (57600 baud) & 1 stop bit
		OUT		(SIOB_C),A

		LD		A,$01			; Select Register 1
		OUT		(SIOB_C),A
		LD		A,$18			; Enable Rx interrupts
		OUT		(SIOB_C),A

		LD		A,$02			; Select Register 2
		OUT		(SIOB_C),A
		LD		A,$E0			; INTERRUPT VECTOR ADDRESS = $E0
		OUT		(SIOB_C),A		

		LD		A,$03			; Select Register 3
		OUT		(SIOB_C),A
		LD		A,$E1			; 8-bit Rx & Enable Rx
		OUT		(SIOB_C),A

		LD		A,$05			; Select Register 5
		OUT		(SIOB_C),A
		LD		A,RTS_LOW
		OUT		(SIOB_C),A

		; Interrupt vector in page FF
		; SIO interrupts will use handler at address $FFE0
		LD		A,$FF
		LD		I,A

		CALL	printInline
		DB FF
		DB 'Z80 CP/M BIOS 1.0 by G. Searle 2007-13'
		DB CR,LF
		DB CR,LF
		DB 'CP/M 2.2 '
		DB 'Copyright'
		DB ' 1979 (c) by Digital Research'
		DB CR,LF,0


		CALL	cfWait
		LD 		A,CF_8BIT		; Set IDE to be 8bit
		OUT		(CF_FEATURES),A
		LD		A,CF_SET_FEAT
		OUT		(CF_COMMAND),A


		CALL	cfWait
		LD 		A,CF_NOCACHE	; No write cache
		OUT		(CF_FEATURES),A
		LD		A,CF_SET_FEAT
		OUT		(CF_COMMAND),A

		XOR		A				; Clear I/O & drive bytes.
		LD		(userdrv),A

		LD		(serABufUsed),A
		LD		(serBBufUsed),A
		LD		HL,serABuf
		LD		(serAInPtr),HL
		LD		(serARdPtr),HL

		LD		HL,serBBuf
		LD		(serBInPtr),HL
		LD		(serBRdPtr),HL

		JP		gocpm

;================================================================================================
; Warm boot
;================================================================================================

wboot:
		DI						; Disable interrupts.
		LD		SP,biosstack	; Set default stack.

		; Interrupt vector in page FF
		LD		A,$FF
		LD		I,A

		LD		B,11 			; Number of sectors to reload

		LD		A,0
		LD		(hstsec),A
		LD		HL,ccp
rdSectors:

		CALL	cfWait

		LD		A,(hstsec)
		OUT 	(CF_LBA0),A
		LD		A,0
		OUT 	(CF_LBA1),A
		OUT 	(CF_LBA2),A
		LD		A,0E0H
		OUT 	(CF_LBA3),A
		LD 		A,1
		OUT 	(CF_SECCOUNT),A

		PUSH 	BC

		CALL 	cfWait

		LD 		A,CF_RD_SEC
		OUT 	(CF_COMMAND),A

		CALL 	cfWait

		LD 		C,4
rd4secs512:
		LD 		B,128
rdByte512:
		in	 	A,(CF_DATA)
		LD 		(HL),A
		iNC 	HL
		DEC 	B
		JR 		NZ, rdByte512
		DEC 	C
		JR 		NZ,rd4secs512

		POP 	BC

		LD		A,(hstsec)
		INC		A
		LD		(hstsec),A

		DJNZ	rdSectors


;================================================================================================
; Common code for cold and warm boot
;================================================================================================

gocpm:
		XOR		A				; 0 to accumulator
		LD		(hstact),A		; host buffer inactive
		LD		(unacnt),A		; clear unalloc count

		LD		HL,serialInt	; Address of serial interrupt.
		LD		($40),HL

		LD		HL,tpabuf		; Address of BIOS DMA buffer.
		LD		(dmaAddr),HL
		LD		A,0C3h			; Opcode for 'JP'.
		LD		(00h),A			; Load at start of RAM.
		LD		HL,wboote		; Address of jump for a warm boot.
		LD		(01h),HL
		LD		(05h),A			; Opcode for 'JP'.
		LD		HL,bdos			; Address of jump for the BDOS.
		LD		(06h),HL
		LD		A,(userdrv)		; Save new drive number (0).
		LD		C,A				; Pass drive number in C.

		IM	2					; Interrupt mode 2
		EI						; Enable interrupts

		JP	ccp					; Start CP/M by jumping to the CCP.

;================================================================================================
; Console I/O routines
;================================================================================================
; Serial interrupt handler
serialInt:
		PUSH	AF
		PUSH	HL

		; Check if there is a char in channel A
		; If not, there is a char in channel B
		SUB		A
		OUT 	(SIOA_C),A
		IN   	A,(SIOA_C)		; Status byte D2=TX Buff Empty, D0=RX char ready	
		RRCA					; Rotates RX status into Carry Flag,	
		JR	NC, serialIntB

serialIntA:
		LD		HL,(serAInPtr)
		INC		HL
		LD		A,L
		CP		(serABuf+SER_BUFSIZE) & $FF
		JR		NZ, notAWrap
		LD		HL,serABuf
notAWrap:
		LD		(serAInPtr),HL	; get SIO_A write pointer
		IN		A,(SIOA_D)		; read the character
		LD		(HL),A			; write to rx buffer

		LD		A,(serABufUsed)	; increment number of chars in rx buffer
		INC		A
		LD		(serABufUsed),A
		CP		SER_FULLSIZE	; is rx buffer approaching full?
		JR		C,rtsA0
	    LD		A,$05			; getting full so set RTS HIGH
		OUT  	(SIOA_C),A
	    LD   	A,RTS_HIGH
		OUT  	(SIOA_C),A
rtsA0:
		POP		HL
		POP		AF
		EI
		RETI

serialIntB:
		LD		HL,(serBInPtr)
		INC		HL
		LD		A,L
		CP		(serBBuf+SER_BUFSIZE) & $FF
		JR		NZ, notBWrap
		LD		HL,serBBuf
notBWrap:
		LD		(serBInPtr),HL	; get SIO_B write pointer
		IN		A,(SIOB_D)		; read the character
		LD		(HL),A			; write to rx buffer

		LD		A,(serBBufUsed)	; increment number of chars in rx buffer
		INC		A
		LD		(serBBufUsed),A
		CP		SER_FULLSIZE	; is rx buffer approaching full?
		JR		C,rtsB0
	    LD   	A,$05			; getting full so set RTS HIGH
		OUT  	(SIOB_C),A
	    LD   	A,RTS_HIGH
		OUT  	(SIOB_C),A
rtsB0:
		POP		HL
		POP		AF
		EI
		RETI

;------------------------------------------------------------------------------------------------
; BIOS FUNCTION 2 - Console Status
; Check for a received byte on the currently assigned console device
; A = $FF if byte available, otherwise A = 0
;------------------------------------------------------------------------------------------------
const:
		LD		A,(iobyte)
		AND		00001011b 	; Mask off console and high bit of reader
		CP		00001010b 	; redirected to reader on UR1/2 (Serial A)
		JR		Z,constA
		CP		00000010b 	; redirected to reader on TTY/RDR (Serial B)
		JR		Z,constB

		AND		$03 		; remove the reader from the mask - only console bits then remain
		CP		$01
		JR		NZ,constB

		; console status for SIO_A
constA:
		PUSH	HL
		LD		A,(serABufUsed)
		CP		$00
		JR		Z, dataAEmpty
 		LD		A,0FFH
		POP		HL
		RET
dataAEmpty:
		LD		A,0
		POP		HL
       	RET

		; console status for SIO_B
constB:
		PUSH	HL
		LD		A,(serBBufUsed)
		CP		$00
		JR		Z, dataBEmpty
 		LD		A,0FFH
		POP		HL
		RET
dataBEmpty:
		LD		A,0
		POP		HL
       	RET

;------------------------------------------------------------------------------------------------
; BIOS FUNCTION 7 - Read from punch paper tape device
; Read a character from the punch paper tape device - will block until a character is recevied
; A = received character
;
; Will read from either SIO_A or SIO_B
;------------------------------------------------------------------------------------------------
reader:		
		PUSH	HL
		PUSH	AF
reader2:
		LD		A,(iobyte)
		AND		$08
		CP		$08
		JR		NZ,coninB		; input from SIO_B
		JR		coninA			; input from SIO_A
		
;------------------------------------------------------------------------------------------------
; BIOS FUNCTION 3 - Read Console Character
; Read a character from the console - will block until a character is recevied
; A = received character
;------------------------------------------------------------------------------------------------
conin:
		PUSH	HL
		PUSH	AF
		LD		A,(iobyte)
		AND		$03
		CP		$02
		JR		Z,reader2		; "BAT:" redirect
		CP		$01
		JR		NZ,coninB

		; read from console port A
coninA:
		POP		AF
waitForCharA:
		LD		A,(serABufUsed)		; get count of charcters in the buffer
		CP		$00					; is it zero?
		JR		Z, waitForCharA		; wait if it is zero
		LD		HL,(serARdPtr)		; advance the read pointer
		INC		HL
		LD		A,L
		CP		(serABuf+SER_BUFSIZE) & $FF
		JR		NZ, notRdWrapA
		LD		HL,serABuf
notRdWrapA:
		DI							; disable interrupts
		LD		(serARdPtr),HL		; get pointer to the character

		; decrement the count of the number of characters in the receive buffer
		LD		A,(serABufUsed)
		DEC		A
		LD		(serABufUsed),A

		; if buffer nearly empty then set RTS low to allow more chars from host
		CP		SER_EMPTYSIZE
		JR		NC,rtsA1
        LD  	A,$05
		OUT 	(SIOA_C),A
        LD  	A,RTS_LOW
		OUT		(SIOA_C),A
rtsA1:
		LD		A,(HL)				; get the character from the buffer
		EI							; re-enable interrupts
		POP		HL
		RET							; Char ready in A

		; read from console port B
coninB:
		POP		AF
waitForCharB:
		LD		A,(serBBufUsed)		; get count of charcters in the buffer
		CP		$00					; is it zero?
		JR		Z, waitForCharB		; wait if it is zero
		LD		HL,(serBRdPtr)		; advance the read pointer
		INC		HL
		LD		A,L
		CP		(serBBuf+SER_BUFSIZE) & $FF
		JR		NZ, notRdWrapB
		LD		HL,serBBuf
notRdWrapB:
		DI							; disable interrupts
		LD		(serBRdPtr),HL		; get pointer to the character

		; decrement the count of the number of characters in the receive buffer
		LD		A,(serBBufUsed)
		DEC		A
		LD		(serBBufUsed),A

		; if buffer nearly empty then set RTS low to allow more chars from host
		CP		SER_EMPTYSIZE
		JR		NC,rtsB1
        LD  	A,$05
		OUT 	(SIOB_C),A
        LD  	A,RTS_LOW
		OUT 	(SIOB_C),A
rtsB1:
		LD		A,(HL)				; get the character from the buffer
		EI							; re-enable interrupts
		POP		HL
		RET							; Char ready in A

;------------------------------------------------------------------------------------------------
; BIOS FUNCTION 4 - Write Console Character
; Write a character to the console - will block until previous character is transmitted
; C = character to send
;------------------------------------------------------------------------------------------------
conout:	PUSH	AF				; Store the character
		LD		A,(iobyte)
		AND		$03
		CP		$02				; is it device #2?
		JR		Z,list2			; "BAT:" redirect
		CP		$01				; is it device #1?
		JR		NZ,conoutB1

		; write to console port A
conoutA1:
		CALL	CKSIOA			; See if SIO channel A has finished transmitting
		JR		Z,conoutA1		; Loop until SIO flag signals ready
		LD		A,C
		OUT		(SIOA_D),A		; Output the character
		POP		AF				; Restore the character
		RET

		; write to console port A
conoutB1:
		CALL	CKSIOB			; See if SIO channel B has finished transmitting
		JR		Z,conoutB1		; Loop until SIO flag signals ready
		LD		A,C
		OUT		(SIOB_D),A		; Output the character
		POP		AF				; Restore the character
		RET

;------------------------------------------------------------------------------------------------
; BIOS FUNCTION 5 - Write to listing device (i.e. printer)
; Write a character to the printer - will block until previous character printed
; C = character to print
;
; Will output to either SIO_A or SIO_B
;------------------------------------------------------------------------------------------------
list:	PUSH	AF				; Store character
list2:	LD		A,(iobyte)
		AND		$C0
		CP		$40
		JR		NZ,conoutB1		; output via SIO_B
		JR		conoutA1		; output via SIO_A
		
;------------------------------------------------------------------------------------------------
; BIOS FUNCTION 6 - Write to punch paper tape device
; Write a character to the punch paper tape device
; C = character to output
;
; Will output to either SIO_A or SIO_B
;------------------------------------------------------------------------------------------------
punch:	PUSH	AF				; Store character
		LD		A,(iobyte)
		AND		$20
		CP		$20
		JR		NZ,conoutB1		; output via SIO_B
		JR		conoutA1		; output via SIO_A

;------------------------------------------------------------------------------------------------




;------------------------------------------------------------------------------------------------
CKSIOA:
		SUB	A
		OUT 	(SIOA_C),A
		IN   	A,(SIOA_C)	; Status byte D2=TX Buff Empty, D0=RX char ready	
		RRCA				; Rotates RX status into Carry Flag,	
		BIT  	1,A			; Set Zero flag if still transmitting character	
        RET

CKSIOB:
		SUB	A
		OUT 	(SIOB_C),A
		IN   	A,(SIOB_C)	; Status byte D2=TX Buff Empty, D0=RX char ready	
		RRCA				; Rotates RX status into Carry Flag,	
		BIT  	1,A			; Set Zero flag if still transmitting character	
        RET

;------------------------------------------------------------------------------------------------
listst:	LD		A,$FF		; Return list status of 0xFF (ready).
		RET

;================================================================================================
; Disk processing entry points
;================================================================================================

seldsk:
		LD		HL,$0000
		LD		A,C
		CP		16				; 16 for 128MB disk, 8 for 64MB disk
		JR		C,chgdsk		; if invalid drive will give BDOS error
		LD		A,(userdrv)		; so set the drive back to a:
		CP		C				; If the default disk is not the same as the
		RET		NZ				; selected drive then return, 
		XOR		A				; else reset default back to a:
		LD		(userdrv),A		; otherwise will be stuck in a loop
		LD		(sekdsk),A
		RET

chgdsk:	LD 		(sekdsk),A
		RLC		A		;*2
		RLC		A		;*4
		RLC		A		;*8
		RLC		A		;*16
		LD 		HL,dpbase
		LD		B,0
		LD		C,A	
		ADD		HL,BC

		RET

;------------------------------------------------------------------------------------------------
; BIOS FUNCTION 8 - Set track 0 on the current drive
;
; NOTE: Set Track should follow this function.
;------------------------------------------------------------------------------------------------
home:
		LD		A,(hstwrt)		; check for pending write
		OR		A
		JR		NZ,homed
		LD		(hstact),A		; clear host active flag
homed:
		LD 		BC,0000h

;------------------------------------------------------------------------------------------------
; BIOS FUNCTION 10 - Set the track number
; BC holds the track number
;------------------------------------------------------------------------------------------------
settrk:	LD 		(sektrk),BC		; Set track passed from BDOS in register BC.
		RET

;------------------------------------------------------------------------------------------------
; BIOS FUNCTION 11 - Set the sector number
; BC holds the sector number
;------------------------------------------------------------------------------------------------
setsec:	LD 		(seksec),BC		; Set sector passed from BDOS in register BC.
		RET

;------------------------------------------------------------------------------------------------
; BIOS FUNCTION 12 - Set the data buffer address
; BC holds the address of the start of the data block
;------------------------------------------------------------------------------------------------
setdma:	LD 		(dmaAddr),BC	; Set DMA address given by registers BC.
		RET

;------------------------------------------------------------------------------------------------
; BIOS FUNCTION 16 - Sector translation
; There's no sector translation needed in this system so simply copy
; BC into HL and return.
;------------------------------------------------------------------------------------------------
sectran:
		PUSH 	BC
		POP 	HL
		RET

;------------------------------------------------------------------------------------------------
; BIOS FUNCTION 13 - Perform a disk read
; Read from the disk location defined by the track and sector variables and store the Data
; starting at the DMA address.
;------------------------------------------------------------------------------------------------
read:
		;read the selected CP/M sector
		XOR		A
		LD		(unacnt),A
		LD		A,1
		LD		(readop),A		; read operation
		LD		(rsflag),A		; must read data
		LD		A,wrual
		LD		(wrtype),A		; treat as unalloc
		JP		rwoper			; to perform the read

;------------------------------------------------------------------------------------------------
; BIOS FUNCTION 14 - Perform a disk write
; Write to the disk location defined by the track and sector variables. The data to write is
; in memory starting at the DMA address.
;------------------------------------------------------------------------------------------------
write:
		;write the selected CP/M sector
		XOR		A				; 0 to accumulator
		LD		(readop),A		; not a read operation
		LD		A,C				; write type in c
		LD		(wrtype),A
		CP		wrual			; write unallocated?
		JR		NZ,chkuna		; check for unalloc
;
;		write to unallocated, set parameters
		LD		A,blksiz/128	; next unalloc recs
		LD		(unacnt),A
		LD		A,(sekdsk)		; disk to seek
		LD		(unadsk),A		; unadsk = sekdsk
		LD		HL,(sektrk)
		LD		(unatrk),hl		; unatrk = sectrk
		LD		A,(seksec)
		LD		(unasec),A		; unasec = seksec
;
chkuna:
;		check for write to unallocated sector
		LD		A,(unacnt)		; any unalloc remain?
		OR		A	
		JR		Z,alloc			; skip if not
;
;		more unallocated records remain
		DEC		A				; unacnt = unacnt-1
		LD		(unacnt),A
		LD		A,(sekdsk)		; same disk?
		LD		HL,unadsk
		CP		(HL)			; sekdsk = unadsk?
		JP		NZ,alloc		; skip if not
;
;		disks are the same
		LD		HL,unatrk
		CALL	sektrkcmp	; sektrk = unatrk?
		JP		NZ,alloc		; skip if not
;
;		tracks are the same
		LD		A,(seksec)		; same sector?
		LD		HL,unasec
		CP		(HL)			; seksec = unasec?
		JP		NZ,alloc		; skip if not
;
;		match, move to next sector for future ref
		INC		(HL)			; unasec = unasec+1
		LD		A,(HL)			; end of track?
		CP		cpmspt			; count CP/M sectors
		JR		C,noovf			; skip if no overflow
;
;		overflow to next track
		LD		(HL),0			; unasec = 0
		LD		HL,(unatrk)
		INC		HL
		LD		(unatrk),HL		; unatrk = unatrk+1
;
noovf:
		;match found, mark as unnecessary read
		XOR		A				; 0 to accumulator
		LD		(rsflag),A		; rsflag = 0
		JR		rwoper			; to perform the write
;
alloc:
		;not an unallocated record, requires pre-read
		XOR		A				; 0 to accum
		LD		(unacnt),A		; unacnt = 0
		INC		A				; 1 to accum
		LD		(rsflag),A		; rsflag = 1

;------------------------------------------------------------------------------------------------
rwoper:
		;enter here to perform the read/write
		XOR		A				; zero to accum
		LD		(erflag),A		; no errors (yet)
		LD		A,(seksec)		; compute host sector
		OR		A				; carry = 0
		RRA						; shift right
		OR		A				; carry = 0
		RRA						; shift right
		LD		(sekhst),A		; host sector to seek
;
;		active host sector?
		LD		HL,hstact		; host active flag
		LD		A,(HL)
		LD		(HL),1			; always becomes 1
		OR		A				; was it already?
		JR		Z,filhst		; fill host if not
;
;		host buffer active, same as seek buffer?
		LD		A,(sekdsk)
		LD		HL,hstdsk		; same disk?
		CP		(HL)			; sekdsk = hstdsk?
		JR		NZ,nomatch
;
;		same disk, same track?
		LD		HL,hsttrk
		CALL	sektrkcmp		; sektrk = hsttrk?
		JR		NZ,nomatch
;
;		same disk, same track, same buffer?
		LD		A,(sekhst)
		LD		HL,hstsec		; sekhst = hstsec?
		CP		(HL)
		JR		Z,match			; skip if match
;
nomatch:
		;proper disk, but not correct sector
		LD		A,(hstwrt)		; host written?
		OR		A
		CALL	NZ,writehst		; clear host buff
;
filhst:
		;may have to fill the host buffer
		LD		A,(sekdsk)
		LD		(hstdsk),A
		LD		HL,(sektrk)
		LD		(hsttrk),hl
		LD		A,(sekhst)
		LD		(hstsec),A
		LD		A,(rsflag)		; need to read?
		OR		A
		CALL	NZ,readhst		; yes, if 1
		XOR		A				; 0 to accum
		LD		(hstwrt),A		; no pending write
;
match:
		;copy data to or from buffer
		LD		A,(seksec)		; mask buffer number
		AND		secmsk			; least signif bits
		LD		L,A				; ready to shift
		LD		H,0				; double count
		ADD		HL,HL
		ADD		HL,HL
		ADD		HL,HL
		ADD		HL,HL
		ADD		HL,HL
		ADD		HL,HL
		ADD		HL,HL
;		hl has relative host buffer address
		LD		DE,hstbuf
		ADD		HL,DE			; hl = host address
		EX		DE,HL			; now in DE
		LD		HL,(dmaAddr)	; get/put CP/M data
		LD		C,128			; length of move
		LD		A,(readop)		; which way?
		OR		A
		JR		NZ,rwmove		; skip if read
;
;	write operation, mark and switch direction
		LD		A,1
		LD		(hstwrt),A		; hstwrt = 1
		EX		DE,HL			; source/dest swap
;
rwmove:
		;C initially 128, DE is source, HL is dest
		LD		A,(DE)			; source character
		INC		DE
		LD		(HL),A			; to dest
		INC		HL
		DEC		C				; loop 128 times
		JR		NZ,rwmove
;
;		data has been moved to/from host buffer
		LD		A,(wrtype)		; write type
		CP		wrdir			; to directory?
		LD		A,(erflag)		; in case of errors
		RET		NZ				; no further processing
;
;		clear host buffer for directory write
		OR		A				; errors?
		RET		NZ				; skip if so
		XOR		A				; 0 to accum
		LD		(hstwrt),A		; buffer written
		CALL	writehst
		LD		A,(erflag)
		RET

;------------------------------------------------------------------------------------------------
;Utility subroutine for 16-bit compare
sektrkcmp:
		;HL = .unatrk or .hsttrk, compare with sektrk
		EX		DE,HL
		LD		HL,sektrk
		LD		A,(DE)			; low byte compare
		CP		(HL)			; same?
		RET		NZ				; return if not
;		low bytes equal, test high 1s
		INC		DE
		INC		HL
		LD		A,(DE)
		CP		(HL)			; sets flags
		RET

;================================================================================================
; Convert CP/M disk, track and sector information into a Logical Block Address for the
; CompactFlash card.
;
; LBA3 is always set to $E0 as we are using the LBA mode of the CF card and we are ONLY
; addressing CF drive 0 and we don't use LBA bits 24..27 (so set them to zero).
;
; The mapping of the disk, track and sector information is as follows:
;
; +------------------------+-------------------------+-------------------------+
; | LBA2                   | LBA1                    | LBA0                    |
; | 0  0  0  0  0  0 D3 D2 | D1 D0 T8 T7 T6 T5 T4 T3 | T2 T1 T0 S4 S3 S2 S1 S0 |
; +------------------------+-------------------------+-------------------------+
;
;================================================================================================
setLBAaddr:
		; Create CompactFlash LBA0
		LD		HL,(hsttrk)		; get the current track
		RLC		L				; shift left 5x
		RLC		L
		RLC		L
		RLC		L
		RLC		L				; L = T0..T2 in correct bit position for LBA0
		LD		A,L
		AND		0E0H			; set S0..S4 bits to zero
		LD		L,A
		LD		A,(hstsec)		; get the current sector
		ADD		A,L				; add in the current sector
		LD		(lba0),A		; save the computed LBA0

		; Create CompactFlash LBA1
		LD		HL,(hsttrk)		; get the current track
		RRC		L				; shift right 3x - NOTE only T0..T7 shifted
		RRC		L
		RRC		L				; L = T3..T7 in correct bit position for LBA1
		LD		A,L
		AND		01FH			; make sure the disk bits are zero - SHOULD THIS BE $3F?
		LD		L,A
		RLC		H				; shift T8 bit left 5x
		RLC		H
		RLC		H
		RLC		H
		RLC		H				; H = T8 in correct bit position for LBA1
		LD		A,H
		AND		020H			; zero all bits apart from T8
		LD		H,A
		LD		A,(hstdsk)		; get the current disk
		RLC		A				; shift left 6x
		RLC		A
		RLC		A
		RLC		A
		RLC		A
		RLC		A				; A = D0..D1 in correct bit position for LBA1
		AND		0C0H			; zero all bits apart from D0 & D1
		ADD		A,H				; insert the T8 bit
		ADD		A,L				; insert the T3..T7 bits
		LD		(lba1),A		; save the computed LBA1
		

		LD		A,(hstdsk)		; get the current disk
		RRC		A				; shift right 2x
		RRC		A				; A = D2..D3 in correct bit position for LBA2
		AND		03H				; zero all bits apart from D2 & D3
		LD		(lba2),A		; save the computed LBA2

		; Tell CompactFlash we are using LBA Mode and drive 0 = E0
		LD		A,0E0H
		LD		(lba3),A		; save LBA3

		; write out the 4 LBA registers to the CF card
		LD		A,(lba0)
		OUT 	(CF_LBA0),A
		LD		A,(lba1)
		OUT 	(CF_LBA1),A
		LD		A,(lba2)
		OUT 	(CF_LBA2),A
		LD		A,(lba3)
		OUT 	(CF_LBA3),A

		; we want to read or write just 1 CF block of 512 bytes
		LD 		A,1
		OUT 	(CF_SECCOUNT),A

		RET				

;================================================================================================
; Read a 512 byte block of data from the CompactFlash card
;================================================================================================
readhst:
		PUSH 	AF
		PUSH 	BC
		PUSH 	HL

		CALL 	cfWait					; wait for the CF card to become ready
		CALL 	setLBAaddr				; compute the CF LBA

		LD 		A,CF_RD_SEC
		OUT 	(CF_COMMAND),A			; issue the CF read command

		CALL 	cfWait					; wait for the read to complete

		LD 		C,4						; going to read 4 blocks of 128 bytes
		LD 		HL,hstbuf				; HL points to our 512 byte buffer
rd4secs:
		LD 		B,128					; going to read 128 bytes
rdByte:
		IN 		A,(CF_DATA)				; read a byte from the CF card
		LD 		(HL),A					; save the byte in our buffer
		INC 	HL						; point to next location in our buffer
		DEC 	B						; decrement byte counter
		JR 		NZ, rdByte				; repeat until 128 bytes read in
		DEC 	C
		JR 		NZ,rd4secs				; repeat until 4 lots of 128 bytes read in

		POP 	HL
		POP 	BC
		POP 	AF

		XOR 	A						; signal that there wasn't an error
		LD		(erflag),A
		RET

;================================================================================================
; Write a 512 byte block of data to the CompactFlash card
;================================================================================================

writehst:
		PUSH 	AF
		PUSH 	BC
		PUSH 	HL

		CALL 	cfWait					; wait for the CF card to become ready
		CALL 	setLBAaddr				; compute the CF LBA

		LD 		A,CF_WR_SEC
		OUT 	(CF_COMMAND),A			; issue the CF write command

		CALL 	cfWait					; wait for the CF card to become ready

		LD 		C,4						; going to write 4 blocks of 128 bytes
		LD 		HL,hstbuf				; HL points to our 512 byte buffer
wr4secs:
		LD 		B,128					; going to write 128 bytes
wrByte:	LD 		A,(HL)
		OUT 	(CF_DATA),A				; write a byte to the CF card
		INC 	HL						; point to next location in our buffer
		DEC 	B						; decrement byte counter
		JR 		NZ, wrByte				; repeat until 128 bytes written out

		DEC 	C
		JR 		NZ,wr4secs				; repeat until 4 lots of 128 bytes written out

		POP 	HL
		POP 	BC
		POP 	AF

		XOR 	A						; signal that there wasn't an error
		LD		(erflag),A
		RET

;================================================================================================
; Wait for the CompactFlash disk to be ready - i.e. not busy
; Waits for the BUSY bit in the CF status register to clear to a 0
;================================================================================================
cfWait:
		PUSH 	AF
cfWait1:
		IN 		A,(CF_STATUS)			; read the CF status register
		AND 	080H
		cp 		080H					; is bit 7 (BUSY) bit a 1?
		JR		Z,cfWait1				; wait until BUSY bit is 0
		POP 	AF
		RET

;================================================================================================
; Utilities
;================================================================================================

printInline:
		EX 		(SP),HL 		; PUSH HL and put RET address into HL
		PUSH 	AF
		PUSH 	BC
nextILChar:
		LD 		A,(HL)
		CP		0
		JR		Z,endOfPrint
		LD  	C,A
		CALL 	conout			; Print to TTY
		iNC 	HL
		JR		nextILChar
endOfPrint:
		INC 	HL 				; Get past "null" terminator
		POP 	BC
		POP 	AF
		EX 		(SP),HL 		; PUSH new RET address on stack and restore HL
		RET

;================================================================================================
; Data storage
;================================================================================================

dirbuf: 	ds 128 		; scratch directory area
alv00: 		ds 257		; allocation vector 0
alv01: 		ds 257		; allocation vector 1
alv02: 		ds 257		; allocation vector 2
alv03: 		ds 257		; allocation vector 3
alv04: 		ds 257		; allocation vector 4
alv05: 		ds 257		; allocation vector 5
alv06: 		ds 257		; allocation vector 6
alv07: 		ds 257		; allocation vector 7
alv08: 		ds 257		; allocation vector 8
alv09: 		ds 257		; allocation vector 9
alv10: 		ds 257		; allocation vector 10
alv11: 		ds 257		; allocation vector 11
alv12: 		ds 257		; allocation vector 12
alv13: 		ds 257		; allocation vector 13
alv14: 		ds 257		; allocation vector 14
alv15: 		ds 257		; allocation vector 15

lba0		db	00h
lba1		db	00h
lba2		db	00h
lba3		db	00h

		ds	020h		; Start of BIOS stack area.
biosstack:	EQU	$

sekdsk:		ds	1		; seek disk number
sektrk:		ds	2		; seek track number
seksec:		ds	2		; seek sector number
; 
hstdsk:		ds	1		; host disk number
hsttrk:		ds	2		; host track number
hstsec:		ds	1		; host sector number
; 
sekhst:		ds	1		; seek shr secshf
hstact:		ds	1		; host active flag
hstwrt:		ds	1		; host written flag
; 
unacnt:		ds	1		; unalloc rec cnt
unadsk:		ds	1		; last unalloc disk
unatrk:		ds	2		; last unalloc track
unasec:		ds	1		; last unalloc sector
; 
erflag:		ds	1		; error reporting
rsflag:		ds	1		; read sector flag
readop:		ds	1		; 1 if read operation
wrtype:		ds	1		; write operation type
dmaAddr:	ds	2		; last dma address
hstbuf:		ds	512		; host buffer - 512 bytes for 1 CF block

hstBufEnd:	EQU	$

serABuf:	ds	SER_BUFSIZE	; SIO A Serial buffer
serAInPtr	DW	00h
serARdPtr	DW	00h
serABufUsed	db	00h
serBBuf:	ds	SER_BUFSIZE	; SIO B Serial buffer
serBInPtr	DW	00h
serBRdPtr	DW	00h
serBBufUsed	db	00h

serialVarsEnd:	EQU	$

biosEnd:	EQU	$

; Disable the ROM, pop the active IO port from the stack (supplied by monitor),
; then start CP/M
popAndRun:
;		LD	A,$01 
;		OUT	($38),A

		POP	AF
		CP	$01
		JR	Z,consoleAtB
		LD	A,$01 ;(List is TTY:, Punch is TTY:, Reader is TTY:, Console is CRT:)
		JR	setIOByte
consoleAtB:
		LD	A,$00 ;(List is TTY:, Punch is TTY:, Reader is TTY:, Console is TTY:)
setIOByte:
		LD (iobyte),A
		JP	bios

;	IM 2 lookup for serial interrupt

		ORG	0FFE0H
		DW	serialInt


;=================================================================================
; Relocate TPA area from 4100 to 0100 then start CP/M
; Used to manually transfer a loaded program after CP/M was previously loaded
;=================================================================================

		ORG	0FFE8H
;		LD	A,$01
;		OUT	($38),A

		LD	HL,04100H
		LD	DE,00100H
		LD	BC,08F00H
		LDIR
		JP	bios

;=================================================================================
; Normal start CP/M vector
;=================================================================================

		ORG 0FFFEH
		DW	popAndRun

		.END
