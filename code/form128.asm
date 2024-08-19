;==================================================================================
; Contents of this file are copyright Grant Searle
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
; - Modified CF card addresses for my Z80 KIO curiosity project

numDrives	EQU	15		; Not including A:


; CF registers
CF_DATA		EQU	$90
CF_FEATURES	EQU	$91
CF_ERROR	EQU	$91
CF_SECCOUNT	EQU	$92
CF_SECTOR	EQU	$93
CF_CYL_LOW	EQU	$94
CF_CYL_HI	EQU	$95
CF_HEAD		EQU	$96
CF_STATUS	EQU	$97
CF_COMMAND	EQU	$97
CF_LBA0		EQU	$93
CF_LBA1		EQU	$94
CF_LBA2		EQU	$95
CF_LBA3		EQU	$96

;CF Features
CF_8BIT		EQU	1
CF_NOCACHE	EQU	082H
;CF Commands
CF_READ_SEC	EQU	020H
CF_WRITE_SEC	EQU	030H
CF_SET_FEAT	EQU 	0EFH

LF		EQU	0AH		;line feed
FF		EQU	0CH		;form feed
CR		EQU	0DH		;carriage RETurn

;====================================================================================

		ORG	5000H		; Format program origin.


		CALL	printInline
		DB 'CP/M Formatter by G. Searle 2012'
		DB CR,LF,0

		LD	A,'A'
		LD	(drvName),A

		CALL	cfWait
		LD 	A,CF_8BIT	; Set IDE to be 8bit
		OUT	(CF_FEATURES),A
		LD	A,CF_SET_FEAT
		OUT	(CF_COMMAND),A

		CALL	cfWait
		LD 	A,CF_NOCACHE	; No write cache
		OUT	(CF_FEATURES),A
		LD	A,CF_SET_FEAT
		OUT	(CF_COMMAND),A


; There are 512 directory entries per disk, 4 DIR entries per sector
; So 128 x 128 byte sectors are to be initialised
; The drive uses 512 byte sectors, so 32 x 512 byte sectors per disk
; require initialisation

;Drive 0 (A:) is slightly different due to reserved track, so DIR sector starts at 32
		LD	A,(drvName)
		RST	08H		; Print drive letter
		INC	A
		LD	(drvName),A

		LD	A,$20
		LD	(secNo),A

processSectorA:
		CALL	cfWait

		LD	A,(secNo)
		OUT 	(CF_LBA0),A
		LD	A,0
		OUT 	(CF_LBA1),A
		LD	A,0
		OUT 	(CF_LBA2),A
		LD	a,$E0
		OUT 	(CF_LBA3),A
		LD 	A,1
		OUT 	(CF_SECCOUNT),A

		call	writeDir

		LD	A,(secNo)
		INC	A
		LD	(secNo),A
		CP	$40
		JR	NZ, processSectorA



;Drive 1 onwards (B: etc) don't have reserved tracks, so sector starts at 0

		LD 	DE,$0040  ; HL increment
		LD 	HL,$0040  ; H = LBA2, L=LBA1, initialise for drive 1 (B:)

		LD	B,numDrives

processDirs:

		LD	A,(drvName)
		RST	08H		; Print drive letter
		INC	A
		LD	(drvName),A

		LD	A,0
		LD	(secNo),A

processSector:
		CALL	cfWait

		LD	A,(secNo)
		OUT 	(CF_LBA0),A
		LD	A,L
		OUT 	(CF_LBA1),A
		LD	A,H
		OUT 	(CF_LBA2),A
		LD	a,0E0H
		OUT 	(CF_LBA3),A
		LD 	A,1
		OUT 	(CF_SECCOUNT),A

		call	writeDir

		LD	A,(secNo)
		INC	A
		LD	(secNo),A
		CP	$20
		JR	NZ, processSector

		ADD	HL,DE

		DEC	B
		JR	NZ,processDirs

		CALL	printInline
		DB CR,LF
		DB 'Formatting complete'
		DB CR,LF,0

		RET				

;================================================================================================
; Write physical sector to host
;================================================================================================

writeDir:
		PUSH 	AF
		PUSH 	BC
		PUSH 	HL

		CALL 	cfWait

		LD 	A,CF_WRITE_SEC
		OUT 	(CF_COMMAND),A

		CALL 	cfWait

		LD 	C,4
wr4secs:
		LD 	HL,dirData
		LD 	B,128
wrByte:	LD 	A,(HL)
		NOP
		NOP
		OUT 	(CF_DATA),A
		INC 	HL
		DEC 	B
		JR 	NZ, wrByte

		DEC 	C
		JR 	NZ,wr4secs

		POP 	HL
		POP 	BC
		POP 	AF

		RET

;================================================================================================
; Utilities
;================================================================================================

printInline:
		EX 	(SP),HL 	; PUSH HL and put RET ADDress into HL
		PUSH 	AF
		PUSH 	BC
nextILChar:	LD 	A,(HL)
		CP	0
		JR	Z,endOfPrint
		RST 	08H
		INC 	HL
		JR	nextILChar
endOfPrint:	INC 	HL 		; Get past "null" terminator
		POP 	BC
		POP 	AF
		EX 	(SP),HL 	; PUSH new RET ADDress on stack and restore HL
		RET

;================================================================================================
; Wait for disk to be ready (busy=0,ready=1)
;================================================================================================
cfWait:
		PUSH 	AF
cfWait1:
		in 	A,(CF_STATUS)
		AND 	080H
		cp 	080H
		JR	Z,cfWait1
		POP 	AF
		RET

secNo		DB	0
drvName		DB	0


; Directory data for 1 x 128 byte sector
dirData:
		DB $E5,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$00,$00,$00,$00
		DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00

		DB $E5,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$00,$00,$00,$00
		DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00

		DB $E5,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$00,$00,$00,$00
		DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00

		DB $E5,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$00,$00,$00,$00
		DB $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00


		.END
	