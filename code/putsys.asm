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

loadAddr	EQU	0D000h
numSecs		EQU	24	; Number of 512 sectors to be loaded


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

;================================================================================================

		ORG	5000H		; Loader origin.

		CALL	printInline
		DB 'CP/M System Transfer by G. Searle 2012'
		DB CR,LF,0

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

		LD	B,numSecs

		LD	A,0
		LD	(secNo),A
		LD	HL,loadAddr
		LD	(dmaAddr),HL
processSectors:

		CALL	cfWait

		LD	A,(secNo)
		OUT 	(CF_LBA0),A
		LD	A,0
		OUT 	(CF_LBA1),A
		OUT 	(CF_LBA2),A
		LD	a,0E0H
		OUT 	(CF_LBA3),A
		LD 	A,1
		OUT 	(CF_SECCOUNT),A

		call	write

		LD	DE,0200H
		LD	HL,(dmaAddr)
		ADD	HL,DE
		LD	(dmaAddr),HL
		LD	A,(secNo)
		INC	A
		LD	(secNo),A

		djnz	processSectors

		CALL	printInline
		DB CR,LF
		DB 'System transfer complete'
		DB CR,LF,0

		RET				


;================================================================================================
; Write physical sector to host
;================================================================================================

write:
		PUSH 	AF
		PUSH 	BC
		PUSH 	HL

		CALL 	cfWait

		LD 	A,CF_WRITE_SEC
		OUT 	(CF_COMMAND),A

		CALL 	cfWait

		LD 	C,4
		LD 	HL,(dmaAddr)
wr4secs:
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
; Wait for disk to be ready (busy=0,ready=1)
;================================================================================================
cfWait:
		PUSH 	AF
cfWait1:
		IN 	A,(CF_STATUS)
		AND 	080H
		CP 	080H
		JR	Z,cfWait1
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

dmaAddr		DW	0
secNo		DB	0

	.END
