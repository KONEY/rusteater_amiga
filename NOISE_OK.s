;*** CODE: KONEY ***
;*** MiniStartup by Photon ***
	INCDIR	"NAS:AMIGA/CODE/rusteater_amiga/"
	SECTION	"Code+PT12",CODE
	INCLUDE	"PhotonsMiniWrapper1.04!.s"
	INCLUDE	"custom-registers.i"	;use if you like ;)
;********** Constants **********
wi		EQU 320
he		EQU 256		; screen height
bpls		EQU 4		; depth
bypl		EQU wi/16*2	; byte-width of 1 bitplane line (40bytes)
bwid		EQU bpls*bypl	; byte-width of 1 pixel line (all bpls)
PIXEL_SIDE	EQU 32
SCROLL_SPEED	EQU 1
TXT_FRMSKIP 	EQU 16
;*************
;********** Demo **********	;Demo-specific non-startup code below.
Demo:			;a4=VBR, a6=Custom Registers Base addr
	;*--- init ---*
	MOVE.L	#VBint,$6C(A4)
	MOVE.W	#%1110000000100000,INTENA
	MOVE.W	#%1000001111100000,DMACON
	;*--- start copper ---*
	; ## EMPTY AREA ##
	LEA	BGEMPTY,A0
	LEA	COPPER\.BplPtrs,A1
	BSR.W	PokePtrs
	LEA	BGEMPTY,A0
	;LEA	-2(A0),A0
	LEA	COPPER\.BplPtrs+8,A1
	BSR.W	PokePtrs

	; # NOISE AREA ##
	LEA	BGNOISE1,A0
	LEA	COPPER\.BplPtrs2,A1
	BSR.W	PokePtrs
	LEA	BGNOISE1,A0
	LEA	-2(A0),A0
	LEA	COPPER\.BplPtrs2+8,A1
	BSR.W	PokePtrs
	LEA	BGNOISE1,A0
	LEA	-40(A0),A0
	LEA	COPPER\.BplPtrs3,A1
	BSR.W	PokePtrs
	LEA	BGNOISE1,A0
	LEA	4(A0),A0
	LEA	COPPER\.BplPtrs3+8,A1
	BSR.W	PokePtrs
	; # NOISE AREA ##

	;LEA	BG_MASK,A0
	LEA	BGEMPTY,A0
	LEA	COPPER\.BplPtrs+16,A1
	BSR.W	PokePtrs
	LEA	BGMASK,A0
	LEA	COPPER\.BplPtrs+24,A1
	BSR.W	PokePtrs


	MOVE.L	#COPPER,COP1LC

	; #### CPU INTENSIVE TASKS BEFORE STARTING MUSIC
	LEA	PIXEL_ON,A0
	MOVE.W	#PIXEL_SIDE-1,D0
	.loop:
	MOVE.L	#-1,(A0)+		; FILL THE PIXEL
	DBRA	D0,.loop

	MOVE.L	#$F80000,A0
	;LEA	BGNOISE1,A4
	;LEA	BGNOISE1,A5
	;LEA	42(A5),A5
	;BSR.W	__RANDOMIZE_PLANE
	; #### CPU INTENSIVE TASKS BEFORE STARTING MUSIC

	;LEA	BGNOISE1,A2
	;LEA	BGPLANE0,A3
	;MOVE.L	BGNOISE1,D7
	;MOVE.B	$DFF006,D7
;********************  main loop  ********************
MainLoop:
	; do stuff here :)
	;MOVE.W	P61_Pos,D5	; SONG_BLOCKS_EVENTS:
	;LEA	TIMELINE,A3	; FOR TIMED EVENTS ON BLOCKS
	;ADD.W	D5,D5		; CALCULATES OFFSET (OPTIMIZED)
	;ADD.W	D5,D5		; CALCULATES OFFSET (OPTIMIZED)
	;MOVE.L	(A3,D5),A3	; THANKS HEDGEHOG!!
	;JSR	(A3)		; EXECUTE SUBROUTINE BLOCK#
	;MOVE.L	KICKSTART_ADDR,A0

	; ## NOISE SECTION ##
	TST.B	FRAME_STROBE
	BNE.W	.oddFrame
	MOVE.B	#1,FRAME_STROBE

	MOVE.W	#(bypl/2)*50-1,D4
	LEA	BGNOISE1,A4
	BSR.W	__RANDOMIZE_PLANE
	ROR.L	D7

	BRA.W	.evenFrame
	.oddFrame:
	MOVE.B	#0,FRAME_STROBE

	MOVE.W	#(bypl/2)*50-1,D4
	LEA	BGNOISE2,A4
	BSR.W	__RANDOMIZE_PLANE
	.evenFrame:
	; ## NOISE SECTION ##

	; ## MASKING SECTION ##
	LEA	BGMASK,A5
	;BSR.W	__SCROLL_X

	LEA	BGMASK,A5
	MOVE.W	#5-1,D0
	.loop:
	BTST	D0,(A0)+
	BNE.S	.off
	LEA	PIXEL_ON,A4
	BRA.S	.on
	.off:
	LEA	PIXEL_OFF,A4
	.on:
	;BSR.W	__BLIT_PIXEL
	ADD.L	#bypl*PIXEL_SIDE,A5
	DBRA	D0,.loop
	; ## MASKING SECTION ##

	; ## TEXT SECTION ##
	;BSR.W	__FETCH_TXT
	; ## TEXT SECTION ##

	.WaitRasterCopper:
	;MOVE.W	#$0F0F,$DFF180	; show rastertime left down to $12c
	BTST	#$4,INTENAR+1
	BNE.S	.WaitRasterCopper
	;MOVE.W	#$0F00,$DFF180	; show rastertime left down to $12c
	MOVE.W	#$8010,INTENA

	;*--- main loop end ---*
	BTST	#6,$BFE001	; POTINP - LMB pressed?
	BEQ.W	.exit
	BTST	#2,$DFF016	; POTINP - RMB pressed?
	BNE.W	MainLoop		; then loop

	;BTST	#6,$BFE001
	;BNE.S	.DontShowRasterTime
	;BSR.W	__BLK_JMP
	;.DontShowRasterTime:
	;BTST	#2,$DFF016	; POTINP - RMB pressed?
	;BNE.W	MainLoop		; then loop

	;*--- exit ---*
	.exit:
	RTS

;********** Demo Routines **********
PokePtrs:				; SUPER SHRINKED REFACTOR
	MOVE.L	A0,-4(A0)		; Needs EMPTY plane to write addr
	MOVE.W	-4(A0),2(A1)	; high word of address
	MOVE.W	-2(A0),6(A1)	; low word of address
	RTS

PokePtrsOld:			; Generic, poke ptrs into copper list
	MOVE.L	A0,D2
	SWAP	D2
	MOVE.W	D2,2(A1)		; high word of address
	MOVE.W	A0,6(A1)		; low word of address
	LEA	8(A1),A1		; OPTIMIZED
	RTS

VBint:				; Blank template VERTB interrupt
	movem.l	d0/a6,-(sp)	; Save used registers
	lea	$dff000,a6
	btst	#5,$1f(a6)	; check if it's our vertb int.
	beq.s	.notvb
	;*--- do stuff here ---*
	moveq	#$20,d0		; poll irq bit
	move.w	d0,$9c(a6)
	move.w	d0,$9c(a6)
	.notvb:	
	movem.l	(sp)+,d0/a6	; restore
	rte

__RANDOMIZE_PLANE:
	BSR.S	_RandomWord
	SWAP	D5
	MOVE.W	D7,D5
	MOVE.L	D5,D1
	.innerloop:
	ROR.L	D1
	MOVE.L	D1,D5
	;MOVE.B	(A0)+,D5
	NOT.L	D5
	EOR.B	D3,D5
	ASR.W	D5
	ROL.L	D5
	EOR.W	D4,D5
	NOT.L	D5
	MOVE.L	D5,(A4)
	BTST	D4,D5
	BNE.S	.skip
	ROL.L	D1
	SWAP	D1
	.skip:
	LEA	2(A4),A4
	DBRA	D4,.innerloop
	;MOVE.W	#$0000,$DFF180	; show rastertime left down to $12c
	RTS

	_RandomWord:
	bsr	_RandomByte
	ROL.W	#8,D5
	_RandomByte:
	MOVE.B	$DFF007,D5	;$dff00a $dff00b for mouse pos
	MOVE.B	$BFD800,D3
	EOR.B	D3,D5
	RTS

__RANDOMIZE_PLANE_V1:
	MOVE.L	#$F80000,A0
	MOVE.W	#(bypl/2/4)*he-1,D4
	BSR.S	_RandomWord1
	SWAP	D5
	MOVE.B	D3,D5
	MOVE.L	D5,D1
	.innerloop:
	ROR.L	D1
	MOVE.W	D1,D5
	SWAP	D5
	MOVE.B	(A0)+,D5
	NOT.L	D5
	AND.B	D4,D5
	ASR.W	D4,D5
	MOVE.B	D5,(A5)
	ROL.L	D5
	;NOT.L	D5
	ASR.W	D4,D5
	MOVE.W	D5,(A4)
	ROR.L	D5
	NOT.L	D5
	MOVE.L	D5,2(A4)
	ROL.L	D5
	MOVE.L	D5,-2(A5)
	BTST	#$0,D5
	BNE.S	.skip
	;NOT.B	D1
	ROR.L	D1
	SWAP	D1

	.skip:
	LEA	2(A4),A4
	LEA	2(A5),A5
	DBRA	D4,.innerloop
	RTS

	_RandomWord1:
	bsr	_RandomByte
	ROL.W	#8,D5
	_RandomByte1:
	MOVE.B	$DFF007,D5	;$dff00a $dff00b for mouse pos
	MOVE.B	$BFD800,D3
	EOR.B	D3,D5
	RTS

__BLIT_PIXEL:
	BSR	WaitBlitter
	MOVE.W	#%0000100111110000,BLTCON0	; BLTCON0
	MOVE.W	#%0000000000000000,BLTCON1	; BLTCON1
	MOVE.L	#$FFFFFFFF,BLTAFWM		; THEY'LL NEVER
	MOVE.W	#$0,BLTAMOD		; BLTAMOD =0 for texture
	MOVE.W	#bypl-(PIXEL_SIDE/16*2),BLTDMOD ; BLTDMOD 40-4=36
	MOVE.L	A4,BLTAPTH		; BLTAPT
	MOVE.L	A5,BLTDPTH
	MOVE.W	#PIXEL_SIDE*64+(PIXEL_SIDE/16),BLTSIZE	; BLTSIZE
	; ## MAIN BLIT ####
	RTS

__SCROLL_X:
	MOVE.L	#%1001111100001000,D1	; %1000100111110000 +ROL.W	#4,D1
	; ## MAIN BLIT ####

	MOVE.W	#SCROLL_SPEED,D0
	MOVE.B	D0,D1
	ROR.W	#4,D1
	bsr	WaitBlitter
	MOVE.W	D1,BLTCON0		; BLTCON0
	MOVE.W	#%0000000000000000,BLTCON1	; BLTCON1 BIT 12 DESC MODE
	MOVE.L	#$FFFFFFFF,BLTAFWM		; THEY'LL NEVER
	MOVE.W	#$0,BLTAMOD		; BLTAMOD
	MOVE.W	#$0,BLTDMOD		; BLTDMOD

	MOVE.W	#bypl*he-1,D6		; OPT
	ADD.W	D6,A5
	MOVE.W	#%0000000000000010,BLTCON1	; BLTCON1 BIT 12 DESC MODE

	MOVE.L	A5,BLTAPTH		; BLTAPT
	MOVE.L	A5,BLTDPTH
	MOVE.W	#he*64+wi/16,BLTSIZE	; BLTSIZE
	; ## MAIN BLIT ####
	RTS

__FETCH_TXT:
	MOVE.W	FRAMESINDEX,D1
	CMPI.W	#TXT_FRMSKIP,D1	; TXT_FRMSKIP
	BNE.W	.skip
	LEA	BGMASK,A4
	LEA	FONT,A5
	LEA	TEXT,A3
	ADD.W	TEXTINDEX,A3
	CLR.L	D2
	MOVE.B	(A3),D2
	CMP.B	#$AA,D2		; Siamo arrivati all'ultima word della TAB?
	BNE.S	.proceed
	MOVE.W	#0,TEXTINDEX	; Riparti a puntare dalla prima word
	LEA	TEXT,A3		; FIX FOR GLITCH (I KNOW IT'S FUN... :)
	.proceed:
	SUBI.B	#$20,D2		; TOGLI 32 AL VALORE ASCII DEL CARATTERE, IN
	LSL.W	#3,D2		; MOLTIPLICA PER 8 IL NUMERO PRECEDENTE,
	ADD.W	D2,A5
	CLR.L	D6		; RESET D6
	MOVE.B	#8-1,D6
	.loop:
	MOVE.B	(A5)+,(A4)
	ADD.W	#bypl,A4		; POSITIONING
	DBRA	D6,.loop
	.skip:
	SUBI.W	#1,D1
	TST.W	D1
	BEQ.W	.reset
	MOVE.W	D1,FRAMESINDEX
	BRA.S	.jump
	.reset:
	ADDI.W	#1,TEXTINDEX
	MOVE.W	#TXT_FRMSKIP,D1
	MOVE.W	D1,FRAMESINDEX	; OTTIMIZZABILE
	.jump:
	RTS

__BLK_0:	RTS

;********** Fastmem Data **********
TIMELINE:		DC.L __BLK_0,__BLK_0,__BLK_0,__BLK_0
FRAME_STROBE:	DC.B 0,0
KICKSTART_ADDR:	DC.L $F80000	; POINTERS TO BITMAPS
TEXTINDEX:	DC.W 0
FRAMESINDEX:	DC.W TXT_FRMSKIP

FONT:		DC.L 0,0		; SPACE CHAR
		INCBIN "cosmicalien_font.raw",0
		EVEN
TEXT:		INCLUDE "textscroller.i"

;*******************************************************************************
	SECTION	ChipData,DATA_C	;declared data that must be in chipmem
;*******************************************************************************

	; INCLUDES HERE

COPPER:
	DC.W $1FC,0	; Slow fetch mode, remove if AGA demo.
	DC.W $8E,$2C81	; 238h display window top, left | DIWSTRT - 11.393
	DC.W $90,$2CC1	; and bottom, right.	| DIWSTOP - 11.457
	DC.W $92,$38	; Standard bitplane dma fetch start
	DC.W $94,$D0	; and stop for standard screen.
	DC.W $106,$0C00	; (AGA compat. if any Dual Playf. mode)
	DC.W $108,0	; BPL1MOD	 Bitplane modulo (odd planes)
	DC.W $10A,0	; BPL2MOD Bitplane modulo (even planes)
	DC.W $102,0	; SCROLL REGISTER (AND PLAYFIELD PRI)

	.Palette:
	DC.W $0180,$0111,$0182,$0333,$0184,$0777,$0186,$0BBB
	DC.W $0188,$0F11,$018A,$01F1,$018C,$011F,$018E,$0F1F
	DC.W $0190,$0111,$0192,$0111,$0194,$0111,$0196,$0111
	DC.W $0198,$0F11,$019A,$0F11,$019C,$0F11,$019E,$0F11
	DC.W $01A0,$0F11,$01A2,$0F11,$01A4,$0F11,$01A6,$0F11
	DC.W $01A8,$0F11,$01AA,$0F11,$01AC,$0F11,$01AE,$0F11

	.SpriteColors:
	DC.W $01A0,$0000
	DC.W $01A2,$0FFF
	DC.W $01A4,$018F
	DC.W $01A6,$07DF

	DC.W $01A8,$0000
	DC.W $01AA,$0FFF
	DC.W $01AC,$018F
	DC.W $01AE,$07DF

	DC.W $01B0,$0000
	DC.W $01B2,$0FFF
	DC.W $01B4,$018F
	DC.W $01B6,$07DF

	DC.W $01B8,$0000
	DC.W $01BA,$0FFF
	DC.W $01BC,$018F
	DC.W $01BE,$07DF

	.BplPtrs:
	DC.W $E0,0
	DC.W $E2,0
	DC.W $E4,0
	DC.W $E6,0
	DC.W $E8,0
	DC.W $EA,0
	DC.W $EC,0
	DC.W $EE,0
	DC.W $F0,0
	DC.W $F2,0
	DC.W $F4,0
	DC.W $F6,0		;full 6 ptrs, in case you increase bpls
	DC.W $100,bpls*$1000+$200	;enable bitplanes

	.SpritePointers:
	DC.W $0120,0,$122,0	; 0
	DC.W $0124,0,$126,0	; 1
	DC.W $0128,0,$12A,0	; 2
	DC.W $012C,0,$12E,0	; 3
	DC.W $0130,0,$132,0	; 4
	DC.W $0134,0,$136,0	; 5
	DC.W $0138,0,$13A,0	; 6
	DC.W $013C,0,$13E,0	; 7

	.Waits:
	DC.W $4D01,$FF00		; ## START ##
	.BplPtrs2:
	DC.W $E0,0
	DC.W $E2,0
	DC.W $E4,0
	DC.W $E6,0
	DC.W $9001,$FF00		; ## START ##
	.BplPtrs3:
	DC.W $E0,0
	DC.W $E2,0
	DC.W $E4,0
	DC.W $E6,0

	DC.W $FFDF,$FFFE	; allow VPOS>$ff
	DC.W $3501,$FF00	; ## RASTER END ## #$12C?
	DC.W $009A,$0010	; CLEAR RASTER BUSY FLAG
	DC.W $FFFF,$FFFE	; magic value to end copperlist

;*******************************************************************************
	SECTION ChipBuffers,BSS_C	;BSS doesn't count toward exe size
;*******************************************************************************

BLEEDTOP:		DS.B bypl
BGNOISE1:		DS.B 50*bypl
BGNOISE2:		DS.B 50*bypl
BGPLANE2:		DS.B he*bypl
BGMASK:		DS.B he*bypl
BGEMPTY:		DS.B he*bypl
PIXEL_ON:		DS.B PIXEL_SIDE*(PIXEL_SIDE/16*2)
PIXEL_OFF:	DS.B PIXEL_SIDE*(PIXEL_SIDE/16*2)
END
