;*** CODE: KONEY ***
;*** MiniStartup by Photon ***
	INCDIR	"NAS:AMIGA/CODE/rusteater_amiga/"
	SECTION	"Code",CODE
	INCLUDE	"PhotonsMiniWrapper1.04!.s"
	INCLUDE	"custom-registers.i"
	INCLUDE	"med/med_feature_control.i"	; MED CFGs
	INCLUDE	"med/MED_PlayRoutine.i"
;********** Constants **********
wi		EQU 320+32
he		EQU 256		; screen height
bpls		EQU 4		; depth
bypl		EQU wi/16*2	; byte-width of 1 bitplane line (40bytes)
bwid		EQU bpls*bypl	; byte-width of 1 pixel line (all bpls)
SCROLL_SPEED	EQU 2
PIXELSIDE_W	EQU 32/SCROLL_SPEED
PIXELSIDE_H	EQU 32
TXT_FRMSKIP 	EQU PIXELSIDE_W*6
;*************
_PushColorsDown:	MACRO
	LEA	\1,A0
	ADD.W	\2,A0		; FASTER THAN LEA \1+16
	LEA	$DFF190,A1
	MOVE.W	(A0),$DFF180
	REPT 2
	MOVE.L	(A0)+,(A1)+
	ENDR
		ENDM
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
	LEA	COPPER\.BplPtrs+8,A1
	BSR.W	PokePtrs
	LEA	BGEMPTY,A0
	LEA	COPPER\.BplPtrs+16,A1
	BSR.W	PokePtrs
	LEA	BGFILLED,A0
	LEA	COPPER\.BplPtrs+24,A1
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
	LEA	COPPER\.BplPtrs4,A1
	BSR.W	PokePtrs
	LEA	BGNOISE1,A0
	LEA	4(A0),A0
	LEA	COPPER\.BplPtrs4+8,A1
	BSR.W	PokePtrs
	; # NOISE AREA ##
	LEA	BGMASK,A0
	LEA	COPPER\.BplPtrs3,A1
	BSR.W	PokePtrs
	LEA	BGFILLED,A0
	LEA	COPPER\.BplPtrs5,A1
	BSR.W	PokePtrs

	; #### CPU INTENSIVE TASKS BEFORE STARTING MUSIC
	LEA	BGMASK,A0
	LEA	BGFILLED,A1
	MOVE.W	#bypl*he/4-1,D0
	.loop:
	MOVE.L	#-1,(A0)+		; FILL THE PIXEL
	MOVE.L	#-1,(A1)+		; FILL THE PIXEL
	DBRA	D0,.loop

	_PushColorsDown	BG_COLS_TBL,#16
	MOVE.L	BGNOISE1,D7
	; #### CPU INTENSIVE TASKS BEFORE STARTING MUSIC

	; in photon's wrapper comment:;move.w d2,$9a(a6) ;INTENA
	;MOVE.W	#17,MED_START_POS	 ; skip to pos# after first block
	JSR	_startmusic
	MOVE.L	#COPPER,COP1LC
;********************  main loop  ********************
MainLoop:
	; do stuff here :)
	ADDQ.W	#1,MED_TRK_0_COUNT	; inc elapsed #calls since last
	ADDQ.W	#1,MED_TRK_1_COUNT
	ADDQ.W	#1,MED_TRK_2_COUNT
	ADDQ.W	#1,MED_TRK_3_COUNT
	; ## NOISE SECTION ##
	TST.B	FRAME_STROBE
	BNE.W	.oddFrame
	MOVE.B	#1,FRAME_STROBE

	CLR.L	D1
	MOVE.B	MED_TRK_1_INST,D1
	LSR.W	D1
	LSL.W	#$4,D1
	_PushColorsDown	BG_COLS_TBL,D1

	MOVE.W	#(bypl/2)*50-1,D4
	LEA	BGNOISE1,A4
	BSR.W	__RANDOMIZE_PLANE

	BRA.W	.evenFrame
	.oddFrame:
	MOVE.B	#0,FRAME_STROBE

	MOVE.W	#(bypl/2)*50-1,D4
	LEA	BGNOISE2,A4
	BSR.W	__RANDOMIZE_PLANE
	.evenFrame:
	; ## NOISE SECTION ##

	; ## TEXT SECTION ##
	BSR.W	__FETCH_TXT
	; ## TEXT SECTION ##

	;BTST	#6,$BFE001	; POTINP - LMB pressed?
	;BNE.W	.dontScroll
	.dontScroll:

	; ## MASKING SECTION ##
	LEA	BGMASK+bypl-PIXELSIDE_W/16*2,A5
	;LEA	BGMASK,A5
	LEA	CHAR_BUFFER,A0
	ADD.W	LINEINDEX,A0
	MOVE.W	#5-1,D0
	.loop:
	BTST	D0,(A0)
	BNE.S	.off
	LEA	BGEMPTY,A4
	BSR.W	__BLIT_PIXEL
	BRA.S	.on
	.off:
	LEA	BGFILLED,A4
	BSR.W	__BLIT_PIXEL	;_TEXTURED
	.on:

	ADD.L	#bypl*PIXELSIDE_H,A5
	DBRA	D0,.loop

	TST.W	DUMMYINDEX
	BNE.S	.sameLine
	MOVE.W	#PIXELSIDE_W+1,DUMMYINDEX
	ADDI.W	#1,LINEINDEX
	.sameLine:
	SUBI.W	#1,DUMMYINDEX
	; ## MASKING SECTION ##

	; ## SCROLLING SECTION ##
	LEA	BGMASK,A5
	BSR.W	__SCROLL_X
	; ## SCROLLING SECTION ##

	;BSR.W	__HW_DISPLACE

	.WaitRasterCopper:
	;MOVE.W	#$0F0F,$DFF180	; show rastertime left down to $12c
	BTST	#$4,INTENAR+1
	BNE.S	.WaitRasterCopper
	;MOVE.W	#$0000,$DFF180	; show rastertime left down to $12c
	MOVE.W	#$8010,INTENA

	;*--- main loop end ---*
	BTST	#6,$BFE001	; POTINP - LMB pressed?
	;BEQ.W	.exit
	BTST	#2,$DFF016	; POTINP - RMB pressed?
	BNE.W	MainLoop		; then loop
	;*--- exit ---*
	.exit:
	; ---  quit MED code  ---
	MOVEM.L	D0-A6,-(SP)
	JSR	_endmusic
	MOVEM.L	(SP)+,D0-A6
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
	RTS

	_RandomWord:
	BSR	_RandomByte
	ROL.W	#$8,D5
	_RandomByte:
	MOVE.B	$DFF007,D5	;$dff00a $dff00b for mouse pos
	;MOVE.W	MED_TRK_2_COUNT,D5
	;MOVE.B	$BFD800,D3
	MOVE.W	MED_TRK_2_COUNT,D3
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
	BSR	_RandomByte
	ROL.W	#8,D5
	_RandomByte1:
	MOVE.B	$DFF007,D5	;$dff00a $dff00b for mouse pos
	MOVE.B	$BFD800,D3
	EOR.B	D3,D5
	RTS

__BLIT_PIXEL:
	BSR	WaitBlitter
	MOVE.W	#%0000100111110000,BLTCON0		; BLTCON0
	MOVE.W	#%0000000000000000,BLTCON1		; BLTCON1
	MOVE.L	#$FFFFFFFF,BLTAFWM		; THEY'LL NEVER
	MOVE.W	#$0,BLTAMOD			; BLTAMOD =0 for texture
	MOVE.W	#bypl-(PIXELSIDE_W/16*2),BLTDMOD	; BLTDMOD 40-4=36
	MOVE.L	A4,BLTAPTH			; BLTAPT
	MOVE.L	A5,BLTDPTH
	MOVE.W	#PIXELSIDE_H*64+(PIXELSIDE_W/16),BLTSIZE ; BLTSIZE
	RTS

__BLIT_PIXEL_TEXTURED:
	BSR	WaitBlitter
	MOVE.W	#%00001111111001010,BLTCON0		; BLTCON0
	MOVE.W	#%0000000000000000,BLTCON1		; BLTCON1
	MOVE.W	#$0,BLTAMOD			; BLTAMOD =0 for texture
	MOVE.W	#bypl-(PIXELSIDE_W/16*2),BLTBMOD	; BLTBMOD 40-4=36
	MOVE.W	#bypl-(PIXELSIDE_W/16*2),BLTCMOD	; BLTCMOD 40-4=36
	MOVE.W	#bypl-(PIXELSIDE_W/16*2),BLTDMOD	; BLTDMOD 40-4=36

	MOVE.L	#BGNOISE2+2,BLTAPTH		; TEXTURE
	MOVE.L	A4,BLTBPTH			; BLTAPT
	MOVE.L	A5,BLTCPTH
	MOVE.L	A5,BLTDPTH
	MOVE.W	#PIXELSIDE_H*64+(PIXELSIDE_W/16),BLTSIZE ; BLTSIZE
	RTS

__SCROLL_X:
	MOVE.L	#%1001111100001000,D1	; %1000100111110000 +ROL.W	#4,D1
	MOVE.W	#SCROLL_SPEED,D0
	MOVE.B	D0,D1
	ROR.W	#4,D1
	BSR	WaitBlitter
	MOVE.W	D1,BLTCON0		; BLTCON0
	MOVE.W	#bypl*(PIXELSIDE_H*5)-1,D6	; POSITION FOR DESC
	ADD.W	D6,A5
	MOVE.W	#%0000000000000010,BLTCON1	; BLTCON1 BIT 12 DESC MODE

	MOVE.L	#$FFFFFFFF,BLTAFWM		; THEY'LL NEVER
	MOVE.L	#$0,BLTAMOD		; BLTAMOD

	MOVE.L	A5,BLTAPTH		; BLTAPT
	MOVE.L	A5,BLTDPTH
	MOVE.W	#(PIXELSIDE_H*5)*64+wi/16,BLTSIZE ; BLTSIZE
	RTS

__FETCH_TXT:
	MOVE.W	FRAMESINDEX,D1
	SUBI.W	#1,FRAMESINDEX
	CMPI.W	#TXT_FRMSKIP,D1	; TXT_FRMSKIP
	BNE.W	.skip
	MOVE.W	#0,LINEINDEX
	LEA	CHAR_BUFFER+5,A4
	LEA	FONT+3,A5
	LEA	TEXT,A3
	ADD.W	TEXTINDEX,A3
	CLR.L	D2
	MOVE.B	(A3),D2
	CMP.B	#$AA,D2		; 010101 for eof
	BNE.S	.proceed
	MOVE.W	#0,TEXTINDEX	; RESTART FROM FIRST
	LEA	TEXT,A3		; FIX FOR GLITCH (I KNOW IT'S FUN... :)
	.proceed:
	SUBI.B	#$20,D2		; ASCII
	LSL.W	#3,D2		; CALCULATIONS
	ADD.W	D2,A5
	CLR.L	D6		; RESET D6
	MOVE.B	#7-1,D6
	.loop:
	MOVE.B	(A5)+,-(A4)
	;ADD.W	#bypl-2,A4	; POSITIONING
	DBRA	D6,.loop
	BSR.W	__ROTATE_MATRIX

	.skip:
	TST.W	FRAMESINDEX
	BNE.S	.jump
	ADDI.W	#1,TEXTINDEX
	MOVE.W	#TXT_FRMSKIP,FRAMESINDEX	; OTTIMIZZABILE
	MOVE.W	#PIXELSIDE_W+1,DUMMYINDEX
	.jump:
	RTS

__ROTATE_MATRIX:
	CLR.L	D0		; RESET D6
	LEA	CHAR_BUFFER,A4
	LEA	CHAR_ROTATION,A5
	LEA	(A4),A1		; BUFFER

	MOVE.L	(A4),(A5)		; COPY
	MOVE.L	4(A4),4(A5)	; TEMP
	MOVE.L	#0,(A4)		; CLEAR
	MOVE.W	#0,4(A4)		; CLEAR

	MOVE.W	#0,D6
	MOVE.W	#6-1,D1
	.iterate:
	MOVE.B	(A5)+,D0
	LEA	(A1),A4		; BUFFER
	MOVE.W	#6-1,D2
	.loop:
	BTST	D2,D0
	BNE.S	.skip0
	BSET.B	D6,(A4)
	.skip0:
	MOVE.B	(A4),(A4)+
	DBRA	D2,.loop
	ADD.W	#1,D6
	DBRA	D1,.iterate
	RTS

__HW_DISPLACE:
	CLR.L	D2
	MOVE.W	$DFF006,D4	; for bug?
	.waitVisibleRaster:
	MOVE.W	$DFF006,D4
	AND.W	#$FF00,D4		; read vertical beam
	CMP.W	#$3700,D4		; 2C
	BNE.S	.waitVisibleRaster

	.waitNextRaster:
	MOVE.W	$DFF006,D2
	AND.W	#$FF00,D2		; read vertical beam
	CMP.W	D4,D2
	BEQ.S	.waitNextRaster

	MOVE.W	D2,D4
	MOVE.B	$DFF007,D5	; $dff00a $dff00b for mouse pos
	MOVE.B	$BFD800,D1
	EOR.B	D1,D5
	MOVE.W	D5,BPLCON1	; 19DEA68E GLITCHA

	MOVE.W	$DFF004,D0	; Read vert most sig. bits
	BTST	#0,D0
	BEQ.S	.waitNextRaster

	CMP.W	#$0A00,D2		; DONT DISPLACE TXT
	BGE.S	.dontSkip		; DONT DISPLACE TXT
	MOVE.W	#0,BPLCON1	; RESET REGISTER
	
	.dontSkip:
	CMP.W	#$2F00,D2		; 12.032
	BNE.S	.waitNextRaster
	MOVE.W	#0,BPLCON1	; RESET REGISTER
	RTS

__BLK_0:	RTS

;********** Fastmem Data **********
TIMELINE:		DC.L __BLK_0,__BLK_0,__BLK_0,__BLK_0
FRAME_STROBE:	DC.B 0,0
KICKSTART_ADDR:	DC.L $F80000	; POINTERS TO BITMAPS
TEXTINDEX:	DC.W 0
LINEINDEX:	DC.W 0
DUMMYINDEX:	DC.W PIXELSIDE_W+1
FRAMESINDEX:	DC.W TXT_FRMSKIP
MED_SONG_POS:	DC.W 0		; Well the position...
MED_BLOCK_LINE:	DC.W 0		; Line of block
AUDIOCHLEV_0:	DC.W 0
AUDIOCHLEV_1:	DC.W 0
AUDIOCHLEV_2:	DC.W 0
AUDIOCHLEV_3:	DC.W 0

FONT:		DC.L 0,0		; SPACE CHAR
		INCBIN "c_font_leftpadding2.raw",0
		EVEN
TEXT:		INCLUDE "textscroller.i"

BG_COLS_TBL:	DC.W $0000,$0000,$0000,$0000
		DC.W $0110,$0110,$0110,$0110
		DC.W $0111,$0111,$0111,$0111
		DC.W $0111,$0111,$0111,$0111
		DC.W $0221,$0221,$0221,$0221
		DC.W $0222,$0222,$0222,$0222
		DC.W $0222,$0222,$0222,$0222
		DC.W $0332,$0332,$0332,$0332
		DC.W $0333,$0333,$0333,$0333
		DC.W $0443,$0443,$0443,$0443
		DC.W $0444,$0444,$0444,$0444
		DC.W $0555,$0555,$0555,$0555
		DC.W $0444,$0444,$0444,$0444
		DC.W $0222,$0222,$0222,$0222
		DC.W $0111,$0111,$0111,$0111
		DC.W $0110,$0110,$0110,$0110
		DC.W $0000,$0000,$0000,$0000

;*******************************************************************************
	SECTION	ChipData,DATA_C	;declared data that must be in chipmem
;*******************************************************************************
MED_MODULE:	INCBIN "med/RustEater_2022_FIX2.med"
_chipzero:	DC.L 0
_MED_MODULE:

COPPER:
	DC.W $1FC,0	; Slow fetch mode, remove if AGA demo.
	DC.W $8E,$2C81	; 238h display window top, left | DIWSTRT - 11.393
	DC.W $90,$2CC1	; and bottom, right.	| DIWSTOP - 11.457
	DC.W $92,$38	; Standard bitplane dma fetch start
	DC.W $94,$D0	; and stop for standard screen.
	DC.W $106,$0C00	; (AGA compat. if any Dual Playf. mode)
	DC.W $108,4	; BPL1MOD	 Bitplane modulo (odd planes)
	DC.W $10A,4	; BPL2MOD Bitplane modulo (even planes)
	DC.W $102,0	; SCROLL REGISTER (AND PLAYFIELD PRI)

	.Palette:
	;DC.W $0180,$0111,
	DC.W $0182,$0443,$0184,$0776,$0186,$0CCB
	DC.W $0188,$0F11,$018A,$01F1,$018C,$011F,$018E,$0F1F
	;DC.W $0190,$0111,$0192,$0111,$0194,$0111,$0196,$0111
	DC.W $0198,$0F11,$019A,$0F11,$019C,$0F11,$019E,$0F11
	DC.W $01A0,$0F11,$01A2,$0F11,$01A4,$0F11,$01A6,$0F11
	DC.W $01A8,$0F11,$01AA,$0F11,$01AC,$0F11,$01AE,$0F11

	.SpritePointers:
	DC.W $0120,0,$122,0	; 0
	DC.W $0124,0,$126,0	; 1
	DC.W $0128,0,$12A,0	; 2
	DC.W $012C,0,$12E,0	; 3
	DC.W $0130,0,$132,0	; 4
	DC.W $0134,0,$136,0	; 5
	DC.W $0138,0,$13A,0	; 6
	DC.W $013C,0,$13E,0	; 7

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

	.Waits:
	DC.W $4D01,$FF00		; ## START ##
	.BplPtrs2:
	DC.W $E0,0
	DC.W $E2,0
	DC.W $E4,0
	DC.W $E6,0
	DC.W $5601,$FF00		; ## START ##
	.BplPtrs3:
	DC.W $EC,0
	DC.W $EE,0
	DC.W $9001,$FF00		; ## START ##
	.BplPtrs4:
	DC.W $E0,0
	DC.W $E2,0
	DC.W $E4,0
	DC.W $E6,0
	DC.W $F601,$FF00		; ## START ##
	.BplPtrs5:
	DC.W $EC,0
	DC.W $EE,0

	DC.W $FFDF,$FFFE	; allow VPOS>$ff
	DC.W $3501,$FF00	; ## RASTER END ## #$12C?
	DC.W $009A,$0010	; CLEAR RASTER BUSY FLAG
	DC.W $FFFF,$FFFE	; magic value to end copperlist

;*******************************************************************************
	SECTION ChipBuffers,BSS_C	;BSS doesn't count toward exe size
;*******************************************************************************

CHAR_BUFFER:	DS.B 8
CHAR_ROTATION:	DS.B 8
BLEEDTOP:		DS.B bypl
BGNOISE1:		DS.B 50*bypl
BGNOISE2:		DS.B 50*bypl
BGPLANE2:		DS.B he*bypl
BGMASK:		DS.B he*bypl
BGEMPTY:		DS.B he*bypl
BGFILLED:		DS.B he*bypl
END
