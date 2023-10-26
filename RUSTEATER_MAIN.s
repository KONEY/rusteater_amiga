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
;*******************************
FONT_SIZE		EQU 6
SCROLL_SPEED	EQU 2
PIXELSIDE_H	EQU 32
PIXELSIDE_W	EQU PIXELSIDE_H/SCROLL_SPEED
TXT_FRMSKIP	EQU PIXELSIDE_W*FONT_SIZE-PIXELSIDE_W/2
COP_WAITS		EQU 56
COP_FRAMES	EQU 50
COP_COLS_REGS	EQU 6
COP_BLIT_SIZE	EQU COP_COLS_REGS*2+2
;*******************************
_PushColors:	
	;MACRO
	;LEA	\1,A0
	;ADD.W	\2,A0		; FASTER THAN LEA \1+16
	;LEA	\3,A1
	;MOVE.L	(A0),D0
	;MOVE.W	D0,-16(A1)
	;REPT 2
	;MOVE.L	D0,(A1)+
	;ENDR
	;ENDM
;********** Demo **********	;Demo-specific non-startup code below.
Demo:			;a4=VBR, a6=Custom Registers Base addr
	;*--- init ---*
	MOVE.L	#VBint,$6C(A4)
	MOVE.W	#%1110000000100000,INTENA
	MOVE.W	#%1000001111100000,DMACON
	;BSET	#10,BPLCON0
	;*--- start copper ---*
	LEA	BGNOISE0,A0
	;LEA	2(A0),A0
	LEA	COPPER\.BplPtrs,A1
	BSR.W	PokePtrs
	LEA	BGNOISE0,A0
	LEA	-2(A0),A0
	MOVE.W	#0,(A0)
	LEA	COPPER\.BplPtrs+8,A1
	BSR.W	PokePtrs
	LEA	TEXTUREPLANE,A0
	LEA	COPPER\.BplPtrs+16,A1
	BSR.W	PokePtrs
	LEA	BGMASK1,A0
	LEA	COPPER\.BplPtrs+24,A1
	BSR.W	PokePtrs

	; #### CPU INTENSIVE TASKS BEFORE STARTING MUSIC
	LEA	TEXTUREPLANE,A4	; FILLS A PLANE
	MOVE.L	#$500000F5,D0
	MOVE.L	#$AFFFFF0A,D1
	BSR.W	__SCANLINIZE_PLANE	; __TEXTURIZE_PLANE
	BSR.W	__SCANLINIZE_PLANE	; __TEXTURIZE_PLANE
	BSR.W	__SCANLINIZE_PLANE	; __TEXTURIZE_PLANE
	MOVE.L	#$5AAAAAF5,D0
	MOVE.L	#$A555550A,D1
	BSR.W	__SCANLINIZE_PLANE	; __TEXTURIZE_PLANE
	BSR.W	__SCANLINIZE_PLANE	; __TEXTURIZE_PLANE
	MOVE.L	#$500000F5,D0
	MOVE.L	#$AFFFFF0A,D1
	BSR.W	__SCANLINIZE_PLANE	; __TEXTURIZE_PLANE
	BSR.W	__SCANLINIZE_PLANE	; __TEXTURIZE_PLANE
	BSR.W	__SCANLINIZE_PLANE	; __TEXTURIZE_PLANE

	LEA	BGMASK1,A1
	LEA	BGMASK2,A2
	LEA	BGFILLED,A0
	MOVE.W	#bypl*he/4-1,D0
	.loop:
	MOVE.L	#-1,(A0)+		; FILL THE PIXEL
	MOVE.L	#-1,(A1)+		; FILL THE PIXEL
	MOVE.L	#-1,(A2)+		; FILL THE PIXEL
	DBRA	D0,.loop

	;BSR.W	__RND
	;LSR.L	#2,D5
	;MOVE.W	D5,NOISE_SEED

	; #### EXTRACT COPPERLISTS  ######
	LEA	GRADIENT_VALS,A0
	LEA	COPPER_BUFFER,A1	; COPPER_BUFFER
	LEA	GRADIENT_REGISTERS,A3
	LEA	GRADIENT_PTRS,A4
	LEA	(A4),A5
	ADD.L	#COP_FRAMES*2*4-4,A5 ; A4 PTR START - A5 PTR STOP
	MOVE.W	#COP_FRAMES-1,D5
	.loop2:
	MOVE.L	A1,(A4)+
	MOVE.L	A1,-(A5)
	BSR.W	__DECRUNCH_COPPERLIST
	DBRA	D5,.loop2
	;LEA	GRADIENT_VALS,A0	; INITIAL COPPER
	;LEA	COPPER\.Waits,A1
	;BSR.W	__DECRUNCH_COPPERLIST
	; #### EXTRACT COPPERLISTS  ######
	LEA	COPPER_BUFFER,A4
	LEA	COPPER\.Waits,A5
	BSR.W	__BLIT_GRADIENT_IN_COPPER
	; #### CPU INTENSIVE TASKS BEFORE STARTING MUSIC

	; in photon's wrapper comment:;move.w d2,$9a(a6) ;INTENA
	;MOVE.W	#27,MED_START_POS	; skip to pos# after first block
	JSR	_startmusic
	MOVE.L	#COPPER,COP1LC
;********************  main loop  ********************
MainLoop:	
	;*--- swap buffers ---*
	MOVEM.L	DrawBuffer(PC),A0-A1
	EXG	A1,A0
	MOVEM.L	A0-A1,DrawBuffer	;draw into a2, show a3
	;*--- show one... ---*
	LEA	COPPER\.BplPtrs+24,A1
	;*--- ...draw into the other(a2) ---*

	BSR.W	PokePtrs
	; ## SCROLLING SECTION ##
	BSR.W	__SCROLL_X
	; ## SCROLLING SECTION ##

	;* FOR TIMED EVENTS ON BLOCKS ****
	;BSR.W	__SET_MED_VALUES
	MOVE.W	MED_SONG_POS,D5
	LSL.W	#$2,D5		; CALCULATES OFFSET (OPTIMIZED)
	LEA	TIMELINE,A3
	MOVE.L	(A3,D5.W),A4	; THANKS HEDGEHOG!!
	JSR	(A4)		; EXECUTE SUBROUTINE BLOCK#

	;MOVE.B	$dff007,$DFF186	; flash test
	;MOVE.B	$dff006,$DFF18E	; test

	; ## TEXT SECTION ##
	BSR.W	__FETCH_TXT
	; ## TEXT SECTION ##

	; ## MASKING SECTION ##
	LEA	__BLIT_PIXEL,A2
	MOVE.W	DUMMYINDEXPLOT,D0
	ADD.W	#$1,D0
	AND.W	#$7,D0
	MOVE.W	D0,DUMMYINDEXPLOT
	TST.W	D0
	BNE.S	.dontPlot
	LEA	BGMASK1+48*bypl+bypl-PIXELSIDE_W/16*2,A5
	LEA	CHAR_BUFFER,A0
	ADD.W	LINEINDEX,A0
	MOVE.W	#5-1,D0
	.loop:
	BTST	D0,(A0)
	BNE.S	.off
	LEA	BGEMPTY,A4
	JSR	(A2)		; __BLIT_PIXEL
	BRA.S	.on
	.off:
	LEA	BGFILLED,A4
	JSR	(A2)		; __BLIT_PIXEL
	.on:

	ADD.L	#bypl*PIXELSIDE_H,A5
	DBRA	D0,.loop

	.dontPlot:
	TST.W	DUMMYINDEX
	BNE.S	.sameLine
	MOVE.W	#PIXELSIDE_W+1,DUMMYINDEX
	ADDI.W	#1,LINEINDEX
	.sameLine:
	SUBI.W	#1,DUMMYINDEX
	; ## MASKING SECTION ##

	;BTST	#6,$BFE001	; POTINP - LMB pressed?
	;BNE.S	.skip
	;.skip:
	.WaitRasterCopper:
	;MOVE.W	#$0A0F,$DFF180	; show rastertime left down to $12c
	BTST	#$4,INTENAR+1
	BNE.S	.WaitRasterCopper
	;MOVE.W	#$0000,$DFF180	; show rastertime left down to $12c
	MOVE.W	#$8010,INTENA
	;*--- main loop end ---*
	BTST	#6,$BFE001	; POTINP - LMB pressed?
	BEQ.W	.exit
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
PokePtrs:				; SUPER SHRUNK REFACTOR
	MOVE.L	A0,-4(A0)		; Needs EMPTY plane to write addr
	MOVE.W	-4(A0),2(A1)	; high word of address
	MOVE.W	-2(A0),6(A1)	; low word of address
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

__DECRUNCH_COPPERLIST:
	MOVE.W	#COP_WAITS,D7
	.loop:
	TST.W	(A0)		; ZEROED WORD = allow VPOS>$ff
	BNE.S	.notFF
	MOVE.L	#$FFDFFFFE,(A1)+	; allow VPOS>$ff
	LEA	2(A0),A0		; NEXT
	BRA.S	.skip
	.notFF:
	MOVE.B	(A0)+,D0		; FIRST WAIT
	LSL.W	#8,D0
	MOVE.B	#$07,D0		; CMD RESTORED $1C07
	MOVE.W	D0,(A1)+		; WAIT
	MOVE.W	#$FFFE,(A1)+	; WAIT
	CLR.L	D1
	MOVE.B	(A0),D1		; BYTE FOR COLOR
	LSL.W	#4,D1		; EXTEND FIRST NIBBLE
	MOVE.B	(A0)+,D1		; FOR RED VALUE
	MOVE.W	#COP_COLS_REGS-1,D6
	.innerLoop:
	LSL.W	D6		; ONLY EVEN VALUES
	MOVE.W	(A3,D6.W),(A1)+	; COLOR REGISTER
	MOVE.W	D1,(A1)+		; COLOR VALUE
	LSR.W	D6		; GO BACK TO COUNTER
	DBRA	D6,.innerLoop
	.skip:
	DBRA	D7,.loop
	RTS

__BLIT_GRADIENT_IN_COPPER:
	MOVE.W	GRADIENT_INDEX,D0
	LEA	GRADIENT_PTRS,A3
	MOVE.L	(A3,D0.W),A4
	ADD.W	#$4,D0
	CMP.W	#COP_FRAMES*2*4-4,D0
	BLO.S	.dontReset
	MOVE.W	#$0,D0
	.dontReset:
	MOVE.W	D0,GRADIENT_INDEX
	LEA	COPPER\.Waits,A5
	MOVE.L	(A4)+,(A5)+	; Trick for alignment ;)
	_WaitBlitterNasty
	MOVE.L	#$FFFFFFFF,BLTAFWM
	MOVE.L	#(%0000100111110000<<16),BLTCON0
	MOVE.W	#0,BLTAMOD
	MOVE.W	#0,BLTDMOD
	MOVE.L	A4,BLTAPTH
	MOVE.L	A5,BLTDPTH
	MOVE.W	#(COP_WAITS<<6)+COP_BLIT_SIZE,BLTSIZE
	RTS

__BLIT_TEXTURE_IN_PLACE:
	;BSR.W	WaitBlitter
	;MOVE.L	#$FFFFFFFF,BLTAFWM		; BLTAFWM
	;MOVE.W	#%0001100111110000,BLTCON0	; BLTCON0
	;MOVE.W	#%0000000000000000,BLTCON1	; BLTCON1
	;MOVE.W	#0,BLTAMOD		; BLTAMOD
	;MOVE.W	#0,BLTDMOD			; Init modulo Dest D
	;MOVE.W	#he/8*64+wi/16,BLTSIZE	; Start Blitter (Blitsize)
	;;ADD.L	#(bypl*he/8),A4
	;;ADD.L	#(bypl*he/8),A5
	;RTS

__SET_MED_VALUES:
	IFNE INSTR_TRACKING
	MOVE.W	MED_STEPSEQ_POS,D0		; UPDATE STEPSEQUENCER
	ANDI.W	#$F,D0			; POSITION (0-15 = 16 LEDS)
	MOVE.W	D0,MED_STEPSEQ_POS

	LEA	MED_TRK_0_COUNT(PC),A0
	LEA	AUDIOCHLEV_0(PC),A2
	LEA	MED_TRK_0_INST(PC),A3
	MOVEQ	#$3,D1
	.loop:
	MOVEQ	#$F,D0			; maxvalue
	SUB.W	(A0)+,D0			; -#frames/irqs since instrument trigger
	BPL.S	.ok			; below minvalue?
	MOVEQ	#$0,D0			; then set to minvalue
	MOVE.W	D0,(A3)			; RESET TWO BYTES (INST+NOTE)
	.ok:
	MOVE.W	D0,(A2)+			; LEVEL VALUE TO USE IN CODE
	LEA	2(A3),A3
	DBF	D1,.loop
	ADD.L	#$10001,MED_TRK_0_COUNT	; inc elapsed #calls since last
	ADD.L	#$10001,MED_TRK_2_COUNT	; use LONG to save 8 cycles
	RTS
	ENDC

__RANDOMIZE_PLANE:
	;SWAP	D5
	;MOVE.B	D7,D5
	MOVE.L	D5,D1
	MOVE.W	#8-1,D2
	.outerLoop:
	MOVE.W	#(bypl/2)*5-1,D4
	.innerLoop:
	ROR.L	D1
	MOVE.L	D1,D5
	NOT.L	D5
	EOR.B	D3,D5		; D3 contains semi-random value from RND
	ASR.W	D5
	ROL.L	D5
	EOR.W	D2,D5
	NOT.L	D5
	MOVE.L	D5,(A4)
	;MOVE.L	#-1,(A4)
	BTST	D4,D5
	BNE.S	.skip
	ROL.L	D1
	SWAP	D1
	.skip:
	LEA	2(A4),A4
	DBRA	D4,.innerLoop
	;LEA	-2(A4),A4
	MOVE.L	D5,(A4)+
	ROR.L	D1
	SWAP	D1
	DBRA	D2,.outerLoop
	;MOVE.L	D5,(A4)
	RTS

__RND:
	BSR	._word
	SWAP	D5
	._word:
	BSR	._byte2
	ROL.W	#$8,D5
	._byte:
	MOVE.B	$DFF007,D5	;$dff00a $dff00b for mouse pos
	MOVE.B	$BFD800,D3
	EOR.B	D3,D5
	BRA.S	.noByte2
	._byte2:
	MOVE.B	$DFF007,D3	;$dff00a $dff00b for mouse pos
	MOVE.B	$BFD800,D5
	OR.B	D3,D5
	.noByte2:
	RTS

__BLIT_PIXEL:
	_WaitBlitterNasty
	MOVE.L	#(%0000100111110000<<16),BLTCON0
	MOVE.L	#$FFFFFFFF,BLTAFWM
	MOVE.W	#$0,BLTAMOD
	MOVE.W	#bypl-(PIXELSIDE_W/16*2),BLTDMOD
	MOVE.L	A4,BLTAPTH
	MOVE.L	A5,BLTDPTH
	MOVE.W	#PIXELSIDE_H*64+(PIXELSIDE_W/16),BLTSIZE
	RTS

__SCROLL_X:
	MOVE.L	ViewBuffer,A4	; DOUBLE
	MOVE.L	DrawBuffer,A5	; BUFFERING ;)
	ADD.L	#bypl*48+bypl*(PIXELSIDE_H*5)-1,A4
	ADD.L	#bypl*48+bypl*(PIXELSIDE_H*5)-1,A5
	;ADD.W	#bypl*(PIXELSIDE_H*5)-1,A5
	;BSR	WaitBlitter
	_WaitBlitterNasty
	MOVE.L	#(((SCROLL_SPEED<<12)+%100111110000)<<16)+%10,BLTCON0
	MOVE.L	#$FFFFFFFF,BLTAFWM
	MOVE.L	#$0,BLTAMOD
	MOVE.L	A4,BLTAPTH
	MOVE.L	A5,BLTDPTH
	MOVE.W	#(PIXELSIDE_H*5)*64+wi/16,BLTSIZE
	RTS

__FETCH_TXT:
	MOVE.W	FRAMESINDEX,D1
	SUBI.W	#$1,FRAMESINDEX
	CMPI.W	#TXT_FRMSKIP,D1	; TXT_FRMSKIP
	BNE.W	.skip
	MOVE.W	#$0,LINEINDEX
	LEA	CHAR_BUFFER+5,A4
	LEA	FONT+3,A5
	LEA	TEXT,A3
	ADD.W	TEXTINDEX,A3
	CLR.L	D2
	MOVE.B	(A3),D2
	CMP.B	#$AA,D2		; 010101 for eof
	BNE.S	.proceed
	MOVE.W	#$0,TEXTINDEX	; RESTART FROM FIRST
	LEA	TEXT,A3		; FIX FOR GLITCH (I KNOW IT'S FUN... :)
	.proceed:
	CMP.B	#$20,D2		; IS SPACE?
	BNE.S	.notSpace
	SUBI.W	#PIXELSIDE_W*4,FRAMESINDEX	; REDUCE SPACE WIDTH
	.notSpace:
	SUBI.B	#$20,D2		; ASCII
	LSL.W	#$3,D2		; CALCULATIONS
	ADD.W	D2,A5
	MOVE.W	#$7-1,D6
	.loop:
	MOVE.B	(A5)+,-(A4)
	DBRA	D6,.loop
	BSR.W	__ROTATE_MATRIX

	.skip:
	TST.W	FRAMESINDEX
	BNE.S	.jump
	ADDI.W	#$1,TEXTINDEX
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

__SCANLINIZE_PLANE:
	MOVE.W	#(bypl/4*he/16)-1,D7
	.outerLoop:
	MOVEM.L	D0-D1,(A4)
	LEA	8(A4),A4
	DBRA	D7,.outerLoop
	RTS

__BLIT_NOISE_FILLED:
	ADD.L	#(bypl*40)-2,A4
	ADD.L	#(bypl*80)-2,A5
	_WaitBlitterNasty
	MOVE.L	#$FFFFFFFF,BLTAFWM
	MOVE.L	#(%0011100111110000<<16)+%10,BLTCON0
	MOVE.W	#0,BLTAMOD
	MOVE.W	#0,BLTDMOD
	MOVE.L	A4,BLTAPTH
	MOVE.L	A5,BLTDPTH
	MOVE.W	#40*64+wi/16,BLTSIZE
	RTS

__NOISE_SECTION:
	MOVEM.L	RNDBYTE0,A1
	MOVE.W	MED_SONG_POS,D5
	TST.B	FRAME_STROBE
	BNE.W	.oddFrame
	MOVE.B	#1,FRAME_STROBE
	LSR.L	D5
	LEA	BGNOISE1,A4
	JSR	(A1)		; BSR.W	__RND\._byte
	MOVE.L	D5,NOISE_SEED
	BSR.W	__RANDOMIZE_PLANE
	LEA	BGNOISE1,A4
	LEA	BGNOISE2,A5
	BSR.W	__BLIT_NOISE_FILLED
	BRA.W	.evenFrame
	.oddFrame:
	MOVE.B	#0,FRAME_STROBE
	LEA	BGNOISE2,A4
	MOVE.L	NOISE_SEED,D5
	LEA.L	RNDBYTE0,A3
	MOVEM.L	RNDBYTE1,A0
	MOVEM.L	A0-A1,(A3)
	BSR.W	__RANDOMIZE_PLANE
	LEA	BGNOISE2,A4
	LEA	BGNOISE1,A5
	BSR.W	__BLIT_NOISE_FILLED
	.evenFrame:
	RTS

__BLK_MAIN:
	BSR.W	__BLIT_GRADIENT_IN_COPPER
	.noGradient:
	; ## NOISE SECTION ##
	BSR.W	__NOISE_SECTION
	; ## NOISE SECTION ##
	RTS

__BLK_HALF:
	TST.B	FRAME_STROBE
	BNE.S	.oddFrame
	BSR.W	__BLIT_GRADIENT_IN_COPPER
	.oddFrame:
	; ## NOISE SECTION ##
	BSR.W	__NOISE_SECTION
	; ## NOISE SECTION ##
	RTS

;********** Fastmem Data **********
TIMELINE:		;DC.L __BLK_MAIN,__BLK_MAIN,__BLK_MAIN,__BLK_MAIN
		;DC.L __BLK_MAIN\.noGradient,__BLK_MAIN\.noGradient,__BLK_HALF,__BLK_HALF
		DC.L __BLK_HALF,__BLK_HALF,__BLK_HALF,__BLK_MAIN
		DC.L __BLK_MAIN,__BLK_HALF,__BLK_HALF,__BLK_MAIN
		DC.L __BLK_MAIN,__BLK_MAIN,__BLK_MAIN,__BLK_MAIN
		DC.L __BLK_HALF,__BLK_HALF,__BLK_HALF,__BLK_HALF
		DC.L __BLK_HALF,__BLK_HALF,__BLK_MAIN,__BLK_MAIN
		DC.L __BLK_MAIN,__BLK_HALF,__BLK_MAIN,__BLK_MAIN
		DC.L __BLK_MAIN,__BLK_MAIN,__BLK_MAIN,__BLK_MAIN
		DC.L __BLK_MAIN,__BLK_MAIN,__BLK_MAIN,__BLK_HALF
		DC.L __BLK_HALF,__BLK_MAIN,__BLK_MAIN,__BLK_MAIN
		DC.L __BLK_MAIN,__BLK_HALF
FRAME_STROBE:	DC.B 0,0
TEXTINDEX:	DC.W 0
LINEINDEX:	DC.W 0
DUMMYINDEX:	DC.W PIXELSIDE_W+1
DUMMYINDEXPLOT:	DC.W 0
FRAMESINDEX:	DC.W TXT_FRMSKIP
MED_SONG_POS:	DC.W 0		; Well the position...
;MED_BLOCK_LINE:	DC.W 0		; Line of block
;AUDIOCHLEV_0:	DC.W 0
;AUDIOCHLEV_1:	DC.W 0
;AUDIOCHLEV_2:	DC.W 0
;AUDIOCHLEV_3:	DC.W 0
NOISE_SEED:	DC.L 0
RNDBYTE0:		DC.L __RND\._byte2
RNDBYTE1:		DC.L __RND\._byte
DrawBuffer:	DC.L BGMASK1		; pointers to buffers
ViewBuffer:	DC.L BGMASK2		; to be swapped

FONT:		DC.L 0,0		; SPACE CHAR
		INCBIN "c_font_leftpadding2.raw",0
		EVEN
TEXT:		INCLUDE "textscroller.i"

BG_COLS_TBL:	;DC.W $0000,$0000
		;DC.W $0110,$0110
		;DC.W $0111,$0111
		;DC.W $0111,$0111
		;DC.W $0221,$0221
		;DC.W $0222,$0222
		;DC.W $0222,$0222
		;DC.W $0332,$0332
		;DC.W $0333,$0333
		;DC.W $0443,$0443
		;DC.W $0444,$0444
		;DC.W $0555,$0555
		;DC.W $0444,$0444
		;DC.W $0222,$0222
		;DC.W $0111,$0111
		;DC.W $0110,$0110
		;DC.W $0000,$0000
FG_COLS_TBL:	;DC.W $0665,$0665
		;DC.W $0555,$0555
		;DC.W $0554,$0554
		;DC.W $0444,$0444
		;DC.W $0443,$0443
		;DC.W $0333,$0333
		;DC.W $0332,$0332
		;DC.W $0222,$0222
		;DC.W $0221,$0221
		;DC.W $0111,$0111
		;DC.W $0110,$0110
		;DC.W $0000,$0000
		;DC.W $0111,$0111
		;DC.W $0222,$0222
		;DC.W $0333,$0333
		;DC.W $0444,$0444
		;DC.W $0555,$0555

GRADIENT_REGISTERS:	DC.W $0182,$0188,$0198,$019A,$019C,$019E
GRADIENT_INDEX:	DC.W 0
GRADIENT_VALS:	INCLUDE "CopGradients.i"

;*******************************************************************************
	SECTION	ChipData,DATA_C	;declared data that must be in chipmem
;*******************************************************************************
MED_MODULE:	INCBIN "med/RustEater_2022_FIX4.med"
_chipzero:	DC.L 0
_MED_MODULE:

COPPER:	; #### COPPERLIST ####################################################
	DC.W $1FC,0	; Slow fetch mode, remove if AGA demo.
	DC.W $8E,$2C81	; 238h display window top, left | DIWSTRT - 11.393
	DC.W $90,$2CC1	; and bottom, right.	| DIWSTOP - 11.457
	DC.W $92,$38	; Standard bitplane dma fetch start
	DC.W $94,$D0	; and stop for standard screen.
	DC.W $106,$0C00	; (AGA compat. if any Dual Playf. mode)
	.OddMod:
	DC.W $108,4	; BPL1MOD	 Bitplane modulo (odd planes)
	.EvenMod:
	DC.W $10A,4	; BPL2MOD Bitplane modulo (even planes)
	;.OddScroll:
	;DC.W $102,$00	; SCROLL REGISTER (AND PLAYFIELD PRI)

	.Palette:
	DC.W $0180,$0000,$0182,$0443,$0184,$0776,$0186,$0FFF
	DC.W $0188,$0443,$018A,$0AAA,$018C,$0888,$018E,$0DDD
	DC.W $0190,$0000,$0192,$0000,$0194,$0000,$0196,$0000
	DC.W $0198,$0443,$019A,$0443,$019C,$0443,$019E,$0443
	DC.W $01A0,$0445,$01A2,$0FFF,$01A4,$0999,$01A6,$0665
	DC.W $01A8,$0555,$01AA,$0666,$01AC,$0775,$01AE,$0889
	DC.W $01B0,$0110,$01B2,$0223,$01B4,$0221,$01B6,$0122
	DC.W $01B8,$0332,$01BA,$0222,$01BC,$0444,$01BE,$0221

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
	DC.W $E0,0,$E2,0
	DC.W $E4,0,$E6,0
	DC.W $E8,0,$EA,0
	DC.W $EC,0,$EE,0
	DC.W $F0,0,$F2,0
	DC.W $F4,0,$F6,0		;full 6 ptrs, in case you increase bpls
	DC.W $100,bpls*$1000+$200	;enable bitplanes
	;DC.W $100,bpls*$1000+%011000000000

	.Waits:
	DS.W COP_BLIT_SIZE*COP_WAITS+2	; +2 vpos >$FF

	;DC.W $FFDF,$FFFE		; allow VPOS>$ff
	DC.W $3507,$FF00		; ## RASTER END ## #$12C?
	DC.W $009A,$0010		; CLEAR RASTER BUSY FLAG
	DC.W $FFFF,$FFFE		; magic value to end copperlist

;*******************************************************************************
	SECTION ChipBuffers,BSS_C	;BSS doesn't count toward exe size
;*******************************************************************************
GRADIENT_PTRS:	DS.L COP_FRAMES*2
COPPER_BUFFER:	DS.W COP_FRAMES*(COP_BLIT_SIZE*COP_WAITS+2)	; +2 vpos >$FF
CHAR_BUFFER:	DS.B 8
CHAR_ROTATION:	DS.B 8
BGNOISE0		DS.B 48*bypl
BGNOISE1:		DS.B 80*bypl
BGNOISE2:		DS.B 80*bypl
TEXTUREPLANE:	DS.B he*bypl
BGEMPTY:		DS.B he*bypl
BGMASK1:		DS.B he*bypl
BGMASK2:		DS.B he*bypl
BGFILLED:		DS.B he*bypl
END
