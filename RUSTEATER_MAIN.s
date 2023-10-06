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
_PushColors:	MACRO
	LEA	\1,A0
	ADD.W	\2,A0		; FASTER THAN LEA \1+16
	LEA	\3,A1
	MOVE.L	(A0),D0
	MOVE.W	D0,-16(A1)
	REPT 2
	MOVE.L	D0,(A1)+
	ENDR
		ENDM
;********** Demo **********	;Demo-specific non-startup code below.
Demo:			;a4=VBR, a6=Custom Registers Base addr
	;*--- init ---*
	MOVE.L	#VBint,$6C(A4)
	MOVE.W	#%1110000000100000,INTENA
	MOVE.W	#%1000001111100000,DMACON
	;BSET	#10,BPLCON0
	;*--- start copper ---*
	; ## EMPTY AREA ##
	LEA	BGEMPTY,A0
	LEA	COPPER\.BplPtrs,A1
	BSR.W	PokePtrs
	LEA	BGEMPTY,A0
	LEA	COPPER\.BplPtrs+8,A1
	BSR.W	PokePtrs
	LEA	TEXTUREPLANE,A0
	LEA	COPPER\.BplPtrs+16,A1
	BSR.W	PokePtrs
	LEA	BGFILLED,A0
	LEA	COPPER\.BplPtrs+24,A1
	BSR.W	PokePtrs
	;LEA	TEXTUREPLANE,A0
	;LEA	44(A0),A0
	;LEA	COPPER\.BplPtrs+32,A1
	;BSR.W	PokePtrs
	; # NOISE AREA ##
	LEA	BGNOISE1,A0
	LEA	COPPER\.BplPtrs2,A1
	BSR.W	PokePtrs
	LEA	BGNOISE1,A0
	LEA	-2(A0),A0
	LEA	COPPER\.BplPtrs2+8,A1
	BSR.W	PokePtrs
	LEA	BGNOISE1,A0
	LEA	COPPER\.BplPtrs4,A1
	BSR.W	PokePtrs
	LEA	BGNOISE1,A0
	LEA	2(A0),A0
	LEA	COPPER\.BplPtrs4+8,A1
	BSR.W	PokePtrs
	; # NOISE AREA ##
	LEA	BGMASK,A0
	LEA	COPPER\.BplPtrs3,A1
	BSR.W	PokePtrs
	LEA	BGEMPTY,A0
	LEA	COPPER\.BplPtrs5,A1
	BSR.W	PokePtrs
	LEA	BGEMPTY,A0
	LEA	COPPER\.BplPtrs5+8,A1
	BSR.W	PokePtrs
	LEA	BGFILLED,A0
	LEA	COPPER\.BplPtrs5+16,A1
	BSR.W	PokePtrs

	; #### CPU INTENSIVE TASKS BEFORE STARTING MUSIC
	LEA	TEXTUREPLANE,A4	; FILLS A PLANE
	MOVE.L	#$F5A505A5,D5	; PARAMS
	BSR.W	__RandomWord	; PARS
	ROR.L	D3,D5		; PARS
	;BSR.W	__TEXTURIZE_PLANE
	MOVE.L	#$5A5A5A5A,D5	; PARAMS
	;BSR.W	__TEXTURIZE_PLANE

	LEA	BGMASK,A0
	LEA	BGFILLED,A1
	MOVE.W	#bypl*he/4-1,D0
	.loop:
	MOVE.L	#-1,(A0)+		; FILL THE PIXEL
	MOVE.L	#-1,(A1)+		; FILL THE PIXEL
	DBRA	D0,.loop

	;_PushColors	BG_COLS_TBL,#0,$DFF190
	;_PushColors	FG_COLS_TBL,#0,$DFF198
	CLR.L	D7
	; #### CPU INTENSIVE TASKS BEFORE STARTING MUSIC

	; in photon's wrapper comment:;move.w d2,$9a(a6) ;INTENA
	MOVE.W	#27,MED_START_POS	; skip to pos# after first block
	JSR	_startmusic
	MOVE.L	#COPPER,COP1LC
;********************  main loop  ********************
MainLoop:	
	; do stuff here :)
	BSR.W	__SET_MED_VALUES
	;* FOR TIMED EVENTS ON BLOCK ****
	MOVE.W	MED_SONG_POS,D5
	LSL.W	#$2,D5		; CALCULATES OFFSET (OPTIMIZED)
	LEA	TIMELINE,A3
	MOVE.L	(A3,D5),A4	; THANKS HEDGEHOG!!
	JSR	(A4)		; EXECUTE SUBROUTINE BLOCK#

	; ## TEXT SECTION ##
	BSR.W	__FETCH_TXT
	; ## TEXT SECTION ##

	; ## MASKING SECTION ##
	MOVE.W	DUMMYINDEXPLOT,D0
	ADD.W	#$1,D0
	AND.W	#$7,D0
	MOVE.W	D0,DUMMYINDEXPLOT
	TST.W	D0
	BNE.S	.dontPlot
	LEA	BGMASK+bypl-PIXELSIDE_W/16*2,A5
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
	;JSR	(A6)		
	BSR.W	__BLIT_PIXEL
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

	; ## SCROLLING SECTION ##
	LEA	BGMASK,A5
	BSR.W	__SCROLL_X
	; ## SCROLLING SECTION ##

	BTST	#6,$BFE001	; POTINP - LMB pressed?
	BNE.S	.skip
	LEA	TEXTUREPLANE,A4	; FILLS A PLANE
	MOVE.L	#$A5A5A5A5,D5	; PARAMS
	BSR.W	__RandomWord	; PARS
	ROR.L	D3,D5		; PARS
	BSR.W	__TEXTURIZE_PLANE
	BSR.W	__TEXTURIZE_PLANE
	BSR.W	__TEXTURIZE_PLANE
	BSR.W	__TEXTURIZE_PLANE
	.skip:

	.WaitRasterCopper:
	;MOVE.W	#$0F0F,$DFF180	; show rastertime left down to $12c
	BTST	#$4,INTENAR+1
	BNE.S	.WaitRasterCopper
	;MOVE.W	#$0000,$DFF180	; show rastertime left down to $12c
	MOVE.W	#$8010,INTENA

	;*--- main loop end ---*
	;BTST	#6,$BFE001	; POTINP - LMB pressed?
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
PokePtrs:				; SUPER SHRUNK REFACTOR
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

; ## VECTOR PART ##
__WIPE_PLANE:				; a1=screen destination address to clear
	BSR	WaitBlitter
	MOVE.W	#$4,BLTDMOD		; Init modulo Sou. A
	MOVE.L	#$01000000,BLTCON0		; set operation type in BLTCON0/1
	MOVE.L	A1,BLTDPTH		; destination address
	MOVE.W	#he*64+(wi-32)/16,BLTSIZE	; Start Blitter (Blitsize)
	;MOVE.W	#he*64+(wi/2)/16,BLTSIZE	; FIGHISSIMO!!
	RTS

__BLIT_VECTORS:
	; ## GLITCH ##
	;ROL.B	#1,D3
	;EXG.L	D0,D3
	;ROL.B	#1,D1
	;LSL.L	#2,D2
	; ## GLITCH ##
	BSR	WaitBlitter
	MOVE.L	#$FFFFFFFF,BLTAFWM	; BLTAFWM/BLTALWM = $FFFF
	MOVE.W	#$8000,BLTADAT	; BLTADAT = $8000
	MOVE.W	#bypl,BLTCMOD	; BLTCMOD = 40
	MOVE.W	#bypl,BLTDMOD	; BLTDMOD = 40
	MOVE.W	#$FFFF,BLTBDAT	; BLTBDAT = pattern della linea!

	LEA	COORDS_1,A0
	BSR.W	UPDATE_COORDS_OPT
	MOVE.W	(A0),D0
	MOVE.W	2(A0),D1
	LEA	COORDS_2,A0
	BSR.W	UPDATE_COORDS_OPT
	MOVE.W	(A0),D2
	MOVE.W	2(A0),D3
	BSR.W	Drawline

	LEA	COORDS_3,A0
	BSR.W	UPDATE_COORDS_OPT
	MOVE.W	(A0),D0
	MOVE.W	2(A0),D1
	LEA	COORDS_4,A0
	BSR.W	UPDATE_COORDS_OPT
	MOVE.W	(A0),D2
	MOVE.W	2(A0),D3
	BSR.W	Drawline

	LEA	COORDS_5,A0
	BSR.W	UPDATE_COORDS_OPT
	MOVE.W	(A0),D0
	MOVE.W	2(A0),D1
	LEA	COORDS_6,A0
	BSR.W	UPDATE_COORDS_OPT
	MOVE.W	(A0),D2
	MOVE.W	2(A0),D3
	BSR.W	Drawline
	RTS

Drawline:				; ROUTINE STOLEN FROM RAM_JAM
	sub.w	d1,d3		; D3=Y2-Y1
	beq.w	.skip		; per il fill non servono linee orizzontali 
	bgt.s	.y2gy1		; salta se positivo..
	exg	d0,d2		; ..altrimenti scambia i punti
	add.w	d3,d1		; mette in D1 la Y piu` piccola
	neg.w	d3		; D3=DY
	.y2gy1:
	mulu.w	#bypl,d1		; offset Y
	add.l	d1,A6
	moveq	#0,d1		; D1 indice nella tabella ottanti
	sub.w	d0,d2		; D2=X2-X1
	bge.s	.xdpos		; salta se positivo..
	addq.w	#2,d1		; ..altrimenti sposta l'indice
	neg.w	d2		; e rendi positiva la differenza
	.xdpos:
	moveq	#$f,D6		; maschera per i 4 bit bassi
	and.w	d0,D6		; selezionali in D4
	
				; solo se DL_Fill=1
	move.b	D6,d5		; calcola numero del bit da invertire
	not.b	d5		; (la BCHG numera i bit in modo inverso	

	lsr.w	#3,d0		; offset X:
				; Allinea a byte (serve per BCHG)
	add.w	d0,A6		; aggiunge all'indirizzo
				; nota che anche se l'indirizzo
				; e` dispari non fa nulla perche`
				; il blitter non tiene conto del
				; bit meno significativo di BLTxPT

	ror.w	#4,D6		; D4 = valore di shift A
	ori.w	#$0B4A,D6		; aggiunge l'opportuno
				; Minterm (OR o EOR)
	swap	D6		; valore di BLTCON0 nella word alta
		
	cmp.w	d2,d3		; confronta DiffX e DiffY
	bge.s	.dygdx		; salta se >=0..
	addq.w	#1,d1		; altrimenti setta il bit 0 del'indice
	exg	d2,d3		; e scambia le Diff
	.dygdx:
	add.w	d2,d2		; D2 = 2*DiffX
	move.w	d2,d0		; copia in D0
	sub.w	d3,d0		; D0 = 2*DiffX-DiffY
	addx.w	d1,d1		; moltiplica per 2 l'indice e
				; contemporaneamente aggiunge il flag
				; X che vale 1 se 2*DiffX-DiffY<0
				; (settato dalla sub.w)
	move.b	OKTS(PC,d1.w),D6	; legge l'ottante
	swap	d2		; valore BLTBMOD in word alta
	move.w	d0,d2		; word bassa D2=2*DiffX-DiffY
	sub.w	d3,d2		; word bassa D2=2*DiffX-2*DiffY
	moveq	#6,d1		; valore di shift e di test per
				; la wait blitter 
	lsl.w	d1,d3		; calcola il valore di BLTSIZE
	add.w	#$42,d3

	BSR	WaitBlitter
	bchg	d5,(A6)		; Inverte il primo bit della linea

	move.l	D6,BLTCON0	; BLTCON0/1
	move.l	D2,BLTBMOD	; BLTBMOD e BLTAMOD
	move.l	A6,BLTCPTH	; BLTCPT
	move.w	D0,BLTAPTL	; BLTAPTL
	move.l	A6,BLTDPTH	; BLTDPT - indirizzo schermo
	move.w	D3,BLTSIZE	; BLTSIZE
	.skip:
	rts

	OKTS:
	DC.B 3,3+$40
	DC.B 19,19+$40
	DC.B 11,11+$40
	DC.B 23,23+$40

__BLIT_3D_IN_PLACE:
	LEA	BUFFER3D,A4
	ADD.L	#(bypl*he)-4-2,A4
	LEA	TEXTUREPLANE,A5
	ADD.L	#(bypl*he)-4-2,A5
	BSR.W	WaitBlitter
	MOVE.W	#$FFFF,BLTAFWM		; BLTAFWM
	MOVE.W	#$FFFF,BLTALWM		; BLTALWM
	MOVE.W	#%0000100111110000,BLTCON0	; BLTCON0
	MOVE.W	#%0000000000010010,BLTCON1	; BLTCON1
	MOVE.W	#4,BLTAMOD		; BLTAMOD
	MOVE.W	#4,BLTDMOD		; Init modulo Dest D
	MOVE.L	A4,BLTAPTH		; BLTAPT  (fisso alla figura sorgente)
	MOVE.L	A5,BLTDPTH
	MOVE.W	#he*64+(wi-32)/16,BLTSIZE	; Start Blitter (Blitsize)
	RTS

UPDATE_COORDS_OPT:			; RETURNS (A0)=X - 2(A0)=Y
	TST.L	(A0)		; X-Y=0?
	BNE.S	.skip1
	MOVE.L	4(A0),D5
	NEG.W	D5
	SWAP	D5
	NEG.W	D5
	MOVE.L	D5,4(A0)
	BRA.S	.skipAll
	.skip1:

	CMP.L	#((wi-32)<<16),(A0)	; X=MAX & Y=0?
	BNE.S	.skip2
	MOVE.L	4(A0),D5
	SWAP	D5
	MOVE.L	D5,4(A0)
	BRA.S	.skipAll
	.skip2:

	CMP.L	#((wi-32)<<16)+he,(A0) ; X=MAX & Y=MAX?
	BNE.S	.skip3
	MOVE.L	4(A0),D5
	NEG.W	D5
	SWAP	D5
	NEG.W	D5
	MOVE.L	D5,4(A0)
	BRA.S	.skipAll
	.skip3:

	CMP.L	#he,(A0)		; X=0 & Y=MAX?
	BNE.S	.skip4
	MOVE.L	4(A0),D5
	SWAP	D5
	MOVE.L	D5,4(A0)
	BRA.S	.skipAll
	.skip4:

	.skipAll:
	MOVE.W	4(A0),D5		; DIR
	ADD.W	D5,(A0)
	MOVE.W	6(A0),D5
	ADD.W	D5,2(A0)
	RTS

UPDATE_COORDS:			; RETURNS (A0)=X - 2(A0)=Y
	TST.W	(A0)		; X=0?
	BNE.S	.skip1
	CMP.W	#he,2(A0)		; Y=MAX?
	BNE.S	.skip1
	MOVE.W	#0,4(A0)
	MOVE.W	#-2,6(A0)
	BRA.S	.skipAll
	.skip1:

	TST.W	(A0)		; X=0?
	BNE.S	.not0
	TST.W	2(A0)		; Y=0?
	BNE.S	.not0
	MOVE.W	#2,4(A0)
	MOVE.W	#0,6(A0)
	BRA.S	.skipAll
	.not0:
	CMP.W	#he,2(A0)		; Y=MAX?
	BNE.S	.notYMax
	MOVE.W	#-2,4(A0)
	MOVE.W	#0,6(A0)
	BRA.S	.skipAll
	.notYMax:
	CMP.W	#wi-32,(A0)	; X=MAX?
	BNE.S	.notXMax
	MOVE.W	#0,4(A0)
	MOVE.W	#2,6(A0)
	.notXMax:

	.skipAll:
	MOVE.W	4(A0),D5		; DIR
	ADD.W	D5,(A0)
	MOVE.W	6(A0),D5
	ADD.W	D5,2(A0)
	RTS
; ## VECTOR PART ##

__SET_MED_VALUES:
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

__RANDOMIZE_PLANE:
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

__RandomWord:
	BSR	._RandomByte
	ROL.W	#$8,D5
	._RandomByte:
	MOVE.B	$DFF007,D5	;$dff00a $dff00b for mouse pos
	;MOVE.W	MED_TRK_3_COUNT,D5	; AUDIOCHLEV_0
	MOVE.B	$BFD800,D3
	;MOVE.W	MED_TRK_1_INST,D3
	EOR.B	D3,D5
	RTS

__RANDOMIZE_PLANE_V1:
	;MOVE.L	#$F80000,A0
	;MOVE.W	#(bypl/2/4)*he-1,D4
	;BSR.S	_RandomWord1
	;SWAP	D5
	;MOVE.B	D3,D5
	;MOVE.L	D5,D1
	;.innerloop:
	;ROR.L	D1
	;MOVE.W	D1,D5
	;SWAP	D5
	;MOVE.B	(A0)+,D5
	;NOT.L	D5
	;AND.B	D4,D5
	;ASR.W	D4,D5
	;MOVE.B	D5,(A5)
	;ROL.L	D5
	;;NOT.L	D5
	;ASR.W	D4,D5
	;MOVE.W	D5,(A4)
	;ROR.L	D5
	;NOT.L	D5
	;MOVE.L	D5,2(A4)
	;ROL.L	D5
	;MOVE.L	D5,-2(A5)
	;BTST	#$0,D5
	;BNE.S	.skip
	;;NOT.B	D1
	;ROR.L	D1
	;SWAP	D1
	;
	;.skip:
	;LEA	2(A4),A4
	;LEA	2(A5),A5
	;DBRA	D4,.innerloop
	;RTS
	;
	;_RandomWord1:
	;BSR	_RandomByte1
	;ROL.W	#8,D5
	;_RandomByte1:
	;MOVE.B	$DFF007,D5	;$dff00a $dff00b for mouse pos
	;MOVE.B	$BFD800,D3
	;EOR.B	D3,D5
	;RTS

__BLIT_PIXEL:
	BSR	WaitBlitter
	MOVE.W	#%0000100111110000,BLTCON0		; BLTCON0
	MOVE.W	#%0000000000000000,BLTCON1		; BLTCON1
	MOVE.L	#$FFFFFFFF,BLTAFWM			; THEY'LL NEVER
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
	MOVE.L	#$AEFFFFFF,BLTAFWM			; THEY'LL NEVER
	MOVE.W	#$0,BLTAMOD			; BLTAMOD =0 for texture
	MOVE.W	#bypl-(PIXELSIDE_W/16*2),BLTBMOD	; BLTBMOD 40-4=36
	MOVE.W	#bypl-(PIXELSIDE_W/16*2),BLTCMOD	; BLTCMOD 40-4=36
	MOVE.W	#bypl-(PIXELSIDE_W/16*2),BLTDMOD	; BLTDMOD 40-4=36

	MOVE.L	#BGNOISE1+4,BLTAPTH			; TEXTURE
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
	CMP.B	#$20,D2		; IS SPACE?
	BNE.S	.notSpace
	SUBI.W	#PIXELSIDE_W*4,FRAMESINDEX	; REDUCE SPACE WIDTH
	.notSpace:
	SUBI.B	#$20,D2		; ASCII
	LSL.W	#3,D2		; CALCULATIONS
	ADD.W	D2,A5
	MOVE.W	#7-1,D6
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

__TEXTURIZE_PLANE:
	MOVE.W	#he/4-1,D4	; QUANTE LINEE
	.outerloop:		; NUOVA RIGA
	NOT.L	D5
	MOVE.W	#(bypl/4)-1,D6	; RESET D6
	.innerloop:		; LOOP KE CICLA LA BITMAP
	MOVE.L	D5,(A4)+
	DBRA	D6,.innerloop
	ROR.L	D5
	DBRA	D4,.outerloop
	RTS

__DITHER_PLANE:
	MOVE.W	#he/4-1,D4	; QUANTE LINEE
	.outerloop:		; NUOVA RIGA
	NOT.L	D5
	MOVE.W	#(bypl/4)-1,D6	; RESET D6
	.innerloop:		; LOOP KE CICLA LA BITMAP
	MOVE.L	D5,(A4)+
	DBRA	D6,.innerloop
	DBRA	D4,.outerloop
	RTS

__BLK_INTRO:
	BSR.W	__BLK_VECT
	MOVE.W	AUDIOCHLEV_3,D1
	LSR.W	#$2,D1
	LSL.W	#$3,D1
	;_PushColors	BG_COLS_TBL,D1,$DFF190
	MOVE.W	#16,D1
	;_PushColors	FG_COLS_TBL,D1,$DFF198

	LEA	__BLIT_PIXEL,A2

	MOVE.B	MED_TRK_1_INST,D1	; ALSO 4 + "E"
	CMP.B	#$4,D1
	BNE.S	.not4
	MOVE.W	AUDIOCHLEV_1,D1
	;LSR.W	D1
	LSL.W	#$2,D1
	;_PushColors	FG_COLS_TBL,D1,$DFF198
	;BRA.W	__BLK_BG
	.not4:

	;MOVE.W	MED_BLOCK_LINE,D7
	BSR.W	__RandomWord
	MOVE.W	MED_TRK_1_INST,D5
	MOVE.W	MED_TRK_3_COUNT,D7

	; ## NOISE SECTION ##
	MOVE.W	#(bypl/2)*50-1,D4
	TST.B	FRAME_STROBE
	BNE.W	.oddFrame
	MOVE.B	#1,FRAME_STROBE
	LEA	BGNOISE1,A4
	BRA.W	.evenFrame
	.oddFrame:
	MOVE.B	#0,FRAME_STROBE
	LEA	BGNOISE2,A4
	.evenFrame:
	BSR.W	__RANDOMIZE_PLANE
	; ## NOISE SECTION ##
	RTS

__BLK_0:
	MOVE.W	MED_BLOCK_LINE,D1
	;MOVE.W	#$0,D1
	LSR.W	#$2,D1
	LSL.W	#$3,D1
	;_PushColors	BG_COLS_TBL,D1,$DFF190

	MOVE.W	AUDIOCHLEV_0,D1
	;MOVE.W	#$0,D1
	LSR.W	D1
	LSL.W	#$3,D1
	;_PushColors	FG_COLS_TBL,D1,$DFF198

	LEA	__BLIT_PIXEL,A2

	MOVE.B	MED_TRK_3_INST,D1	; ALSO 4 + "E"
	CMP.B	#$7,D1
	BNE.S	.not7
	MOVE.W	AUDIOCHLEV_3,COPPER\.OddMod+2

	;LEA	TEXTUREPLANE,A4	; FILLS A PLANE
	;LSL.W	#$4,D1
	;LSL.W	#$4,D1
	;ADD.L	D1,A4
	;MOVE.L	#$A5A5A5A5,D5	; PARAMS
	;BSR.W	__RandomWord	; PARS
	;ROR.L	D3,D5		; PARS
	;BSR.W	__TEXTURIZE_PLANE
	;BRA.W	.evenFrame

	.not7:
	CMP.B	#$2,D1
	BNE.S	.not2
	SUB.W	#$1,COPPER\.OddMod+2
	.not2:
	CMP.B	#$8,D1
	BNE.S	.not8
	MOVE.W	#$4,COPPER\.OddMod+2
	.not8:

	;MOVE.B	MED_TRK_1_INST,D0	; ALSO 4 + "E"
	;CMP.B	#$4,D0
	;BNE.S	.not4
	;ADD.W	#$1,COPPER\.EvenMod+2
	;BRA.S	.reset
	;.not4:
	;MOVE.W	#$4,COPPER\.EvenMod+2
	;.reset:

	;MOVE.W	MED_BLOCK_LINE,D7
	BSR.W	__RandomWord
	MOVE.W	MED_TRK_1_INST,D5
	MOVE.W	MED_TRK_3_COUNT,D7

	; ## NOISE SECTION ##
	MOVE.W	#(bypl/2)*50-1,D4
	TST.B	FRAME_STROBE
	BNE.W	.oddFrame
	MOVE.B	#1,FRAME_STROBE
	LEA	BGNOISE1,A4
	BRA.W	.evenFrame
	.oddFrame:
	MOVE.B	#0,FRAME_STROBE
	LEA	BGNOISE2,A4
	.evenFrame:
	BSR.W	__RANDOMIZE_PLANE
	; ## NOISE SECTION ##
	RTS

__BLK_BG:
	;MOVE.W	#16,D1
	;_PushColors	FG_COLS_TBL,D1,$DFF198

	;MOVE.L	TMP_TEXTURE_SEED,D5	; SAVE TEMP DATA
	;MOVE.L	TMP_TEXTUREPLANE,A4
	;TST.L	(A4)
	;BNE.S	.bplNotFilled
	LEA	TEXTUREPLANE,A4	; FILLS A PLANE
	;MOVE.L	#$A5A5A5A5,D5	; PARAMS
	;MOVE.L	MED_TRK_1_INST,D5
	BSR.W	__RandomWord	; PARS
	MOVE.L	(A4),D5		; PARAMS
	ROR.L	D3,D5		; PARS
	;.bplNotFilled:

	BSR.W	__TEXTURIZE_PLANE
	;MOVE.L	A4,TMP_TEXTUREPLANE	; SAVE TEMP DATA
	;MOVE.L	D5,TMP_TEXTURE_SEED	; SAVE TEMP DATA
	RTS

	; ## NOISE SECTION ##
	MOVE.W	#(bypl/2)*50-1,D4
	TST.B	FRAME_STROBE
	BNE.W	.oddFrame
	MOVE.B	#1,FRAME_STROBE
	LEA	BGNOISE1,A4
	BRA.W	.evenFrame
	.oddFrame:
	MOVE.B	#0,FRAME_STROBE
	LEA	BGNOISE2,A4
	.evenFrame:
	BSR.W	__RANDOMIZE_PLANE
	; ## NOISE SECTION ##
	RTS

__BLK_VECT:
	LEA	BUFFER3D,A1
	;LEA	TEXTUREPLANE,A1
	MOVE.L	A1,A6
	BSR.W	__WIPE_PLANE
	BSR.W	__BLIT_VECTORS
	BSR.W	__BLIT_3D_IN_PLACE

	MOVE.W	AUDIOCHLEV_0,D1
	LSR.W	#1,D1
	LSL.W	#1,D1
	ADD.W	#2,D1
	RTS

;********** Fastmem Data **********
TIMELINE:		;DC.L __BLK_0,__BLK_0,__BLK_0,__BLK_0
		DC.L __BLK_INTRO,__BLK_INTRO,__BLK_INTRO,__BLK_INTRO
		DC.L __BLK_INTRO,__BLK_INTRO,__BLK_INTRO,__BLK_INTRO
		DC.L __BLK_0,__BLK_0,__BLK_0,__BLK_0
		DC.L __BLK_0,__BLK_0,__BLK_0,__BLK_0
		DC.L __BLK_0,__BLK_0,__BLK_0,__BLK_0
		DC.L __BLK_0,__BLK_0,__BLK_0,__BLK_0
		DC.L __BLK_0,__BLK_0,__BLK_0,__BLK_0
		DC.L __BLK_0,__BLK_0,__BLK_0,__BLK_0
		DC.L __BLK_0,__BLK_0,__BLK_0,__BLK_0
		DC.L __BLK_0,__BLK_0,__BLK_0,__BLK_0
		DC.L __BLK_0,__BLK_0,__BLK_0,__BLK_0
FRAME_STROBE:	DC.B 0,0
KICKSTART_ADDR:	DC.L $F80000	; POINTERS TO BITMAPS
TEXTINDEX:	DC.W 0
LINEINDEX:	DC.W 0
DUMMYINDEX:	DC.W PIXELSIDE_W+1
DUMMYINDEXPLOT:	DC.W 0
FRAMESINDEX:	DC.W TXT_FRMSKIP
MED_SONG_POS:	DC.W 0		; Well the position...
MED_BLOCK_LINE:	DC.W 0		; Line of block
AUDIOCHLEV_0:	DC.W 0
AUDIOCHLEV_1:	DC.W 0
AUDIOCHLEV_2:	DC.W 0
AUDIOCHLEV_3:	DC.W 0
COORDS_1:		DC.W 320,250,0,-1	; WPOS, HPOS, WDIR, HDIR
COORDS_2:		DC.W 0,2,0,1	; WPOS, HPOS, WDIR, HDIR
COORDS_3:		DC.W 300,0,-4,0	; WPOS, HPOS, WDIR, HDIR
COORDS_4:		DC.W 0,250,0,4	; WPOS, HPOS, WDIR, HDIR
COORDS_5:		DC.W 140,0,2,0	; WPOS, HPOS, WDIR, HDIR
COORDS_6:		DC.W 180,256,-2,0	; WPOS, HPOS, WDIR, HDIR

FONT:		DC.L 0,0		; SPACE CHAR
		INCBIN "c_font_leftpadding2.raw",0
		EVEN
TEXT:		INCLUDE "textscroller.i"

BG_COLS_TBL:	DC.W $0000,$0000
		DC.W $0110,$0110
		DC.W $0111,$0111
		DC.W $0111,$0111
		DC.W $0221,$0221
		DC.W $0222,$0222
		DC.W $0222,$0222
		DC.W $0332,$0332
		DC.W $0333,$0333
		DC.W $0443,$0443
		DC.W $0444,$0444
		DC.W $0555,$0555
		DC.W $0444,$0444
		DC.W $0222,$0222
		DC.W $0111,$0111
		DC.W $0110,$0110
		DC.W $0000,$0000
FG_COLS_TBL:	DC.W $0665,$0665
		DC.W $0555,$0555
		DC.W $0554,$0554
		DC.W $0444,$0444
		DC.W $0443,$0443
		DC.W $0333,$0333
		DC.W $0332,$0332
		DC.W $0222,$0222
		DC.W $0221,$0221
		DC.W $0111,$0111
		DC.W $0110,$0110
		DC.W $0000,$0000
		DC.W $0111,$0111
		DC.W $0222,$0222
		DC.W $0333,$0333
		DC.W $0444,$0444
		DC.W $0555,$0555

;*******************************************************************************
	SECTION	ChipData,DATA_C	;declared data that must be in chipmem
;*******************************************************************************
MED_MODULE:	INCBIN "med/RustEater_2022_FIX3.med"
_chipzero:	DC.L 0
_MED_MODULE:

COPPER:
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
	DC.W $0180,$0000,$0182,$0443,$0184,$0776,$0186,$0CCB
	DC.W $0188,$0443,$018A,$0777,$018C,$0555,$018E,$0AA9
	DC.W $0190,$0000,$0192,$0000,$0194,$0000,$0196,$0000
	DC.W $0198,$0443,$019A,$0443,$019C,$0443,$019E,$0443
	DC.W $01A0,$0111,$01A2,$0222,$01A4,$0111,$01A6,$0665
	DC.W $01A8,$0111,$01AA,$0666,$01AC,$0111,$01AE,$0111
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
	;DC.W $100,bpls*$1000+%011000000000

	.Waits:
	DC.W $4D07,$FF00		; ## START OF NOISE PART ##
	.BplPtrs2:
	DC.W $E0,0
	DC.W $E2,0
	DC.W $E4,0
	DC.W $E6,0

	DC.W $5607,$FF00		; ## NOISE REPOINTED ONLY ON ONE PLANE ##
	.BplPtrs3:
	DC.W $EC,0
	DC.W $EE,0

	DC.W $9A07,$FF00		; ## NOISE REPOINTED ONLY ON BOTH PLANES ##
	.BplPtrs4:
	DC.W $E0,0
	DC.W $E2,0
	DC.W $E4,0
	DC.W $E6,0

	DC.W $F607,$FF00		; ## END OF NOISE PART ##
	.BplPtrs5:
	DC.W $E0,0
	DC.W $E2,0
	DC.W $E4,0
	DC.W $E6,0
	DC.W $EC,0
	DC.W $EE,0

	DC.W $FFDF,$FFFE		; allow VPOS>$ff
	DC.W $3507,$FF00		; ## RASTER END ## #$12C?
	DC.W $009A,$0010		; CLEAR RASTER BUSY FLAG
	DC.W $FFFF,$FFFE		; magic value to end copperlist

;*******************************************************************************
	SECTION ChipBuffers,BSS_C	;BSS doesn't count toward exe size
;*******************************************************************************

CHAR_BUFFER:	DS.B 8
CHAR_ROTATION:	DS.B 8
BLEEDTOP:		DS.B bypl
BGNOISE1:		DS.B 50*bypl
BGNOISE2:		DS.B 50*bypl
BLEEDVECTOR:	DS.B he/4*bypl
TEXTUREPLANE:	DS.B he*bypl
BGEMPTY:		DS.B he*bypl
BGMASK:		DS.B he*bypl
BGFILLED:		DS.B he*bypl
BUFFER3D:		DS.B he*bypl	; bigger to hold zoom
END
