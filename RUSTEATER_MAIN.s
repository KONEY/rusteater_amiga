;*** CODE: KONEY ***
;*** MiniStartup by Photon ***
	INCDIR	"NAS:AMIGA/CODE/rusteater_amiga/"
	SECTION	"Code+PT12",CODE
	INCLUDE	"PhotonsMiniWrapper1.04!.s"
	INCLUDE	"custom-registers.i"	;use if you like ;)
;********** Constants **********
wi		EQU 320
he		EQU 256		; screen height
bpls		EQU 3		; depth
bypl		EQU wi/16*2	; byte-width of 1 bitplane line (40bytes)
bwid		EQU bpls*bypl	; byte-width of 1 pixel line (all bpls)
;*************
;********** Demo **********	;Demo-specific non-startup code below.
Demo:			;a4=VBR, a6=Custom Registers Base addr
	;*--- init ---*
	MOVE.L	#VBint,$6C(A4)
	MOVE.W	#%1110000000100000,INTENA
	MOVE.W	#%1000001111100000,DMACON
	;*--- start copper ---*
	LEA	BGPLANE0,A0
	LEA	COPPER\.BplPtrs,A1
	BSR.W	PokePtrs
	LEA	BGPLANE1,A0
	LEA	COPPER\.BplPtrs+8,A1
	BSR.W	PokePtrs
	LEA	BGPLANE2,A0
	LEA	COPPER\.BplPtrs+16,A1
	BSR.W	PokePtrs

	; #### CPU INTENSIVE TASKS BEFORE STARTING MUSIC
	; #### CPU INTENSIVE TASKS BEFORE STARTING MUSIC

	MOVE.L	#COPPER,COP1LC
;********************  main loop  ********************
MainLoop:
	; do stuff here :)
	;MOVE.W	P61_Pos,D5	; SONG_BLOCKS_EVENTS:
	LEA	TIMELINE,A3	; FOR TIMED EVENTS ON BLOCKS
	ADD.W	D5,D5		; CALCULATES OFFSET (OPTIMIZED)
	ADD.W	D5,D5		; CALCULATES OFFSET (OPTIMIZED)
	MOVE.L	(A3,D5),A3	; THANKS HEDGEHOG!!
	JSR	(A3)		; EXECUTE SUBROUTINE BLOCK#

	.WaitRasterCopper:
	;MOVE.W	#$0FF0,$DFF180	; show rastertime left down to $12c
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

__BLK_0:	RTS

;********** Fastmem Data **********
TIMELINE:		DC.L __BLK_0,__BLK_0,__BLK_0,__BLK_0

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
	DC.W $0180,$0000,$0182,$0FFF,$0184,$0F69,$0186,$0F08
	DC.W $0188,$0F00,$018A,$0F70,$018C,$0FB0,$018E,$0906
	DC.W $0190,$0406,$0192,$0015,$0194,$0014,$0196,$0002
	DC.W $0198,$07DF,$019A,$005B,$019C,$0BF4,$019E,$01C7

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
	DC.W $FFDF,$FFFE	; allow VPOS>$ff
	DC.W $3501,$FF00	; ## RASTER END ## #$12C?
	DC.W $009A,$0010	; CLEAR RASTER BUSY FLAG
	DC.W $FFFF,$FFFE	; magic value to end copperlist

;*******************************************************************************
	SECTION ChipBuffers,BSS_C	;BSS doesn't count toward exe size
;*******************************************************************************

BGPLANE0:		DS.B he/2*bypl
BGPLANE1:		DS.B he/2*bypl
BGPLANE2:		DS.B he/2*bypl
END
