;------------------------------------------------------------------------
;-  Written by Bazz
;-     This code demonstrates displaying a simple tile on the screen.
;-     This can be expanded on easily, and will be used as a base for
;-     later examples.
;-
;-     All I want is feedback, so please tell me if my tutorials suck,
;-     are decent, or whatever. I'd also like to know what needs
;-     better explanations and so on. I must improve the tutorials!
;------------------------------------------------------------------------


;============================================================================
; Includes
;============================================================================

;== Include MemoryMap, Vector Table, and HeaderInfo ==
.INCLUDE "header.inc"

;== Include SNES Initialization routines ==
.INCLUDE "init.inc"

.EQU PalNum $0000

.MACRO Stall
    .REPT 7
        WAI
    .ENDR
.ENDM
;============================================================================
; Macros
;============================================================================
;============================================================================
;LoadPalette - Macro that loads palette information into CGRAM
;----------------------------------------------------------------------------
; In: SRC_ADDR -- 24 bit address of source data,
;     START -- Color # to start on,
;     SIZE -- # of COLORS to copy
;----------------------------------------------------------------------------
; Out: None
;----------------------------------------------------------------------------
; Modifies: A,X,Y
; Requires: mem/A = 8 bit, X/Y = 16 bit
;----------------------------------------------------------------------------
.MACRO LoadPalette
    lda #\2
    sta $2121       ; Start at START color
    lda #:\1        ; Using : before the parameter gets its bank.
    ldx #\1         ; Not using : gets the offset address.
    ldy #(\3 * 2)   ; 2 bytes for every color
    jsr DMAPalette
.ENDM

;============================================================================
; LoadBlockToVRAM -- Macro that simplifies calling LoadVRAM to copy data to VRAM
;----------------------------------------------------------------------------
; In: SRC_ADDR -- 24 bit address of source data
;     DEST -- VRAM address to write to (WORD address!!)
;     SIZE -- number of BYTEs to copy
;----------------------------------------------------------------------------
; Out: None
;----------------------------------------------------------------------------
; Modifies: A, X, Y
;----------------------------------------------------------------------------

;LoadBlockToVRAM SRC_ADDRESS, DEST, SIZE
;   requires:  mem/A = 8 bit, X/Y = 16 bit
.MACRO LoadBlockToVRAM
    lda #$80
    sta $2115
    ldx #\2         ; DEST
    stx $2116       ; $2116: Word address for accessing VRAM.
    lda #:\1        ; SRCBANK
    ldx #\1         ; SRCOFFSET
    ldy #\3         ; SIZE
    jsr LoadVRAM
.ENDM



;============================================================================
; Main Code
;============================================================================

.BANK 0 SLOT 0
.ORG 0
.SECTION "MainCode"

Start:
    InitSNES    ; Clear registers, etc.

    ; Load Palette for our tiles
    LoadPalette BG_Palette, 0, 16

    ; Load Tile data to VRAM
	LoadBlockToVRAM Tiles, $0000, $0020	; 2 tiles, 2bpp, = 32 bytes
	LoadBlockToVRAM Sprite, $0020, $08000

	; LoadBlockToVRAM Sprite, $0000, $08000

	LoadPalette SpritePal, 128, 16

	jsr SpriteInit

	; lda #(256/2 - 16)
	lda #10
	sta $0000

	lda #(224/2 - 16)   ; Sprite Y-Coordinate
	sta $0001

	lda #%01010100  ; clear X-MSB
	sta $0200

    ; Now, load up some data into our tile map
    ; (If you had an full map, you could use LoadBlockToVRAM)
    ; Remember that in the default map, all entries point to tile #0
    lda #$80
    sta $2115

	; ldx #$0400
    ; stx $2116
    ; lda #$01
    ; sta $2118

ldx #$0400
Loop:
	stx $2116
	lda #$01
	sta $2118
	inx
	cpx #$0440
	bne Loop

	lda #$80
	sta $4200

    ; Setup Video modes and other stuff, then turn on the screen
    jsr SetupVideo

Infinity:
	Stall

    lda PalNum
    clc
    adc #$04
    and #$1C        ; If palette starting color > 28 (00011100), make 0
    sta PalNum
    jmp Infinity    ; bwa hahahahaha

VBlank:
	rep #$10
	sep #$20

	stz $2115
	ldx #$0400
	stx $2116
	lda PalNum
	sta $2119

	lda $4210

	rti

SpriteInit:
	php
	rep #$30

	ldx #$0000
_setoffscr:
	lda #$0001
	sta $0000, X
	inx
	inx
	lda #$0002
	sta $0000, X
	inx
	inx
	cpx #$0200
	bne _setoffscr

	ldx #$0000
	lda #$5555
_clr:
	sta $0200, X
	inx
	inx
	cpx #$0020
	bne _clr
	
	plp
	rts

;============================================================================
; SetupVideo -- Sets up the video mode and tile-related registers
;----------------------------------------------------------------------------
; In: None
;----------------------------------------------------------------------------
; Out: None
;----------------------------------------------------------------------------
SetupVideo:
    php

	rep #$10
	sep #$20

    lda #$00
    sta $2105           ; Set Video mode 0, 8x8 tiles, 4 color BG1/BG2/BG3/BG4

    lda #$04            ; Set BG1's Tile Map offset to $0400 (Word address)
    sta $2107           ; And the Tile Map size to 32x32

    stz $210B           ; Set BG1's Character VRAM offset to $0000 (word address)

    lda #$01            ; Enable BG1
    sta $212C

    lda #$FF
    sta $210E
    sta $210E

	stz $2102
	stz $2103

	ldy #$0400
	sty $4300           ; CPU -> PPU, auto inc, $2104 (OAM write)
    stz $4302
    stz $4303

	lda #$7E
    sta $4304           ; CPU address 7E:0000 - Work RAM
    ldy #$0220
    sty $4305           ; #$220 bytes to transfer
    lda #$01
    sta $420B

	lda #%10100000      ; 32x32 and 64x64 size sprites (we are using a 32x32)
    sta $2101

	lda #%00010000      ; Enable Sprites
    sta $212C

    lda #$0F
    sta $2100           ; Turn on screen, full Brightness

    plp
    rts
;============================================================================

;============================================================================
; LoadVRAM -- Load data into VRAM
;----------------------------------------------------------------------------
; In: A:X  -- points to the data
;     Y     -- Number of bytes to copy (0 to 65535)  (assumes 16-bit index)
;----------------------------------------------------------------------------
; Out: None
;----------------------------------------------------------------------------
; Modifies: none
;----------------------------------------------------------------------------
; Notes:  Assumes VRAM address has been previously set!!
;----------------------------------------------------------------------------
LoadVRAM:
    php         ; Preserve Registers

    stx $4302   ; Store Data offset into DMA source offset
    sta $4304   ; Store data Bank into DMA source bank
    sty $4305   ; Store size of data block

    lda #$01
    sta $4300   ; Set DMA mode (word, normal increment)
    lda #$18    ; Set the destination register (VRAM write register)
    sta $4301
    lda #$01    ; Initiate DMA transfer (channel 1)
    sta $420B

    plp         ; restore registers
    rts         ; return
;============================================================================

;============================================================================
; DMAPalette -- Load entire palette using DMA
;----------------------------------------------------------------------------
; In: A:X  -- points to the data
;      Y   -- Size of data
;----------------------------------------------------------------------------
; Out: None
;----------------------------------------------------------------------------
; Modifies: none
;----------------------------------------------------------------------------
DMAPalette:
    php         ; Preserve Registers

    stx $4302   ; Store data offset into DMA source offset
    sta $4304   ; Store data bank into DMA source bank
    sty $4305   ; Store size of data block

    stz $4300  ; Set DMA Mode (byte, normal increment)
    lda #$22    ; Set destination register ($2122 - CGRAM Write)
    sta $4301
    lda #$01    ; Initiate DMA transfer
    sta $420B

    plp
    rts         ; return from subroutine


.ENDS

;============================================================================
; Character Data
;============================================================================
.BANK 1 SLOT 0
.ORG 0
.SECTION "CharacterData"

    .INCLUDE "grass.inc"

Sprite:
	.INCBIN "biker.pic"
SpritePal
	.INCBIN "biker.clr"

.ENDS
