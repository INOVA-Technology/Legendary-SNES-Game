.INCLUDE "header.inc"
.INCLUDE "init.inc"

.BANK 0 SLOT 0
.ORG 0
.SECTION "MainCode"

Start:
	InitSNES

	stz $2121
	lda #%00011111
	sta $2122
	stz $2122

	lda #$0F
	sta $2100

forever:
	jmp forever

.ENDS

