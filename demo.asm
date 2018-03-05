
.const scr_bitmap= %00111001
.const mem_schermo=1024+40*10
.const chrout=$ffd2

.label COUNTER0	= $ff
.label COUNTER1	= $ff

* = $0801                       // BASIC start address (#2049)
BasicUpstart(codice)

.pc=$1000 "codice"
codice:

	jsr scr_init		// imposta schermo, riproducendo il testo "RetroMagazine" tra due barre orizzontali piene
	
main:
	ldx COUNTER0			
	lda sinusTable0,x	//Recupera il nuovo valore di linea raster
	cmp $D012		//dalla relativa tabella, con indice COUNTER0, e lo confronta con l'attuale...
	bne no_barra0
	sei			//disabilita le interruzioni
	jsr fai_barra0		//se corrispondente, crea barra raster tipo "0"
	cli			//riabilita le interruzioni

no_barra0:
	ldx COUNTER1
	lda sinusTable1,x	//Recupera il nuovo valore di linea raster 
	cmp $D012		//dalla relativa tabella, con indice COUNTER1, e lo confronta con l'attuale...
	bne no_barra1
	sei			//disabilita le interruzioni
	jsr fai_barra1		//se corrispondente, crea barra raster tipo "1"	
	cli			//riabilita le interruzioni

no_barra1:

	jmp main

scr_init:
{
		lda #$00
		sta $d020					// Bordo NERO
		sta $d021					// Schermo NERO
		
		lda #147					// Pulisce lo schermo video, 
		jsr chrout					// chiamando la famigerata routine kernal chrout con argomento 147 (codice carattere x clear screen)
	
		lda #29						// Abilita la modalità schermo
		sta $d018					// con set di caratteri ridefiniti

		ldx #00
scr_clear:
		lda #$71					// Riempie la memoria video con il valore $71 (#143) e $72 (#144)
		sta $0400,x					// che corrisponde al carattere 'muro'...
		sta $0500,x					//
		lda #$72
		sta $0600,x					//
		sta $06e8,x					//
				
		lda #$01					// Setta il colore dello schermo al valore '01'
		sta $d800,x					// ovvero BIANCO
		sta $d900,x					//
		sta $da00,x					//
		sta $dae8,x					//

		inx						// Incrementa x
		bne scr_clear					// torna su...

		ldx #39
		lda #$03
s1:
		sta $0400+40*0,x
		sta $0400+40*1,x
		sta $0400+40*2,x
		sta $0400+40*3,x
		sta $0400+40*4,x
		sta $0400+40*5,x
		
		sta $0400+40*20,x
		sta $0400+40*21,x
		sta $0400+40*22,x
		sta $0400+40*23,x
		sta $0400+40*24,x
		sta $0400+40*25,x
		dex
		bpl s1
			
		// DISEGNA BARRA SUPERIORE
		ldx #39
		lda #$70
s2:		sta mem_schermo-40,x
		sta mem_schermo-40*5,x
		dex
		bpl s2

		// DISEGNA SCRITTA RETROMAGAZINE
		ldx #0
ancora:
		lda map_data,x
		sta mem_schermo,x
		inx
		cpx #40*6
		bne ancora
	
		// DISEGNA BARRA INFERIORE
		ldx #39
		lda #$70
s3:		sta mem_schermo+40*6,x
		sta mem_schermo+40*10,x
		dex
		bpl s3

		rts
}

//================================================================================================================
fai_barra0:
{
	ldy #10			//Tempo di attesa
idle1:	dey			//per minimizzare il "tremolio"
	bne idle1		//all'inizio dell'effetto

//------------------------------------------------------------------
// Ciclo per stampare la barra raster '0'
//------------------------------------------------------------------
	ldx #00		
loop:	lda colorBar0		//Assegna colore al bordo schermo
   	sta $d020		//ed all'interno schermo
	sta $d021

	ldy #$08		//Tempo di attesa per minimizzare
idle2:	dey			//per minimizzare
	bne idle2		//il tremolio alla fine dell'effetto
				//e rendere la barra più spessa...

	inx 			//
	cpx #09			//
	bne loop		//

//------------------------------------------------------------------
// Fine del ciclo 
//------------------------------------------------------------------

    	lda #$00		// Assegna il colore #00 (NERO)
    	sta $d020		// al bordo
	sta $d021		// ed allo schermo video

	inc COUNTER0

	rts
}
//================================================================================================================

fai_barra1:
{
	ldy #10			//Tempo di attesa
idle1:	dey			//per minimizzare il "tremolio"
	bne idle1		//all'inizio dell'effetto

//------------------------------------------------------------------
// Ciclo per stampare la barra raster '1'
//------------------------------------------------------------------
	ldx #00		
loop:	lda colorBar1		//Assegna colore al bordo schermo
   	sta $d020		//ed all'interno schermo
	sta $d021

	ldy #$08		//Tempo di attesa per minimizzare
idle2:	dey			//per minimizzare
	bne idle2		//il tremolio alla fine dell'effetto
				//e rendere la barra più spessa...

	inx 			//
	cpx #09			//
	bne loop		//

	
//------------------------------------------------------------------
// Fine del ciclo
//------------------------------------------------------------------

    	lda #$00		// Assegna il colore #00 (NERO)
    	sta $d020		// al bordo
	sta $d021		// ed allo schermo video

	inc COUNTER1

	rts
}
//================================================================================================================

*=* "dati"

colorBar0:
.byte 02

colorBar1:
.byte 06

sinusTable0:
.fill 256, 150 + round(110 * sin(toRadians(360 * i / 256)))

sinusTable1:
.fill 256, 150 + round(110 * cos(toRadians(360 * i / 256))) 

.pc=$3000 "charset_data"
.byte $00,$00,$00,$00,$00,$00,$00,$01,$00,$00,$00,$00,$00,$00,$00,$ff
.byte $00,$00,$00,$00,$00,$00,$00,$c0,$00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$03,$00,$00,$00,$00,$00,$00,$00,$18
.byte $01,$01,$01,$01,$01,$01,$01,$01,$ff,$ff,$f3,$e0,$e0,$e0,$e0,$e0
.byte $f0,$f8,$fc,$3c,$3c,$1e,$1e,$1e,$00,$00,$00,$00,$00,$00,$03,$0f
.byte $00,$00,$00,$00,$00,$00,$f8,$fe,$00,$00,$03,$03,$03,$03,$0f,$0f
.byte $00,$80,$c0,$c0,$c0,$c0,$fe,$fe,$00,$00,$00,$00,$00,$00,$1c,$1e
.byte $00,$00,$00,$00,$00,$00,$7c,$fc,$00,$00,$00,$00,$00,$00,$0f,$1f
.byte $00,$00,$00,$00,$00,$00,$e0,$f8,$03,$03,$07,$07,$07,$07,$07,$07
.byte $00,$00,$80,$80,$c0,$c0,$c0,$c0,$18,$18,$3c,$3c,$7c,$7c,$7c,$7e
.byte $00,$00,$00,$00,$00,$00,$03,$07,$00,$00,$00,$00,$00,$00,$fc,$ff
.byte $00,$00,$00,$00,$00,$00,$3f,$ff,$00,$00,$00,$00,$00,$20,$70,$f8
.byte $00,$00,$00,$00,$00,$00,$3f,$7f,$00,$00,$00,$00,$00,$00,$c0,$f0
.byte $00,$00,$00,$00,$00,$00,$7f,$7f,$00,$00,$00,$00,$00,$00,$ff,$ff
.byte $07,$07,$07,$00,$00,$00,$9f,$1f,$80,$80,$80,$00,$00,$00,$83,$87
.byte $00,$00,$00,$00,$00,$00,$1f,$bf,$00,$00,$00,$00,$00,$00,$80,$e0
.byte $00,$00,$00,$00,$00,$00,$07,$1f,$00,$00,$00,$00,$00,$00,$f0,$fc
.byte $e0,$e0,$e0,$ff,$ff,$ff,$e1,$e1,$1c,$3c,$fc,$f8,$f0,$c0,$e0,$e0
.byte $1f,$3e,$3c,$78,$78,$7f,$7f,$7f,$ff,$0f,$07,$03,$03,$ff,$ff,$ff
.byte $0f,$83,$83,$83,$c3,$c3,$c3,$c3,$fe,$c0,$c0,$c0,$c0,$c0,$c0,$c0
.byte $1f,$1f,$1f,$1e,$1e,$1e,$1e,$1e,$f8,$80,$00,$00,$00,$01,$01,$01
.byte $3f,$78,$f0,$f0,$e0,$e0,$e0,$e0,$fc,$3e,$1e,$0f,$0f,$0f,$0f,$07
.byte $0f,$0f,$0e,$0e,$0e,$1e,$1e,$1c,$e0,$e0,$e0,$f1,$71,$71,$79,$3b
.byte $fe,$fe,$ee,$ee,$ef,$cf,$c7,$87,$0f,$06,$00,$00,$00,$00,$03,$07
.byte $ff,$07,$03,$03,$03,$ff,$ff,$ff,$81,$83,$c3,$c3,$c3,$c3,$c3,$c3
.byte $ff,$e1,$c0,$c0,$80,$80,$c0,$c0,$e0,$f0,$f0,$70,$70,$70,$70,$f0
.byte $7f,$70,$00,$00,$00,$0f,$1f,$7f,$f8,$78,$3c,$3c,$1c,$fc,$fc,$fc
.byte $7f,$00,$00,$00,$00,$00,$00,$01,$ff,$1e,$1c,$3c,$78,$70,$f0,$e0
.byte $1f,$07,$07,$07,$07,$07,$07,$07,$87,$87,$87,$87,$87,$87,$87,$87
.byte $ff,$e0,$c0,$80,$80,$80,$80,$80,$e0,$f0,$f0,$70,$70,$70,$70,$70
.byte $3f,$7c,$78,$f0,$e0,$ff,$ff,$ff,$fe,$1f,$0f,$07,$07,$ff,$ff,$ff
.byte $00,$00,$00,$80,$80,$80,$80,$80,$e0,$e0,$e0,$e0,$e0,$e0,$e0,$e0
.byte $f0,$f0,$78,$3c,$3c,$1e,$1f,$0f,$70,$70,$78,$78,$3c,$3e,$1f,$0f
.byte $00,$00,$00,$00,$00,$06,$ff,$ff,$03,$03,$03,$03,$03,$03,$01,$01
.byte $c0,$c0,$c0,$c0,$c0,$c0,$ff,$ff,$1e,$1e,$1e,$1e,$1e,$1e,$1e,$1e
.byte $01,$01,$01,$00,$00,$00,$00,$00,$e0,$e0,$e0,$f0,$f0,$78,$7f,$3f
.byte $0f,$0f,$0f,$0e,$1e,$3e,$fc,$f8,$1c,$1c,$1c,$3c,$3c,$3c,$38,$78
.byte $3b,$3f,$1f,$1f,$0f,$0e,$0e,$06,$87,$87,$87,$07,$03,$03,$03,$03
.byte $0f,$8f,$8e,$9e,$9e,$8e,$cf,$c7,$83,$03,$03,$03,$03,$07,$bf,$fd
.byte $c1,$c1,$c0,$c0,$c1,$c3,$c3,$e1,$e1,$ff,$7f,$7f,$c0,$80,$ff,$ff
.byte $e0,$e0,$c0,$01,$00,$00,$e0,$f0,$f8,$f0,$e0,$e0,$e0,$e0,$f9,$7f
.byte $1c,$1c,$1c,$1c,$3c,$7c,$fc,$de,$03,$03,$07,$0f,$0f,$1e,$3f,$3f
.byte $c0,$c0,$80,$00,$00,$00,$ff,$ff,$07,$07,$07,$07,$07,$07,$87,$87
.byte $80,$80,$80,$80,$80,$80,$80,$80,$70,$70,$70,$70,$70,$70,$70,$70
.byte $e0,$e0,$f0,$f0,$78,$7c,$3f,$1f,$00,$00,$00,$00,$00,$0c,$fe,$fe
.byte $01,$00,$00,$00,$00,$00,$00,$00,$e0,$00,$00,$00,$00,$00,$00,$00
.byte $07,$00,$00,$00,$00,$00,$00,$00,$87,$00,$00,$00,$00,$00,$00,$00
.byte $fc,$00,$00,$00,$00,$00,$00,$00,$7f,$00,$00,$00,$00,$00,$00,$00
.byte $1c,$00,$00,$00,$00,$00,$00,$00,$0f,$00,$00,$00,$00,$00,$00,$00
.byte $f0,$00,$00,$00,$00,$00,$00,$00,$78,$00,$00,$00,$00,$00,$00,$00
.byte $06,$00,$00,$00,$00,$00,$00,$00,$03,$00,$00,$00,$00,$00,$00,$00
.byte $c3,$00,$00,$00,$00,$00,$00,$00,$f8,$00,$00,$00,$00,$00,$00,$00
.byte $e0,$00,$00,$00,$01,$03,$01,$00,$ff,$00,$00,$00,$c0,$ff,$ff,$7f
.byte $f8,$78,$38,$38,$78,$f0,$e0,$c0,$3f,$00,$00,$00,$00,$00,$00,$00
.byte $8f,$00,$00,$00,$00,$00,$00,$00,$ff,$00,$00,$00,$00,$00,$00,$00
.byte $80,$00,$00,$00,$00,$00,$00,$00,$70,$00,$00,$00,$00,$00,$00,$00
.byte $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff

// Carattere "muro" che scorrerà verso dx
.byte %11111111
.byte %10000000
.byte %10000000
.byte %10000000
.byte %11111111
.byte %00001000
.byte %00001000
.byte %00001000

// Carattere "muro" che scorrerà verso sx
.byte %11111111
.byte %10000000
.byte %10000000
.byte %10000000
.byte %11111111
.byte %00001000
.byte %00001000
.byte %00001000

map_data:
.byte $00,$01,$02,$03,$03,$03,$03,$03,$03,$03,$03,$04,$03,$05,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03
.byte $06,$07,$08,$09,$0a,$0b,$0c,$0d,$0e,$0f,$10,$11,$12,$13,$14,$15,$00,$16,$17,$18,$19,$1a,$1b,$1c,$1d,$1e,$1f,$20,$21,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03
.byte $06,$22,$23,$24,$25,$26,$27,$28,$29,$2a,$2b,$2c,$2d,$2e,$2f,$30,$31,$32,$33,$34,$35,$36,$37,$38,$39,$3a,$3b,$3c,$3d,$3e,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03
.byte $06,$3f,$40,$41,$42,$43,$44,$45,$46,$47,$48,$49,$4a,$4b,$4c,$4d,$4e,$4f,$50,$51,$52,$53,$54,$55,$39,$56,$57,$58,$59,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03
.byte $5a,$5b,$5c,$5d,$5e,$03,$5f,$60,$03,$61,$62,$63,$64,$65,$66,$67,$68,$69,$6a,$6b,$6c,$5f,$6d,$5d,$5d,$6e,$6f,$61,$67,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03
.byte $03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03
