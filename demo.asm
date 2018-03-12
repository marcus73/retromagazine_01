
/*
    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.

*/

.const mem_schermo=1024+40*10
.const chrout=$ffd2

.const debug=0			// impostare a 0 per rilasci finali. Diverso da 0 SOLO per eventuale debugging.

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
	
		.if (debug==0)
		{	
			/////////////////////////////////////////////////////////////////////////////////////////////////////	
			// Abilita la modalità schermo con set di caratteri ridefiniti
			
			.const screen=$0400						// locazione memoria di schermo video
			.const charset=$3000						// locazione memoria set di caratteri

			lda #[[screen & $3FFF] / 64] | [[charset & $3FFF] / 1024]	//
			sta $D018							//
		}

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
.import binary "retromagazine_demo.bin"

map_data:
.byte $00,$01,$02,$03,$03,$03,$03,$03,$03,$03,$03,$04,$03,$05,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03
.byte $06,$07,$08,$09,$0a,$0b,$0c,$0d,$0e,$0f,$10,$11,$12,$13,$14,$15,$00,$16,$17,$18,$19,$1a,$1b,$1c,$1d,$1e,$1f,$20,$21,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03
.byte $06,$22,$23,$24,$25,$26,$27,$28,$29,$2a,$2b,$2c,$2d,$2e,$2f,$30,$31,$32,$33,$34,$35,$36,$37,$38,$39,$3a,$3b,$3c,$3d,$3e,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03
.byte $06,$3f,$40,$41,$42,$43,$44,$45,$46,$47,$48,$49,$4a,$4b,$4c,$4d,$4e,$4f,$50,$51,$52,$53,$54,$55,$39,$56,$57,$58,$59,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03
.byte $5a,$5b,$5c,$5d,$5e,$03,$5f,$60,$03,$61,$62,$63,$64,$65,$66,$67,$68,$69,$6a,$6b,$6c,$5f,$6d,$5d,$5d,$6e,$6f,$61,$67,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03
.byte $03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03,$03

