; GIF reader v0.4b for ZX Spectrum
; 256 x 192 only.
; Not used Bright. Ignores color collision.
; The effort to maintain the continuity of values Paper and Ink.
; Simple rules to minimize the inverted matrix of 8x8 pixels.
; Support for calling directly from Basic.
; Support Error codes.
; Overflow screens with large images is detected.
;
; http://www.matthewflickinger.com/lab/whatsinagif/bits_and_bytes.asp
; http://www.matthewflickinger.com/lab/whatsinagif/lzw_image_data.asp
;
; Compile:
; pasmo -d gif-reader.asm gif-reader.bin > test.asm

ADR_SEG_SLOVNIKU	EQU	5 * %00100000		; $A000..$BFFF, granularita 8 KB
ADR_SLOVNIKU		EQU	256 * ADR_SEG_SLOVNIKU
progStart		EQU	ADR_SLOVNIKU + $2000	; $C000 = 49152

ORG progStart

; sizeof(SLOVNIK) = 4096 * 2 bytes = 8 KB
; HI        LO
; GRBiiiii iiiiiiiS = S = stop bit, nastaven u prvni polozky slova, tzn. maji ho indexy mensi jak CLEARCODE
;  +
; AAA..... ........ = segment adresy musi lezet na adrese delitelne 8 KB
;  =
; AAAiiiii iiiiiii0 = adresa predchozi polozky slovniku

; Globalni pouziti registru:
;	IXH = MIN_SIRKA_LZW
;	IXL = AKT_SIRKA_LZW
  
ERR_OK			EQU	0
ERR_GIF_NOT_FOUND	EQU	1	; != "G"
ERR_UNKNOWN_FRAME	EQU	2	; Plain Text Extension Block, Application Frame
ERR_OWERFLOW_SCREEN	EQU	3	; Pixelu je vice jak 192*256

MAIN:
	EXX				;  4:1
	PUSH	HL			; 11:1
	
; PRINT USR progStart, ADR_GIF
; Nacteni druheho parametru do HL
	RST	$20			; 11:1
	CALL	$24FB			; 17:3
	CALL	$2DD5			; 17:3
	LD	H,B			;  4:1
	LD	L,C			;  4:1 HL = ADR_GIF
	
	LD	(ERROR_EXIT+1),SP	; 20:4
	CALL	INIT			; 17:3
	JP	DECODE			; 10:3 Na zasobniku mame data a chceme minimalizovat naklady k pristupu k nim.
EXIT_DECODE:
	XOR	A			;  4:1 = ERR_OK
ERROR_EXIT:
; stala se chyba, ukoncime program
	LD	SP,$0000		; 10:3
	POP	HL			; 10:1
	EXX				;  4:1
	LD	C,A			;  4:1 = exit code
	LD	B,$00			;  7:2
	RET				; 10:1



; ----------------------------------------------
; Vstup: 
;	A  = "Packed byte"
;	HL = Adresa posledniho bajtu hlavicky
;	D  = 0
; Vystup:
;	HL = HL + 1, DE = 0
;	HL = sizeof(PALETA), DE = HL + 1
;	Plati HL + DE = DE + HL
READ_PALETTE:
	LD	E,D			;  4:1 DE = 0
	INC	HL			;  6:1
	BIT	7,A			;  8:2
	RET	z			; 11/5:1 Neni tam paleta

	LD	(ADR_PALETY+1),HL	; 16:3
	AND	$07			;  7:2 nejnizsi tri bity udavaji velikost palety barev (2^(n+1)
	LD	B,A			;  4:1
	INC	B			;  4:1 1..8
	LD	DE,$0001		; 10:3
	EX	DE,HL			;  4:1	
RP_LOOP:
	ADD	HL,HL			; 11:1
	DJNZ	RP_LOOP			;13/8:2
	
	LD	B,H			;  4:1
	LD	C,L			;  4:1 BC = HL
	ADD	HL,HL			; 11:1
	ADD	HL,BC			; 11:1 3x kvuli RGB na polozku

	RET				; 10:1


; **********************************************************************
; Vstup: 
;   HL =  adresa pocatku GIFu
INIT:	
; Cteni hlavicky GIFu
	LD	A,(HL)			;  7:1
	CP	$47			;  7:2 ASCI 'G'
	LD	A,ERR_GIF_NOT_FOUND	;  7:2
	JR	nz,ERROR_EXIT		;12/7:2
	LD	DE,$000A		; 10:3 = 10 = offset "Packed bajtu" v hlavicce gifu
	ADD	HL,DE			; 11:1
	LD	A,(HL)			;  7:1 "Packed byte"
	INC	HL			;  6:1 Background color index
	INC	HL			;  6:1 Pixel aspect ratio
	CALL	READ_PALETTE		; 17:3

; -------------------
	ADD	HL,DE			; 11:1  Pozor je soucasti i nasledujici smycky
I_NEXT_FRAME:
; Cteni hlavicky framu
	LD	A,(HL)			;  7:1
	INC	HL			;  6:1
	CP	$21			;  7:2 
	JR	nz,I_FRAME_0x2C		;12/7:2 Aspon doufam ze to bude 0x2C :)

; Framy 0x21
	LD	A,(HL)			;  7:1 Cteni Labelu
	INC	HL			;  6:1

	LD	DE,$0006		; 10:3
	CP	$f9			; Gif Graphics Control Extension
	JR	z,I_NEXT_FRAME-1	;12/7:2

;	CP	$01			; Reading Plain Text Extension Block
;	CP	$ff			; Reading Application Frame
	CP	$fe			; Reading Gif Comment Extension
	
	LD	A,ERR_UNKNOWN_FRAME	;  7:2
	JR	nz,ERROR_EXIT		;12/7:2
	LD	E,(HL)			;  7:1 DE = Size of Comment
	INC	HL			;  6:1 preskocime Size of Comment
	INC	HL			;  6:1 preskocime Terminator
	JR	I_NEXT_FRAME-1		; 12:2


I_FRAME_0x2C:
	LD	DE,$0008		; 10:3
	ADD	HL,DE			; 11:1 jsme na "Packed bajtu"
	LD	A,(HL)			;  7:1 "Packed byte" 
	CALL	READ_PALETTE		; 17:3
	ADD	HL,DE			; 11:1	

; minimalni velikost LZW 
	LD	A,(HL)			;  7:1 LZW minimum code size
	LD	B,A			;  4:1 pouzijeme v I_LOOP
	INC	A			;  4:1
	LD	IXL,A			;  8:2 MIN_SIRKA_LZW
	LD	IXH,A			;  8:2 AKT_SIRKA_LZW

	INC	HL			;  6:1 Konecne jsme na hlavicce prvniho bloku obsahujici bitovy proud

; Ulozeni promennych pro bitovy proud do pameti
	LD	D,(HL)			;  7:1 Hlavicka datoveho bloku obsahuje pocet nasledujicich bajtu
	INC	HL			;  6:1
	DEC	D			;  4:1
	DEC	D			;  4:1 snizime pocet o 2 bajty, ktere rovnou nacteme do pomocne 16 bitove promenne
	LD	E,$09			;  7:2 pocet zbyvajicich bitu se odcita pred ctenim
	POP	AF			; 10:1 Nacti RET aby nezavazelo na zasobniku
	PUSH	DE			; 11:1 D = "zbyva bajtu", E = "zbyva bitu"
	LD	E,(HL)			;  7:1 nizsi bajt do pomocne 16 bitove promenne
	INC	HL			;  6:1
	LD	D,(HL)			;  7:1 vyssi bajt do pomocne 16 bitove promenne
	INC	HL			;  6:1
	PUSH	HL			; 11:1 adresa na bajt cteny z bitoveho proudu ( ted uz je to treti )
	LD	H,(HL)			;  7:1 hodnota aktualniho (tretiho) bajtu z bitoveho proudu
	PUSH	HL			; 11:1
	PUSH	DE			; 11:1 ulozeni 16 bitove promenne na zasobnik
	PUSH	AF			; 11:1 RET

	LD	HL,$ffff		; 10:3
	LD	(ADR_FRAMEBUFF+1),HL	; 16:3 "curzor" do obrazu nastavime na 0,0
	INC	HL			;  6:1 =0
	INC	HL			;  6:1 =1
I_LOOP:
	ADD	HL,HL			; 11:1
	DJNZ	I_LOOP			;13/8:2

	LD	B,L			;  4:1 realny CLEARCODE = 4,8,16,32,64,128,0(=256) 

	INC	HL			;  6:1
	LD	(STOPCODE+1),HL		; 16:3 zbytek jako LZW_OVERFLOWS a MAX_INDEX se nastavi po prvnim CLEARCODE

; Nastaveni slovniku
; B = realny CLEARCODE
; GRBiiiii iiiiiiiS = GRB jsou barevne slozky a S je stop bit, indexu nizsi jak CLEARCODE ho maji nastaven
ADR_PALETY:
	LD	HL,$0000		; 10:3 ADR_PALETY
	LD	DE,ADR_SLOVNIKU		; 10:3
	LD	C,$7F			;  7:2
I_SLOVNIK:
	LD	A,C			;  4:1 
	LD	(DE),A			;  7:1 ........ .......s stop bit
	INC	DE			;  6:1
; Red
	LD	A,(HL)			;  7:1
	INC	HL			;  6:1
	RRCA				;  4:1 .R......
; Green
	XOR	(HL)			;  7:1
	AND	C			;  7:2 0.......
	XOR	(HL)			;  7:1 GR......
	INC	HL			;  6:1
; Blue		
	RLCA				;  4:1
	RLCA				;  4:1 ......GR
	XOR	(HL)			;  7:1
	AND	C			;  4:1 0.......
	XOR	(HL)			;  7:1 B.....GR
	INC	HL			;  6:1
	RRCA				;  4:1
	RRCA				;  4:1

	LD	(DE),A			;  7:1 GRB.....
	INC	DE			;  6:1
	DJNZ	I_SLOVNIK		;13/8:2
	RET				; 10:1



; ----------------------------------------------
; Fce nacte IXL bitu z bitoveho proudu
; Vstup: IXL = kolik nacist bitu
; Vystup: DE
READxBITS:
; Nacteni promennych ze zasobniku

	POP	DE			; 10:1 Nacti RET READxBITS
	POP	BC			; 10:1 Nacteme uz rotovane 16 bitove cislo
	LD	HL,(LZW_OVERFLOWS+1)	; 16:3
	DEC	HL			;  6:1
	LD	A,L			;  4:1 Maska nizsiho bajtu
	AND	C			;  4:1 Hodnota nizsiho bajtu
	LD	L,A			;  4:1
	LD	A,H			;  4:1 Maska vyssiho bajtu
	AND	B			;  4:1 Hodnota vyssiho bajtu
	LD	H,A			;  4:1 HL = vysledek, DE = RET READxBITS
	EX	DE,HL			;  4:1 prohodime

; ted bychom uz mohli zavolat JP (HL), ale musime pripravit na pristi cteni to 16 bitove cislo ulozene v BC

	EXX				;  4:1
	POP	AF			; 10:1 A = aktualni bajt z bitoveho proudu
	POP	HL			; 10:1 adresa na bajt cteny z bitoveho proudu
	POP	DE			; 10:1 D = "zbyva bajtu", E = "zbyva bitu"
	LD	B,IXL			;  8:2 AKT_SIRKA_LZW = pocet ctenych bitu
RxB_LOOP:
	DEC	E			;  4:1 "zbyva bitu", pred prvnim ctenim musi obsahovat 9, a ne 8
	JR	nz,RxB_ROTATE		;12/7:2

	INC	HL			;  6:1 zvysime adresu o jedna
	LD	A,(HL)			;  7:1 cteny bajt z bitoveho proudu
	LD	E,$08			;  7:2 "zbyva bitu", tady staci uz 8, protoze jeden bit ted precteme

	DEC	D			;  4:1 zbyva bajtu
	JR	nz,RxB_ROTATE		;12/7:2

	LD	D,A			;  7:1 byla to hlavicka s poctem bajtu dalsiho bloku
	INC	HL			;  6:1 zvysime adresu o jedna
	LD	A,(HL)			;  7:1 cteny bajt z bitoveho proudu

RxB_ROTATE:
	RRCA				;  4:1
	
	EXX				;  4:1
        RR	B			;  8:2
        RR	C			;  8:2
	EXX				;  4:1
        DJNZ	RxB_LOOP		;13/8:2
        
; Ulozeni promennych na zasobnik
	PUSH	DE			; 11:1 D = "zbyva bajtu", E = "zbyva bitu"
	PUSH	HL			; 11:1 adresa na bajt cteny z bitoveho proudu
	PUSH	AF			; 11:1 A = aktualni bajt z bitoveho proudu
	EXX				;  4:1
	PUSH	BC			; 11:1 Ulozime uz rotovane 16 bitove cislo
	JP	(HL)			;  4:1
;  ----------------------------------------------





; **********************************************************************
; Initialize code table
; let CODE be the first code in the code stream
; output {CODE} to index stream
; 
; <LOOP POINT>
; let CODE be the next code in the code stream
; is CODE in the code table?
; Yes:
;   let K be the first index in {CODE}
;   output {CODE} to index stream
; No:
;   let K be the first index of {CODE-1}
;   output {CODE-1}+K to index stream  
; add {CODE-1}+K to code table
; return to LOOP POINT
; **********************************************************************
DECODE:
	CALL	READxBITS		; 17:3 vystup: DE
; vysledek ignorujeme melo by to byt CLEARCODE
; navic je spatne vymaskovane neinicializovanou hodnotou LZW_OVERFLOWS

D_CLEAR_CODE:
	LD	HL,(STOPCODE+1)		; 10:3 = STOPCODE
	INC	HL			;  6:1 = MAX_INDEX
	LD	(MAX_INDEX+1),HL	; 16:3
	DEC	HL			;  6:1
	DEC	HL			;  6:1 = CLEARCODE
	ADD	HL,HL			; 11:1
	LD	(LZW_OVERFLOWS+1),HL	; 16:3 2x realne CLEARCODE
	LD	IXL,IXH			;  8:2 AKT_SIRKA_LZW = MIN_SIRKA_LZW

; nacteme prvni index, ktery musi byt mensi jak CLEARCODE
	CALL	READxBITS		; 17:3 vystup: DE
	EX	DE,HL			;  4:1
	LD	(PREVIOUS_INDEX+1),HL	; 16:3

	ADD	HL,HL			; 11:1
	LD	A,H			;  4:1
	OR	ADR_SEG_SLOVNIKU	;  7:2
	LD	H,A			;  4:1
	CALL	DRAW_BIT		; 17:3

D_READ_CODE:
	CALL	READxBITS		; 17:3 vystup: DE

STOPCODE:
	LD	HL,$0000		; 10:3 = STOPCODE
	AND	A			;  4:1 clear carry
	SBC	HL,DE			; 15:2 clear carry from READxBITS
; Exit DECODE
	JP	z,EXIT_DECODE		; 10:3
	
; test CLEARCODE
	LD	A,L			;  4:1 HL = $0001?
	DEC	A			;  4:1
	OR	H			;  4:1 clear carry
	JR	z,D_CLEAR_CODE		;12/7:2

	PUSH	DE			; 11:1 nacteny index !!!!!!!!!!!!!!!!!!!!!!!! na konci ho ulozime do PREVIOUS_INDEX
	
; zjisteni zda je nacteny index uz ve slovniku
; old index => HL = CODE
; new index => HL = CODE-1 = PREVIOUS_INDEX
	LD	HL,(MAX_INDEX+1)	; 16:3 prvni nevyuzity index
	SBC	HL,DE			; 15:2
	PUSH	AF			; 11:1 !!!!!!!!!!!! zero flag = new index
	EX	DE,HL			;  4:1
	JR	nz,D_INDEX_FOUND	;12/7:2
	LD	HL,(PREVIOUS_INDEX+1)	; 16:3 DE = 0
D_INDEX_FOUND:

;   HL = 0000iiii iiiiiiii

	ADD	HL,HL			; 11:1 clear carry
	LD	DE,ADR_SLOVNIKU		; 10:3
	ADD	HL,DE			; 11:1
	LD	D,E			;  4:1 DE = 0

; Slovnik je ulozen jako 4096 = 2^12 16 bitovych polozek = 8 KB
; format polozek je: GRBiiiii iiiiiiis
; kde GRB..... je posledni polozka slova
; odkaz na predchozi polozku slova ziskame sectenim indexu "...iiiii iiiiiii0" a segmentu slovniku "AAA..... ........"
; ".......s" je stop bit znacici ze jsme uplne na prvnim polozce slova
;
; Takze postupnym zpetnym prolezanim vytvorime cele slovo a dostaneme se az na prvni polozku K.

; [HL] = GRBiiiii iiiiiiis 
;   HL = AAAiiiii iiiiiii0

	JR	D_ENTRY			; 12:2
D_LOOP:

; L =  [HL]
; H = ([HL+1] & $1F) + ADR_SEG_SLOVNIKU

	RLCA				;  4:1
	INC	HL			;  6:1
	LD	H,(HL)			;  7:1 GRBiiiii
	LD	L,A			;  4:1
	LD	A,$1F			;  7:2 00011111
	AND	H			;  4:1 000iiiii
	OR	ADR_SEG_SLOVNIKU	;  7:2 AAAiiiii
	LD	H,A			;  4:1
D_ENTRY:
	INC	DE			;  6:1
	PUSH	HL			; 11:1
	LD	A,(HL)			;  7:1
	RRCA				;  4:1 stop bit?
	JR	nc,D_LOOP		;12/7:2
	
; Dosli jsme na prvni index slova
; BC obsahuje pocet polozek na zasobniku
; Zasobnik je plny sudych adres na predchozi index ve slove
	LD	(D_LOAD_K+1),HL		; 16:3 zasobnik nelze pouzit a volna instrukce DE je pouzita na citac
	
D_DRAW_PIXELS:
	POP	HL
	CALL	DRAW_BIT		; 17:3
	DEC	DE			;  6:1
	LD	A,D			;  4:1
	OR	E			;  4:1
	JR	nz,D_DRAW_PIXELS	;12/7:2

D_LOAD_K:
	LD	BC,$0000		; 10:3 = K
	LD	H,B			;  4:1
	LD	L,C			;  4:1
	POP	AF			; 10:1 !!!!!!!!!!!! zero flag = new_index
	CALL	z,DRAW_BIT		;17/10:3

; Vytvoreni nove polozky ve slovniku
; BC = K
; [BC] = GRB..... ........
PREVIOUS_INDEX:
	LD	HL,$0000		; 10:3 0000iiii iiiiiiii, na tohle se budeme odkazovat
	ADD	HL,HL			; 11:1 000iiiii iiiiiii0
	INC	BC			;  6:1
	LD	A,(BC)			;  7:1 GRB.....
	AND	$E0			;  7:2 GRB00000
	OR	H			;  4:1 GRBiiiii
	LD	B,A			;  4:1
	LD	C,L			;  4:1 BC = obsah nove polozky slovniku

MAX_INDEX:
	LD	DE,$0000		; 10:3 prvni nevyuzity index
	INC	DE			;  6:1

; Pokud je to posledni kod, co se vleze do soucasne sirky bitu LZW kodu, tak priste cteme kody o bit vetsi
LZW_OVERFLOWS:
	LD	HL,$0000		; 10:3
	SBC	HL,DE			; 15:2 clear carry <- OR H
	
; encoder keeps on writing codes (deferred operation)
	JR	c,D_POP_AND_READ_CODE	;12/7:2 MAX_INDEX+1 = 4097? -> Slovnik je zaplnen, uz dale nezapisujeme

	JR	nz,D_WRITE_CODE		;12/7:2

	LD	A,D			;  4:1
	CP	$10			;  7:2 4096 = %00010000 00000000
	JP	z,D_WRITE_CODE		; 10:3 MAX_INDEX+1 = 4096? -> tak nezvedam sirku na 13 bitu a naposledy ulozim slovo do slovniku

	INC	IXL			;  8:2
	ADD	HL,DE			; 11:1
	ADD	HL,HL			; 11:1
	LD	(LZW_OVERFLOWS+1),HL	; 16:3 LZW_OVERFLOWS = 2 * LZW_OVERFLOWS
		
D_WRITE_CODE:
	EX	DE,HL			;  4:1
	LD	(MAX_INDEX+1),HL	; 16:3
	DEC	HL			;  4:1	
	ADD	HL,HL			; 11:1 000iiiii iiiiiii0
	LD	A,ADR_SEG_SLOVNIKU	;  7:2 AAA00000
	OR	H			;  4:1 AAAiiiii
	LD	H,A			;  4:1 HL = adresa nove polozky slovniku 

	LD	(HL),C			;  7:1
	INC	HL			;  6:1
	LD	(HL),B			;  7:1
	
D_POP_AND_READ_CODE:

; Zamena predchozi za posledni nacteny index
	POP	HL			; 11:1 nacteny index !!!!!!!!!!!!!!!!!!!!!!!!
	LD	(PREVIOUS_INDEX+1),HL	; 16:3 0000iiii iiiiiiii

	JP	D_READ_CODE		; 10:3



; ----------------------------------------------
; Vstup: 
;  HL  = (AAAiiiii iiiiiii0)
; [HL] = (GRBiiiii iiiiiiis)
; Vystup:
; HL = HL+1, DE = DE, BC = BC, A = ?, flags = ?
DRAW_BIT:
	INC	HL			;  6:1
	LD	A,(HL)			;  7:1 GRB?????
	RRCA				;  4:1
	RRCA				;  4:1
	AND	$38			;  7:2 00GRB000
	EXX				;  4:1
	LD	C,A			;  4:1 Nactena hodnota je presunuta na pozici Paper

ADR_FRAMEBUFF:
	LD	DE,$0000		; 10:3
	INC	DE			;  6:1
	LD	(ADR_FRAMEBUFF+1),DE	; 20:4 Aktualni pixel na obrazovce
 
	LD	A,$C0			;  7:2 $C000 = 49152 = 256x192
	CP	D			;  4:1
	LD	A,ERR_OWERFLOW_SCREEN	;  7:2
	JP	z,ERROR_EXIT		; 10:3

; D = BBRRRSSS E = CCCCC... 
	LD	A,D			;  4:1 BBRRR...
	RLCA				;  4:1
	RLCA				;  4:1 
	LD	B,A			;  4:1 RRR...BB
	AND	$03			;  7:2
	OR	%01011000		;  7:2 010110BB
	LD	H,A			;  4:1
	LD	A,E			;  4:1 CCCCC... 
	RRCA				;  4:1
	RRCA				;  4:1
	RRCA				;  4:1 ...CCCCC
	XOR	B			;  4:1
	AND	$1F			;  7:2 000?????
	XOR	B			;  4:1 RRRCCCCC
	LD	L,A			;  4:1
; H = 010110BB L = RRRCCCCC
	
	LD	B,$07			;  7:2
	LD	A,E			;  4:1 X
	OR	D			;  4:1 Y
	AND	B			;  4:1 
	JR	nz,DB_TEST_PAPER	;12/7:2 Prvni pixel matice?
	
; Prvnim pixel bude vzdy PAPER, ten ma hodnotu pixelu 0.
; Vytvorime si k nemu falesny INK = ( PAPER + 4 ) & $07
; Tzn. pro zelenou barvu (4) PAPERu a vyssi, bude mit INK nizsi hodnotu.
; Pokud bude ve znaku jen jedna barva, a bude to ta zelena (4) nebo vyssi, tak se diky falesnemu INK aktivuje inverze bitu matice 8x8
; U obrazku jen ze 2 barev, jejichz rozdil je aspon 4, nevzniknou v obraze inverzni matice. 

	LD	A,C			;  7:1
	RRCA				;  4:1
	RRCA				;  4:1
	RRCA				;  4:1
	ADD	A,$04			;  7:2 +4 k barve
	AND	B			;  4:1 4..7 pretece na 0..3
	OR	C			;  4:1 + PAPER, clear carry
	LD	(HL),A			;  7:1 Smazeme Flash, PAPER, falesny INK
	JR	DB_CLEAR_PIXEL		; 12:2

DB_TEST_PAPER:
	LD	A,(HL)			;  7:1
	AND	$38			;  7:2
	CP	C			;  4:1
	JR	z,DB_CLEAR_PIXEL	; 12/7:2 je to PAPER
	
	LD	A,C			;  4:1
	RRCA				;  4:1
	RRCA				;  4:1
	RRCA				;  4:1
	XOR	(HL)			;  7:1
	AND	B			;  4:1
	XOR	(HL)			;  7:1
	LD	(HL),A			;  7:1 INK neustale prepisujeme na posledni variantu

; ----------------------
DB_SET_PIXEL:
	SCF				;  4:1

DB_CLEAR_PIXEL:
	LD	A,$00			;  7:2 Seznam uz nactenych pixelu
	ADC	A,A			;  4:1
	LD	(DB_CLEAR_PIXEL+1),A	; 13:3
	LD	C,A			;  4:1 8 pixels

; Test praveho konce znaku
	LD	A,E			;  4:1 reg. E je odted volny
	INC	A			;  4:1
	AND	B			;  4:1
	JR	nz,DB_EXIT		;12/4:2
	
; Test spodniho konce znaku
	LD	A,D			;  4:1 reg. D je odted volny
	INC	A			;  4:1
	AND	B			;  4:1
	EX	AF,AF'			;  4:1 odlozime vykonani

; D = BBRRRSSS E = CCCCC...
; H = 010110BB L = RRRCCCCC
	LD	A,H			;  4:1
	ADD	A,A			;  4:1
	ADD	A,A			;  4:1
	ADD	A,A			;  4:1 110BB000
	XOR	D			;  4:1
	AND	$F8			;  7:2 
	XOR	D			;  4:1 110BBSSS
	ADD	A,A			;  4:1 10BBSSS0
	RRCA				;  4:1 010BBSSS
	LD	D,A			;  4:1
	LD	E,L			;  4:1
; D = 010BBSSS E = RRRCCCCC

	LD	A,C			;  4:1
	LD	(DE),A			;  7:1 Ulozeni 8 pixelu na obrazku

; Test spodniho konce znaku, dokonceni
	EX	AF,AF'			;  4:1 obnovime priznaky
	JR	nz,DB_EXIT		;12/4:2

; Jsme na poslednim pixelu matice 8x8, zkontrolujeme zda nemame udelat inverzi matice

	LD	A,(HL)			;  7:1 FBpppiii
	AND	B			;  4:1 .....iii
	RRCA				;  4:1 i.....ii
	RRCA				;  4:1 ii.....i
	XOR	(HL)			;  7:1 ??pppii?
	AND	$C7			;  7:2 ??000ii? 
	XOR	(HL)			;  7:1 iippp..i
	RRCA				;  4:1
	RRCA				;  4:1
	RRCA				;  4:1 ..iiippp
	LD	C,A			;  4:1 Prohozeny INK a PAPER

	XOR	(HL)			;  7:1
	AND	B			;  4:1 %00000???
	XOR	(HL)			;  7:1 PAPER PAPER  
	CP	C			;  4:1 "PAPER PAPER" - "INK PAPER" -> PAPER - INK
	JR	c,DB_EXIT		;12/7:2
	
; Inverze matice
	LD	(HL),C			;  7:1 Prohozeny INK a PAPER
	INC	B			;  4:1
DB_INVERT_MATRIX:
	LD	A,(DE)			;  7:1
	CPL				;  4:1
	LD	(DE),A			;  7:1
	DEC	D			;  4:1
	DJNZ	DB_INVERT_MATRIX	;13/8:2
	
DB_EXIT:
	EXX				;  4:1
	RET				; 10:1
