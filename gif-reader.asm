; GIF reader v0.4b for ZX Spectrum
; Not used BRIGHT.
; Simple rules to minimize the inverted matrix of 8x8 pixels.
; Striving for the binding of color PAPER and INK.
; CLEARCODE fixed
; Support for calling directly from BASIC
; Support error codes
; Overflow screens with large images is detected
; 256 x 192 only
;
; http://www.matthewflickinger.com/lab/whatsinagif/bits_and_bytes.asp
; http://www.matthewflickinger.com/lab/whatsinagif/lzw_image_data.asp
;
; Compile:
; pasmo -d zxgif.asm zxgif.bin > test.asm

ADR_SEG_SLOVNIKU	EQU	5 * %00100000		; $A000..$BFFF, granularita 8 KB
ADR_SLOVNIKU		EQU	256 * ADR_SEG_SLOVNIKU
progStart 		EQU	ADR_SLOVNIKU + $2000	; $C000 = 49152

ORG progStart

; sizeof(SLOVNIK) = 4096 * 2 bytes = 8 KB
; HI        LO
; GRBiiiii iiiiiiiS = S = stop bit, nastaven u prvni polozky slova, tzn. maji ho indexy mensi jak CLEARCODE
;  +
; AAA..... ........ = segment adresy musi lezet na adrese delitelne 16kb
;  =
; AAAiiiii iiiiiii0 = adresa predchozi polozky slovniku
  
ERR_OK			EQU	0
ERR_GIF_NOT_FOUND	EQU	1	; != "G"
ERR_UNKNOWN_FRAME	EQU	2	; Plain Text Extension Block, Application Frame
ERR_OWERFLOW_SCREEN	EQU	3	; Pixelu je vice jak 192*256

MAIN:
	EXX	
	PUSH	HL
	
; PRINT USR progStart, ADR_GIF
; Nacteni druheho parametru do DE
	RST	$20			; 11:1
	CALL	$24FB			; 17:3
	CALL	$2DD5			; 17:3
	LD	H,B			;  4:1
	LD	L,C			;  4:1 HL = ADR_GIF
	
	LD	(ERROR_EXIT+1),SP	; 20:4
	CALL	INIT			; 17:3
	CALL	DECODE			; 17:3
MAIN_EXIT:
	POP	HL
	EXX
	LD	C,A			;  4:1 = exit code
	LD	B,$00			;  7:2
	RET 				; 10:1


DATA:
DW	0	;    adresa odkud
DB	0	; +2 zbyva bitu
DB	0	; +3 zbyva bajtu
DB	0	; +4 zbytek uz rotovaneho bajtu
DB	0	; +5 (last index != new index)?


PREVIOUS_INDEX:	; predchozi index z bitoveho proudu
DW	0	;

; IXH = MIN_SIRKA_LZW:
; IXL = AKT_SIRKA_LZW:


; ----------------------------------------------
ERROR_EXIT:
; stala se chyba, ukoncime program
	LD	SP,$0000		; 10:3
	JR	MAIN_EXIT		; 10:1



; ----------------------------------------------
; Vstup: 
;	A  = "Packed byte"
;	HL = Adresa posledniho bajtu hlavicky
; 	D  = 0
; Vystup:
;	HL = HL + 1, DE = 0
;	HL = sizeof(PALETA), DE = HL + 1
; 	Plati HL + DE = DE + HL
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
READ_PALETTE_LOOP:
	ADD	HL,HL			; 11:1
	DJNZ	READ_PALETTE_LOOP	; 13/8:2
	
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
	JR	nz,ERROR_EXIT		;
	LD	DE,$000A		; 10:3 = 10 = offset "Packed bajtu" v hlavicce gifu
	ADD	HL,DE			; 11:1
	LD	A,(HL)			;  7:1 "Packed byte"
	INC	HL			;  6:1 Background color index
	INC	HL			;  6:1 Pixel aspect ratio
	CALL	READ_PALETTE		; 17:3
	
	ADD	HL,DE			; 11:1  Pozor je soucasti i nasledujici smycky
INIT_NEXT_FRAME:
; Cteni hlavicky framu
	LD	A,(HL)			;  7:1
	INC	HL			;  6:1
	CP	$21			;  7:2 
	JR	nz,INIT_FRAME_0x2C	; Aspon doufam ze to bude 0x2C :)

; Framy 0x21
	LD	A,(HL)			;  7:1 Cteni Labelu
	INC	HL			;  6:1

	LD	DE,$0006		; 10:3
	CP	$f9			; Gif Graphics Control Extension
	JR	z,INIT_NEXT_FRAME-1

; 	CP	$01			; Reading Plain Text Extension Block
; 	CP	$ff			; Reading Application Frame
	CP	$fe			; Reading Gif Comment Extension
	
	LD	A,ERR_UNKNOWN_FRAME	;  7:2
	JR	nz,ERROR_EXIT
	LD	E,(HL)			;  7:1 DE = Size of Comment
	INC	HL			;  6:1 preskocime Size of Comment
	INC	HL			;  6:1 preskocime Terminator
	JR	INIT_NEXT_FRAME-1


INIT_FRAME_0x2C:
	LD	DE,$0008		; 10:3
	ADD	HL,DE			; 11:1 jsme na "Packed bajtu"
	LD	A,(HL)			;  7:1 "Packed byte" 
	CALL	READ_PALETTE		; 17:3
	ADD	HL,DE			; 11:1	

; minimalni velikost LZW 
	LD	A,(HL)			;  7:1 LZW minimum code size
	LD	B,A			;  4:1 pouzijeme v INIT_LOOP
	INC	A			;  4:1
	LD	IXL,A			;  8:2 MIN_SIRKA_LZW
	LD	IXH,A			;  8:2 AKT_SIRKA_LZW

	INC	HL			;  6:1 Konecne jsme na hlavicce prvniho bloku obsahujici bitovy proud

; Ulozeni promennych pro bitovy proud do pameti
	LD	D,(HL)			;  7:1 nacteni hlavicky prvniho datoveho bloku
	INC	HL			;  6:1
	LD	(DATA),HL		; 16:3 adresa do bitoveho proudu LZW kodu
	LD	E,$09			;  7:2 "zbyva bitu"
	LD	(DATA+2),DE		; 20:4 D = "zbyva bajtu"
	LD	A,(HL)			;  7:1
	LD	(DATA+4),A		; 13:3 aktualni bajt z bitoveho proudu	
	
	LD	HL,$ffff		; 10:3
	LD	(ADR_FRAMEBUFF+1),HL	; 16:3 "curzor" do obrazu nastavime na 0,0
	INC	HL			;  6:1 =0
	INC	HL			;  6:1 =1
INIT_LOOP:
	ADD	HL,HL			; 11:1
	DJNZ	INIT_LOOP

	LD	B,L			;  4:1 4,8,16,32,64,128,0=256 = realny CLEARCODE

	INC 	HL			;  6:1
	LD	(STOPCODE+1),HL		; 16:3
	INC	HL			;  6:1
	LD	(MAX_INDEX+1),HL	; 16:3 ukazuje na novou, jeste nevyplnenou polozku
	DEC	HL			;  6:1
	DEC	HL			;  6:1
	ADD	HL,HL			; 11:1
	LD	(LZW_OVERFLOWS+1),HL	; 16:3 2x realne CLEARCODE

; Nastaveni slovniku
; B = realny CLEARCODE
; GRBiiiii iiiiiiiS = GRB jsou barevne slozky a S je stop bit, indexu nizsi jak CLEARCODE ho maji nastaven
ADR_PALETY:
	LD	HL,$0000		; 10:3 ADR_PALETY
	LD	DE,ADR_SLOVNIKU		; 10:3
	LD	C,$7F			;  7:2
INIT_SLOVNIK:
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
	DJNZ	INIT_SLOVNIK

	LD	BC,$00C0		;  7:2 $00C0 => Set Flash + Bright + Black + Black
SET_FLASH_BRIGHT:			;      $3F00 => Clear Flash + Bright
	LD	HL,$5800		; 10:3
CLEAR_LOOP:
	LD	A,(HL)			;  7:1
	AND	B			;  4:1
	OR	C			;  4:1
	LD	(HL),A			;  7:1 Clear Flash + Bright / Set Flash + Bright + Black + Black 
	INC	HL			;  6:1
	LD	A,$5B			;  7:2
	SUB	H			;  4:1
	JR	nz,CLEAR_LOOP		;    2
	RET				; 10:1 A = 0 = ERR_OK



; ----------------------------------------------
; Vstup: 
; 	HL  adresa prave cteneho bajtu
; 	E   "zbyva bitu", 
; 	D   "zbyva bajtu"
; 	A   uz rotovany prave cteny bajt
READ_BIT:
	EXX				;  4:1
	DEC	E			;  4:1 "zbyva bitu", pred prvnim ctenim musi obsahovat 9, a ne 8
	JR	nz,READ_BIT_ROTATE

	INC	HL			;  6:1 zvysime adresu o jedna
	LD	A,(HL)			;  7:1 cteny bajt z bitoveho proudu
	LD	E,$08			;  7:2 "zbyva bitu", tady staci uz 8, protoze jeden bit ted precteme

	DEC	D			;  4:1 zbyva bajtu
	JR	nz,READ_BIT_ROTATE

	LD	D,A			;  7:1 byla to hlavicka s poctem bajtu dalsiho bloku
	INC	HL			;  6:1 zvysime adresu o jedna
	LD	A,(HL)			;  7:1 cteny bajt z bitoveho proudu

READ_BIT_ROTATE:
	RRCA				;  4:1
	EXX				;  4:1
	RET				; 10:1



; ----------------------------------------------
; Vstup: IXL = kolik nacist bitu
; Vystup: HL, clear carry
; Nemeni: DE
READxBITS:
	LD	C,IXL			;  8:2 AKT_SIRKA_LZW = pocet ctenych bitu
; Nacteni promennych z pameti
	EXX				;  4:1
	LD	HL,(DATA)		; 16:3 adresa do bitoveho proudu LZW kodu
	LD	DE,(DATA+2)		; 20:4 E = "zbyva bitu", D = "zbyva bajtu"
	LD	A,(DATA+4)		; 13:3 cteny a uz rotovany bajt z bitoveho proudu
	EXX				;  4:1
	
	LD	HL,$0000		; 10:3
	LD	B,$08			;  7:2
READxBITS_LOOP_LO:
	DEC 	C			;  4:1
	CALL	p,READ_BIT		; 17/10:3
	RR	L			;  8:2 clear carry
	DJNZ	READxBITS_LOOP_LO	;

	BIT	7,C			;  8:2
	JR	nz,READxBITS_EXIT	

	LD	B,$08			;  7:2
READxBITS_LOOP_HI:
	DEC 	C			;  4:1
	CALL	p,READ_BIT		; 17/10:3
	RR	H			;  8:2 clear carry
	DJNZ	READxBITS_LOOP_HI	;

READxBITS_EXIT:
; Ulozeni promennych do pameti
	EXX				;  4:1
	LD	(DATA),HL		; 16:3 adresa do bitoveho proudu LZW kodu
	LD	(DATA+2),DE		; 20:4 E = "zbyva bitu", D = "zbyva bajtu"
	LD	(DATA+4),A		; 13:3 cteny a uz rotovany bajt z bitoveho proudu
	EXX				;  4:1
	
	RET				; 10:1
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
	CALL	READxBITS		; 17:3 vystup: HL, clear carry
; vysledek ignorujeme melo by to byt CLEARCODE
; nacteme prvni index, ktery musi byt mensi jak CLEARCODE

DECODE_FIRST_INDEX:
	CALL	READxBITS		; 17:3 vystup: HL, clear carry
	LD	(PREVIOUS_INDEX),HL	; 16:3
	
	ADD	HL,HL			; 11:1
	LD	A,H			;  4:1
	OR	ADR_SEG_SLOVNIKU	;  7:2
	LD	H,A			;  4:1
	CALL	DRAW_BIT		; 17:3
	
DECODE_NEXT_INDEX:
	CALL	READxBITS		; 17:3 vystup: HL, clear carry

	EX	DE,HL			;  4:1
STOPCODE:
	LD	HL,$0000		; 10:3 = STOPCODE
	SBC	HL,DE			; 15:2 clear carry from READxBITS
; Exit DECODE
	LD	BC,$3F00		; 10:3 $3F00 => Clear Flash + Bright
	JR	z,SET_FLASH_BRIGHT	; 
	
; CLEARCODE test
	LD	A,L			;  4:1 HL = $0001?
	DEC	A			;  4:1
	OR	H			;  4:1
	JR	nz,NO_CLEAR

	ADD	HL,DE			; 11:1 1 + CLEARCODE = STOPCODE
	INC	HL			;  6:1 = MAX_INDEX
	LD	(MAX_INDEX+1),HL	; 16:3
	EX	DE,HL			;  4:1 = CLEARCODE
	ADD	HL,HL			; 11:1
	LD	(LZW_OVERFLOWS+1),HL	; 16:3 2x realne CLEARCODE
	LD	IXL,IXH			;  8:2 AKT_SIRKA_LZW = MIN_SIRKA_LZW
	JR	DECODE_FIRST_INDEX

NO_CLEAR:
	PUSH	DE			; 11:1 nacteny index !!!!!!!!!!!!!!!!!!!!!!!! na konci ho ulozime do PREVIOUS_INDEX
	
; zjisteni zda je nacteny index uz ve slovniku
; old index => HL = CODE
; new index => HL = CODE-1 = PREVIOUS_INDEX
MAX_INDEX:
	LD	HL,$0000		; 10:3 prvni nevyuzity index
	PUSH	HL			; 11:1 MAX_INDEX !!!!!!!!!!!!!!!!!!!!!!!! ulozime si index na novy prvek, na konci do nej dame novou polozku
	
	AND	A			;  4:1 clear carry
	SBC	HL,DE			; 15:2
	PUSH	AF			; 11:1 !!!!!!!!!!!! zero flag = new index
	EX	DE,HL			;  4:1
	JR	nz,INDEX_FOUND
	LD	HL,(PREVIOUS_INDEX)	; 16:3 DE = 0
INDEX_FOUND:

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


LOOP:
	INC	DE			;  6:1
	PUSH	HL			; 11:1
	LD	A,(HL)			;  7:1
	RRCA				;  4:1 stop bit?
	JR	c,PRVNI_INDEX_SLOVA

; L =  [HL]
; H = ([HL+1] & $1F) + ADR_SEG_SLOVNIKU

	LD	A,(HL)			;  7:1
	INC	HL			;  6:1
	LD	H,(HL)			;  7:1 GRBiiiii
	LD	L,A			;  4:1
	LD	A,$1F			;  7:2 00011111
	AND	H			;  4:1 000iiiii
	OR	ADR_SEG_SLOVNIKU	;  7:2 AAAiiiii
	LD	H,A			;  4:1
	JR	LOOP
	
	
PRVNI_INDEX_SLOVA:
; BC obsahuje pocet polozek na zasobniku
; Zasobnik je plny sudych adres na predchozi index ve slove
	LD	(LOAD_K+1),HL		; 16:3 zasobnik nelze pouzit a volna instrukce DE je pouzita na citac
	
VYKRESLI_PIXELY:
	POP	HL
	CALL	DRAW_BIT		; 17:3
	DEC	DE			;  6:1
	LD	A,D			;  4:1
	OR	E			;  4:1
	JR	nz,VYKRESLI_PIXELY	;

LOAD_K:
	LD	DE,$0000		; 10:3 = K
	POP	AF			; 10:1 !!!!!!!!!!!! zero flag = new_index
	JR	nz,MAKE_NEW_INDEX
	
	LD	H,D			;  4:1
	LD	L,E			;  4:1
	CALL	DRAW_BIT		; 17:3

; Vytvoreni nove polozky ve slovniku
; DE = K
; [DE] = GRBiiiii iiiiiiis
MAKE_NEW_INDEX:

	LD	HL,(PREVIOUS_INDEX)	; 16:3 0000iiii iiiiiiii, na tohle se budeme odkazovat
	ADD	HL,HL			; 11:1
	INC	DE			;  6:1
	LD	A,(DE)			;  7:1 GRB.....
	AND	$E0			;  7:2 GRB00000
	OR	H			;  4:1 GRBiiiii
	LD	H,A			;  4:1
	
	EX	DE,HL			;  4:1
	POP	HL			; 10:1 MAX_INDEX !!!!!!!!!!!!!!!!!!!!!!!!
	PUSH	HL			; 11:1
; HL = 0000iiii iiiiiiii = adresa nove polozky slovniku
	ADD	HL,HL			; 11:1 0000iiii iiiiiii0
	LD	A,ADR_SEG_SLOVNIKU	;  7:2 AAA00000
	OR	H			;  4:1 AAAiiiii
	LD	H,A			;  4:1 AAAiiiii iiiiiii0
	
	LD	(HL),E			;  7:1
	INC	HL			;  6:1
	LD	(HL),D			;  7:1
	
	POP	HL			; 10:1
	INC	HL			;  6:1
	LD	(MAX_INDEX+1),HL	; 16:3
	
LZW_OVERFLOWS:
	LD	DE,$0000		; 10:3
	SBC	HL,DE			; 15:2
	
; Zamena predchozi za posledni nacteny index
	POP	HL			; 11:1 nacteny index !!!!!!!!!!!!!!!!!!!!!!!!
	LD	(PREVIOUS_INDEX),HL	; 16:3 0000iiii iiiiiiii

	JP	nz,DECODE_NEXT_INDEX

	EX	DE,HL			;  4:1
	ADD	HL,HL			; 11:1
	LD	(LZW_OVERFLOWS+1),HL	; 16:3
	
; 13 bitova nedokumentovana obskurni praktika...  :(
; Pokud cteni dalsiho kodu znamena, ze potrebuji cist o bit vice, tak o ten bit vice ctu.
; Pokud ale o bit vice znamena cist 13 bitu, tak ctu stale 12 bitu a doufam, ze dalsi kod bude signal "clear code" nebo nizsi jak 4096.
	
	LD	A,IXL			;  8:2
	CP	$0C			;  7:2 - 12
	ADC	A,$00			;  7:2
	LD	IXL,A			;  8:2
		
	JP	DECODE_NEXT_INDEX




; ----------------------------------------------
; Vstup: 
;  HL  = (AAAiiiii iiiiiii0)
; [HL] = (GRBiiiii iiiiiiis)
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
	JP	z,ERROR_EXIT		; 
 
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

; ----------------------------------------------------------
; Rozlisujeme 3 varianty podle toho kolik je znamo uz barev:
; 1. Zadna? ( FIRST_SEARCH )
;       Je cerna? 
; 		ANO: Bude to Paper. ( tohle by melo vyresit monochromaticke obrazky )
;	Shodna se sousednim Paper? ( i kdyz neni cerna )
;		ANO: Bude to novy Paper. Exit.
;		 NE: Bude to novy Ink!  ( cerna u monochromatickeho jeste dojde )  Draw.
; 2. Vsechny nebo jen Paper? ( COLORS_FIXED ) 
;	Shodna s Paper?
;		ANO: Je to Paper. Exit.
;		 NE: Bude to novy Ink. ( nebo prepsany ) Draw.
; 3. Jen Ink? ( INK_FIXED )
;	Shodny s Ink?
;		ANO: Je to Ink. Draw.
;		 NE: Bude to novy Paper. Exit.

	LD	A,(HL)			;  7:1
	AND	%11000000		;  7:2 Flash (Paper undefined) + Bright (Ink undefined)
	CP	%10000000		;  7:2
	JR	c,COLORS_FIXED		;      => 2. Paper found + Ink found / undefined
	JR	nz,FIRST_SEARCH		;      => 1. Paper undefined + Ink undefined

; 3. Paper undefined + Ink found -----------------------
INK_FIXED:
	LD	A,(HL)			;  7:1
	AND	$07			;  7:2
	RLCA				;  4:1
	RLCA				;  4:1
	RLCA				;  4:1
	CP	C			;  4:1 Test Ink
	JR	z,DRAW_BIT_SCREEN_ADR	
	JR	NEW_PAPER

; 1. Paper undefined + Ink undefined -------------------
FIRST_SEARCH:
	LD	A,C			;  4:1
	OR	A			;  4:1 Black?
	JR	z,NEW_PAPER
	
; Pokus o kontinuitu v barvach mezi 8x8 maticemi. Porovname s levou matici, a pokud to nejde tak s horni.
	PUSH	HL			; 11:1
	LD	A,L			;  4:1
	AND	$1F			;  7:2
	JR	nz,DALSI_SLOUPEC
	ADD	A,$21			;  7:2
	LD	L,A			;  4:1
DALSI_SLOUPEC:
	DEC	HL			;  4:1
; TEST_PAPER
	LD	A,(HL)			;  7:1 Sousedni atribut
	AND	$38			;  7:2 Sousedni Paper
	CP	C			;  4:1
	POP	HL			; 10:1
	JR	nz,NEW_INK
	
NEW_PAPER:
	LD	A,(HL)			;  7:1
	OR	C			;  4:1
	AND	%01111111		;  7:2 Flash clear
	LD	(HL),A			;  4:1
; 	Propadne na DRAW_BIT_EXIT 

; 2. Paper found + Ink found / undefined ---------------
COLORS_FIXED:
	LD	A,(HL)			;  7:1
	AND	$38			;  7:2
	CP	C			;  4:1 Test Paper
	JR	z,DRAW_BIT_EXIT		; 

NEW_INK:
	LD	A,C			;  4:1
	RRCA				;  4:1
	RRCA				;  4:1
	RRCA				;  4:1 .0...PPP
	XOR	(HL)			;  7:1
	AND	%01000111		;  7:2
	XOR	(HL)			;  7:1 ?0???GRB, clear Bright
	LD	(HL),A			;  7:1

; ------------------------------------------------------
DRAW_BIT_SCREEN_ADR:
 
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
	LD	H,A			;  4:1
; H = 010BBSSS L = RRRCCCCC

	LD	A,E			;  4:1
	AND	$07			;  7:2
	LD	B,A			;  4:1
	INC	B			;  4:1
	LD	A,$01			;  7:2  
DRAW_BIT_LOOP:
	RRCA				;  4:1 rotace doprava
	DJNZ	DRAW_BIT_LOOP		;13/8:2
	OR	(HL)			;  7:1
	LD	(HL),A			;  7:1
	
DRAW_BIT_EXIT:
	EXX				;  4:1
	RET				; 10:1
