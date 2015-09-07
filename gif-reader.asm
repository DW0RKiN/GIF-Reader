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
; pasmo -d gif-reader.asm gif-reader.bin > test.asm

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
	CALL	DECODE			; 17:3
MAIN_EXIT:
	POP	HL			; 10:1
	EXX				;  4:1
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

; Globalni pouziti registru:
; 	IXH = MIN_SIRKA_LZW:
; 	IXL = AKT_SIRKA_LZW:


; ----------------------------------------------
ERROR_EXIT:
; stala se chyba, ukoncime program
	LD	SP,$0000		; 10:3
	JR	MAIN_EXIT		; 12:2



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

; 	CP	$01			; Reading Plain Text Extension Block
; 	CP	$ff			; Reading Application Frame
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
I_LOOP:
	ADD	HL,HL			; 11:1
	DJNZ	I_LOOP			;13/8:2

	LD	B,L			;  4:1 realny CLEARCODE = 4,8,16,32,64,128,0(=256) 

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
;	Propadne na dalsi fci SET_FLASH_BRIGHT
	LD	BC,$00C0		;  7:2 set Flash + Bright + Black + Black


; -----------------------
; Fce edituje hodnoty vsech barev
Vstup:
;	B = co nesmazeme z puvodniho
;	C = co k tomu pridame
;	$00C0 => %11000000 set Flash + Bright + Black + Black
;	$3F00 => %00?????? Clear Flash + Bright
SET_FLASH_BRIGHT:
	LD	HL,$5800		; 10:3
SFB_LOOP:
	LD	A,(HL)			;  7:1
	AND	B			;  4:1
	OR	C			;  4:1
	LD	(HL),A			;  7:1 Clear Flash + Bright / Set Flash + Bright + Black + Black 
	INC	HL			;  6:1
	LD	A,$5B			;  7:2
	SUB	H			;  4:1
	JR	nz,SFB_LOOP		;12/7:2
	RET				; 10:1 A = 0 = ERR_OK



; ----------------------------------------------
; Fce cte jeden bit z bitoveho proudu
; Vstup: 
; 	HL  adresa prave cteneho bajtu
; 	E   "zbyva bitu", 
; 	D   "zbyva bajtu"
; 	A   uz rotovany prave cteny bajt
; Vystup: 
; 	hodnota cteneho bitu je v carry
READ_BIT:
	EXX				;  4:1
	DEC	E			;  4:1 "zbyva bitu", pred prvnim ctenim musi obsahovat 9, a ne 8
	JR	nz,RB_ROTATE		;12/7:2

	INC	HL			;  6:1 zvysime adresu o jedna
	LD	A,(HL)			;  7:1 cteny bajt z bitoveho proudu
	LD	E,$08			;  7:2 "zbyva bitu", tady staci uz 8, protoze jeden bit ted precteme

	DEC	D			;  4:1 zbyva bajtu
	JR	nz,RB_ROTATE		;12/7:2

	LD	D,A			;  7:1 byla to hlavicka s poctem bajtu dalsiho bloku
	INC	HL			;  6:1 zvysime adresu o jedna
	LD	A,(HL)			;  7:1 cteny bajt z bitoveho proudu

RB_ROTATE:
	RRCA				;  4:1
	EXX				;  4:1
	RET				; 10:1



; ----------------------------------------------
; Fce nacte IXL bitu z bitoveho proudu
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
RxB_LOOP_LO:
	DEC 	C			;  4:1
	CALL	p,READ_BIT		; 17/10:3
	RR	L			;  8:2 clear carry
	DJNZ	RxB_LOOP_LO		;13/8:2

	BIT	7,C			;  8:2
	JR	nz,RxB_EXIT		;12/7:2

	LD	B,$08			;  7:2
RxB_LOOP_HI:
	DEC 	C			;  4:1
	CALL	p,READ_BIT		; 17/10:3
	RR	H			;  8:2 clear carry
	DJNZ	RxB_LOOP_HI		;13/8:2

RxB_EXIT:
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

D_FIRST_INDEX:
	CALL	READxBITS		; 17:3 vystup: HL, clear carry
	LD	(PREVIOUS_INDEX),HL	; 16:3
	
	ADD	HL,HL			; 11:1
	LD	A,H			;  4:1
	OR	ADR_SEG_SLOVNIKU	;  7:2
	LD	H,A			;  4:1
	CALL	DRAW_BIT		; 17:3
	
D_NEXT_INDEX:
	CALL	READxBITS		; 17:3 vystup: HL, clear carry

	EX	DE,HL			;  4:1
STOPCODE:
	LD	HL,$0000		; 10:3 = STOPCODE
	SBC	HL,DE			; 15:2 clear carry from READxBITS
; Exit DECODE
	LD	BC,$3F00		; 10:3 $3F00 => Clear Flash + Bright
	JR	z,SET_FLASH_BRIGHT	;12/7:2
	
; test CLEARCODE
	LD	A,L			;  4:1 HL = $0001?
	DEC	A			;  4:1
	OR	H			;  4:1
	JR	nz,D_NOT_FOUND_CC	;12/7:2
; CLEARCODE
	ADD	HL,DE			; 11:1 1 + CLEARCODE = STOPCODE
	INC	HL			;  6:1 = MAX_INDEX
	LD	(MAX_INDEX+1),HL	; 16:3
	EX	DE,HL			;  4:1 = CLEARCODE
	ADD	HL,HL			; 11:1
	LD	(LZW_OVERFLOWS+1),HL	; 16:3 2x realne CLEARCODE
	LD	IXL,IXH			;  8:2 AKT_SIRKA_LZW = MIN_SIRKA_LZW
	JR	D_FIRST_INDEX	; 12:2

D_NOT_FOUND_CC:
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
	JR	nz,D_INDEX_FOUND	;12/7:2
	LD	HL,(PREVIOUS_INDEX)	; 16:3 DE = 0
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


D_LOOP:
	INC	DE			;  6:1
	PUSH	HL			; 11:1
	LD	A,(HL)			;  7:1
	RRCA				;  4:1 stop bit?
	JR	c,D_FOUND_K		;12/7:2

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
	JR	D_LOOP			; 12:2
	
	
D_FOUND_K:
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
	LD	DE,$0000		; 10:3 = K
	POP	AF			; 10:1 !!!!!!!!!!!! zero flag = new_index
	JR	nz,D_MAKE_NEW_INDEX	;12/7:2
	
	LD	H,D			;  4:1
	LD	L,E			;  4:1
	CALL	DRAW_BIT		; 17:3

; Vytvoreni nove polozky ve slovniku
; DE = K
; [DE] = GRBiiiii iiiiiiis
D_MAKE_NEW_INDEX:

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

	JP	nz,D_NEXT_INDEX

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
		
	JP	D_NEXT_INDEX




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
; 1. Zadna? ( DB_FIRST_SEARCH )
;       Je cerna? 
; 		ANO: Bude to Paper. ( tohle by melo vyresit monochromaticke obrazky )
;	Shodna se sousednim Paper? ( i kdyz neni cerna )
;		ANO: Bude to novy Paper. Exit.
;		 NE: Bude to novy Ink!  ( cerna u monochromatickeho jeste dojde )  Draw.
; 2. Vsechny nebo jen Paper? ( DB_COLORS_FIXED ) 
;	Shodna s Paper?
;		ANO: Je to Paper. Exit.
;		 NE: Bude to novy Ink. ( nebo prepsany ) Draw.
; 3. Jen Ink? ( DB_INK_FIXED )
;	Shodny s Ink?
;		ANO: Je to Ink. Draw.
;		 NE: Bude to novy Paper. Exit.

	LD	A,(HL)			;  7:1
	AND	%11000000		;  7:2 Flash (Paper undefined) + Bright (Ink undefined)
	CP	%10000000		;  7:2
	JR	c,DB_COLORS_FIXED	;12/7:2  => 2. Paper found + Ink found / undefined
	JR	nz,DB_FIRST_SEARCH	;12/7:2  => 1. Paper undefined + Ink undefined

; 3. Paper undefined + Ink found -----------------------
DB_INK_FIXED:
	LD	A,(HL)			;  7:1
	AND	$07			;  7:2
	RLCA				;  4:1
	RLCA				;  4:1
	RLCA				;  4:1
	CP	C			;  4:1 Test Ink
	JR	z,DB_SCREEN_ADR		;12/7:2
	JR	DB_NEW_PAPER		; 12:2

; 1. Paper undefined + Ink undefined -------------------
DB_FIRST_SEARCH:
	LD	A,C			;  4:1
	OR	A			;  4:1 Black?
	JR	z,DB_NEW_PAPER		;12/7:2
	
; Pokus o kontinuitu v barvach mezi 8x8 maticemi. Porovname s levou matici, a pokud to nejde tak s horni.
	PUSH	HL			; 11:1
	LD	A,L			;  4:1
	AND	$1F			;  7:2
	JR	nz,DB_DALSI_SLOUPEC	;12/7:2
	ADD	A,$21			;  7:2
	LD	L,A			;  4:1
DB_DALSI_SLOUPEC:
	DEC	HL			;  4:1
; test Paper
	LD	A,(HL)			;  7:1 Sousedni atribut
	AND	$38			;  7:2 Sousedni Paper
	CP	C			;  4:1
	POP	HL			; 10:1
	JR	nz,DB_NEW_INK		;12/7:2
	
DB_NEW_PAPER:
	LD	A,(HL)			;  7:1
	OR	C			;  4:1
	AND	%01111111		;  7:2 Flash clear
	LD	(HL),A			;  4:1
; 	Propadne na DRAW_BIT_EXIT 

; 2. Paper found + Ink found / undefined ---------------
DB_COLORS_FIXED:
	LD	A,(HL)			;  7:1
	AND	$38			;  7:2
	CP	C			;  4:1 Test Paper
	JR	z,DB_EXIT		;12/7:2

DB_NEW_INK:
	LD	A,C			;  4:1
	RRCA				;  4:1
	RRCA				;  4:1
	RRCA				;  4:1 .0...PPP
	XOR	(HL)			;  7:1
	AND	%01000111		;  7:2
	XOR	(HL)			;  7:1 ?0???GRB, clear Bright
	LD	(HL),A			;  7:1

; ------------------------------------------------------
DB_SCREEN_ADR:
 
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
DB_LOOP:
	RRCA				;  4:1 rotace doprava
	DJNZ	DB_LOOP			;13/8:2
	OR	(HL)			;  7:1
	LD	(HL),A			;  7:1
	
DB_EXIT:
	EXX				;  4:1
	RET				; 10:1
