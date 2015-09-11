# GIF-Reader
GIF Reader displays GIF images on the ZX Spectrum

Convert 256 x 192 GIF to SCR

Support for calling directly from Basic:

    PRINT USR progStart, ADR_GIF

515 bytes code + 8 KB LZW dictionary ( 4096 * 2 bytes )

    HI       LO
    GRBiiiii iiiiiiiS = S = stop bit, nastaven u indexu mensich jak CLEARCODE
    +
    AAA..... ........ = segment adresy musi lezet na adrese delitelne 8 KB
    =
    AAAiiiii iiiiiii0 = adresa predchozi polozky slovniku

Not used Bright. Ignores color collision.

Simple rules to minimize the inverted matrix of 8x8 pixels. The effort to maintain the continuity of values Paper and Ink.

Support error codes:

    ERR_OK		    	EQU	0
    ERR_GIF_NOT_FOUND	EQU	1	; != "G"
    ERR_UNKNOWN_FRAME	EQU	2	; Plain Text Extension Block, Application Frame
    ERR_OWERFLOW_SCREEN	EQU	3	; Pixelu je vice jak 192*256

Compile:

    pasmo -d gif-reader.asm gif-reader.bin > test.asm
