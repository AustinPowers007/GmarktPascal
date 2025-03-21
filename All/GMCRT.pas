{============================
 Dateiname:   $Source: $
 Version:     $Revision: $
 Autor:       $Author: $
 Gelockt von: $Locker: $
 Originaldatum : Fri Jan 07 17:59:52 GMT+01:00 2000
 Dateihistorie am Ende der Datei
 ============================}

{*******************************************************}
{                                                       }
{       Turbo Pascal Runtime Library                    }
{       GMCRT Interface Unit                            }
{                                                       }
{       Copyright (C) 1988,92 Borland International     }
{  !!!!! Achtung angepasste Version ohne nur READKEY und 
{        Keypress enthalten          }
{  und Initialize !!!!!                                 }
{*******************************************************}

unit GMCrt;

{$I-,S-}

interface

const

{ CRT modes }

  BW40          = 0;            { 40x25 B/W on Color Adapter }
  CO40          = 1;            { 40x25 Color on Color Adapter }
  BW80          = 2;            { 80x25 B/W on Color Adapter }
  CO80          = 3;            { 80x25 Color on Color Adapter }
  Mono          = 7;            { 80x25 on Monochrome Adapter }
  Font8x8       = 256;          { Add-in for ROM font }

{ Mode constants for 3.0 compatibility }

  C40           = CO40;
  C80           = CO80;

{ Foreground and background color constants }

  Black         = 0;
  Blue          = 1;
  Green         = 2;
  Cyan          = 3;
  Red           = 4;
  Magenta       = 5;
  Brown         = 6;
  LightGray     = 7;

{ Foreground color constants }

  DarkGray      = 8;
  LightBlue     = 9;
  LightGreen    = 10;
  LightCyan     = 11;
  LightRed      = 12;
  LightMagenta  = 13;
  Yellow        = 14;
  White         = 15;

{ Add-in for blinking }

  Blink         = 128;

var

{ Interface variables }

  CheckBreak: Boolean;    { Enable Ctrl-Break }
  CheckEOF: Boolean;      { Enable Ctrl-Z }
  DirectVideo: Boolean;   { Enable direct video addressing }
  CheckSnow: Boolean;     { Enable snow filtering }
  LastMode: Word;         { Current text mode }
  TextAttr: Byte;         { Current text attribute }
  WindMin: Word;          { Window upper left coordinates }
  WindMax: Word;          { Window lower right coordinates }

{ Interface procedures }

function KeyPressed: Boolean;
function ReadKey: Char;

implementation

VAR Scancode : BYTE;

function KeyPressed; ASSEMBLER;
ASM
	CMP	ScanCode,0
	JNE	@@1
	MOV	AH,1
	INT	16H
	MOV	AL,0
	JE	@@2
@@1:	MOV	AL,1
@@2:	
END;

function ReadKey;ASSEMBLER;
{; Read character from keyboard}
{; Out	AL = Character}
ASM
	MOV	AL,ScanCode
	MOV	ScanCode,0
	OR	AL,AL
	JNE	@@1
	XOR	AH,AH
	INT	16H
	OR	AL,AL
	JNE	@@1
	MOV	ScanCode,AH
	OR	AH,AH
	JNE	@@1
	MOV	AL,'C'-64
@@1:	
END;


begin
end.
{============================
 Versionshistorie
 $Log:$
 ============================}
