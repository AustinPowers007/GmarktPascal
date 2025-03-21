{============================
 Dateiname:   $Source: $
 Version:     $Revision: $
 Autor:       $Author: $
 Gelockt von: $Locker: $
 Originaldatum : Sun Oct 14 17:47:26 GMT+02:00 2001
 Dateihistorie am Ende der Datei
 ============================}
UNIT GMConst;
{$O+}
INTERFACE

Type TPrintStyles = (Bold,
                     NoBold,
                     ITalic,
                     NoItalic,
                     ULine,
                     NoUline,
                     BoldItalic,
                     NoBoldItalic,
                     ShiftIn,
                     ShiftOut);
TYPE TPrintStylesArray = array[Bold..ShiftOut] OF PChar;

TYPE TPCharArray = array[0..16380] of PChar;
     PPCharArray = ^TPCharArray;

     PPrinterCodes = ^TPrinterCodes;

     TPrinterCodes = RECORD
      { Number of characters per Line in Condensed an normal mode }
      LineLengthCond,
      LineLengthNorm,
      { Number of preamble strings in the Preamble array. }
      PreambleCount: Byte;
         { Pointer to an array of PChars that define the preamble sequence for
          this printer. Sent at the start of a print job. }
      Preamble: PPCharArray;
        { Pointer to an array of PChars that define the code sequences for
          changing the current attribute. }
      CodeArray: PPCharArray;
      { Codes sent at the start of a page. }
      StartPage: PChar;
        { Codes sent at the end of a page. }
      EndPage: PChar;
        { Codes sent at the end of a line. }
      EndLine: PChar;
        { Codes sent at the end of the print job. }
      Postamble  : PChar;
      PageLength : BYTE;
   END;


CONST ZahlWeiseTypen = 9;
CONST ZahlWeiseText : ARRAY [0..ZahlWeiseTypen] OF STRING =
       ('Neukunde!  Ware nur gegen Barzahlung!',
        '',
        'šberweisungskunde! Rechnung hinterlegen!',
        'šberweisungskunde! Detaillierten Rechnungsausdruck hinterlegen!',
        'šberweisungskunde! Lieferschein hinterlegen!',
        'šberweisungskunde! Detaillierten Lieferschein hinterlegen!',
        'Monatskunde! Rechnung hinterlegen!',
        'Monatskunde! Detaillierten Rechnungsausdruck hinterlegen!',
        'Ware nur gegen Barzahlung',
        'Aktionskunde! Ware nur gegen Barzahlung');



VAR PrinterCodes : TPrinterCodes;
    PrintStyles  : TPrintStylesArray;

PROCEDURE InitPrintCodes(PrinterType:LONGINT);

IMPLEMENTATION

CONST

  { EPSON Printer code definition }

  EpsonItalic   = #27'4';
  EpsonNoItalic = #27'5';
  EpsonBold     = #27'E';
  EpsonNoBold   = #27'F';
  EpsonULine    = #27'-'#1;
  EpsonNoULine  = #27'-0';
  EpsonShiftIn  = #15;
  EpsonShiftOut  = #18;

  EpsonCodeArray: array[0..9] of PChar = (
    EpsonBold,
    EpsonNoBold,
    EpsonItalic,
    EpsonNoItalic,
    EpsonULine,
    EpsonNoULine,
    EpsonBold + EpsonItalic,
    EpsonNoBold + EpsonNoItalic,
	 EpsonShiftIn,
	 EpsonShiftOut);

  EpsonCodes: TPrinterCodes = (
    LineLengthCond : 120;
    LineLengthNorm : 79;
    PreambleCount: 0;
    Preamble: nil;
    CodeArray: @EpsonCodeArray;
    StartPage: '';
    EndPage: #12;
    EndLine: #13#10;
    Postamble: '';
    PageLength : 62
  );

  { HP LaserJet code definition }

  HPInit      = #27'E'#27'(10U'#27'&k0S'#27'(s3T';
  HPItalic    = #27'(s1S';
  HPNoItalic  = #27'(s0S';
  HPBold      = #27'(s3B';
  HPNoBold    = #27'(s0B';
  HPULine     = #27'&dD';
  HPNoULine   = #27'&d@';
  HPShiftIn   = #27'(s16.67h12V';
  HPShiftOut  = #27'(s10.00h12V';

  HPCodeArray: array[0..9] of PChar = (
    HPBold,
    HPNoBold,
    HPItalic,
    HPNoItalic,
    HPULine,
    HPNoULine,
    HPBold + HPItalic,
    HPNoBold + HPNoItalic,
	 HPShiftIn,
    HPShiftOut);

  LaserJetPreamble: PChar = HPInit;
  LaserJetCodes: TPrinterCodes = (
    LineLengthCond : 130;
    LineLengthNorm : 78;
    PreambleCount: 1;
    Preamble: @LaserJetPreamble;
    CodeArray: @HPCodeArray;
    StartPage: '';
    EndPage: #12;
    EndLine: #13#10;
    Postamble: #12;
    PageLength : 60
  );

  { Raw ASCII definition }

  AsciiCodes: TPrinterCodes = (
    LineLengthCond : 120;
    LineLengthNorm : 79;
    PreambleCount: 0;
    Preamble: nil;
    CodeArray: nil;
    StartPage: '';
    EndPage: #12;
    EndLine: #13#10;
    Postamble: '';
    PageLength : 70
  );

   PROCEDURE InitPrintCodes(PrinterType:LONGINT);
      VAR   StylesLauf  : TPrintStyles;
      VAR   Lauf :BYTE;
      BEGIN
         CASE PrinterType OF
            1    : PrinterCodes := EpsonCodes;
            2    : PrinterCodes := LaserJetCodes;
            3    : PrinterCodes := AsciiCodes;
            ELSE   PrinterCodes := AsciiCodes;
         END;
         Lauf := 0;
         IF (PrinterCodes.CodeArray <> NIL) THEN BEGIN
            FOR StylesLauf := Bold TO ShiftOut DO BEGIN
               PrintStyles[StylesLauf] :=PrinterCodes.CodeArray^[Lauf];
               INC(Lauf);
            END;
         END;
      END;

END.
{============================
 Versionshistorie
 $Log:$
 ============================}
