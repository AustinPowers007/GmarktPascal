{============================
 Dateiname:   $Source: $
 Version:     $Revision: $
 Autor:       $Author: $
 Gelockt von: $Locker: $
 Originaldatum : Tue Feb 08 19:39:42 GMT+01:00 2000
 Dateihistorie am Ende der Datei
 ============================}
UNIT ARTDATEN;

INTERFACE

  TYPE TArtDaten = RECORD
  {*} ArtNr       : STRING[5];                 { Artikelnummer GS              }
  {*} ArtBez      : STRING[40];                { Bezeichnung       "                 }
  {2} EK_Preis    : ARRAY[1..5] OF REAL;       { (1) Flaschen EK (2) Kiste Schneider }
  {1} HNr         : ARRAY[1..5] OF STRING[10]; { Artikelnr Gro·hÑndler (1) Schneider }
  {*} Typ         : LONGINT;                   { Anrechnung fÅr Fahrer               }
  {*} PreisKa,                                 { VK Kasten                           }
  {*} PreisFl,                                 { Gutschrift bei fehlender Flasche    }
      PreisKaL,                                { Ladenpreis }
  {*} Marge,                                   { Marge pro Kiste                     }
  {*} PfandRah,                                { Pfand Rahmen  }
  {*} PfandGes,                                { Pfand Gesamt  }
  {*} PfandFla,                                { Pfand Flasche }
  {*} AnzahlFl    : REAL;                      { Anzahl Flaschen }
      AnzahlFPauschalen : LONGINT;             { Anzahl der Fahrerpauschalen         }
      Angebot     : BOOLEAN;
  END;

  PROCEDURE InitTArtDaten(VAR Daten:TArtDaten);
  FUNCTION  MakeArtNr(Nr:LONGINT):STRING;
  FUNCTION  NextArtNr(Nr:STRING):STRING;

IMPLEMENTATION

  USES ZUSAETZE;

  PROCEDURE InitTArtDaten;
    BEGIN
      WITH Daten DO BEGIN
        Ek_Preis[1]:= 0;Ek_Preis[2]:= 0;Ek_Preis[3]:= 0;
        Ek_Preis[4]:= 0;Ek_Preis[5]:= 0;
        HNr[1] := '';HNr[2] := '';HNr[3] := '';HNr[4] := '';
        HNr[5] := '';
        ArtNr := '';ArtBez := '';PreisKA := 0;PreisFl := 0;
        PreisKaL := 0;Marge := 0;PfandRah := 0;PfandGes := 0;
        PfandFla := 0;AnzahlFl := 0;Angebot := FALSE;
        Typ := 0; AnzahlFPauschalen:=0;
      END;
    END;

  FUNCTION MakeArtNr;
  BEGIN
     MakeArtNr := LeadingZeros(Nr,5);
  END;

  FUNCTION NextArtNr;
  BEGIN
    NextArtNr := LeadingZeros((S2L(Nr)+1),5)
  END;
END.
{============================
 Versionshistorie
 $Log:$
 ============================}
