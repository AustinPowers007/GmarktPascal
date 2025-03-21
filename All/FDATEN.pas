{============================
 Dateiname:   $Source: $
 Version:     $Revision: $
 Autor:       $Author: $
 Gelockt von: $Locker: $
 Originaldatum : Fri Oct 11 15:41:36 GMT+02:00 1996
 Dateihistorie am Ende der Datei
 ============================}
UNIT KUNDATEN;
{$N+}

INTERFACE

   USES ZUSAETZE,
        CASHDATA;

   CONST Anr : ARRAY [1..3] OF STRING[10] = ('Herr',
                                             'Frau',
                                             'Firma');

   TYPE TOPosten     = RECORD
                          Art   : CHAR;
                          Betrag: REAL;
                          LiefNr: STRING[5];
                          Bemerk: STRING[40];
                       END;
   TYPE TStdBest     = RECORD
                          Menge       : LONGINT;
                          ArtNr       : STRING[5];
                          ArtBez      : STRING[40];
                          Preis       : REAL;
                          Uebernehmen : BOOLEAN;
                       END;
   TYPE TKundenDaten = RECORD
                          PlanQ       : STRING[5];
                          PlanQFalk   : STRING[10];
                          LfdNr       : LONGINT;
                          LieferANr   : BYTE;
                          Firma       : BOOLEAN;
                          RechName    : STRING[40];
                          RechVorname     : STRING[20];
                          RechAnrede      : WORD;
                          RechTitel       : STRING[20];
                          RechStrasse     : STRING[25];
                          RechPLZ         : LONGINT;
                          RechOrt         : STRING[25];
                          RechAPartner: STRING[20];
                          RechAPAnrede : WORD;
                          RechAPTitel  : STRING[20];
                          LiefSZeit: BOOLEAN;
                          LiefName :STRING[40];
                          LiefVorname : STRING[20];
                          LiefAnrede  : WORD;
                          LiefTitel   : STRING[20];
                          LiefStrasse : STRING[25];
                          LiefPlz     : LONGINT;
                          LiefOrt     : STRING[25];
                          LiefAPartner: STRING[20];
                          LiefAPAnrede: WORD;
                          LiefAPTitel : STRING[20];
                          Etage       : STRING[2];
                          Aufzug      : BOOLEAN;

                          TelefonBem  : ARRAY[1..3] OF STRING[20];
                          Telefon     : ARRAY[1..3] OF STRING[15];
                          StdVon      : TZeit;
                          StdBis      : TZeit;
                          STDBest     : ARRAY[1..5] OF TStdBest;
                          Zahlweise : WORD;
                          LiefOk,
                          LiefNOk,
                          LiefEigen,
                          Angespr     : LONGINT;
                          EinzahlDat,
                          AuszahlDat  : TDatum;
                          Betrag      : REAL;
                          OPosten     : ARRAY[1..10] OF TOPosten;
                          AnlDatum    : TDatum;
                          AnlUhrzeit  : STRING[10];
                          Bemerkung   : ARRAY[1..2] OF STRING[40];
                          KWerbKz     : STRING[2];
                          KWerbText   : STRING[40];
                          GInfo       : ARRAY[1..4] OF STRING[3];
                          WegBes      : ARRAY[1..8] OF STRING[40];
      END;

   PROCEDURE InitTKundenDaten(VAR Daten:TKundenDaten);
   FUNCTION MakeLfdNr(LfdNr:LONGINT;LiefANr:BYTE):STRING;
   FUNCTION MakeFPIndex(Name:STRING;Firma:BOOLEAN):STRING;

IMPLEMENTATION

   CONST FSymbol = '#';
         PSymbol = '*';

   PROCEDURE InitTKundenDaten(VAR Daten:TKundenDaten);
      VAR Lauf : BYTE;

      BEGIN
         WITH Daten DO BEGIN
            PlanQ := '';PlanQFalk:='';LfdNr:=0;LieferANr :=0;
            Firma:=FALSE;
            RechStrasse:='';RechPLZ:=0;RechOrt:='';
            RechAnrede:=1;RechName:='';RechVorname:='';
            RechTitel := '';
            RechAPAnrede:=1;RechAPartner:='';RechAPTitel:='';
            Aufzug := FALSE;
            LiefSZeit := FALSE;
            LiefAnrede:=1;LiefName := '';LiefVorname := '';
            LiefStrasse:= '';LiefPlz:=0;LiefOrt :='';
            LiefTitel := '';
            LiefAPAnrede:= 1;LiefAPartner := '';LiefAPTitel:='';
            Etage:= '';
            StdVon := '';
            STdBis := '';
            Zahlweise:=0;AnlDatum:='';AnlUhrzeit:='';
            LiefOk := 0;LiefNOk := 0;LiefEigen := 0;
            Angespr := 0;
            FOR Lauf := 1 TO 3 DO BEGIN
               Telefonbem[Lauf] := '';
               Telefon[Lauf] := '';
            END;
            FOR Lauf := 1 TO 2 DO BEGIN
               Bemerkung[Lauf] := '';
            END;
            FOR Lauf := 1 TO 8 DO
               WegBes[Lauf] := '';
            FOR Lauf := 1 TO 5 DO
               WITH StdBest[Lauf] DO BEGIN
                   Menge:=0;
                   ArtNr := '';
                   ArtBez :='';
                   Preis  :=0.0;
                   Uebernehmen := FALSE;
               END;
            EinzahlDat := '';
            AuszahlDat := '';
            Betrag:=0;
            FOR Lauf := 1 TO 10 DO
               WITH OPosten[Lauf] DO BEGIN
                   Art := #0;
                   Betrag := 0;
                   LiefNr := '';
                   Bemerk := '';
               END;
            KWerbKz := '';
            KWerbText :='';
            FOR Lauf := 1 TO 4 DO
               GInfo[Lauf] := '';
         END;
      END;


   FUNCTION MakeFPIndex;
      BEGIN
         IF Firma THEN
            MakeFPIndex := FSymbol+Name
         ELSE
            MakeFPIndex := PSymbol+Name;
      END;

   FUNCTION MakeLfdNr;
      BEGIN
         MakeLfdNr := LeadingZeros(LfdNr*10+LiefANr,6);
      END;


BEGIN
END.
{============================
 Versionshistorie
 $Log:$
 ============================}
