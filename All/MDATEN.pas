{============================
 Dateiname:   $Source: $
 Version:     $Revision: $
 Autor:       $Author: $
 Gelockt von: $Locker: $
 Originaldatum : Sun Oct 14 16:40:14 GMT+02:00 2001
 Dateihistorie am Ende der Datei
 ============================}
{ Fahrerdaten }

UNIT MDATEN;
{$N+}

INTERFACE

   USES  ZUSAETZE,
         CASHDATA;

   CONST Anr : ARRAY [1..2] OF STRING[4] = ('Herr','Frau');

   TYPE  WochenTag = (_Montag,_Dienstag,_Mittwoch,_Donnerstag,_Freitag,_Samstag);
   TYPE  Bereich   = (_Vormittag,_Nachmittag,_Abend);
   TYPE  Grenze    = (_Early,_Late);
   TYPE  GehaltArt = (_Kiste,_Sonder,_Stunde);

   TYPE  TTouren =      RECORD
                          Datum:   TDatum;
                          Kisten,
                          Sonder:  LONGINT;
                        END;

   TYPE  TSonder =      RECORD
                          Datum:   TDatum;
                          Stunden: REAL;
                        END;

   TYPE  TMitarbeiterDaten = RECORD
                               Anrede:         WORD;
                               Name,
                               Vorname:        STRING[20];
                               Plz:            LONGINT;
                               Strasse,
                               Ort:            STRING[25];
                               TelefonBem:     ARRAY[1..3] OF STRING[20];
                               Telefon:        ARRAY[1..3] OF STRING[15];
                               GebDat:         TDatum;
                               GebName:        STRING[20];
                               GebOrt:         STRING[25];

                               Regelzeiten:    ARRAY[Wochentag,Bereich,Grenze] OF TZeit;

                               BeginndB,
                               EndedB:         TDatum;

                               Gehalt:         ARRAY[Gehaltart] OF REAL;

                               Krankenkasse:   STRING[60];
                               SoziNr,
                               PANr:           STRING[40];
                               FuehrerscheinD: TDatum;
                               LStKarte,
                               LStKVorhanden,
                               Stundentaw:     BOOLEAN;

                               Touren:         ARRAY[1..50] OF TTouren;
                               Sonderarbeit:   ARRAY[1..50] OF TSonder;

                               Vorschuss:      ARRAY[1.. 5] OF REAL;
                               AuszahlungsGrenze:       REAL;
                             END;

   PROCEDURE InitTKundenDaten(VAR Daten:TMitarbeiterDaten);
   FUNCTION  MakeLfdNr(LfdNr:LONGINT;LiefANr:BYTE):STRING;
   FUNCTION  MakeFPIndex(Name:STRING;Firma:BOOLEAN):STRING;

IMPLEMENTATION

   CONST FSymbol = '#';
         PSymbol = '*';

   PROCEDURE InitTKundenDaten;
      VAR Lauf:  BYTE;
          Lauf1: WochenTag;
          Lauf2: Bereich;
          Lauf3: Grenze;
          Lauf4: GehaltArt;
      BEGIN
         WITH Daten DO BEGIN
           Anrede:=1;
           Name:='';
           Vorname:='';
           Plz:=0;
           Strasse:='';
           Ort:='';
           FOR Lauf:=1 TO 3 DO BEGIN
             TelefonBem[Lauf]:='';
             Telefon   [Lauf]:='';
           END;
           GebDat:='';
           GebName:='';
           GebOrt:='';
           FOR Lauf1 := _Montag TO _Samstag DO
             FOR Lauf2 := _Vormittag TO _Abend DO
               FOR Lauf3 := _Early TO _Late DO
                 Regelzeiten[Lauf1,Lauf2,Lauf3]:='';
           BeginndB:='';
           EndedB:='';
           FOR Lauf4 := _Kiste TO _Stunde DO
             Gehalt[Lauf4]:=0;

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
