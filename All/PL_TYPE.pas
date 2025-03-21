{============================
 Dateiname:   $Source: $
 Version:     $Revision: $
 Autor:       $Author: $
 Gelockt von: $Locker: $
 Originaldatum : Fri Jan 07 18:04:36 GMT+01:00 2000
 Dateihistorie am Ende der Datei
 ============================}
   { Units zum Lesen der GMARKT-Daten }

   USES  PL_CONST,
         GMCONST,
         ZUSAETZE,
         PLACEWIN,
         CASHDATA,
         CASHNEU,
         GMDATEI,
         ASTDATA,
         BUTTONS,
         GMSCROLL,
         LIEDATEN,
         LIEFFILE,
         KUNDATEN,
         KUNFILE,
         ARTDATEN,
         ARTFILE,
         GMPLDO,

   { Units fÅr Planung }

         GMCRT,
         GRAPH,
         PRINTER,
         GRAPNEU,
         FENSTER,
         CASHPROC,
         MAUSUNIT;

   VAR   PF:   FILE OF PlanungsRec;
         PR:   PlanungsRec;

         Fahrer:        ARRAY[1..15] OF STRING[30];
         LaufKunde,
         LaufKiste:     ARRAY[1..15] OF INTEGER;
         LeerKunde,
         LeerKiste:     ARRAY[1.. 3] OF INTEGER;

         HighX,LowX,
         HighY,LowY,
         MHighX,MLowX,
         MHighY,MLowY,
         DiffX,
         DiffY,

         Th,Tm,

         GesKistCount,
         GesKundCount:INTEGER;
         THighX,TLowX,
         THighY,TLowY:LONGINT;
         MinKun:       ARRAY[1..15] OF INTEGER;

         TourStart:    ARRAY[1..15] OF Planungszeiger;
         TourAktPs,
         TimeStart,
         First,
         Merke,
         AktPr:        PlanungsZeiger;

         Changes:      BOOLEAN;
         Feld:         ARRAY[1..30,1..30,1..4] OF BYTE;
         StartT:       ARRAY[1..15] OF STRING[5];
         IsPQ,
         TimeS,TimeSOld,
         TimeE,TimeEOld: STRING[5];
         Ths,
         Tms:          STRING[2];
         TimeIs:       STRING;
         TimeNrS,
         TimeNrE:      BYTE;

         UebDatumFenster: TASFenster;
         UebDatum:        TDatum;
         Lieferung:       TLiefDaten;
         LieferDB:        TLiefFile;
         Anz:             LONGINT;
         KundDB:          TKundenFile;
         ArtDB:           TArtFile;
         Kunde:           TKundenDaten;

         DLSchluessel : TDatum;

         AktFahrer : STRING;
         AktJear   : STRING;

         KSchluessel1,
         KSchluessel2,
         LSchluessel1,
         LSchluessel2 :STRING;

         Taste : CHAR;
         TNr,
         laktTourKey,
         T100Tag,
         T100Tour,
         Auswahl : BYTE;
         EndCode : INTEGER;
         Ende : BOOLEAN;
         LAktTNr : WORD;

         NA,FA,
         KLfd,
         KLfd1,Klfd2 : STRING;
         KlfdNo,
         Hilfszahl :   LONGINT;
         PQ:           ARRAY[72..97,73..90,1..4] OF BYTE;
         PQSum:        ARRAY[0..15,1..2] OF BYTE;
         PQAlle:       WORD;

         TDiffX,
         TDiffY:       INTEGER;
         XLen,
         YLen:         INTEGER;

         FSuchFenster: TSearchFenster;
{============================
 Versionshistorie
 $Log:$
 ============================}
