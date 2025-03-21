{============================
 Dateiname:   $Source: $
 Version:     $Revision: $
 Autor:       $Author: $
 Gelockt von: $Locker: $
 Originaldatum : Sat Jan 08 16:49:58 GMT+01:00 2000
 Dateihistorie am Ende der Datei
 ============================}
UNIT GMDATEI;

INTERFACE
   USES KUNFILE,KUNDATEN,
        ARTDATEN,ARTFILE,
        LIEDATEN,LIEFFILE,
        KBDATEN,KBFILE,
        LOGFILE;

   VAR  Kunde       : TKundenDaten;
        Lieferung   : TLiefDaten;
        KBEintrag   : TKBDatenPtr;
   VAR  KUNDDB   : TKundenFile;
        ARTDB    : TArtFile;
        LieferDB : TLiefFile;
        KassenDB : TKBFilePtr;
        OPLogFile: TLogFilePtr;

PROCEDURE DisposeGMDateiPtrs;
FUNCTION InitKassenDB(DateiName:STRING;Verzeichnis:STRING;
                    MultipleFiles:BOOLEAN;Datum:STRING):BOOLEAN;
IMPLEMENTATION
FUNCTION InitKassenDB(DateiName:STRING;Verzeichnis:STRING;
                    MultipleFiles:BOOLEAN;Datum:STRING):BOOLEAN;
  VAR Ergebnis : BOOLEAN;
  BEGIN
    Ergebnis := FALSE;
    IF KassenDB = NIL THEN BEGIN
      NEW(KassenDB,OpenFile(Dateiname,Verzeichnis,MultipleFiles,Datum));
      Ergebnis := TRUE
    END;
    InitKassenDB := Ergebnis
  END;
PROCEDURE DisposeGMDateiPtrs;
  BEGIN
    DISPOSE(OPLogFile,Done);
    IF KassenDB <> NIL THEN
       DISPOSE(KassenDB);
    DISPOSE(KBEintrag);
  END;

BEGIN
   KassenDB := NIL;
   NEW(OPLogFile,Init);
   NEW(KBEintrag);
END.
{============================
 Versionshistorie
 $Log:$
 ============================}
