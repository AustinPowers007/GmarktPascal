{============================
 Dateiname:   $Source: $
 Version:     $Revision: $
 Autor:       $Author: $
 Gelockt von: $Locker: $
 Originaldatum : Fri Jan 03 12:04:20 GMT+01:00 1997
 Dateihistorie am Ende der Datei
 ============================}
UNIT CopyUnit;

   INTERFACE

      USES DOS;

      FUNCTION CreateVerz(Verzeichnis:STRING):BYTE;
      FUNCTION CopyDatei(Source,Destination:STRING):BYTE;

   IMPLEMENTATION

      FUNCTION CreateVerz;
         BEGIN
           {$I-} MKDIR(Verzeichnis); {$I+}
           CreateVerz:=IORESULT
         END;

      FUNCTION CopyDatei;
         VAR F:TEXT;
         BEGIN
           ASSIGN(F,Destination);
           {$I-} ERASE(F); {$I+}
           IF (IORESULT <> 0) THEN ;
           ASSIGN(F,Source);
           {$I-} RENAME(F,Destination); {$I+}
           CopyDatei:=IORESULT
         END;

   END.
{============================
 Versionshistorie
 $Log:$
 ============================}
