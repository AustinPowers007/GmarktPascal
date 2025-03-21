{============================
 Dateiname:   $Source: $
 Version:     $Revision: $
 Autor:       $Author: $
 Gelockt von: $Locker: $
 Originaldatum : Fri Oct 11 15:41:36 GMT+02:00 1996
 Dateihistorie am Ende der Datei
 ============================}
UNIT ARTFILE;

INTERFACE

  USES ARTDATEN,
       DOS;

  TYPE TFArtDaten = RECORD
     RecNum    : LONGINT;
     Vorg,
     Nachf     : ARRAY[1..2] OF LONGINT;
     Index     : ARRAY[1..2] OF STRING[40];
     Artikel   : TArtDaten;
  END;

  TYPE FArtStamm = FILE OF TFArtDaten;
  TYPE FArtRecs = FILE OF LONGINT;

  TYPE TArtFilePtr = ^TArtFile;
       TArtFile = OBJECT
      DBVerzeichnis: STRING;
      FDatei       : FArtStamm;
      FRecFrei     : FArtRecs;
      Geoeffnet    : BOOLEAN;
      Rec          : TFArtDaten;

      CONSTRUCTOR OpenFile(DateiName:STRING;Verzeichnis:STRING);
      FUNCTION ReadRec(Pos:LONGINT):BOOLEAN;
      FUNCTION WriteRec:BOOLEAN;
      FUNCTION GetTop(Kennung:BYTE;VAR Daten:TArtdaten):BOOLEAN;
      FUNCTION GetNext(Kennung:BYTE;VAR Daten:TArtdaten):BOOLEAN;
      FUNCTION NewRec(Daten:TArtDaten):BOOLEAN;
      FUNCTION GetRec(Kennung:BYTE;Schluessel:STRING;VAR Daten:TArtDaten):BOOLEAN;
      FUNCTION ChangeRec(Daten:TArtDaten):BOOLEAN;
      FUNCTION SeekRec(Kennung:BYTE;Schluessel:STRING):BOOLEAN;
      FUNCTION DelRec(Schluessel:STRING):BOOLEAN;
      FUNCTION CloseFile:BOOLEAN;
      FUNCTION GetLfdANr:STRING;
      PROCEDURE SetLfdANr(Nr :STRING);
    END;



IMPLEMENTATION
    USES ZUSAETZE;
    CONST DefaultArtDBName = 'ARTDB'; {Wird verwendet wenn kein
                                       Name bergeben wird}

	CONSTRUCTOR TArtFile.OpenFile;
	  VAR FileInfo : SearchRec;
	      Frei     : STRING;
         Lauf     : BYTE;
         D        : BOOLEAN;

	  BEGIN
               DBVerzeichnis := MakeFilePath(Verzeichnis);
               Dateiname := CutSuffix(Dateiname);
               IF (DateiName = '') THEN
                  DateiName := DefaultArtDBName;
	       Geoeffnet := FALSE;
       Frei :=DBVerzeichnis+Dateiname+'.FRE';
       DateiName:=DBVerzeichnis+Dateiname+'.DAT';
       ASSIGN(FDatei,Dateiname);
       ASSIGN(FRecFrei,Frei);
       IF NOT SeekFile(Dateiname) THEN BEGIN
         {$I-}
           REWRITE(FDatei);
           REWRITE(FRecFrei);
         {$I+}
         IF IORESULT = 0 THEN BEGIN
           Rec.RecNum := 0;
           InitTArtDaten(Rec.Artikel);
           Rec.Artikel.ArtNr := '';
           FOR Lauf := 1 TO 2 DO BEGIN
             Rec.Index[Lauf] :='';
             Rec.Vorg [Lauf] := 0;
             Rec.Nachf[Lauf] := -1;
           END;
           {$I-}
             WRITE(FDatei,Rec);
           {$I+}
           Geoeffnet := (IORESULT = 0);
         END;
       END ELSE BEGIN
         {$I-}
           RESET(FDatei);
          FINDFirst(Frei,AnyFile,FileInfo);
          if doserror = 0 THEN
            RESET(FRecFrei)
          ELSE
            rewrite(frecfrei);
           D:=ReadRec(0);
        {$I+}
        Geoeffnet := (IORESULT = 0 );
      END;
    END;

   FUNCTION TArtFile.GetLfdANr;
     VAR Dateizugriff : BOOLEAN;

     BEGIN
       Dateizugriff := ReadRec(0);
       GetLfdANr := Rec.Artikel.ArtNr;
     END;

   PROCEDURE TArtFile.SetLfdANr;
     VAR Dateizugriff : BOOLEAN;

     BEGIN
       Dateizugriff := ReadRec(0);
       Rec.Artikel.ArtNr := Nr;
       Dateizugriff := WriteRec;
     END;


	FUNCTION TArtFile.ReadRec;

	  BEGIN
		 {$I-}
         SEEK(FDatei,Pos);
         READ(FDatei,Rec);
         SEEK(FDatei,Pos);
		 {$I+}
		 ReadRec := (IORESULT = 0);
	  END;

	FUNCTION TArtFile.WriteRec;

	  BEGIN
		 {$I-}
         SEEK(Fdatei,Rec.RecNum);
         WRITE(FDatei,Rec);
         SEEK(Fdatei,Rec.RecNum);
		 {$I+}
		 WriteRec:=(IORESULT = 0);
	  END;

  FUNCTION TArtFile.NewRec;
    VAR NRec         : TFArtDaten;
        Neu          : LONGINT;
        Dahinter,
        Davor        : ARRAY[1..2] OF LONGINT;
        Schliessen,
        Dateizugriff,
        Seekzugriff  : BOOLEAN;
        Lauf         : BYTE;

    BEGIN
      NewRec := FALSE;
      IF Geoeffnet THEN BEGIN
        IF (NOT SeekRec(1,Daten.ArtNr)) THEN BEGIN
          Schliessen := TRUE;
          IF FILESIZE(FRecFrei) > 0 THEN BEGIN
            {$I-};
              SEEK(FRecFrei,FILESIZE(FRecFrei)-1);
              READ(FRecFrei,Neu);
              SEEK(FRecFrei,FILESIZE(FRecFrei)-1);
              TRUNCATE(FRecFrei);
            {$I+}
            IF IORESULT <> 0 THEN
              Neu := FILESIZE(FDatei)
            ELSE
              Schliessen := FALSE;
          END ELSE
            Neu := FILESIZE(FDatei);
          NRec.RecNum   := Neu;
 	       NRec.Index[1] := UPPER(Daten.ArtNr);
          NRec.Index[2] := UPPER(Daten.ArtBez);
          NRec.Artikel  := Daten;
          Dateizugriff := TRUE;

          FOR Lauf := 1 TO 2 DO BEGIN
            SeekZugriff := SeekRec(Lauf,NRec.Index[Lauf]);
            Davor     [Lauf] := Rec.RecNum;
            Dahinter  [Lauf] := Rec.Nachf[Lauf];
            NRec.Vorg [Lauf] := Rec.RecNum;
            NRec.Nachf[Lauf] := Rec.Nachf[Lauf];
          END;
          Rec := NRec;
          IF WriteRec THEN BEGIN
            FOR Lauf := 1 TO 2 DO BEGIN
              IF ReadRec(Davor[Lauf]) THEN BEGIN
                Rec.Nachf[Lauf]:=Neu;
                Dateizugriff:= Dateizugriff AND WriteRec;
              END;
              IF Dahinter[Lauf] <> -1 THEN
                IF ReadRec(Dahinter[Lauf]) THEN BEGIN
                  Rec.Vorg[Lauf] := Neu;
                  Dateizugriff := Dateizugriff AND WriteRec;
                END;
            END;
            IF Schliessen THEN BEGIN
             {$I-}
              Close(FDatei);
              RESET(FDatei);
             {$I+}
            END;
            NewRec := Dateizugriff AND ReadRec(NRec.RecNum);
          END;
        END;
      END;
    END;

  FUNCTION TArtFile.GetTop;
    VAR Dateizugriff : BOOLEAN;

    BEGIN
      Dateizugriff:=ReadRec(0);
      IF (Rec.Nachf[Kennung]<>-1 ) AND Dateizugriff THEN BEGIN
        GetTop := ReadRec(Rec.Nachf[Kennung]);
        Daten := Rec.Artikel;
      END ELSE
        GetTop := FALSE;
    END;

  FUNCTION TArtFile.GetNext;

    BEGIN
      IF Rec.Nachf[Kennung]<>-1 THEN BEGIN
        GetNext:=ReadRec(Rec.Nachf[Kennung]);
        Daten := Rec.Artikel;
      END ELSE
        GetNext := FALSE;
    END;

  FUNCTION TArtFile.GetRec;

    BEGIN
      GetRec := FALSE;
      IF SeekRec(Kennung,Schluessel) THEN BEGIN
        Daten:=Rec.Artikel;
	GetRec := TRUE;
      END;
    END;

  FUNCTION TArtFile.ChangeRec;

    BEGIN
      ChangeRec:=FALSE;
      IF SeekRec(1,Daten.ArtNr) THEN BEGIN
        IF  DelRec(Daten.ArtNr) THEN
          ChangeRec:= NewRec(Daten)
      END;
    END;

  FUNCTION TArtFile.SeekRec;
    VAR Pos          : LONGINT;
	Dateizugriff : BOOLEAN;

    BEGIN
      SeekRec := FALSE;
      Schluessel := UPPER(Schluessel);
      IF Geoeffnet THEN BEGIN
        IF ( Rec.Index[Kennung] < Schluessel) OR
           ((Kennung=1) AND (Rec.Index[Kennung] <= Schluessel))  THEN
          Pos :=Rec.RecNum
        ELSE
          Pos := 0;
        REPEAT
          Dateizugriff := ReadRec(Pos);
          Pos:=Rec.Nachf[Kennung];
        UNTIL (Schluessel <= Rec.Index[Kennung]) OR
              (Pos = -1) OR (NOT Dateizugriff);
        IF Dateizugriff THEN BEGIN
          IF (Schluessel = Rec.Index[Kennung]) THEN BEGIN
            IF (Rec.RecNum <> 0) THEN
              SeekRec := TRUE;
          END ELSE
            IF (Schluessel < Rec.Index[Kennung]) THEN
              Dateizugriff := ReadRec(Rec.Vorg[Kennung]);
        END;
      END;
    END;

  FUNCTION TArtFile.DelRec;

    VAR LRec         : TFArtDaten;
        Recover,
        Dateizugriff : BOOLEAN;
        Lauf         : BYTE;

    BEGIN
      DelRec:=FALSE;
      IF SeekRec(1,Schluessel) THEN BEGIN
        LRec:=Rec;
        Dateizugriff := TRUE;
        FOR Lauf := 1 TO 2 DO BEGIN
          Dateizugriff:=Dateizugriff AND ReadRec(LRec.Vorg[Lauf]);
          Rec.Nachf[Lauf]:=LRec.Nachf[Lauf];
          Dateizugriff:=Dateizugriff AND WriteRec;
          IF LRec.Nachf[Lauf] <>-1 THEN BEGIN
            Dateizugriff := Dateizugriff AND ReadRec(LRec.Nachf[Lauf]);
            Rec.Vorg[Lauf]:=LRec.Vorg[Lauf];
            Dateizugriff := Dateizugriff AND WriteRec;
          END;
        END;
        IF Dateizugriff THEN BEGIN
          IF LRec.RecNum = FILESIZE(FDatei) - 1 THEN BEGIN
            {$I-}
              SEEK(FDatei,LRec.RecNum);
              TRUNCATE(FDatei);
              SEEK(FDatei,LRec.Vorg[1]);
            {$I+}
          END ELSE BEGIN
            {$I-}
              SEEK(FRecFrei,FILESIZE(FRecFrei));
              WRITE(FRecFrei,LRec.RecNum);
              SEEK(FDatei,LRec.Vorg[1]);
            {$I+}
          END;
        END ELSE BEGIN
          FOR Lauf := 1 TO 2 DO BEGIN
            Recover:=ReadRec(LRec.Vorg[Lauf]);
            Rec.Nachf[Lauf]:=LRec.RecNum;
            IF Recover THEN Recover := WriteRec;
            IF LRec.Nachf[Lauf] <>-1 THEN BEGIN
              Recover := ReadRec(LRec.Nachf[Lauf]);
              Rec.Vorg[Lauf]:=LRec.Recnum;
              IF Recover THEN Recover := WriteRec;
            END;
          END;
        END;
        DelRec:=Dateizugriff;
      END;
    END;


  FUNCTION TArtFile.CloseFile;

    BEGIN
      CloseFile := FALSE;
      IF Geoeffnet THEN BEGIN
        {$I-}
          Close(FDatei);
          Close(FRecFrei);
        {$I+}
        CloseFile:=(IORESULT = 0);
       END;
     END;

BEGIN
END.
{============================
 Versionshistorie
 $Log:$
 ============================}
