{============================
 Dateiname:   $Source: $
 Version:     $Revision: $
 Autor:       $Author: $
 Gelockt von: $Locker: $
 Originaldatum : Sun Oct 14 17:47:46 GMT+02:00 2001
 Dateihistorie am Ende der Datei
 ============================}
UNIT GMSetup;
   {$N+}
INTERFACE
   USES  Environ;
   CONST IniFileName = 'GMARKT';
   VAR   GMEnvObj:TEnvObjPtr;
   FUNCTION GetGMKey(Nr:BYTE;VAR Key:TEnvIdent;VAR KeyType:BYTE):BOOLEAN;
   PROCEDURE InitGMEnvObj(VAR EnvObject: TEnvObj);
   PROCEDURE DisposeGMSetupPtrs;
IMPLEMENTATION
   CONST MaxGMKey = 49;
   CONST GMKeyWords:ARRAY[1..MaxGMKey] OF TEnvIdent =
         ('KUNDIR'    ,'ARTDIR'    ,'FAHRDIR'   ,'TOHANDYDIR','FRHANDYDIR',
          'LGUTDIR'   ,'LOGDIR'    ,'KBDIR'     ,'BASEDIR'   ,'LIEFDIR'   ,
          'LIEFDIR2'  ,'LIEFDIR3'  ,'LIEFDIR4'  ,'LIEFDIR5'  ,'LZMO'      ,
          'LZDI'      ,'LZMI'      ,'LZDO'      ,'LZFR'      ,'LZSA'      ,
          'TARIF1'    ,'TARIF2'    ,'TARIF3'    ,'B1TOUREN'  ,'B2TOUREN'  ,
          'B3TOUREN'  ,'B4TOUREN'  ,'B5TOUREN'  ,'B6TOUREN'  ,'MWST'      ,
          'APLATZ'    ,'PRINTER'   ,'KOPFZEILE1','KOPFZEILE2','FUSSZEILE1',
          'FUSSZEILE2','GSNAME'    ,'WARTEZEIT' ,'HORIZADRV','VERTIADRV',
          'APLDBNAME' ,'WAEHRUNG'  ,'PFAND'     ,'PFANDFLA', 'DRLOHNAUSZ',
          'KUNDDB'    ,'ARTDB'     ,'PFANDRAH'  ,'STUNDLOHN');
   CONST GMKType:ARRAY[1..MaxGMKey] OF BYTE =          (3,3,3,3,3,
                                                        3,3,3,3,3,
                                                        3,3,3,3,1,
                                                        1,1,1,1,1,
                                                        1,1,1,1,1,
                                                        1,1,1,1,2,
                                                        2,2,1,1,1,
                                                        1,1,2,2,2,
                                                        1,1,1,1,4,
                                                        1,1,1,1);
   VAR Lauf : BYTE;

   FUNCTION GetGMKey;
      VAR Ergebnis : BOOLEAN;
      BEGIN
         Ergebnis := FALSE;
         Key:= '';
         KeyType := 0;
         IF (Nr >0) AND (Nr <= MaxGMKey) THEN BEGIN
            Ergebnis := TRUE;
            Key := GMKeyWords[Nr];
            KeyType := GMKType[Nr];
         END;

      END;
  PROCEDURE InitGMEnvObj(VAR EnvObject: TEnvObj);
     BEGIN
        FOR Lauf := 1 TO MaxGMKey DO
          EnvObject.AddKey(GMKeyWords[Lauf],GMKType[Lauf]);
     END;

PROCEDURE DisposeGMSetupPtrs;
   BEGIN
      DISPOSE(GMEnvObj,Done);
   END;

BEGIN
   NEW(GMEnvObj,Init);
   InitGMEnvObj(GmEnvObj^);
END.
{============================
 Versionshistorie
 $Log:$
 ============================}
