{============================
 Dateiname:   $Source: $
 Version:     $Revision: $
 Autor:       $Author: $
 Gelockt von: $Locker: $
 Originaldatum : Sun Feb 13 13:13:54 GMT+01:00 2000
 Dateihistorie am Ende der Datei
 ============================}

{*******************************************************}
{                                                       }
{       Turbo Pascal Runtime Library                    }
{       GMDelay Interface Unit                          }
{                                                       }
{       Copyright (C) 1988,92 Borland International     }
{*******************************************************}

unit GMDelay;

{$I-,S-}

interface

PROCEDURE Delay(MS:LONGINT);

implementation
   USES GMSETUP,
        ZUSAETZE;
   CONST DelayLaufConst = 1000;


PROCEDURE Delay;
   VAR DelayTime : LONGINT;
       DelayLauf : LONGINT;
       DelayLauf2: LONGINT;
       Lauf      : LONGINT;
       ZwischenErgebnis : LONGINT;

   BEGIN
       IF MS > 0 THEN BEGIN
          DelayTime := S2L(GMEnvObj^.GetEntry('WARTEZEIT'));
          IF DelayTime = 0 THEN
             DelayTime := 1;
          FOR Lauf := 1 TO MS DO BEGIN
             DelayLauf := DelayTime;
             WHILE DelayLauf > 0 DO BEGIN
                FOR DelayLauf2:= 1 TO DelayLaufConst DO BEGIN
                   ZwischenErgebnis :=DelayLauf2 MOD 13;
                END;
                DelayLauf:= DelayLauf-1;
             END;
          END;
       END;
   END;


begin
end.
{============================
 Versionshistorie
 $Log:$
 ============================}
