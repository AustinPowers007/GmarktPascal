{============================
 Dateiname:   $Source: $
 Version:     $Revision: $
 Autor:       $Author: $
 Gelockt von: $Locker: $
 Originaldatum : Fri Oct 11 15:41:36 GMT+02:00 1996
 Dateihistorie am Ende der Datei
 ============================}
PROGRAM TestSetup;


USES ZUSAETZE,
     GMSETUP;

VAR Ergebnis : BOOLEAN;
BEGIn
  Ergebnis := ValidPath('F:\');
  Ergebnis := ValidPath('G:\');
  Ergebnis := ValidPath('F:\LIEFERDB');
  GMEnvObj.GetSetup(IniFileName);
END.
{============================
 Versionshistorie
 $Log:$
 ============================}
