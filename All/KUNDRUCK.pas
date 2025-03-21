{============================
 Dateiname:   $Source: $
 Version:     $Revision: $
 Autor:       $Author: $
 Gelockt von: $Locker: $
 Originaldatum : Tue Feb 08 20:51:08 GMT+01:00 2000
 Dateihistorie am Ende der Datei
 ============================}
UNIT Kundruck;
{$O+}

INTERFACE
  USES KUNDATEN;

  PROCEDURE KDruckeKunden(Druckdaten:TKundendaten);


IMPLEMENTATION
  USES PRINTER,
       ZUSAETZE,
       GMSETUP,
       CASHDATA;


  PROCEDURE KDruckeKunden;

    VAR Lauf : BYTE;
        PlzS : STRING;
        NAFA :STRING;

    BEGIN
      {$I-}
        WITH Druckdaten DO BEGIN
          WRITELN(Lst);WRITELN(Lst);
          WRITELN(Lst,'   ',GmEnvObj^.GetEntry('GSNAME'),
                      REPLICATE(' ',35-LENGTH(GmEnvObj^.GetEntry('GSNAME'))),
                      '   Kunde ',LiefName);
          WRITELN(Lst,REPLICATE(' ',48),REPLICATE('-',LENGTH(LiefName)));
          WRITELN(Lst);WRITELN(Lst);
          WRITE(Lst,'   Laufende Nr.: ',LeadingZeros(LfdNr,5),
                      REPLICATE(' ',20));
          WRITELN(Lst,'   ',REPLICATE('=',19));
          WRITELN(Lst);
          WRITELN(Lst,'   Anrede:       ',Anr[LiefAnrede]);
          IF Firma THEN
             NAFA :='   Firma:        '
          ELSE
             NAFA :='   Firma:        ';
          IF LiefName <> '' THEN
            WRITE(Lst,NAFA,LiefName,REPLICATE(' ',(25-LENGTH(LiefName))))
          ELSE
            WRITE(Lst,REPLICATE(' ',43));
          IF LiefVorname <> '' THEN
            WRITE(Lst, 'Vorname:   ',LiefVorname);
          WRITELN(LST);
          WRITELN(LST);

          WRITELN(Lst,'   Straáe:       ',LiefStrasse);
          STR(LiefPlz,PlzS);
          WRITELN(Lst,'   Ort:          ',PlzS,' ',LiefOrt);
          WRITELN(Lst);
          WRITELN(Lst,'   Planquadrat:  ',PlanQ,
                      REPLICATE(' ',25-LENGTH(PlanQ)));
          WRITELN(Lst);
          FOR Lauf := 1 TO 3 DO BEGIN
            IF Lauf = 1 THEN
              WRITE(Lst,'   Telefon:    ')
            ELSE
              WRITE(Lst,'               ');
            WRITELN(Lst,Telefonbem[Lauf],
                        REPLICATE(' ',20-LENGTH(Telefonbem[Lauf])),
                        Telefon[Lauf]);
          END;
          WRITELN(Lst);
          FOR Lauf := 1 TO 2 DO BEGIN
            IF Lauf = 1 THEN
              WRITE(Lst,'   Bemerkungen:     ')
            ELSE
              WRITE(Lst,'                    ');
            WRITELN(Lst,Bemerkung[Lauf]);
          END;
          WRITELN(Lst);
          FOR Lauf := 1 TO 8 DO BEGIN
            IF Lauf = 1 THEN
              WRITE(Lst,'   Wegbeschreibung: ')
            ELSE
              WRITE(Lst,'                    ');
            WRITELN(Lst,WegBes[Lauf]);
          END;
          WRITELN(Lst);WRITELN(Lst,'   ',REPLICATE('-',70));WRITELN(Lst);
          WRITELN(Lst,'   Anlage-Datum:    ',AnlDatum);
          WRITELN(Lst,'   Anlage-Uhrzeit:  ',AnlUhrzeit);

          WRITELN(Lst);
          WRITELN(Lst,'   Zahlungsweise:  ',Zahlweise:2);
          WRITELN(Lst,CHR(12));
        END;
      {$I+}
    END;


BEGIN
END.
{============================
 Versionshistorie
 $Log:$
 ============================}
