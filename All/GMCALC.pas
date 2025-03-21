{============================
 Dateiname:   $Source: $
 Version:     $Revision: $
 Autor:       $Author: $
 Gelockt von: $Locker: $
 Originaldatum : Mon Jul 07 09:59:52 GMT+02:00 1997
 Dateihistorie am Ende der Datei
 ============================}
UNIT GMCALC;

INTERFACE

   USES LIEDATEN;

   VAR  LieBem: ARRAY[1..2] OF STRING[40];

   PROCEDURE InitCalcInput;
   PROCEDURE InitLeerGut;
   PROCEDURE StartCalculator(Betrag:REAL;L:TLiefDaten);

IMPLEMENTATION
   USES GMBASE,
        GLDRUCK,
        CASHNEU,
        CASHDATA,
        ZUSAETZE,
        GMT100DR;

   CONST PfandKeys : ARRAY [1..PfandTAnzahl] OF CHAR = ('A','S','D','F',
                                                       'G','H','J','K',
                                                       'L','Y','X','C',
                                                       'V','B','N');

   VAR RechSumme : REAL;
   VAR GesamtSumme : REAL;
   VAR CalcFenster : TEingabeFensterPtr;
       AktPfandVal : BYTE;
       LD:TLiefDaten;

   PROCEDURE Berechnen;
      VAR Lauf : BYTE;
          LeerGutWert : REAL;
      BEGIN
         GesamtSumme := RechSumme;
         FOR Lauf := 1 TO PfandTAnzahl DO BEGIN
            LeerGutWert := (PfandTypen[Lauf] /100);
            GesamtSumme := GesamtSumme -(LeerGutArray[Lauf] * LeerGutWert);
         END;
      END;

   FUNCTION AddPfand(VAR Taste:INTEGER;VAR Nr:BYTE;VAR InpString:STRING;
                            VAR Abbruch,Check,MoreEval:BOOLEAN):BOOLEAN;FAR;
       VAR AktKey  : CHAR;
           AktPTyp : BYTE;
           Lauf    : BYTE;
           KString : STRING;
           Ende    : BOOLEAN;
       BEGIN
          MoreEval := FALSE;
          AktKey:= UPCASE(CHR(Taste));
          Lauf := 1;
          Ende := FALSE;
          WHILE (Lauf <= PfandTAnzahl) AND (NOT Ende) DO BEGIN
             IF (AktKey = PfandKeys[Lauf]) THEN BEGIN
                Ende := TRUE;
                AktPTyp := Lauf;
             END ELSE
                INC(Lauf);
          END;
          IF (AktPTyp <> AktPfandVal) THEN BEGIN
             IF (AktPfandVal <> 0) THEN BEGIN
               KString :=PfandKeys[AktPfandVal];
               CalcFenster^.SetCColor(KString+'V',0,7);
               CalcFenster^.Refresh(KString+'V');
             END;
             KString :=PfandKeys[AktPTyp];
             CalcFenster^.SetCColor(KString+'V',7,2);
             AktPfandVal := AktPTyp;
          END;
          IF (LeerGutArray[AktPfandVal] < 999) THEN BEGIN
             KString:= PfandKeys[AktPfandVal];
             INC(LeerGutArray[AktPfandVal]);
             Berechnen;
             CalcFenster^.Refresh(KString+'V');
             CalcFenster^.Refresh('GS');
          END ELSE
             WRITE(CHR(7));
          Abbruch := FALSE;
          Check := FALSE;
          AddPfand := FALSE;
       END;

   FUNCTION SubPfand(VAR Taste:INTEGER;VAR Nr:BYTE;VAR InpString:STRING;
                            VAR Abbruch,Check,MoreEval:BOOLEAN):BOOLEAN;FAR;
       VAR KString : STRING;
       BEGIN
          MoreEval:=FALSE;
          IF (AktPfandVal <> 0) AND ((LeerGutArray[AktPfandVal] > 0) OR (AktPfandVal > 9)) THEN BEGIN
             IF (LeerGutArray[AktPfandVal] > -999 ) THEN BEGIN
                KString := PfandKeys[AktPfandVal];
                DEC(LeerGutArray[AktPfandVal]);
                Berechnen;
                CalcFenster^.Refresh(KString+'V');
                CalcFenster^.Refresh('GS');
             END ELSE
                WRITE(CHR(7));

          END ELSE
             WRITE(CHR(7));
          Abbruch := FALSE;
          Check := FALSE;
          SubPfand := FALSE;
       END;

   FUNCTION ESCBack(VAR Taste:INTEGER;VAR Nr:BYTE;VAR InpString:STRING;
                            VAR Abbruch,Check,MoreEval:BOOLEAN):BOOLEAN;FAR;
      VAR KString : STRING;
      BEGIN
         MoreEval := FALSE;
         IF (AktPfandVal <> 0) THEN BEGIN
            KString :=PfandKeys[AktPfandVal];
            CalcFenster^.SetCColor(KString+'V',0,7);
            AktPfandVal := 0;
         END;
         Abbruch := FALSE;
         Check := FALSE;
         EscBack := TRUE;
      END;

   FUNCTION LDruckProc(VAR Taste:INTEGER;VAR Nr:BYTE;VAR InpString:STRING;
                            VAR Abbruch,Check,MoreEval:BOOLEAN):BOOLEAN;FAR;
      BEGIN
        LDrucken(LD);
        Abbruch := FALSE;
        Check := FALSE;
      END;

   PROCEDURE InitLeerGut;
       VAR Lauf : BYTE;
       BEGIN
          FOR Lauf := 1 TO PfandTAnzahl DO BEGIN
             LeerGutArray[Lauf] := 0;
          END;
       END;


   PROCEDURE InitCalcInput;
      VAR XPos,YPos: BYTE;
          Lauf : BYTE;
      VAR KString: STRING[1];
          WertString : STRING;
          Wert : REAL;
      BEGIN
         NEW (CalcFenster);
         CalcFenster^.Init(90,100,550,350,0,'Rechnungssumme berechnen','',Aktiv);
         WITH CalcFenster^ DO BEGIN
            FOR Lauf := 1 TO 4 DO BEGIN
               XPos := ((Lauf -1) MOD 4) *14;
               AddConst('0',Xpos,1,'Typ');
               AddConst('0',Xpos+8,1,'Anz');
            END;
            AddHline('',0,55,2,0);
            AddHline('',0,55,2,-2);
            FOR Lauf := 1 TO PfandTAnzahl DO BEGIN
               XPos := ((Lauf -1) MOD 4) *14;
               YPos := ((Lauf -1) DIV 4) * 2+3;
               KString := PfandKeys[Lauf];
               Wert := (PfandTypen[Lauf] /100);
               WertString := PrintReal(Wert,3,2,TRUE);
               AddConst(KString+'K',XPos,YPos,KString);
               SetCColor(KString+'K',8,6);
               SetCHeight(KString+'K',1);
               SetCWidth(KString+'K',1);
               AddConst(KString+'P',XPos+2,YPos,WertString+':');
               AddCLong(KString+'V',XPos+8,YPos,LeerGutArray[Lauf],4);
            END;
            AddConst ('GS',13,12,'Summe: ');
            AddCReal ('GS',19,12,GesamtSumme,7,2);
{            AddString('','30','Bem:', 1,14,LieBem[1],'',40);
            AddString('','30','Bem:', 1,15,LieBem[2],'',40);
            Disable('30');}
            SetCColor    ('GS',8,6);
            SetCHeight   ('GS',3);
            SetCrQuit(FALSE);
            SetFKeyQuit(FALSE);
            FOR Lauf := 1 TO PfandTAnzahl DO BEGIN
               AddActionKey('_',ORD(PfandKeys[Lauf]),AddPfand,FALSE,'Leergut hinzufÅgen ');
               AddActionKey('_',ORD(LowCase(PfandKeys[Lauf])),AddPfand,FALSE,'');
            END;
            AddActionKey('_', 45,SubPfand,FALSE,'Leergut abziehen');
            AddActionKey('_', 27,EscBack,FALSE,'ZurÅck in Lieferschein');
            AddActionKey('_',-63,LDruckProc,FALSE,'Lieferschein drucken');
         END;
         AktPfandVal := 0;
      END;

   PROCEDURE StartCalculator;
      BEGIN
         LD:=L;
         RechSumme := Betrag;
         LieBem[1]:=L.Bemerkungen[1];
         LieBem[2]:=L.Bemerkungen[2];


         Berechnen;

         CalcFenster^.Input;
         CalcFenster^.Hide;
      END;
END.

{============================
 Versionshistorie
 $Log:$
 ============================}
