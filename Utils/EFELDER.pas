{============================
 Dateiname:   $Source: $
 Version:     $Revision: $
 Autor:       $Author: $
 Gelockt von: $Locker: $
 Originaldatum : Fri Jan 07 18:02:58 GMT+01:00 2000
 Dateihistorie am Ende der Datei
 ============================}

                               {ษอออออออออออป
   ษออออออออออออออออออออออออออออน Bemerkung ฬอออออออออออออออออออออออออออออออป
   บ                            ศอออออออออออผ                               บ
   บ Aufgabe:     Liefert Basistyp fr alle Eingabefelder in Unit FELDART   บ
   ศออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ}

UNIT EFELDER;
{$O+}

INTERFACE

USES GMCRT,
     GRAPH,
     GRAPNEU,
     FARBEN,
     CASHDATA,
     PLACEWIN;

                               {ษอออออออออออป
   ษออออออออออออออออออออออออออออน Bemerkung ฬอออออออออออออออออออออออออออออออป
   บ                            ศอออออออออออผ                               บ
   บ Aufgabe:     TYP fr Zeiger auf ein Eingabefeld fr Unit CASHNEU       บ
   ศออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ}

       TYPE TEFeldZeiger = ^TEFeld;

                               {ษอออออออออออป
   ษออออออออออออออออออออออออออออน Bemerkung ฬอออออออออออออออออออออออออออออออป
   บ                            ศอออออออออออผ                               บ
   บ Aufgabe:     Eingabefeld-Objekt kann nur typisiert verwendet werden    บ
   บ              (siehe UNIT Feldart) Basis-Objekt fr alle typisierten    บ
   บ              Eingabefelder                                             บ
   ศออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ}

            TEFeld = OBJECT (TDialogObj)
              Titel       : STRING;       { Eingabefeldtitel               }
              FeldLaenge  : BYTE;         { Lnge des Eingabefeldes        }
              TiSpalte,                   { Titelposition                  }
              TiZeile,                    {       "                        }
              ISpalte,                    { Eingabefeldposition            }
              IZeile      : WORD;         {       "                        }
              SubFeldPos  : WORD;         { Unterposition in Multifelder   }
              InfoKennung : STRING[8];    { Zugeh”rige InfoKennung         }
              FeldModus   : TModi;        { FeldModus                      }
              ModusAktiv  : TModi;        { Startmodus EingabeFeld         }
              FeldArt     : TPrae;        { Runde oder normale Kanten      }
              Maske       : STRING;       { Feste EingabeMaske Insert
                                            unm”glich                      }
              CONSTRUCTOR Init(TiSP,TiZE,ISP,IZE:WORD;Len:BYTE;Txt:STRING;
                               InfNo:STRING;M:TModi;EA:TPrae);
              DESTRUCTOR Release;VIRTUAL;
              FUNCTION  GetInfoNo:STRING;VIRTUAL;
              PROCEDURE SetData(DataSeg,DataOfs:WORD);VIRTUAL;
              FUNCTION  MakeInpStr:STRING;VIRTUAL;
              FUNCTION  MakeClear:STRING;VIRTUAL;
              PROCEDURE MakeVal(Eingabe:STRING);VIRTUAL;
              FUNCTION ValidInput(VAR Eingabe:STRING):BOOLEAN;VIRTUAL;
              FUNCTION ValidKey(VAR Code:INTEGER;VAR CPos:BYTE):BOOLEAN;VIRTUAL;
              FUNCTION Input:INTEGER;VIRTUAL;
              FUNCTION SpecialEval(VAR Eingabe:STRING;VAR InputCode:INTEGER;
                                    VAR Cpos:BYTE):BOOLEAN;VIRTUAL;
              PROCEDURE Show(K:STRING);VIRTUAL;
              PROCEDURE SetActive;VIRTUAL;
              FUNCTION  Disabled(K:STRING):BOOLEAN;VIRTUAL;
              PROCEDURE Enable(K:STRING);VIRTUAL;
              PROCEDURE Disable(K:STRING);VIRTUAL;
              FUNCTION  Left:WORD;VIRTUAL;
              FUNCTION  Right:WORD;VIRTUAL;
              FUNCTION  Top:WORD;VIRTUAL;
              FUNCTION  Bottom:WORD;VIRTUAL;
              PROCEDURE BackInput;
              PROCEDURE PrintInhalt(Inhalt:STRING);VIRTUAL;
              PROCEDURE PrintTitel;VIRTUAL;
              PROCEDURE Save(K:STRING);VIRTUAL;
              PROCEDURE Restore(K:STRING);VIRTUAL;
              FUNCTION  Changed(K:STRING):BOOLEAN;VIRTUAL;
              FUNCTION  Valid(K:STRING):BOOLEAN;VIRTUAL;
              PROCEDURE SetVal(ItK:STRING;ValString:STRING);VIRTUAL;
              FUNCTION  GetVal(ItK:STRING):STRING;VIRTUAL;
              PROCEDURE SetInputStartPos(ColPos:WORD);VIRTUAL;
              FUNCTION  GetInputStartPos:WORD;VIRTUAL;
            END;


                               {ษอออออออออออป
   ษออออออออออออออออออออออออออออน Bemerkung ฬอออออออออออออออออออออออออออออออป
   บ                            ศอออออออออออผ                               บ
   บ Aufgabe:     Farbenverwaltung in Eingabefenster aus CASHNEU            บ
   ศออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ}

  VAR AktuelleFarben : ^TFarben;


IMPLEMENTATION

   USES ZUSAETZE;

                               {ษอออออออออออป
   ษออออออออออออออออออออออออออออน Bemerkung ฬอออออออออออออออออออออออออออออออป
   บ                            ศอออออออออออผ                               บ
   บ Aufgabe:     Eingabefeldvariablen mit Initialwerten besetzen           บ
   บ                                                                        บ
   บ Parameter:   TitelSpalte,TitelZeile,Inhaltsspalte,InhaltsZeile:WORD,   บ
   บ              Feldlnge:BYTE,FeldTitel,InfoDatei:STRING,EingabeModus:   บ
   บ              TModi, Eingabefeldarstellung:TPrae                        บ
   บ                                                                        บ
   บ Resultate:   Keine                                                     บ
   ศออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ}

      CONSTRUCTOR TEFeld.Init;
        BEGIN
           inherited Init;
           TiSpalte    := TiSP;
           TiZeile     := TiZE;
           ISpalte    := ISP;
           IZeile     := IZE;
           FeldLaenge := Len;
           Titel      := Txt;
           InfoKennung:= InfNo;
           FeldModus  := EN;
           ModusAktiv := M;
           FeldArt    := EA;
           Maske      := '';
           Default := FALSE;
        END;

      FUNCTION TEFeld.Left;
        BEGIN
           Left := TISpalte;
        END;

      FUNCTION TEFeld.Right;
        BEGIN
           Right := ISpalte+FeldLaenge*8;
        END;

      FUNCTION TEFeld.Top;
        BEGIN
           Top := IZeile;
        END;

      FUNCTION TEFeld.Bottom;
        BEGIN
           Bottom := IZeile+8;
        END;


                               {ษอออออออออออป
   ษออออออออออออออออออออออออออออน Bemerkung ฬอออออออออออออออออออออออออออออออป
   บ                            ศอออออออออออผ                               บ
   บ Aufgabe:     Eingabefeldobjekt freigeben                               บ
   บ                                                                        บ
   บ Parameter:   Keine                                                     บ
   บ                                                                        บ
   บ Resultate:   Keine                                                     บ
   ศออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ}

      DESTRUCTOR TEFeld.Release;
        BEGIN
           inherited RELEASE
        END;

      FUNCTION TEFeld.GetInfoNo;
        BEGIN
           GetInfoNo := InfoKennung;
        END;

                               {ษอออออออออออป
   ษออออออออออออออออออออออออออออน Bemerkung ฬอออออออออออออออออออออออออออออออป
   บ                            ศอออออออออออผ                               บ
   บ Aufgabe:     Eingabefeldobjekt mit Wertevariable verbinden, nur bei    บ
   บ              Nachfolgern dieses Objektes definiert                     บ
   บ                                                                        บ
   บ Parameter:   DataSeg,DataOfs:WORD,bilden Speicheradresse               บ
   บ                                                                        บ
   บ Resultate:   Keine                                                     บ
   ศออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ}

      PROCEDURE TEFeld.SetData;
        BEGIN
          RUNERROR(211)
        END;


                               {ษอออออออออออป
   ษออออออออออออออออออออออออออออน Bemerkung ฬอออออออออออออออออออออออออออออออป
   บ                            ศอออออออออออผ                               บ
   บ Aufgabe:     Eingabefeldwert in Text umwandeln                         บ
   บ                                                                        บ
   บ Parameter:   Keine                                                     บ
   บ                                                                        บ
   บ Resultate:   Eingabe:STRING,Nur bei Nachfolgern des Objektes definiert บ
   ศออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ}

      FUNCTION TEFeld.MakeInpStr;

        BEGIN
         RUNERROR(211);
        END;
                               {ษอออออออออออป
   ษออออออออออออออออออออออออออออน Bemerkung ฬอออออออออออออออออออออออออออออออป
   บ                            ศอออออออออออผ                               บ
   บ Aufgabe:     Eingabefeldwert mit aktueller Eingabe belegen             บ
   บ                                                                        บ
   บ Parameter:   Eingabe:STRING,nur bei Nachfolgern des Objektes def.      บ
   บ                                                                        บ
   บ Resultate:   Keine                                                     บ
   ศออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ}

      PROCEDURE TEFeld.MakeVal;

        BEGIN
           RUNERROR(211);
        END;

                               {ษอออออออออออป
   ษออออออออออออออออออออออออออออน Bemerkung ฬอออออออออออออออออออออออออออออออป
   บ                            ศอออออออออออผ                               บ
   บ Aufgabe:     Eingabefeld mit Maske ohne Inhalt erzeugen. Wird bei      บ
   บ              bestimmten Nachfolgern neu definiert                      บ
   บ                                                                        บ
   บ Parameter:   Keine                                                     บ
   บ                                                                        บ
   บ Resultate:   Keine                                                     บ
   ศออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ}

      FUNCTION TEFeld.MakeClear;

        BEGIN
          IF Maske <> '' THEN
            MakeClear := Maske
          ELSE
            MakeClear :='';
        END;
                               {ษอออออออออออป
   ษออออออออออออออออออออออออออออน Bemerkung ฬอออออออออออออออออออออออออออออออป
   บ                            ศอออออออออออผ                               บ
   บ Aufgabe:     Bestimmung der zulssigen Tasten bei der Eingabe,wird bei บ
   บ              Nachfolgern des Objektes neu definiert                    บ
   บ                                                                        บ
   บ Parameter:   InputCode:INTEGER                                         บ
   บ                                                                        บ
   บ Resultate:   Zulssig:BOOLEAN                                          บ
   ศออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ}

      FUNCTION TEFeld.ValidKey;
        BEGIN
          RUNERROR(211);
        END;

                               {ษอออออออออออป
   ษออออออออออออออออออออออออออออน Bemerkung ฬอออออออออออออออออออออออออออออออป
   บ                            ศอออออออออออผ                               บ
   บ Aufgabe:     Bestimmung der zulssigen Eingaben, wird bei Nachfolgern  บ
   บ              neu definiert                                             บ
   บ                                                                        บ
   บ Parameter:   Eingabe:STRING                                            บ
   บ                                                                        บ
   บ Resultate:   Zulssig:BOOLEAN                                          บ
   ศออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ}
      FUNCTION TEFeld.ValidInput;
        BEGIN
          ValidInput := TRUE;
        END;

                               {ษอออออออออออป
   ษออออออออออออออออออออออออออออน Bemerkung ฬอออออออออออออออออออออออออออออออป
   บ                            ศอออออออออออผ                               บ
   บ Aufgabe:     Darstellung des Eingabefeldes auf den Bildschirm bringen  บ
   บ              abhngig von Variable Feldart des Objektes                บ
   บ                                                                        บ
   บ Parameter:   Keine                                                     บ
   บ                                                                        บ
   บ Resultate:   Keine                                                     บ
   ศออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ}
       PROCEDURE TEFeld.BackInput;
             { Feldlayout fr normal }
         PROCEDURE BackNormal;
          VAR Xe: WORD;
          BEGIN
             DisplayText(ISpalte,IZeile,1,1,AktuelleFarben^.FeldHG[FeldModus],
                         AktuelleFarben^.FeldHG[FeldModus],REPLICATE(' ',FeldLaenge));
          END;

          { Feldlayout fr abgerundet }


         BEGIN
           CASE FeldArt OF
             Normal : BackNormal;
             Extra  : BackExtra(ISpalte,IZeile,(FeldLaenge*8),AktuelleFarben^.FeldUM[FeldModus],
                        AktuelleFarben^.FeldHG[FeldModus]);
           END;
         END;

                               {ษอออออออออออป
   ษออออออออออออออออออออออออออออน Bemerkung ฬอออออออออออออออออออออออออออออออป
   บ                            ศอออออออออออผ                               บ
   บ Aufgabe:     Feldwert einlesen, wird bei Nachfolgern evtl. neu def.    บ
   บ                                                                        บ
   บ Parameter:   Keine                                                     บ
   บ                                                                        บ
   บ Resultate:   EndCode:INTEGER                                           บ
   ศออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ}
   FUNCTION  TEFeld.Input;

      VAR   Ende,
            Break,
            Check,
            MoreEval,
            ActionDone :    BOOLEAN;
            Nr       : BYTE;
            MText,
            Eingabe:     STRING;
            CursorPos:   BYTE;
            InputCode:   INTEGER;

      FUNCTION Next(CPos:BYTE;Richtung:SHORTINT):BYTE;
         BEGIN
            CPos:= CPos+Richtung;
            IF (CPos < 1) OR (CPos > FeldLaenge) THEN
               CPos:= CPos-Richtung;
            IF Richtung = 0 THEN
               INC(Richtung);
            IF Maske <> '' THEN
               WHILE Maske[Cpos] <> ' 'DO
                  CPos := CPos+Richtung;
            Next := CPos
         END;

      PROCEDURE DelFrom(VAR Txt:STRING;CursorPos:BYTE);
         VAR P : BYTE;
             Part : STRING;
         BEGIN
            DELETE(Txt,CursorPos,1);
            IF Maske <> '' THEN BEGIN
               Part := COPY(Maske,CursorPos+1,FeldLaenge-CursorPos);
               P := FirstMaskChar(Part);
               IF P = 0 THEN
                  P := LENGTH(Part)+1;
               INSERT(' ',Txt,P-1+CursorPos);
            END;
         END;

      PROCEDURE PutIn(VAR Txt:STRING;CursorPos:BYTE;InputCode:INTEGER);
         VAR P     : BYTE;
             Part  : STRING;
         BEGIN
            IF FeldModus <> i THEN
               DELETE(Txt,CursorPos,1)
            ELSE
               IF Maske <> '' THEN BEGIN
                  Part := COPY(Maske,CursorPos+1,FeldLaenge-CursorPos);
                  P := FirstMaskChar(Part);
                  IF P = 0 THEN
                     P := LENGTH(Part)+1;
                  DELETE(Txt,CursorPos-1+P,1);
               END;
            INSERT(CHR(INPUTCODE),Txt,CursorPos);
            Txt:=COPY(Txt,1,FeldLaenge);
         END;

      PROCEDURE PrintCursor(S:TSchalter;Xa,Ya,Xp:WORD;Inhalt:STRING);
         VAR C: BYTE;
         PROCEDURE CursorInsert(Xa,Ya:WORD;Xp,C:BYTE);
           BEGIN
              MOVETO  (Xa-11+Xp*8,Ya-3); LINETO(Xa- 6+Xp*8,Ya-3);
              MOVETO  (Xa-10+Xp*8,Ya-2); LINETO(Xa- 7+Xp*8,Ya-2);
              MOVETO  (Xa-10+Xp*8,Ya+8); LINETO(Xa- 7+Xp*8,Ya+8);
              MOVETO  (Xa-11+Xp*8,Ya+9); LINETO(Xa- 6+Xp*8,Ya+9);
              PUTPIXEL(Xa- 8+Xp*8,Ya-1,C);
              PUTPIXEL(Xa- 8+Xp*8,Ya+7,C);
              PUTPIXEL(Xa- 9+Xp*8,Ya-1,C);
              PUTPIXEL(Xa- 9+Xp*8,Ya+7,C)
           END;

         PROCEDURE CursorOverwrite(Xa,Ya,Xp:WORD);
            BEGIN
               MOVETO(Xa-9+Xp*8,Ya-3); LINETO(Xa-1+Xp*8,Ya-3);
               MOVETO(Xa-9+Xp*8,Ya-2); LINETO(Xa-1+Xp*8,Ya-2);
               MOVETO(Xa-9+Xp*8,Ya+8); LINETO(Xa-1+Xp*8,Ya+8);
               MOVETO(Xa-9+Xp*8,Ya+9); LINETO(Xa-1+Xp*8,Ya+9)
            END;

         BEGIN
            CASE S OF
                 On : BEGIN
                         SETCOLOR      (AktuelleFarben^.CursorInhVG[FeldModus]);
                         SETFILLSTYLE(1,AktuelleFarben^.CursorHG[FeldModus])
                      END;
                 Off: BEGIN
                         SETCOLOR      (AktuelleFarben^.InhaltVG[FeldModus]);
                         SETFILLSTYLE(1,AktuelleFarben^.InhaltHG[FeldModus])
                      END
            END;
            BAR(Xa+(Xp-1)*8,Ya-4,Xa+Xp*8-1,Ya+10);
            OUTTEXTXY(Xa+(Xp-1)*8,Ya,COPY(Inhalt,Xp,1));
            IF (Feldart = Extra) THEN BEGIN
               CASE S OF
                  On:  C:=AktuelleFarben^.CursorVG[FeldModus];
                  Off: C:=AktuelleFarben^.InhaltHG[FeldModus]
               END;
               SETCOLOR      (C);
               SETFILLSTYLE(1,C);
               CASE FeldModus OF
                  i:   CursorInsert(Xa,Ya,Xp,C);
                  o,a: CursorOverwrite(Xa,Ya,Xp)
               END
            END;
         END;

      PROCEDURE Cashback(Full:BOOLEAN;CursorPos:BYTE;Txt:STRING);
          BEGIN
             IF Full THEN BEGIN
                BackInput;
                PrintTitel
             END;
             PrintInhalt(Txt);
             CASE FeldModus OF
                i,o,a: PrintCursor(On,ISpalte,IZeile,CursorPos,Txt)
             END;
          END;

      { Anfang von Input HauptProgramm }
      BEGIN
         Nr := 0;
         InputCode := 0;
         Done := FALSE;
         KeyDone := FALSE;
         Abbruch := FALSE;
         IF NOT Disabled('') THEN BEGIN
            SetActive;
            Eingabe := MakeInpStr;
            Ende   := FALSE;
            MText  := Eingabe;
            CursorPos := LENGTH(Eingabe)+1;
            IF CursorPos > FeldLaenge THEN
               CursorPos:=FeldLaenge;
            IF Eingabe = Maske THEN
               CursorPos:=1
            ELSE
               IF CursorPos > 0 THEN
                  FeldModus := A;
            InputCode := 0;
            CASHBACK(TRUE,CursorPos,Eingabe);
            REPEAT
               IF (MText <> Eingabe) THEN
                  CASHBACK(FALSE,CursorPos,Eingabe)
               ELSE
                  PrintCursor(On,ISpalte,IZeile,CursorPos,Eingabe);
               MText:=Eingabe;
               ActionTMod:=FeldModus;
               ActionText:=MText;
               InputCode:=KeyInput;
               Check:=TRUE;
               Ende := KeyAction(InputCode,Nr,Eingabe,Abbruch,Done,Check,MoreEval);
               KeyDone := Done AND (NOT MoreEval);
               IF NOT Done THEN BEGIN
                  IF (FeldModus = a) THEN BEGIN
                     IF NOT SpecialEval(Eingabe,InputCode,CursorPos) THEN BEGIN
                        IF ValidKey(InputCode,CursorPos) THEN BEGIN
                           CursorPos:=1;
                           Eingabe:=MakeClear
                        END;
                     END;
                     FeldModus:=i;
                     CASHBACK(FALSE,CursorPos,Eingabe)
                  END;
                  PrintCursor(Off,ISpalte,IZeile,CursorPos,Eingabe);
                  CASE InputCode OF
                     -68..-59,
                     -113..-84: BEGIN
                                  Ende:=TRUE;
                                  Done:= TRUE;
                               END;
                     -15,-72,
                     -80,9,13: BEGIN
                                  Ende := TRUE;
                                  Done:= TRUE;
                               END;
                     27      : BEGIN
                                  Ende := TRUE;
                                  Done:= TRUE;
                                  Abbruch := TRUE;
                               END;
                     -71     : CursorPos:=Next(1,0);
                     -75     : CursorPos:=Next(CursorPos,-1);
                     -77     : IF (CursorPos <= LENGTH(Eingabe)) THEN
                                  CursorPos:=Next(CursorPos,1);
                     -79     : IF LENGTH(Eingabe) < FeldLaenge THEN
                                  CursorPos:=Next(LENGTH(Eingabe),1)
                               ELSE
                                  CursorPos:=Next(LENGTH(Eingabe),0);
                     -82     : IF (FeldModus <> i) THEN
                                  FeldModus := i
                               ELSE
                                  FeldModus := o;
                     -83     : DelFrom(Eingabe,CursorPos);
                     8       : IF (CursorPos > 1) THEN BEGIN
                                  CursorPos:=Next(CursorPos,-1);
                                  DelFrom(Eingabe,CursorPos);
                               END;
                     127     : BEGIN
                                  Eingabe:=MakeClear;
                                  CursorPos:=Next(1,0);
                               END;
                     ELSE
                        IF NOT SpecialEval(Eingabe,InputCode,Cursorpos) THEN
                           IF ValidKey(InputCode,CursorPos) THEN BEGIN
                              PutIn(Eingabe,CursorPos,InputCode);
                              CursorPos:=Next(CursorPos,1);
                           END ELSE
                              WRITE(CHR(7));
                  END;
               END;
               IF Done THEN BEGIN
                  IF CursorPos > LENGTH(Eingabe) THEN
                     CursorPos:=Next(LENGTH(Eingabe),1);
               END;
               IF Ende THEN BEGIN
                  IF Abbruch THEN
                     Eingabe := MakeInpStr;
                  IF Check THEN BEGIN
                     IF NOT ValidInput(Eingabe) THEN BEGIN
                        WRITE(CHR(7));
                        Ende :=FALSE;
                        Done :=FALSE;
                        Abbruch:= FALSE;
                       IF CursorPos > LENGTH(Eingabe) THEN
                           CursorPos:=Next(LENGTH(Eingabe),1);
                     END ELSE
                        MakeVal(Eingabe);
                  END;
               END
            UNTIL Ende;
            IF (FeldModus <> DI) THEN
               Enable('');
            CashBack (TRUE,CursorPos,MakeInpStr);
            PrintCursor(Off,ISpalte,IZeile,CursorPos,MakeInpStr);
            Show('');
         END;
         Input := InputCode;
      END;

   FUNCTION TEFeld.Disabled;
     BEGIN
       Disabled := (FeldModus = DI);
     END;
                               {ษอออออออออออป
   ษออออออออออออออออออออออออออออน Bemerkung ฬอออออออออออออออออออออออออออออออป
   บ                            ศอออออออออออผ                               บ
   บ Aufgabe:     Eingabefeld in EingabeModus umsetzen                      บ
   บ                                                                        บ
   บ Parameter:   Keine                                                     บ
   บ                                                                        บ
   บ Resultate:   Keine                                                     บ
   ศออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ}

   PROCEDURE TEFeld.SetActive;
     BEGIN
       FeldModus := ModusAktiv;
     END;

                               {ษอออออออออออป
   ษออออออออออออออออออออออออออออน Bemerkung ฬอออออออออออออออออออออออออออออออป
   บ                            ศอออออออออออผ                               บ
   บ Aufgabe:     Eingabefeld in Modus Enabled umsetzen                     บ
   บ                                                                        บ
   บ Parameter:   Keine                                                     บ
   บ                                                                        บ
   บ Resultate:   Keine                                                     บ
   ศออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ}

   PROCEDURE TEFeld.Enable;
     BEGIN
       FeldModus := EN;
     END;

                               {ษอออออออออออป
   ษออออออออออออออออออออออออออออน Bemerkung ฬอออออออออออออออออออออออออออออออป
   บ                            ศอออออออออออผ                               บ
   บ Aufgabe:     Eingabefeld in Modus Diabled umsetzen                     บ
   บ                                                                        บ
   บ Parameter:   Keine                                                     บ
   บ                                                                        บ
   บ Resultate:   Keine                                                     บ
   ศออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ}

   PROCEDURE TEFeld.Disable;
     BEGIN
       FeldModus := DI;
     END;

                               {ษอออออออออออป
   ษออออออออออออออออออออออออออออน Bemerkung ฬอออออออออออออออออออออออออออออออป
   บ                            ศอออออออออออผ                               บ
   บ Aufgabe:     FeldTitel, abhngig von Feldmodus ausgeben                บ
   บ                                                                        บ
   บ Parameter:   Keine                                                     บ
   บ                                                                        บ
   บ Resultate:   Keine                                                     บ
   ศออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ}

        PROCEDURE TEFeld.PrintTitel;
           BEGIN
             DisplayText(TiSpalte,TiZeile,1,1,AktuelleFarben^.TitelHG[FeldModus],
                         AktuelleFarben^.TitelVG[FeldModus],Titel);
           END;

                               {ษอออออออออออป
   ษออออออออออออออออออออออออออออน Bemerkung ฬอออออออออออออออออออออออออออออออป
   บ                            ศอออออออออออผ                               บ
   บ Aufgabe:     Text im Feld auf den Bilschirm ausgeben, abh. von Eing.Mo.บ
   บ                                                                        บ
   บ Parameter:   Ausgabetext:STRING                                        บ
   บ                                                                        บ
   บ Resultate:   Keine                                                     บ
   ศออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ}

        PROCEDURE TEFeld.PrintInhalt;

           BEGIN
             Inhalt:=COPY(Inhalt,1,FeldLaenge);
             DisplayText(ISpalte,IZeile,1,1,AktuelleFarben^.FeldHG[FeldModus],
                         AktuelleFarben^.FeldHG[FeldModus],REPLICATE(' ',FeldLaenge));
             DisplayText(ISpalte,IZeile,1,1,AktuelleFarben^.InhaltHG[FeldModus],
                         AktuelleFarben^.InhaltVG[FeldModus],Inhalt);
           END;


                               {ษอออออออออออป
   ษออออออออออออออออออออออออออออน Bemerkung ฬอออออออออออออออออออออออออออออออป
   บ                            ศอออออออออออผ                               บ
   บ Aufgabe:     Anzeige des Feldes (Inhalt+Titel), Unterscheidung nur     บ
   บ              zwischen Modus Enabled und Disabled                       บ
   บ                                                                        บ
   บ Parameter:   Keine                                                     บ
   บ                                                                        บ
   บ Resultate:   Keine                                                     บ
   ศออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ}

      PROCEDURE TEFeld.Show;
        VAR Txt  : STRING;
            Modus : TModi;
        BEGIN
          inherited Show('');
          Txt := MakeInpStr;
          PrintTitel;
          Modus := FeldModus;
          IF FeldModus <> DI THEN
             FeldModus := EN;
          IF (FeldModus = EN) AND IsDefault('') THEN
             FeldModus := DE;
          PrintInhalt(Txt);
          FeldModus := Modus
        END;

    FUNCTION TEFeld.SpecialEval;
      BEGIN
         SpecialEval:=FALSE;
         IF SpecialEvalActive THEN BEGIN
            SpecialEval := SpecialEvalFunc(Eingabe,InputCode,CPos);
         END
      END;

    PROCEDURE TEFeld.Save;
      BEGIN
         RUNERROR(211);
      END;

    PROCEDURE TEFeld.Restore;
      BEGIN
         RUNERROR(211);
      END;

    FUNCTION TEFeld.Changed;
      BEGIN
        Changed := FALSE;
      END;

    FUNCTION TEFeld.GetVal;
      BEGIN
         GetVal := MakeInpStr;
      END;

    PROCEDURE TEFeld.SetVal;
      BEGIN
         MakeVal(ValString);
      END;

    FUNCTION TEFeld.Valid;
      VAR Eingabe : STRING;
      BEGIN
         Eingabe := MakeInpStr;
         Valid := ValidInput(Eingabe);
      END;

    FUNCTION TEFeld.GetInputStartPos;
      BEGIN
         GetInputStartPos :=ISpalte;
      END;

    PROCEDURE TEFeld.SetInputStartPos;
      BEGIN
         ISpalte := ColPos;
      END;

BEGIN
END.
{============================
 Versionshistorie
 $Log:$
 ============================}
