{============================
 Dateiname:   $Source: $
 Version:     $Revision: $
 Autor:       $Author: $
 Gelockt von: $Locker: $
 Originaldatum : Fri Jan 07 18:02:20 GMT+01:00 2000
 Dateihistorie am Ende der Datei
 ============================}
UNIT CASHPROC;

   INTERFACE

      USES  GMCRT,
            GRAPH;

      TYPE  Prae       = (Normal,  { Normale Darstellung Kasten       }
                          Extra);  { Extra Darstellung runde Enden    }

          { In welchem Modus befindet sich die CASHPROC derzeit       }

            Modo       = (i,       { Insert           Aktiver Modus   }
                          o,       { Overwrite           "      "     }
                          a,       { Overwrite All       "      "     }
                          en,      { Enabled          Inaktiver Modus }
                          di);     { Disabled            "      "     }

            Modi       = ARRAY[i..di] OF BYTE;

            Menge      = SET OF CHAR;

          { Soll der Cursor an- oder ausgeschaltet sein }

            Schalter   = (On,Off);

          { Welche Funktion soll die CASHPROC derzeit bernehmen }

            Funktionen = (Ausgabe,Eingabe);

          { Wertemenge fr die Zust„nde "Egal, Ja, Nein" }

            Trelean    = (E,J,N);

      CONST MCounts    = [ '0'..'9'];  { Menge der Zahlen fr CASHNINT }

            MTexter    = [#32..#255];  { Menge der aller Zeichen fr CASHTEXT }

      VAR   DA:        PRAE; { Bedeutung entsprechend SetLayout **p }
            UM,HG,           {     "           "          "      "  }
            IV,IH,           {     "           "      SetText    "  }
            CV,CH,CT,        {     "           "      SetCursor  "  }
            TV,TH:     Modi; {     "           "      SetTitel   "  }

            Modus,
            Startmodus:Modo;

            MemFunk,
            AktFunk,               { Aktuelle Funktion    }
            EndFunk:   Funktionen; { Letzte auszufhrende }

            Xpos,                  { Gegenw„rtige Position im Feld            }
            Mpos,                  { Letzte Position im Feld                  }
            RPos,                  { Eine Merkeposition                       }
            ComeBack:  BYTE;       { Wunschposition bei Eintritt in Eingabe   }

            MLinks,                { Menge der Feldbeendenden Zeichen LoByte  }
            MRechts,               {   "    "        "           "    HiByte  }
            MLegal:    Menge;      { Menge der gltigen Zeichen Feldinhalt    }

            MA:        ARRAY[1..255] OF Modo;  { Feld fr En- und Disabled
                                                 fr jedes Feld               }

            Sign1,                 { Eingegebenes Zeichen LoByte              }
            Sign2:     CHAR;       {      "          "      "                 }


      PROCEDURE SetLayout(DAp:PRAE;     { Darstellungsart des Feldes          }
                          UMp,          { Farben der Umrandung                }
                          HGp:STRING);  {   "    des Hintergrundes im Feld    }

      PROCEDURE SetText  (IVp,          { Farben des Feldinhaltes             }
                          IHp:STRING);  {   "    Hintergrund Feldinhalt       }

      PROCEDURE SetCursor(CVp,          { Farben des Cursors Vordergund       }
                          CHp,          {   "     "  Cursorfeldes Hintergr.   }
                          CTp:STRING);  {   "     "  Cursorinhaltes Vordergr. }

      PROCEDURE SetTitel (TVp,          { Farben des Titels Vordergrund       }
                          THp:STRING);  {   "     "    "    Hintergrund       }

      PROCEDURE StartCashN;
      PROCEDURE StartCashE;

      PROCEDURE CASHBACK (Full:Boolean;CP,XT,YT,XI,YI,XL:WORD;Titel,Inhalt:STRING;M:Modo);

      FUNCTION  CASHTEXT(info,   pe,x1,y1,x2,y2,xl,xs:WORD;Titel,default:STRING):STRING;

      FUNCTION  CASHNINT(info,   pe,x1,y1,x2,y2,xl:WORD;titel:STRING;default,min,max:INTEGER):INTEGER;

      FUNCTION  CASHREAL(info,   pe,x1,y1,x2,y2,xl:WORD;titel:STRING;default:   REAL):REAL;

      FUNCTION  CASHDATE(info,pa,pe,x1,y1,x2,y2   :WORD;titel,       default: STRING):STRING;

      FUNCTION  CASHTIME(info,pa,pe,x1,y1,x2,y2   :WORD;titel,       default: STRING):STRING; FUNCTION  SIGN:BYTE;

      FUNCTION  CASHBOOL(info,   pe,x1,y1,x2,y2   :WORD;titel:STRING;default:BOOLEAN):BOOLEAN;

      FUNCTION  CASHBOOL2(info,  pe,x1,y1,x2,y2   :WORD;titel,default:STRING):STRING;

   IMPLEMENTATION

      { Layoutfarben setzen }

        PROCEDURE SetLayout;
           VAR Lauf,C: INTEGER;
           BEGIN
             DA:=DAp;
             Lauf:=1;
             FOR Modus:=i TO di DO BEGIN
               VAL(COPY(UMp,Lauf,2),UM[Modus],C);
               VAL(COPY(HGp,Lauf,2),HG[Modus],C);
               INC(Lauf,3)
             END;
             FOR Lauf:=1 TO 255 DO
               MA[Lauf]:=En;   { Default Zugriffsrecht "alle Enabled" }
             ComeBack:=1       { Default Rckschritt auf "1"          }
           END;

      { Feldinhaltsfarben setzen }

        PROCEDURE SetText;
           VAR Lauf,C: INTEGER;
           BEGIN
             Lauf:=1;
             FOR Modus:=i TO di DO BEGIN
               VAL(COPY(IVp,Lauf,2),IV[Modus],C);
               VAL(COPY(IHp,Lauf,2),IH[Modus],C);
               INC(Lauf,3)
             END
           END;

      { Cursorfarben setzen }

        PROCEDURE SetCursor;
           VAR Lauf,C: INTEGER;
           BEGIN
             Lauf:=1;
             FOR Modus:=i TO di DO BEGIN
               VAL(COPY(CVp,Lauf,2),CV[Modus],C);
               VAL(COPY(CHp,Lauf,2),CH[Modus],C);
               VAL(COPY(CTp,Lauf,2),CT[Modus],C);
               INC(Lauf,3)
             END
           END;

      { Titelfarben setzen }

        PROCEDURE SetTitel;
           VAR Lauf,C: INTEGER;
           BEGIN
             Lauf:=1;
             FOR Modus:=i TO di DO BEGIN
               VAL(COPY(TVp,Lauf,2),TV[Modus],C);
               VAL(COPY(THp,Lauf,2),TH[Modus],C);
               INC(Lauf,3)
             END
           END;

      { Ausgabe eines Feldbezeichners }

        PROCEDURE PrintTitel(XT,YT:WORD;Titel:STRING;M:Modo);
           BEGIN
             SETCOLOR      (TV[M]);
             SETFILLSTYLE(1,TH[M]);
             BAR(XT-2,YT-4,XT+LENGTH(Titel)*8+2,YT+10);
             OUTTEXTXY(XT,YT,Titel)
           END;

      { Ausgabe eines Feldinhaltes }

        PROCEDURE PrintInhalt(XI,YI,XL:WORD;Inhalt:STRING;M:Modo);
           BEGIN
             IF (LENGTH(Inhalt) > 0) THEN BEGIN
               SETCOLOR      (IV[M]);
               SETFILLSTYLE(1,IH[M]);
               BAR(XI-2,YI-4,XI+LENGTH(Inhalt)*8+2,YI+10);
               OUTTEXTXY(XI,YI,Inhalt);
               SETFILLSTYLE(1,HG[M]);
               IF (XL > LENGTH(Inhalt)) THEN
                 BAR(XI+LENGTH(Inhalt)*8+2,YI-4,XI+XL*8,YI+10)
             END
           END;

      { Ausgabe eines Cursors }

        PROCEDURE PrintCursor(S:Schalter;Xa,Ya,Xp:WORD;Inhalt:STRING;M:Modo);

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
               On:  BEGIN
                      SETCOLOR      (CT[M]);
                      SETFILLSTYLE(1,CH[M])
                    END;
               Off: BEGIN
                      SETCOLOR      (IV[M]);
                      SETFILLSTYLE(1,IH[M])
                    END
             END;
             BAR(Xa+(Xp-1)*8,Ya-4,Xa+Xp*8-1,Ya+10);
             OUTTEXTXY(Xa+(Xp-1)*8,Ya,COPY(Inhalt,Xp,1));
             IF (DA = Extra) THEN BEGIN
               CASE S OF
                 On:  C:=CV[M];
                 Off: C:=IH[M]
               END;
               SETCOLOR      (C);
               SETFILLSTYLE(1,C);
               CASE M OF
                 i:   CursorInsert(Xa,Ya,Xp,C);
                 o,a: CursorOverwrite(Xa,Ya,Xp)
               END
             END;
           END;

        FUNCTION Sign:BYTE;
           BEGIN
             IF (Sign1 = #0) THEN
               Sign:=ORD(Sign2)+100
             ELSE
               Sign:=ORD(Sign1)
           END;

      { Eingabefelder auf Bildschirm vorlegen }

        PROCEDURE CASHBACK;

           { Feldlayout fr normal }

             PROCEDURE BackNormal(Xa,Xl,Ya:WORD;M:Modo);
                VAR Xe: WORD;
                BEGIN
                  Xe:=Xa+Xl*8;
                  SETFILLSTYLE(1,HG[M]);
                  BAR(Xa-2,Ya- 4,Xe+2,Ya+10)
                END;

           { Feldlayout fr abgerundet }

             PROCEDURE BackExtra (Xa,Xl,Ya:WORD;M:Modo);
                VAR Xe: WORD;
                BEGIN
                  Xe:=Xa+Xl*8;
                  SETCOLOR      (UM[M]);
                  SETFILLSTYLE(1,HG[M]);
                  PIESLICE(Xa,ya+ 3,  180,  270, 8);
                  LINE    (Xa,ya- 5,Xe   ,ya- 5);
                  PIESLICE(Xe,ya+ 3,    0,   90, 8);
                  LINE    (Xa,ya+11,Xe   ,ya+11);
                  PIESLICE(Xa,ya+ 3,   90,  180, 8);
                  PIESLICE(Xe,ya+ 3,  270,  360, 8);
                  BAR     (Xa,ya- 4,Xe   ,ya+10);
                  SETCOLOR      (HG[M]);
                  LINE    (Xa-7,Ya+3,Xe+7,Ya+3)
                END;

           BEGIN
             IF (Full = TRUE) THEN BEGIN
               CASE DA OF
                 Normal: BackNormal(XI,Xl,YI,M);
                 Extra:  BackExtra (XI,Xl,YI,M)
               END;
               PrintTitel (XT,YT,Titel,M)
             END;
             PrintInhalt(XI,YI,XL,Inhalt,M);
             CASE M OF
               i,o,a: PrintCursor(On,XI,YI,CP,Inhalt,M)
             END;
           END;

    { Einfgen eines Zeichens in einen String abh„ngig von MODUS }

      FUNCTION  PutIn(Text:STRING;cp,xl:BYTE):STRING;
         VAR P:      BYTE;
             PartA,
             PartB: STRING;
         BEGIN
           IF Modus = i THEN P:=0
                        ELSE P:=1;
           PartA:=COPY(text,   1,cp-1);
           PartB:=COPY(text,cp+P,xl-cp-P+1);
           Text:=COPY(PartA+Sign1+PartB,1,xl);
           PutIn := Text
         END;

    FUNCTION  CASHWORK(info,pe,x1,y1,x2,y2,xl,xs:WORD;titel,default:STRING):STRING;
       VAR   ende:     BOOLEAN;
             MText,
             Text:     STRING;
             Npos,
             cp:       BYTE;
       BEGIN
         ende   := FALSE;
         Text   := Default;
         IF (xs = 0) THEN
           cp     := LENGTH(text)+1
         ELSE
           cp     := xs;
         IF cp > xl THEN cp:=xl;
         IF (AktFunk = Eingabe) THEN BEGIN
          Modus:=StartModus;
          IF (MA[xpos] = Di) THEN BEGIN
            CASE Sign1 OF
               #0: CASE Sign2 OF
                     #15,
                     #72,
                     #75: DEC(Xpos);
                     #77,
                     #80: INC(Xpos)
                   END;
               #9,
              #13: INC(Xpos)
            ELSE IF (Sign1 IN MLinks) OR (Sign2 IN MRechts) THEN
                   INC(Xpos)
            END;
            IF (Xpos <  1) THEN Xpos:=pe;
            IF (Xpos > pe) THEN Xpos:= 1;
            CASHWORK := Default
          END ELSE BEGIN
           Mpos:=Xpos;
           Sign1:=#0;
           Sign2:=#0;
           CASHBACK(TRUE,CP,x1,y1,x2,y2,xl,titel,text,Modus);
           REPEAT
             IF (MText <> Text) THEN
               CASHBACK(FALSE,CP,x1,y1,x2,y2,xl,titel,text,Modus)
             ELSE
               PrintCursor(On,X2,Y2,CP,Text,Modus);
             MText:=Text;
             Sign1:=READKEY;
             IF NOT(Sign1 IN MLinks) THEN BEGIN

               { Spezialfall Modus A (Overwrite all) }

                 IF (Modus = a) THEN BEGIN
                   IF (Sign1 <> #0) THEN BEGIN
                     cp:=1;
                     text:=''
                   END;
                   Modus:=i;
                   CASHBACK(TRUE,CP,x1,y1,x2,y2,xl,titel,text,Modus)
                 END;

               PrintCursor(Off,X2,Y2,CP,Text,Modus);
               CASE Sign1 OF
                  #0: BEGIN
                        Sign2:=READKEY;
                        IF NOT(Sign2 IN MRechts) THEN
                          CASE Sign2 OF
                            { #25: GraphToBitMapFile(0,0,639,479); }
                            { #59: SetInfos(info,FALSE,TRUE); }
                            #60..#68: ende:=TRUE;
                            #71: cp:=1;
                            #72: BEGIN
                                   XPos:=((Xpos DIV 20)-1) * 20 + 1;
                                   IF (Xpos < 1) THEN XPos:=1;
                                 END;
                            #75: IF cp >  1 THEN DEC(cp);
                            #77: IF (cp <= LENGTH(text)) AND (cp < xl) THEN
                                   INC(cp);
                            #79: IF LENGTH(text) < xl THEN
                                   cp:=LENGTH(text)+1
                                 ELSE
                                   cp:=LENGTH(text);
                            #82: IF (Modus <> i) THEN Modus := i
                                                 ELSE Modus := o;
                            #83: DELETE(text,cp,1);
                            #104..#113: Ende:=TRUE
                          ELSE WRITE(CHR(7))
                          END
                        ELSE BEGIN
                          CASE Sign2 OF
                            #15,#72: IF xpos > 1 THEN
                                       DEC(xpos)
                                     ELSE xpos:=pe;
                            #80:     IF xpos < pe THEN
                                       INC(xpos)
                                     ELSE xpos:=1
                          END;
                          ende:=TRUE
                        END
                      END;
                  #8: IF (cp > 1) THEN BEGIN
                        DELETE(text,cp-1,1);
                        DEC(cp)
                      END;
                #127: BEGIN
                        text:='';
                        cp:=1
                      END;
                 #27: BEGIN
                        Text:=Default;
                        ende:=TRUE
                      END
                 ELSE
                   IF (Sign1 IN MLegal) THEN BEGIN
                     text:=PutIn(text,cp,xl);
                     IF (cp < xl) THEN
                       INC(cp)
                   END ELSE
                     WRITE(CHR(7))
               END
             END ELSE BEGIN
               CASE Sign1 OF
                 #9,
                #13,
                #44: IF xpos < pe THEN
                       INC(xpos)
                     ELSE xpos:=1
               END;
               ende:=TRUE
             END;
           UNTIL ende;
           IF Sign1 = #27 THEN BEGIN
             text   := default;
             Sign2 := #13
           END;
           CASHWORK := text;
           CASHBACK(TRUE,cp,x1,y1,x2,y2,xl,Titel,Text,MA[Mpos])
          END
         END ELSE BEGIN
           CASHBACK(TRUE,CP,x1,y1,x2,y2,xl,Titel,Text,MA[Xpos]);
           INC(Xpos);
           IF (Xpos > pe) AND (EndFunk = Eingabe) THEN BEGIN
             Xpos:=ComeBack;
             Modus    := a;
             AktFunk  := Eingabe
           END;
           CASHWORK := Default
         END
       END;

    FUNCTION  CASHTEXT;
       BEGIN
         MLegal:=MTexter;
         CASHTEXT:=CASHWORK(info,pe,x1,y1,x2,y2,xl,xs,titel,default);
       END;

    FUNCTION  CASHNINT;
       VAR text:   STRING;
           c,memo: INTEGER;
           OK:     BOOLEAN;
       BEGIN
         MLegal:=MCounts;
         OK:=TRUE;
         REPEAT
           IF (default <>  0) THEN STR(default,text)
                              ELSE text:='';
           memo:=xpos;
           text:=CASHWORK(info,pe,x1,y1,x2,y2,xl,0,titel,text);
           VAL(text,default,c);
           IF NOT((default >=min) AND (default <= max)) THEN BEGIN
             IF (default > max) THEN default:=max;
             IF (default < min) THEN default:=min;
             WRITE(CHR(7));
             xpos:=memo
           END ELSE OK:=TRUE
         UNTIL (OK) OR (Sign1 = #27);
{         IF (default = 0) THEN CASHBACK(TRUE,1,x1,y1,x2,y2,xl,titel,'',Modus); }
         CASHNINT:=default
       END;

    FUNCTION  CASHREAL;
       BEGIN
       END;

    FUNCTION  mm(text:STRING):STRING;
       VAR x:INTEGER;
       BEGIN
         FOR x := LENGTH(text)+1 TO 2 DO
           text:='0'+text;
         mm:=text
       END;

    FUNCTION  CASHDATE;
       VAR ok:             BOOLEAN;
           day,month,year: STRING[2];
           lpos,
           tag,monat,jahr: BYTE;
           c:              INTEGER;
       BEGIN
         MLinks  := MLinks  + ['.'];
         MLegal  := MCounts;
         day  :=COPY(default,1,2);
         month:=COPY(default,4,2);
         year :=COPY(default,7,2);
         CASHBACK(TRUE, 0,x1,y1,x2,y2, 8,titel,default,Modus);
         REPEAT
           lpos:=xpos-pa+1;
           CASE lpos OF
             1: REPEAT
                  ok:=TRUE;
                  day := CASHWORK(info,pe,x1,y1,x2  ,y2, 2,0,titel,day);
                  VAL(day,tag,c);
                  IF ((tag < 0) OR (tag > 31)) AND (day <> '') THEN ok:=FALSE;
                UNTIL ok OR (Sign1 = #27);
             2: REPEAT
                  ok:=TRUE;
                  month := CASHWORK(info,pe,x1,y1,x2+24,y2, 2,0,titel,month);
                  VAL(month,monat,c);
                  IF ((monat < 0) OR (monat > 12)) AND (month <> '') THEN ok:=FALSE;
                UNTIL ok OR (Sign1 = #27);
             3: REPEAT
                  ok:=TRUE;
                  year := CASHWORK(info,pe,x1,y1,x2+48,y2, 2,0,titel,year);
                  VAL(year,jahr,c);
                  IF ((jahr < 0) OR (jahr > 99)) AND (Year <> '') THEN ok:=FALSE;
                UNTIL ok OR (Sign1 = #27)
           END;
         UNTIL (Sign1 = #27) OR (Sign2 = #68) OR
               (xpos > pa+2) OR (xpos < pa);
         IF (Sign1 <> #27) THEN default:=mm(day)+'.'+mm(month)+'.'+mm(year);
         CASHDATE := default
       END;

    FUNCTION  CASHTIME;
       VAR ok:                    BOOLEAN;
           stunde,minute,sekunde: STRING[2];
           lpos,
           hour,  minit, second:  BYTE;
           c:                     INTEGER;
       BEGIN
         MLinks  := MLinks  + [':'];
         MLegal  := MCounts;
         stunde   := COPY(default,1,2);
         minute   := COPY(default,4,2);
         sekunde  := COPY(default,7,2);
         CASHBACK(TRUE, 0,x1,y1,x2,y2, 8,titel,default,Modus);
         REPEAT
           lpos:=xpos-pa+1;
           CASE lpos OF
             1: REPEAT
                  ok:=TRUE;
                  stunde := CASHWORK(info,pe,x1,y1,x2  ,y2, 2,0,titel,stunde);
                  VAL(stunde,hour,c);
                  IF ((hour  < 0) OR (hour  > 23)) AND (stunde <> '') THEN ok:=FALSE;
                UNTIL ok;
             2: REPEAT
                  ok:=TRUE;
                  minute := CASHWORK(info,pe,x1,y1,x2+24,y2, 2,0,titel,minute);
                  VAL(minute,minit,c);
                  IF ((minit < 0) OR (minit > 59)) AND (minute <> '') THEN ok:=FALSE;
                UNTIL ok;
             3: REPEAT
                  ok:=TRUE;
                  sekunde := CASHWORK(info,pe,x1,y1,x2+48,y2, 2,0,titel,sekunde);
                  VAL(sekunde,second,c);
                  IF ((second < 0) OR (second > 23)) AND (sekunde <> '') THEN ok:=FALSE;
                UNTIL ok
           END;
         UNTIL (Sign1 =  #27) OR (Sign2 = #68) OR
               (xpos   > pa+2) OR (xpos < pa);
         IF (Sign1 <> #27) THEN default:=mm(stunde)+':'+mm(minute)+':'+mm(sekunde);
         CASHTIME := default
       END;

    FUNCTION  CASHBOOL;
       VAR traeger:STRING[1];
           merke:  BYTE;
       BEGIN
         IF (default = TRUE) THEN
           traeger:='J'
         ELSE
           traeger:='N';
         merke:=xpos;
         REPEAT
           xpos    := merke;
           traeger := CASHWORK(info,pe,x1,y1,x2,y2,1,0,titel,traeger);
         UNTIL (Sign1 = #27) OR
               (traeger = 'J') OR (traeger = 'j') OR
               (traeger = 'N') OR (traeger = 'n');
         IF (Sign1 = #27) THEN
           CASHBOOL:=default
         ELSE
         IF (traeger = 'j') OR (traeger = 'J') THEN
           CASHBOOL:=TRUE
         ELSE
           CASHBOOL:=FALSE
       END;

    FUNCTION  CASHWORK2(info,pe,x1,y1,x2,y2,xl:WORD;titel,default:STRING):STRING;
       VAR   ende:     BOOLEAN;
             MText,
             Text:     STRING;
             Npos,
             cp:       BYTE;
       BEGIN
         ende   := FALSE;
         Text   := Default;
         cp     := LENGTH(text)+1;
         IF cp > xl THEN cp:=xl;
         IF (AktFunk = Eingabe) THEN BEGIN
          Modus:=StartModus;
          IF (MA[xpos] = Di) THEN BEGIN
            CASE Sign1 OF
               #0: CASE Sign2 OF
                     #15,
                     #72,
                     #75: DEC(Xpos);
                     #77,
                     #80: INC(Xpos)
                   END;
               #9,
              #13: INC(Xpos)
            ELSE IF (Sign1 IN MLinks) OR (Sign2 IN MRechts) THEN
                   INC(Xpos)
            END;
            IF (Xpos <  1) THEN Xpos:=pe;
            IF (Xpos > pe) THEN Xpos:= 1;
            CASHWORK2 := Default
          END ELSE BEGIN
           Mpos:=Xpos;
           Sign1:=#0;
           Sign2:=#0;
           CASHBACK(TRUE,CP,x1,y1,x2,y2,xl,titel,text,Modus);
           REPEAT
             IF (MText <> Text) THEN
               CASHBACK(FALSE,CP,x1,y1,x2,y2,xl,titel,text,Modus)
             ELSE
               PrintCursor(On,X2,Y2,CP,Text,Modus);
             MText:=Text;
             Sign1:=READKEY;
             IF NOT(Sign1 IN MLinks) THEN BEGIN

               { Spezialfall Modus A (Overwrite all) }

                 IF (Modus = a) THEN BEGIN
                   IF (Sign1 <> #0) THEN BEGIN
                     cp:=1;
                     text:=''
                   END;
                   Modus:=i;
                   CASHBACK(TRUE,CP,x1,y1,x2,y2,xl,titel,text,Modus)
                 END;

               PrintCursor(Off,X2,Y2,CP,Text,Modus);
               CASE Sign1 OF
                  #0: BEGIN
                        Sign2:=READKEY;
                        IF NOT(Sign2 IN MRechts) THEN
                          CASE Sign2 OF
                            { #59: SetInfos(info,FALSE,TRUE); }
                            #60..#68: ende:=TRUE;
                            #71: cp:=1;
                            #75: IF cp >  1 THEN DEC(cp);
                            #77: IF (cp <= LENGTH(text)) AND (cp < xl) THEN
                                   INC(cp);
                            #79: IF LENGTH(text) < xl THEN
                                   cp:=LENGTH(text)+1
                                 ELSE
                                   cp:=LENGTH(text);
                            #82: IF (Modus <> i) THEN Modus := i
                                                 ELSE Modus := o;
                            #83: DELETE(text,cp,1);
                            #104..#113: Ende:=TRUE
                          ELSE WRITE(CHR(7))
                          END
                        ELSE BEGIN
                          CASE Sign2 OF
                            #15,#72: IF xpos > 1 THEN
                                       DEC(xpos)
                                     ELSE xpos:=pe;
                            #80:     IF xpos < pe THEN
                                       INC(xpos)
                                     ELSE xpos:=1
                          END;
                          ende:=TRUE
                        END
                      END;
                  #8: IF (cp > 1) THEN BEGIN
                        DELETE(text,cp-1,1);
                        DEC(cp)
                      END;
                #127,
                 #44: BEGIN
                        text:='';
                        cp:=1
                      END;
                 #27: BEGIN
                        Text:=Default;
                        ende:=TRUE
                      END
                 ELSE
                   IF (Sign1 IN MLegal) THEN BEGIN
                     IF (Sign1 = '+') THEN Sign1:='J';
                     IF (Sign1 = '0') THEN Sign1:='N';
                     text:=PutIn(text,cp,xl);
                     IF (cp < xl) THEN
                       INC(cp)
                   END ELSE
                     WRITE(CHR(7))
               END
             END ELSE BEGIN
               CASE Sign1 OF
                 #9,
                #13,
                #44: IF xpos < pe THEN
                       INC(xpos)
                     ELSE xpos:=1
               END;
               ende:=TRUE
             END;
           UNTIL ende;
           IF Sign1 = #27 THEN BEGIN
             text   := default;
             Sign2 := #13
           END;
           CASHWORK2 := text;
           CASHBACK(TRUE,cp,x1,y1,x2,y2,xl,Titel,Text,MA[Mpos])
          END
         END ELSE BEGIN
           CASHBACK(TRUE,CP,x1,y1,x2,y2,xl,Titel,Text,MA[Xpos]);
           INC(Xpos);
           IF (Xpos > pe) AND (EndFunk = Eingabe) THEN BEGIN
             Xpos:=ComeBack;
             Modus    := a;
             AktFunk  := Eingabe
           END;
           CASHWORK2 := Default
         END
       END;


    FUNCTION  CASHBOOL2;
       VAR traeger:STRING[1];
           merke:  BYTE;
       BEGIN
         MLegal:=MTexter;
         IF (default = 'j') OR (default = 'J') OR (default = '+') THEN
           traeger:='J'
         ELSE IF (default <> '') THEN traeger:='N'
                                 ELSE traeger:='';
         merke:=xpos;
         REPEAT
           xpos    := merke;
           traeger := CASHWORK2(info,pe,x1,y1,x2,y2,1,titel,traeger);
         UNTIL (Sign1 = #27) OR
               (traeger = 'N') OR (traeger = 'J') OR
               (AktFunk = Ausgabe) OR (MA[merke] = Di);
         IF (Sign1 = #27) OR (AktFunk = Ausgabe) THEN CASHBOOL2:=default
                                                 ELSE CASHBOOL2:=traeger
       END;



    PROCEDURE StartCashE;
       BEGIN
         SetLayout (Extra,  '00 00 00 08 08','07 07 07 08 08');
         SetText   (        '00 00 07 00 03','07 07 06 08 08');
         SetTitel  (        '00 00 00 00 03','07 07 07 08 08');
         SetCursor (        '00 00 00 00 00','07 07 07 00 00','00 00 00 00 00');
         MLinks :=[#13,#9];
         MRechts:=[#15,#72,#80];

         XPos       := 1;
         StartModus := A;
         AktFunk    := Ausgabe;
         EndFunk    := Eingabe;
         Sign1:=#32;
         Sign2:=#32
       END;




    PROCEDURE StartCashN;
       BEGIN
         SetLayout (Normal,  '00 00 00 08 08','07 07 07 08 08');
         SetText   (        '00 00 07 00 03','07 07 06 08 08');
         SetTitel  (        '00 00 00 00 03','07 07 07 08 08');
         SetCursor (        '00 00 00 00 00','07 07 07 00 00','00 00 00 00 00');
         MLinks :=[#13,#9];
         MRechts:=[#15,#72,#80];

         XPos       := 1;
         StartModus := A;
         AktFunk    := Ausgabe;
         EndFunk    := Eingabe;
         Sign1:=#32;
         Sign2:=#32
       END;


   END.
{============================
 Versionshistorie
 $Log:$
 ============================}
