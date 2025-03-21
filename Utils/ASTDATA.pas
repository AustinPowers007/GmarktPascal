{============================
 Dateiname:   $Source: $
 Version:     $Revision: $
 Autor:       $Author: $
 Gelockt von: $Locker: $
 Originaldatum : Tue Oct 05 14:25:06 GMT+02:00 1999
 Dateihistorie am Ende der Datei
 ============================}
UNIT ASTDATA;

INTERFACE

   USES  CASHDATA,
         FELDART,
         EFELDER,
         PLACEWIN,
         CASHNEU,
         GRAPNEU;


   TYPE  TSpecialProc = PROCEDURE(Nr:BYTE);

   TYPE  TFAnlegenFunc = FUNCTION(VAR Aktuell:STRING):BOOLEAN;

   TYPE  TGetArtFunc = FUNCTION(VAR ArtNr:STRING; ArrayPos:WORD):BOOLEAN;


   TYPE  TSpecialMenuePtr = ^TSpecialMenue;
         TSpecialMenue = OBJECT(TAuswahlMenue)
                            BeforeAction : TSpecialProc;
                            AfterAction  : TSpecialProc;

                            CONSTRUCTOR Init(za,Farbe1,Farbe2:BYTE; Titel,Info:STRING; BVG,BHG:BYTE;
                                                BPROC,AProc:TSpecialProc);
                            FUNCTION Select(Loeschen:BOOLEAN):INTEGER; VIRTUAL;
                         END;


   TYPE  TKNrFeld = OBJECT(TNumTextFeld)
                       CONSTRUCTOR Init(TiSP,TiZE,ISP,IZE:WORD; Txt:STRING;
                                           Info:STRING; M:TModi; EA:TPrae);
                       FUNCTION MakeInpStr:STRING; VIRTUAL;
                       PROCEDURE MakeVal(Eingabe:STRING); VIRTUAL;
                    END;

   TYPE  TFahrerFeld = OBJECT(TTextFeld)
                          FahrerSearchFenster : TSearchFensterPtr;
                          Fahreranlegen: TFAnlegenFunc;
                          Fahreraendern: TFAnlegenFunc;
                          CONSTRUCTOR Init(TiSP,TiZE,ISP,IZE:WORD; Txt:STRING;
                                              Info:STRING; M:TModi; EA:TPrae;
                                              SFenster:TSearchFensterPtr;
                                              FAFunc:TFanlegenFunc;
                                              FCFunc:TFanlegenFunc);
                          FUNCTION Input:INTEGER; VIRTUAL;
                       END;

   TYPE  TArtNumFeld = OBJECT(TNumTextFeld)
                          GetArt   : TGetArtFunc;
                          ArrayPos : WORD;

                          CONSTRUCTOR Init(TiSP,TiZE,ISP,IZE:WORD; Len:BYTE; Txt:STRING;
                                              Info:STRING; M:TModi; EA:TPrae;
                                              Func:TGetArtFunc; P:WORD);
                          FUNCTION ValidInput(VAR Eingabe:STRING):BOOLEAN; VIRTUAL;
                       END;

   TYPE  TSpecialRealFeld = OBJECT(TRealFeld)
                          GetArt   : TGetArtFunc;
                          ArrayPos : WORD;
                          CONSTRUCTOR Init(TiSP,TiZE,ISP,IZE:WORD;Len:BYTE;Txt:STRING;
                                           Info:STRING;M:TModi;EA:TPrae;
                                           NKo:BYTE;Mi,Ma:REAL;Gfunc:TGetArtFunc;P:WORD);
                          FUNCTION ValidInput(VAR Eingabe:STRING):BOOLEAN; VIRTUAL;
                       END;

   TYPE TASFEnsterPtr = ^TASFenster;
        TASFenster = OBJECT(TEingabeFenster)
                         CONSTRUCTOR Init(x1,y1,x2,y2:WORD; Za:BYTE; Titel,Info:STRING;
                                             AnfZust:TFensterZustand);
                         PROCEDURE AddKnr(Itk:STRING;K:STRING; TX:STRING; SP,ZE:BYTE; VAR Def;
                                             Info:STRING); VIRTUAL;
                         PROCEDURE AddArtNum(Itk:STRING;K:STRING; TX:STRING; SP,ZE:BYTE; VAR Def;
                                                Info:STRING; Len:BYTE;
                                                GFunc:TGetArtFunc;P:WORD); VIRTUAL;
                         PROCEDURE AddSpecialReal(Itk:STRING;K:STRING; TX:STRING; SP,ZE:BYTE; VAR Def;
                                                  Info:STRING; Len,Nko:BYTE; UG,OG:REAL;
                                                  GFunc:TGetArtFunc;P:WORD); VIRTUAL;
                         PROCEDURE AddFahrerFeld(Itk:STRING;K:STRING; TX:STRING; SP,ZE:BYTE; VAR Def;
                                                    Info:STRING; Len:BYTE;
                                                    SFenster:TSearchFensterPtr;
                                                    FAFunc:TFanlegenFunc;
                                                    FCFunc:TFanlegenFunc); VIRTUAL;
                      END;


IMPLEMENTATION
      USES ZUSAETZE;

      FUNCTION EscLeave(VAR Taste:INTEGER;VAR Nr:BYTE;VAR InpString:STRING;
                           VAR Abbruch,Check,MoreEval:BOOLEAN):BOOLEAN;FAR;
         BEGIN
            MoreEval := FALSE;
            Taste := -1;
            Abbruch := FALSE;
            Check := FALSE;
            EscLeave := TRUE;
         END;

      CONSTRUCTOR TSpecialMenue.Init;
         BEGIN
            inherited Init(za,Farbe1,Farbe2,Titel,Info,BVG,BHG);
            BeforeAction := BPROC;
            AfterAction  := APROC;
         END;

      FUNCTION TSpecialMenue.Select;
         VAR   Ende,
               Ergebnis,
               Angezeigt : BOOLEAN;
               EndCode : INTEGER;
               Auswahl : BYTE;
         BEGIN
            Ende := FALSE; 
            Angezeigt := FALSE; 
            REPEAT
               EndCode := MakeChoice(Angezeigt);
               Auswahl := GetPos;
               Angezeigt := TRUE;
               IF EndCode = 13 THEN BEGIN
                  Angezeigt := FALSE;
                  Hide;
                  BeforeAction(Auswahl);
                  Ergebnis := Action(Auswahl,EndCode);
                  AfterAction(Auswahl); 
               END; 
               IF (NOT Ergebnis) OR (EndCode = 27) OR (EndCode = -68) THEN
                  Ende := TRUE; 
            UNTIL Ende;
            IF Loeschen THEN
               Hide; 
            Select := EndCode; 
         END; 
      
      CONSTRUCTOR TKNrFeld.Init;

         BEGIN
            inherited Init(TiSP,TiZE,ISP,IZE,15,Txt,Info,M,EA);
            Maske :='     -   -     '; 
         END;
      
      FUNCTION TKNrFeld.MakeInpStr;

         BEGIN
            IF Wert^ = '' THEN
               MakeInpStr := Maske
            ELSE
               MakeInpStr:=COPY(Wert^,1,5)+'-'+COPY(Wert^,6,3)+
               '-'+COPY(Wert^,9,5);
         END; 
      
      PROCEDURE TKNrFeld.MakeVal;
         
         BEGIN
            Wert^:= COPY(Eingabe,1,5)+COPY(Eingabe,7,3)+COPY(Eingabe,11,5);
         END;

      CONSTRUCTOR TSpecialRealFeld.Init;
        BEGIN
          inherited Init(TiSP,TiZE,ISP,IZE,Len,Txt,
                         Info,M,EA,NKo,Mi,Ma);
          GetArt:=GFunc;
          ArrayPos:=P;
        END;

      FUNCTION TSpecialRealFeld.ValidInput;
         BEGIN
           ValidInput := GetArt(Eingabe,ArrayPos)
         END;


      CONSTRUCTOR TFahrerFeld.Init;

         BEGIN
            inherited Init(TiSP,TiZE,ISP,IZE,15,Txt,Info,M,EA);
            Fahreranlegen := FAFunc;
            Fahreraendern := FCFunc;
            FahrerSearchFenster:= SFenster;
            FahrerSearchFenster^.AddActionKey('_',27,EscLeave,FALSE,'');
         END;


      FUNCTION TFahrerFeld.Input;
         VAR   FahrerSik      : STRING;
               FahrerSuchWert : STRING;
         VAR   EndCode : INTEGER;
               InputCode : INTEGER;
               Nr : BYTE;
               MoreEval,
               Ende : BOOLEAN;
               Check : BOOLEAN;
               OldEndCode : INTEGER;
               InpString: STRING;
         BEGIN
            InputCode := 0;
            Abbruch := FALSE;
            Done := FALSE;
            IF NOT Disabled('') THEN BEGIN
               IF FahrerSearchFenster^.StartUp THEN BEGIN
                  Ende := FALSE;
                  FahrerSuchWert := Wert^;
                  IF (COPY(FahrerSuchWert,1,5) = 'Herr ') THEN
                     DELETE(FahrerSuchWert,1,5);
                  REPEAT
                     FahrerSearchFenster^.StartUp;
                     FahrerSearchFenster^.SetValue(FahrerSuchWert);
                     EndCode:=FahrerSearchFenster^.Input;
                     IF (EndCode = -1) THEN BEGIN
                        Ende := TRUE;
                        Done := TRUE;
                     END ELSE BEGIN
                        FahrerSik := CutBSpaces(FahrerSearchFenster^.GetResultText);
                        Ende := KeyAction(EndCode,Nr,InpString,Abbruch,Done,Check,MoreEval);
                        KeyDone := Done AND (NOT MoreEval);
                        IF (NOT Done) THEN BEGIN
                           CASE EndCode OF
                              -68..-63,
                              -60,-59,
                              -113..-84 : BEGIN
                                             Ende := TRUE;
                                             Done := TRUE;
                                          END;
                              -61       : BEGIN
                                             FahrerSik := CutBSpaces(FahrerSearchFenster^.GetResultText);
                                             IF FahrerAnlegen(FahrerSuchWert) THEN BEGIN
                                                FahrerSearchFenster^.StartUp;
                                                FahrerSearchFenster^.SetValue(FahrerSuchWert);
                                             END ELSE
                                                FahrerSuchWert:= FahrerSik;
                                          END;
                              -62       : BEGIN
                                             FahrerSik := CutBSpaces(FahrerSearchFenster^.GetResultText);
                                             FahrerSuchWert:=FahrerSik;
                                             IF FahrerAendern(FahrerSuchWert) THEN BEGIN
                                                FahrerSearchFenster^.StartUp;
                                                FahrerSearchFenster^.SetValue(FahrerSuchWert);
                                             END ELSE
                                                FahrerSuchWert:= FahrerSik;
                                          END;
                              13        : BEGIN
                                             FahrerSuchWert := CutBSpaces(FahrerSearchFenster^.GetResultText);
                                             IF (COPY(FahrerSuchWert,1,5) <> 'Herr ') AND
                                                (CutBSpaces(FahrerSuchWert) <> '') THEN
                                                FahrerSuchWert := 'Herr '+FahrerSuchWert;
                                             Wert^:= FahrersuchWert;
                                             Ende := TRUE;
                                          END;
                              27        : BEGIN
                                             Ende := TRUE;
                                             EndCode := 0;
                                          END;
                              -15,9     : BEGIN
                                             Ende := TRUE;
                                          END;
                              ELSE        FahrerSuchWert:= FahrerSik;
                           END;
                        END;
                     END;
                  UNTIL Ende;
                  FahrerSearchFenster^.Hide;
               END ELSE
                  EndCode := 0;
               InputCode := EndCode;
            END;
            Refresh('');
            Input := InputCode;
         END;


      CONSTRUCTOR TArtNumFeld.Init;

         BEGIN
            inherited Init(TiSP,TiZE,ISP,IZE,Len,Txt,Info,M,EA);
            GetArt := Func;
            ArrayPos := P;
         END;

      FUNCTION TArtNumFeld.ValidInput;

         BEGIN
            IF (S2L(Eingabe) <> S2L(Wert^)) THEN BEGIN
               IF (S2L(Eingabe) <> 0 ) THEN
                  ValidInput := GetArt(Eingabe,ArrayPos)
               ELSE
                  ValidInput := TRUE;
            END
            ELSE
               ValidInput := TRUE;
         END;

      CONSTRUCTOR TASFenster.Init;

         BEGIN
            inherited Init(x1,y1,x2,y2,Za,Titel,Info,AnfZust);
         END;


      PROCEDURE TASFenster.AddArtNum;
         VAR   Zeiger : ^TArtNumFeld;
               ItkF,
               ItkR : STRING;

         BEGIN
            NEW(Zeiger);
            Zeiger^.Init(XPoint(SP),YPoint(ZE),XPoint(SP+LENGTH(TX)+1)+4,
            YPoint(ZE),Len,Tx,InfoTx(Info),I,Extra,GFunc,P);
            IF (Itk <> '') THEN BEGIN
               SplitString(Itk,'_',ItkF,ItKR);
               AddSubField(ItkF,ItkR,K,Zeiger,Def)
            END ELSE
               AddNew(K,Zeiger,Def);
         END;


      PROCEDURE TASFenster.AddSpecialReal;
         VAR Zeiger: ^TSpecialRealFeld;
               ItkF,
               ItkR : STRING;

         BEGIN
            NEW(Zeiger);
            Zeiger^.Init(XPoint(SP),YPoint(ZE),XPoint(SP+LENGTH(TX)+1)+4,YPoint(ZE),
            Len,Tx,InfoTx(Info),I,Extra,Nko,UG,OG,GFunc,P);
            IF (Itk <> '') THEN BEGIN
               SplitString(Itk,'_',ItkF,ItKR);
               AddSubField(ItkF,ItkR,K,Zeiger,Def)
            END ELSE
               AddNew(K,Zeiger,Def);
         END;


      PROCEDURE TASFenster.AddKnr;
         VAR   Zeiger : ^TKNrFeld;
               ItkF,
               ItkR : STRING;


         BEGIN
            NEW(Zeiger);
            Zeiger^.Init(XPoint(SP),YPoint(ZE),XPoint(SP+LENGTH(TX)+1)+4,
            YPoint(ZE),Tx,InfoTx(Info),I,Extra);
            IF (Itk <> '') THEN BEGIN
               SplitString(Itk,'_',ItkF,ItKR);
               AddSubField(ItkF,ItkR,K,Zeiger,Def)
            END ELSE
               AddNew(K,Zeiger,Def);
         END;

      PROCEDURE TASFenster.AddFahrerFeld;
         VAR   Zeiger : ^TFahrerFeld;
               ItkF,
               ItkR : STRING;


         BEGIN
            NEW(Zeiger);
            Zeiger^.Init(XPoint(SP),YPoint(ZE),XPoint(SP+LENGTH(TX)+1)+4,
            YPoint(ZE),Tx,InfoTx(Info),I,Extra,SFenster,FAFunc,FCFunc);
            IF (Itk <> '') THEN BEGIN
               SplitString(Itk,'_',ItkF,ItKR);
               AddSubField(ItkF,ItkR,K,Zeiger,Def)
            END ELSE
               AddNew(K,Zeiger,Def);
         END;


END.
{============================
 Versionshistorie
 $Log:$
 ============================}
