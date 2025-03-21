{============================
 Dateiname:   $Source: $
 Version:     $Revision: $
 Autor:       $Author: $
 Gelockt von: $Locker: $
 Originaldatum : Thu May 08 16:35:54 GMT+02:00 1997
 Dateihistorie am Ende der Datei
 ============================}
UNIT CASHNEU;
   {$O+}
INTERFACE

   USES CASHDATA,
        GRAPNEU,
        FELDART,
        PLACEWIN,
        EFELDER,
        ZUSAETZE,
        FARBEN;



   TYPE TScrollFunc = FUNCTION(Direction:INTEGER):BOOLEAN;
   TYPE TSearchStartEndFunc = FUNCTION(Start:BOOLEAN;VAR ErrMsg:STRING):BOOLEAN;
   TYPE TSearchFunc = FUNCTION(Eingabe:STRING;MaxRow:BYTE):BOOLEAN;
   TYPE TOutputFunc = FUNCTION(Zeile:BYTE):STRING;
   TYPE TSearchScrollFunc = FUNCTION(Direction:INTEGER;MaxRow:BYTE):INTEGER;
      { -1 : Einen zurck (MaxRow letztes zu scrollendes Element)
        -2 : Zum Anfang (in Element 1 speichern)
         0 : Gibt MIN (Anzahl der Elemente, MaxRow) zurck
         1 : Einen vor (MaxRow h”chstes zu scrollendes Element)
         2 : Ende (in Element 1 speichern) }

        TMaskZgr = ^TMask;

        TMask = RECORD
                   Nachfolger         : TMaskZgr;
                   Kennung            : STRING;
                   Typ                : BYTE;
                   Txt                : STRING;
                   LongVar            : ^LONGINT;
                   RealVar            : ^REAL;
                   StringVar          : ^STRING;
                   ExtraHoehe,
                   ExtraBreite,
                   TxFarbe,
                   HgFarbe            : BYTE;
                   Zeile,Spalte       : BYTE;
                   EndZeile,EndSpalte : INTEGER;
                END;

        TAuswahlMenuePtr= ^TAuswahlMenue;
        TAuswahlMenue = OBJECT (TAuswahlFenster)
           CONSTRUCTOR Init(za,Farbe1,Farbe2:BYTE;Titel,Info:STRING;
                              BVG,BHG:BYTE);
           PROCEDURE MenuItem(Kennung:STRING;IText:STRING;Func:TMenuFunc);

           FUNCTION  Action(Nr:BYTE;VAR Taste:INTEGER):BOOLEAN;VIRTUAL;
           FUNCTION Select(Loeschen:BOOLEAN):INTEGER;VIRTUAL;
        END;

        TEingabeFensterPtr= ^TEingabeFenster;
        TEingabeFenster = OBJECT(TBrainFenster)
           Beschreibung      : STRING;
           TitelNr           : ^LONGINT;
           CheckFuncActive   : BOOLEAN;
           CheckFunc         : TBoolFunc;
           AInputFuncActive  : BOOLEAN;
           AInputFunc        : TBoolFunc;
           DOBrain           : BOOLEAN;
           FensterFarben     : ^TFarben;
           FensterInfo       : STRING[8];
           Zustand           : TFensterZustand;
           LetzteTaste       : INTEGER;
           MaskenListe       : TMaskZgr;

           CONSTRUCTOR Init(x1,y1,x2,y2:WORD; Za:BYTE; Titel,Info:STRING;
                            AnfZust:TFensterZustand);
           DESTRUCTOR RELEASE; VIRTUAL;
           PROCEDURE DisposeFields(K:STRING);VIRTUAL;
           PROCEDURE DisposeConst(K:STRING);VIRTUAL;
           PROCEDURE SetTitel(TTxt:STRING);VIRTUAL;
           PROCEDURE SetTitelNr(VAR Wert);VIRTUAL;
           PROCEDURE SetCheckFunc(Func:TBoolFunc);VIRTUAL;
           PROCEDURE SetAInputFunc(Func:TBoolFunc);VIRTUAL;
           FUNCTION  Input:INTEGER;VIRTUAL;
           FUNCTION  InputWithClear:INTEGER; VIRTUAL;
           FUNCTION  InputWithBrain:INTEGER; VIRTUAL;

           PROCEDURE AddConst(K:STRING; SP,ZE:BYTE; TX:STRING); VIRTUAL;
           PROCEDURE AddCLong(K:STRING; SP,ZE:BYTE; VAR LVar:LONGINT;Stellen:BYTE); VIRTUAL;
           PROCEDURE AddCReal(K:STRING; SP,ZE:BYTE;VAR RVar:REAL; Stellen,NachK:BYTE); VIRTUAL;
           PROCEDURE AddCString(K:STRING; SP,ZE:BYTE;VAR SVar ); VIRTUAL;
           PROCEDURE AddVLine(K:STRING; SP,ZEA,ZEE:BYTE;XDif:INTEGER); VIRTUAL;
           PROCEDURE AddHLine(K:STRING; SPA,SPE,ZE:BYTE;YDif:INTEGER); VIRTUAL;
           PROCEDURE AddCrossPoint(K:STRING;SP,ZE,TYP:BYTE);VIRTUAL;
           PROCEDURE SetCColor(K:STRING;TxF,HGF:BYTE);VIRTUAL;
           PROCEDURE SetCHeight(K:STRING;CH:BYTE);VIRTUAL;
           PROCEDURE SetCWidth(K:STRING;CW:BYTE);VIRTUAL;
           PROCEDURE AddString(ItK:STRING;K:STRING; TX:STRING; SP,ZE:BYTE; VAR Def;
                               Info:STRING; Len:BYTE); VIRTUAL;
           PROCEDURE AddChar(ItK:STRING;K:STRING; TX:STRING; SP,ZE:BYTE; VAR Def;
                               Info:STRING); VIRTUAL;
           PROCEDURE AddTabZeile(ItK:STRING;K:STRING; TX:STRING; SP,ZE:BYTE;VAR Def;
                                 Info:STRING); VIRTUAL;
           PROCEDURE AddInt(ItK:STRING;K:STRING; TX:STRING; SP,ZE:BYTE; VAR Def;
                            Info:STRING; Len:BYTE; UG,OG:INTEGER); VIRTUAL;
           PROCEDURE AddByte(ItK:STRING;K:STRING; TX:STRING; SP,ZE:BYTE; VAR Def;
                             Info:STRING; Len:BYTE; UG,OG:BYTE); VIRTUAL;
           PROCEDURE AddWord(ItK:STRING;K:STRING; TX:STRING; SP,ZE:BYTE; VAR Def;
                             Info:STRING; Len:BYTE; UG,OG:WORD); VIRTUAL;
           PROCEDURE AddLongInt(ItK:STRING;K:STRING; TX:STRING; SP,ZE:BYTE; VAR Def;
                                Info:STRING; Len:BYTE; UG,OG:LONGINT); VIRTUAL;
           PROCEDURE AddReal(ItK:STRING;K:STRING; TX:STRING; SP,ZE:BYTE; VAR Def;
                             Info:STRING; Len,Nko:BYTE; UG,OG:REAL); VIRTUAL;
           PROCEDURE AddDate(ItK:STRING;K:STRING; TX:STRING; SP,ZE:BYTE; VAR Def;
                             Info:STRING); VIRTUAL;
           PROCEDURE AddTime(ItK:STRING;K:STRING; TX:STRING; SP,ZE:BYTE; VAR Def;
                             Info:STRING); VIRTUAL;
           PROCEDURE AddBool(ItK:STRING;K:STRING; TX:STRING; SP,ZE:BYTE; VAR Def;
                             Info:STRING); VIRTUAL;
           PROCEDURE AddSwitch(ItK:STRING;K:STRING;TX:STRING;SP,ZE:BYTE;VAR Def;
                               Info:STRING);VIRTUAL;
           PROCEDURE AddRoll(ItK:STRING;K:STRING; TX:STRING; SP,ZE:BYTE; VAR Def;
                             Info:STRING; Len:BYTE); VIRTUAL;
           PROCEDURE AddChoice(ItK:STRING;K:STRING; TX:STRING; SP,ZE:BYTE; VAR Def;
                               Info:STRING; Len:BYTE; FTxt:STRING); VIRTUAL;
           PROCEDURE AddKw(ItK:STRING;K:STRING; TX:STRING; SP,ZE:BYTE; VAR Def;
                           Info:STRING); VIRTUAL;
           PROCEDURE AddNumText(ItK:STRING;K:STRING; TX:STRING; SP,ZE:BYTE; VAR Def;
                                Info:STRING; Len:BYTE); VIRTUAL;
           PROCEDURE AddMitNull(ItK:STRING;K:STRING; TX:STRING; SP,ZE:BYTE; VAR Def;
                                Info:STRING; Len:BYTE;Min,Max:LONGINT); VIRTUAL;
           PROCEDURE ShowKeyHelp;
           FUNCTION  GetFocusID:STRING;VIRTUAL;
           PROCEDURE SetFocusOnId(ItK:STRING);VIRTUAL;
           PROCEDURE AddItem(ItK:STRING; Tx:STRING); VIRTUAL;
           FUNCTION  ItemDisabled(K:STRING;It:WORD):BOOLEAN; VIRTUAL;
           PROCEDURE EnableItem(ItK:STRING;It:WORD); VIRTUAL;
           PROCEDURE DisableItem(ItK:STRING;It:WORD); VIRTUAL;
           PROCEDURE SetToggle(K:STRING;VAL:BOOLEAN);VIRTUAL;
           PROCEDURE Disable(K:STRING); VIRTUAL;
           PROCEDURE Enable(K:STRING); VIRTUAL;
           FUNCTION  Disabled(ItK:STRING):BOOLEAN; VIRTUAL;
           PROCEDURE SetVal(ItK:STRING;ValString:STRING);VIRTUAL;
           FUNCTION  GetVal(ItK:STRING):STRING;VIRTUAL;
           PROCEDURE AlignFields; VIRTUAL;
           PROCEDURE Show(K:STRING); VIRTUAL;
           PROCEDURE ShowConst(K:STRING); VIRTUAL;
           PROCEDURE ClearConst(K:STRING); VIRTUAL;
           PROCEDURE ShowFields(K:STRING); VIRTUAL;
           PROCEDURE Refresh(K:STRING); VIRTUAL;
           FUNCTION  GetLastKey:INTEGER; VIRTUAL;
           PROCEDURE Save   (K:STRING);VIRTUAL;
           PROCEDURE Restore(K:STRING);VIRTUAL;
           FUNCTION  Changed (K:STRING):BOOLEAN; VIRTUAL;
           FUNCTION  Valid(K:STRING):BOOLEAN; VIRTUAL;
           PROCEDURE SetCRQuit(Val:BOOLEAN);VIRTUAL;
           PROCEDURE SetFKeyQuit(Val:BOOLEAN);VIRTUAL;

           FUNCTION XPoint(SP:BYTE):WORD; VIRTUAL;
           FUNCTION YPoint(ZE:BYTE):WORD; VIRTUAL;
           FUNCTION InfoTx(Text:STRING):STRING; VIRTUAL;
           PRIVATE
               PROCEDURE AddLine(K:STRING;ZEA,SPA:BYTE;ZEE,SPE:INTEGER;LTyp:BYTE);
               PROCEDURE AddCPtr(Neu:TMaskZgr);
        END;

   TYPE TTabFensterPtr = ^TTabFenster;
        TTabFenster = OBJECT(TEingabeFenster)
           ScrollTab : TScrollFunc;
           MaxTabZeilen,
           TabSpalten,
           TabZeilen : BYTE;
           AktRow    : BYTE;
           AktCol    : BYTE;
           LastEdited: TFeldZgr;

           CONSTRUCTOR Init(x1,y1,x2,y2:WORD; Za:BYTE; Titel,Info:STRING;
                            AnfZust:TFensterZustand; TSP,TZE:BYTE; STab:TScrollFunc);
           FUNCTION  Input:INTEGER;VIRTUAL;
           PROCEDURE SetMaxRows(RNr:BYTE); VIRTUAL;
        END;

   TYPE TSearchFensterPtr = ^TSearchFenster;
        TSearchFenster = OBJECT(TEingabeFenster)
           StartEndeFunc : TSearchStartEndFunc;
           ScrollLine : TSearchScrollFunc;
           SuchString : STRING;
           SearchFunc : TSearchFunc;
           MakeOutStr : TOutputFunc;
           HighLightPos: BOOLEAN;
           ValueSet   : BOOLEAN;
           Gestartet  : BOOLEAN;
           FirstScrollLine : BYTE;
           AktTabZeilen,
           MaxTabZeilen,
           TabZeilen : BYTE;
           AktRow    : BYTE;
           CONSTRUCTOR Init(x1,y1,x2,y2:WORD; Za:BYTE; Titel,Info:STRING;
                            AnfZust:TFensterZustand;STab:TSearchScrollFunc;
                            SFunc:TSearchFunc;OFunc:TOutputFunc;
                            SEFunc:TSearchStartEndFunc);
           PROCEDURE SetSeekInfo(VAL:BOOLEAN);VIRTUAL;
           PROCEDURE SetPosShow(VAL:BOOLEAN);VIRTUAL;
           PROCEDURE SetFirstScrollLine(Zeile:BYTE);
           FUNCTION  StartUp:BOOLEAN;VIRTUAL;
           FUNCTION  EndWin:BOOLEAN;VIRTUAL;

           FUNCTION  Input:INTEGER;VIRTUAL;
           PROCEDURE SetValue(Eingabe:STRING);VIRTUAL;
           PROCEDURE Refresh(K:STRING); VIRTUAL;
           FUNCTION  ResultRow:BYTE; VIRTUAL;
           FUNCTION  GetResultText:STRING; VIRTUAL;
           PRIVATE
              PROCEDURE GetInitialState; VIRTUAL;
              PROCEDURE ScrollToPosition;VIRTUAL;
        END;

   FUNCTION SwitchStatus(St:TFensterZustand):BOOLEAN;

IMPLEMENTATION

   VAR   FirstHelpKey : TFKeyInfoPtr;
         ErsterHelpKey: TFKeyInfoPtr;
         HelpKey      : WORD;
   VAR   ZustandAktiv : TFensterzustand;


   FUNCTION HelpSEFunc(Start:BOOLEAN;VAR Msg:STRING):BOOLEAN;FAR;
         VAR Ergebnis : BOOLEAN;
      BEGIN
         Ergebnis := TRUE;
         IF Start THEN
            HelpKey := 1;
         HelpSEFunc := Ergebnis;
      END;


   FUNCTION SearchHelp(Eingabe:STRING; MaxZeilen:BYTE):BOOLEAN;FAR;
      BEGIN
         SearchHelp :=TRUE;
      END;


   FUNCTION ScrollHelp(Direct:INTEGER; MaxZeilen:BYTE):INTEGER;FAR;
      VAR   HLauf,
            FLauf,
            Lauf : BYTE;
            HKeyLauf : WORD;
            FSEARCH : TFKeyInfoPtr;
      BEGIN
         CASE Direct OF
            0: BEGIN
                  IF (FirstHelpKey = NIL) THEN
                     ScrollHelp := -1
                  ELSE BEGIN
                     FLauf := 1;
                     FSEARCH := FirstHelpKey;
                     WHILE (FLauf < MaxZeilen) AND (FSEARCH^.Next <> NIL) DO
                        INC(FLauf);
                     ScrollHelp := FLauf;
                  END;
               END;
            -2: BEGIN
                   ErsterHelpKey := FirstHelpKey;
                   HelpKey := 1;
                   ScrollHelp := 0;
                END;
            2: BEGIN
                  FLauf := 0;
                  FSEARCH := FirstHelpKey;
                  HelpKey := 1;
                  WHILE (FSEARCH^.Next <> NIL) DO BEGIN
                     INC(FLauf);
                     INC(HelpKey);
                     FSEARCH :=FSEARCH^.Next;
                  END;
                  ErsterHelpKey := FSearch;
                  ScrollHelp := 0;
               END;
            -1: BEGIN
                   IF (HelpKey > 1) THEN BEGIN
                      FSEARCH := FirstHelpKey;
                      HKeyLauf := 1;
                      WHILE (HKeyLauf < (HelpKey-1)) DO BEGIN
                         FSEARCH := FSEARCH^.Next;
                         INC(HKeyLauf);
                      END;
                      DEC(HelpKey);
                      ErsterHelpKey := FSearch;
                      ScrollHelp := 0;
                   END
                   ELSE
                      ScrollHelp :=-1;
                END;
            1: BEGIN
                  HKeyLauf := HelpKey;
                  FLauf := 1;
                  FSEARCH := ErsterHelpKey;
                  IF (FSearch^.Next <> NIL) THEN BEGIN
                     ErsterHelpKey := FSearch^.Next;
                     INC(HelpKey);
                     ScrollHelp := 0;
                  END ELSE
                     ScrollHelp := -1;
               END;
         END;

      END;

   FUNCTION OutPutHelp(Eingabe:BYTE):STRING;FAR;
      VAR Ergebnis: STRING;
          FSearch : TFKeyInfoPtr;
          FLauf   : BYTE;
      BEGIN
         FSearch:= ErsterHelpKey;
         FLauf := 1;
         WHILE (Flauf < Eingabe) AND (FSearch^.Next <> NIL) DO BEGIN
            INC(Flauf);
            FSearch :=FSearch^.Next;
         END;
         IF FLauf = Eingabe THEN
            Ergebnis := PrintString(KeyId(FSearch^.Key),10)+' - '+FSearch^.Info
         ELSE
            Ergebnis := '';
          OutPutHelp :=PrintString(Ergebnis,63);
      END;
         {ษอออออออออออป
         ษออออออออออออออออออออออออออออน Bemerkung ฬอออออออออออออออออออออออออออออออป
         บ                            ศอออออออออออผ                               บ
         บ Aufgabe:     Umschalten des aktiven Status auf den bergeben Status    บ
         บ                                                                        บ
         บ Parameter:   Status:TFensterzustand                                    บ
         บ                                                                        บ
         บ Resultate:   Verndert:BOOLEAN                                         บ
         ศออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ}

   FUNCTION SwitchStatus;

      BEGIN
         IF ZustandAktiv <> St THEN BEGIN
            ZustandAktiv := St;
            SwitchStatus := TRUE
         END
         ELSE
            SwitchStatus := FALSE
      END;

   CONSTRUCTOR TAuswahlMenue.Init;

      BEGIN
         inherited Init(za,Farbe1,Farbe2,Titel,Info,BVG,BHG);
      END;


   PROCEDURE TAuswahlMenue.MenuItem;

      BEGIN
         inherited AddItem(Kennung,IText);
         ItemList[Anzahl]^.Func := FUNC;
         IF Anzahl = 1 THEN
            Wert := 1;
      END;

   FUNCTION TAuswahlMenue.Action;
      VAR Lauf : BYTE;
          Zgr : TAItemPtr;

      BEGIN
         Action := ItemList[Nr]^.FUNC(Nr,Taste);
      END;


   FUNCTION TAuswahlMenue.Select;
      VAR Ende,
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
               Ergebnis := Action(Auswahl,EndCode);
            END;
            IF (NOT Ergebnis) OR Done THEN
               Ende := TRUE;
         UNTIL Ende;
         IF Loeschen THEN
            Hide;
         Select := EndCode;
      END;

   {ษอออออออออออป
   ษออออออออออออออออออออออออออออน Bemerkung ฬอออออออออออออออออออออออออออออออป
   บ                            ศอออออออออออผ                               บ
   บ Aufgabe:     Echte Bildschirmspalte aus Fensterspaltenangabe berechnen บ
   บ                                                                        บ
   บ Parameter:   Fensterspalte:BYTE                                        บ
   บ                                                                        บ
   บ Resultate:   Bildschirmspalte:WORD                                     บ
   ศออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ}

   FUNCTION TEingabeFenster.XPoint;

      BEGIN
         XPoint := ErsteSpalte+SP*8
      END;

   {ษอออออออออออป
   ษออออออออออออออออออออออออออออน Bemerkung ฬอออออออออออออออออออออออออออออออป
   บ                            ศอออออออออออผ                               บ
   บ Aufgabe:     Echte Bildschirmzeile aus Fensterzeilenangabe berechnen   บ
   บ                                                                        บ
   บ Parameter:   Fensterzeile:BYTE                                         บ
   บ                                                                        บ
   บ Resultate:   Bildschirmzeile:WORD                                      บ
   ศออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ}

   FUNCTION TEingabeFenster.YPoint;

      BEGIN
         YPoint := ErsteZeile+ZE*ZeilenAbstand
      END;

   {ษอออออออออออป
   ษออออออออออออออออออออออออออออน Bemerkung ฬอออออออออออออออออออออออออออออออป
   บ                            ศอออออออออออผ                               บ
   บ Aufgabe:     InfoDatei fr Felder festlegen                            บ
   บ                                                                        บ
   บ Parameter:   Infodateiname:STRING, falls '' wird Fensterinfodateiname  บ
   บ              bernommen                                                บ
   บ                                                                        บ
   บ Resultate:   Infodateiname:STRING                                      บ
   ศออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ}

   FUNCTION TEingabeFenster.InfoTx;

      BEGIN
         IF Text = '' THEN InfoTx := FensterInfo
         ELSE InfoTx := Text
      END;

   PROCEDURE TEingabeFenster.ShowKeyHelp;
      VAR ItK :STRING;
          HelpFenster : TSearchFensterPtr;
      BEGIN
        Itk := GetFocusId;
        FirstHelpKey := GetKeyInfoList(Itk);
        StripKeyInfoList(FirstHelpKey);
        IF (FirstHelpKey <> NIL) THEN BEGIN
          IF (MAXAVAIL > SIZEOF(TSearchFenster)) THEN BEGIN
             NEW(HelpFenster);
              HelpFenster^.Init(45,95,595,305,0,'Hilfe - Tastenbelegung','',Aktiv,
                ScrollHelp,SearchHelp,OutputHelp,HelpSEFunc);
              HelpFenster^.AddActionKey('_',-59,DoNothing,FALSE,'');
              HelpFenster^ .SetSeekInfo(FALSE);
              HelpFenster^.SetPosShow(FALSE);
              HelpFenster^.Input;
              HelpFenster^.Hide;
              DISPOSE(HelpFenster,RELEASE);
           END;
        END ELSE BEGIN
          FaultBox.Act(0,'Keine Informationen ber Tastenbelegung gefunden');
        END;
        DisposeKeyInfoList(FirstHelpKey);
     END;



   FUNCTION TEingabeFenster.GetFocusID;
      VAR   Zeiger  : TFeldZgr;
            LAUF  : BYTE;
            HText : STRING;

      BEGIN
         Htext := '';
         IF (FocusField > 0) THEN BEGIN
            Zeiger := FeldListenAnfang;
            FOR Lauf := 1 TO FocusField-1 DO
                Zeiger := Zeiger^.Nachfolger;
            HText:= Zeiger^.Feld^.GetFocusId;
            IF (HText <> '') THEN
               HText := Zeiger^.Feldkennung+'_'+HText
            ELSE
               HText := Zeiger^.FeldKennung;
         END;
         GetFocusId := HText;
      END;

   PROCEDURE TEingabeFenster.SetFocusOnID;
      VAR   Zeiger  : TFeldZgr;
            LAUF  : BYTE;
            Ende  : BOOLEAN;
            ItkF,itKR : STRING;

      BEGIN
         Lauf := 1;
         Zeiger := FeldListenAnfang;
         SplitString(ItK,'_',ItkF,ItkR);
         Ende := FALSE;
         WHILE (Lauf <= AnzahlFelder) AND NOT Ende DO BEGIN
            IF (Zeiger^.FeldKennung = ItKF) AND (ItkR ='') THEN BEGIN
               Ende := TRUE;
               SetFocusField(Lauf);
            END;
            IF (Zeiger^.FeldKennung = ItKF) AND (Zeiger^.Feld^.MultiObj)
               AND (ItKR <>'') THEN BEGIN
              Zeiger^.Feld^.SetFocusOnId(ItkR);
              SetFocusField(Lauf);
              Ende := TRUE;
            END;
            INC(Lauf);
            Zeiger := Zeiger^.Nachfolger;
         END;
      END;

   FUNCTION TEingabeFenster.GetVal;
      VAR   Zeiger  : TFeldZgr;
            LAUF  : BYTE;
            Ergebnis : STRING;
            Ende  : BOOLEAN;
            ItkF,itKR : STRING;

      BEGIN
         Lauf := 1;
         Zeiger := FeldListenAnfang;
         SplitString(ItK,'_',ItkF,ItkR);
         Ende := FALSE;
         WHILE (Lauf <= AnzahlFelder) AND NOT Ende DO BEGIN
            IF (Zeiger^.FeldKennung = ItKF) AND (ItkR ='') THEN BEGIN
               Ende := TRUE;
               Ergebnis :=Zeiger^.Feld^.GetVal(ItkR);
            END;
            IF (Zeiger^.FeldKennung = ItKF) AND (Zeiger^.Feld^.MultiObj)
               AND (ItKR <>'') THEN BEGIN
              Ergebnis:=Zeiger^.Feld^.GetVal(ItkR);
              Ende := TRUE;
            END;
            INC(Lauf);
            Zeiger := Zeiger^.Nachfolger;
         END;
         GetVal := Ergebnis;
      END;

   PROCEDURE TEingabeFenster.SetVal;
      VAR   Zeiger  : TFeldZgr;
            LAUF  : BYTE;
            Ergebnis : STRING;
            Ende  : BOOLEAN;
            ItkF,itKR : STRING;

      BEGIN
         Lauf := 1;
         Zeiger := FeldListenAnfang;
         SplitString(ItK,'_',ItkF,ItkR);
         Ende := FALSE;
         WHILE (Lauf <= AnzahlFelder) AND NOT Ende DO BEGIN
            IF (Zeiger^.FeldKennung = ItKF) AND (ItkR ='') THEN BEGIN
               Ende := TRUE;
               Zeiger^.Feld^.SetVal(ItkR,ValString);
            END;
            IF (Zeiger^.FeldKennung = ItKF) AND (Zeiger^.Feld^.MultiObj)
               AND (ItKR <>'') THEN BEGIN
              Zeiger^.Feld^.SetVal(ItkR,ValString);
              Ende := TRUE;
            END;
            INC(Lauf);
            Zeiger := Zeiger^.Nachfolger;
         END;
      END;
   {ษอออออออออออป
   ษออออออออออออออออออออออออออออน Bemerkung ฬอออออออออออออออออออออออออออออออป
   บ                            ศอออออออออออผ                               บ
   บ Aufgabe:     Eingabefenster mit Initialwerten besetzen                 บ
   บ                                                                        บ
   บ Parameter:   Links,Oben,Rechts,Unten:WORD,Zeilenabstand:BYTE,          บ
   บ              Fenstertitel,Infodatei:STRING,                            บ
   บ              Anfangsstatus:TFensterzustand                             บ
   บ                                                                        บ
   บ Resultate:   Keine                                                     บ
   ศออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ}
   
   CONSTRUCTOR TEingabeFenster.Init;

      VAR   Farbe : BYTE;
            Hg    : STRING;

      BEGIN
         Farbe := 8;
         Hg:='08 08 08';
         IF AnfZust = NichtAktiv THEN BEGIN
            Farbe:=12;
            Hg :='12 12 12'
         END;
         inherited Init(x1,y1,x2,y2,za,Farbe,2,Titel);
         LeaveOnFKey:= TRUE;
         LeaveOnCr  := TRUE;
         TitelNr := NIL;
         Beschreibung := Titel;
         MultiObj := TRUE;
         CheckFuncActive := FALSE;
         AInputFuncActive := FALSE;
         FensterInfo:=Info;
         AnzahlFelder := 0;
         Zustand := AnfZust;
         DOBrain := FALSE;
         LetzteTaste := 0;
         NEW(FeldListenAnfang);
         NEW(FensterFarben);
         FeldListenAnfang^.Nachfolger:=FeldListenAnfang;
         FeldListenAnfang^.Vorgaenger:=FeldListenAnfang;
         Maskenliste := NIL;
         FensterFarben^.SetLayout ('00 00 00 '+Hg,'07 07 07 '+Hg);
         FensterFarben^.SetText   ('00 00 07 00 03 02','07 07 06 '+Hg);
         FensterFarben^.SetTitel  ('02 02 02 00 03 02','07 07 07 '+Hg);
         FensterFarben^.SetCursor ('00 00 00 00 00 00','07 07 07 00 00 00',
         '00 00 00 00 00 00');
      END;

   PROCEDURE TEingabeFenster.SetTitel;
      BEGIN
         Beschreibung := TTxt;
         inherited SetTitel(TTxt);
      END;

   PROCEDURE TEingabeFenster.SetTitelNr;
      BEGIN
         TitelNr:=PTR(SEG(Wert),OFS(Wert));
      END;

   {ษอออออออออออป
   ษออออออออออออออออออออออออออออน Bemerkung ฬอออออออออออออออออออออออออออออออป
   บ                            ศอออออออออออผ                               บ
   บ Aufgabe:     L”schen des Eingabefenster-Objektes                       บ
   บ                                                                        บ
   บ Parameter:   Keine                                                     บ
   บ                                                                        บ
   บ Resultate:   Keine                                                     บ
   ศออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ}

   PROCEDURE TEingabeFenster.DisposeConst;
      VAR   MZeiger1,MZeiger2 : TMaskZgr;

      BEGIN
         MZeiger1:=MaskenListe;
         WHILE MZeiger1<> NIL DO BEGIN
            MZeiger2:=MZeiger1^.Nachfolger;
            DISPOSE(MZeiger1);
            MZeiger1:=MZeiger2;
         END;
         MaskenListe := NIL;
      END;

   PROCEDURE TEingabeFenster.DisposeFields;
      VAR Zeiger1 : TFeldZgr;
          Lauf : BYTE;
      BEGIN
         IF (AnzahlFelder > 1) THEN BEGIN
            Zeiger1 := FeldListenAnfang^.Vorgaenger;
            FOR Lauf := 1 TO AnzahlFelder-1 DO BEGIN
               DISPOSE(Zeiger1^.Feld,Release);
               Zeiger1:=Zeiger1^.Vorgaenger;
               DISPOSE(Zeiger1^.Nachfolger);
            END;
         END;
         IF (AnzahlFelder > 0) THEN BEGIN
           DISPOSE(FeldListenAnfang^.Feld,Release);
           DISPOSE(FeldListenAnfang);
           FeldListenAnfang := NIL;
           AnzahlFelder := 0;
         END;
      END;

   DESTRUCTOR TEingabeFenster.RELEASE;

      BEGIN
         DisposeFields('');
         DisposeConst('');
         inherited RELEASE;
      END;

   PROCEDURE TEingabeFenster.SetCheckFunc;
      BEGIN
         CheckFuncActive := TRUE;
         CheckFunc := Func;
      END;

   PROCEDURE TEingabeFenster.SetAInputFunc;
      BEGIN
         AInputFuncActive := TRUE;
         AInputFunc := Func;
      END;

   PROCEDURE TEingabeFenster.SetCRQuit;
      BEGIN
         LeaveOnCR := VAL;
      END;

   PROCEDURE TEingabeFenster.SetFKeyQuit;
      BEGIN
         LeaveOnFKey := VAL;
      END;


   FUNCTION TEingabeFenster.InputWithClear;

      BEGIN
         SetFocusField(1);
         InputWithClear :=Input;
         ClearBackground;
      END;

   FUNCTION TEingabeFenster.InputWithBrain;

      BEGIN
         SetFocusField(1);
         DOBrain:= TRUE;
         InputWithBrain := Input;
         Hide;
         DOBrain:=FALSE;
      END;

   PROCEDURE TEingabeFenster.Disable;
      VAR   Zeiger  : TFeldZgr;
            Lauf    : BYTE;

      BEGIN
         IF (K = '_') THEN BEGIN
         END ELSE BEGIN
            Zeiger := FeldListenAnfang;
            FOR Lauf := 1 TO AnzahlFelder DO BEGIN
               WITH Zeiger^ DO BEGIN
                  IF (K='') OR (FeldKennung = K) OR (Feld^.MultiObj) THEN
                     Feld^.Disable(K);
                   IF (Feld^.MultiObj) AND ((K = '') AND (K = FeldKennung)) THEN
                      Feld^.Disable('_');
               END;
               Zeiger:=Zeiger^.Nachfolger;
            END
         END;
      END;

   PROCEDURE TEingabeFenster.Enable;
      VAR   Zeiger  : TFeldZgr;
            Lauf    : BYTE;

      BEGIN
         IF (K = '_') THEN BEGIN
         END ELSE BEGIN
            Zeiger := FeldListenAnfang;
            FOR Lauf := 1 TO AnzahlFelder DO BEGIN
               WITH Zeiger^ DO BEGIN
                  IF (K='') OR (FeldKennung = K) OR (Feld^.MultiObj) THEN
                     Feld^.Enable(K);
                  IF (Feld^.MultiObj) AND ((K = '') AND (K = FeldKennung)) THEN
                     Feld^.Enable('_');
               END;
               Zeiger:=Zeiger^.Nachfolger;
            END
         END;
      END;

   FUNCTION  TEingabeFenster.Disabled;
         VAR   Lauf     : BYTE;
               Ergebnis : BOOLEAN;
               Zeiger : TFeldZgr;
               ItkF,
               ItkR   : STRING;

      BEGIN
         Ergebnis := FALSE;
         IF (ItK = '') THEN BEGIN
            Ergebnis := inherited Disabled('');
         END ELSE BEGIN
            Ergebnis := TRUE;
            Lauf := 1;
            Zeiger := FeldListenAnfang;
            SplitString(Itk,'_',ItkF,ItkR);
            WHILE (Lauf <= AnzahlFelder) AND Ergebnis DO BEGIN
               IF (Zeiger^.FeldKennung = ItKF) AND
                  ((ItKR = '') OR (Zeiger^.Feld^.MultiObj)) THEN
                  Ergebnis := Zeiger^.Feld^.Disabled(ItKR) AND Ergebnis;
               INC(Lauf);
               Zeiger := Zeiger^.Nachfolger;
            END;
         END;
         Disabled :=Ergebnis;
      END;


   PROCEDURE TEingabeFenster.SetToggle;
      VAR   Zeiger  : TFeldZgr;
            Lauf    : BYTE;

      BEGIN
         IF (K = '_') THEN BEGIN
            inherited SetToggle('',Val);
         END ELSE BEGIN
            Zeiger := FeldListenAnfang;
            FOR Lauf := 1 TO AnzahlFelder DO BEGIN
               WITH Zeiger^ DO BEGIN
                  IF (Feld^.MultiObj) AND ((K = '') OR (K = FeldKennung))THEN
                     Feld^.SetToggle('_',Val);
                  IF (K = '') OR (FeldKennung = K) OR (Feld^.MultiObj) THEN
                     Feld^.SetToggle(K,Val);
               END;
               Zeiger:=Zeiger^.Nachfolger;
            END
         END;
      END;


   PROCEDURE TEingabeFenster.Save;
      VAR Zeiger  : TFeldZgr;
          Lauf : BYTE;
      BEGIN
         IF (K = '_') THEN BEGIN
         END ELSE BEGIN
            Zeiger := FeldListenAnfang;
            FOR Lauf := 1 TO AnzahlFelder DO BEGIN
               WITH Zeiger^ DO BEGIN
                  IF (Feld^.MultiObj) AND ((K= '') OR (FeldKennung = K)) THEN
                     Feld^.Save('_');
                  IF (K = '') OR (FeldKennung = K) OR (Feld^.MultiObj) THEN
                     Feld^.Save(K);
               END;
               Zeiger:=Zeiger^.Nachfolger;
            END;
         END;
      END;
   PROCEDURE TEingabeFenster.Restore;
      VAR Zeiger  : TFeldZgr;
          Lauf : BYTE;
      BEGIN
         IF (K = '_') THEN BEGIN
         END ELSE BEGIN
            Zeiger := FeldListenAnfang;
            FOR Lauf := 1 TO AnzahlFelder DO BEGIN
               WITH Zeiger^ DO BEGIN
                  IF (Feld^.MultiObj) AND ((K= '') OR (FeldKennung = K)) THEN
                     Feld^.Restore('_');
                  IF (K = '') OR (FeldKennung = K) OR (Feld^.MultiObj) THEN
                     Feld^.Restore(K);
               END;
               Zeiger:=Zeiger^.Nachfolger;
            END;
         END;
      END;

   FUNCTION TEingabeFenster.Changed;
      VAR   Ergebnis : BOOLEAN;
            Lauf : BYTE;
            Zgr  : TFeldZgr;

      BEGIN
         Ergebnis := FALSE;
         IF (K = '_') THEN BEGIN
         END ELSE BEGIN
            Zgr := FeldListenAnfang;
            Lauf := 1;
            WHILE (Lauf <= AnzahlFelder) AND (NOT Ergebnis) DO BEGIN
               WITH Zgr^ DO BEGIN
                  IF (K = '') OR (FeldKennung = K) OR (Feld^.MultiObj) THEN BEGIN
                     IF (Feld^.MultiObj AND Feld^.Changed('_')) OR
                        Feld^.Changed(K) THEN
                        Ergebnis := TRUE;
                  END;
               END;
               Zgr:= Zgr^.Nachfolger;
               INC(Lauf);
            END;
         END;
         Changed := Ergebnis;
     END;

   {ษอออออออออออป
   ษออออออออออออออออออออออออออออน Bemerkung ฬอออออออออออออออออออออออออออออออป
   บ                            ศอออออออออออผ                               บ
   บ Aufgabe:     Anzeigen des Eingabefensters, ohne Statuswechsel          บ
   บ                                                                        บ
   บ Parameter:   Keine                                                     บ
   บ                                                                        บ
   บ Resultate:   Keine                                                     บ
   ศออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ}

   PROCEDURE TEingabeFenster.Show;

      BEGIN
         IF Geschlossen THEN BEGIN
            IF (TitelNr <> NIL) AND (TitelNr^ <> -1) THEN BEGIN
               FensterTitel := Beschreibung+'  '+L2S(TitelNr^,0)
            END;
            inherited Show(K);
            Geschlossen := FALSE;
            FensterTitel := Beschreibung;
         END;
         AktuelleFarben := FensterFarben;
         ShowConst(K);
         ShowFields(K);
      END;

   PROCEDURE TEingabeFenster.ShowConst;
      VAR   MZeiger : TMaskZgr;
            Lauf    : BYTE;
            VGFarbe,
            HGCol : BYTE;

      BEGIN
         MZeiger:=Maskenliste;
         WHILE MZeiger <> NIL DO BEGIN
            IF (K = MZeiger^.Kennung) OR (K= '') THEN BEGIN
               WITH MZeiger^ DO BEGIN
                  IF (TxFarbe =  255) THEN
                     VgFarbe :=AktuelleFarben^.TitelVG[EN]
                  ELSE
                     VGFarbe := TxFarbe;
                  IF (HGFarbe = 255) THEN
                     HGCol :=AktuelleFarben^.TitelHG[EN]
                  ELSE
                     HGCol := HGFarbe;
                  CASE Typ OF
                     1 : PLACETEXT(Spalte,Zeile,Txt,VGFarbe,HGCol,ExtraBreite,ExtraHoehe);
                     2 : PLACETEXT(Spalte,Zeile,PrintLong0(LongVar^,Endzeile),
                            VGFarbe,HGCol,ExtraBreite,ExtraHoehe);
                     3 : PLACETEXT(Spalte,Zeile,PrintReal0(RealVar^,EndZeile,EndSpalte),
                            VGFarbe,HGCol,ExtraBreite,ExtraHoehe);
                     4 : PLACETEXT(Spalte,Zeile,StringVar^,VGFarbe,HGCol,ExtraBreite,ExtraHoehe);
                     5 : DrawHLine(Spalte,EndSpalte,Zeile,EndZeile,VGFarbe);
                     6 : DrawVLine(Spalte,Zeile,EndZeile,EndSpalte,VGFarbe);
                     9 : DrawCPoint(Spalte,Zeile,EndZeile,VGFarbe);
                  END;
               END;
            END;
            MZeiger:=Mzeiger^.NachFolger;
         END;
      END;

   PROCEDURE TEingabeFenster.ClearConst;
      VAR   MZeiger : TMaskZgr;
            Lauf    : BYTE;
            VGFarbe,
            HGCol : BYTE;

      BEGIN
         MZeiger:=Maskenliste;
         WHILE MZeiger <> NIL DO BEGIN
            IF (K = MZeiger^.Kennung) OR (K= '') THEN BEGIN
               WITH MZeiger^ DO BEGIN
                  HGCol :=AktuelleFarben^.TitelHG[EN];
                  VGFarbe :=AktuelleFarben^.TitelHG[EN];
                  CASE Typ OF
                     1 : PLACETEXT(Spalte,Zeile,REPLICATE(' ',LENGTH(Txt)),VGFarbe,HGCol,ExtraBreite,ExtraHoehe);
                     2 : PLACETEXT(Spalte,Zeile,REPLICATE(' ',LENGTH(PrintLong0(LongVar^,Endzeile))),
                            VGFarbe,HGCol,ExtraBreite,ExtraHoehe);
                     3 : PLACETEXT(Spalte,Zeile,
                         REPLICATE(' ',LENGTH(PrintReal0(RealVar^,Endzeile,EndSpalte))),
                               VGFarbe,HGCol,ExtraBreite,ExtraHoehe);
                     4 : PLACETEXT(Spalte,Zeile,REPLICATE(' ',LENGTH(StringVar^)),VGFarbe,HGCol,ExtraBreite,ExtraHoehe);
                     5 : DrawHLine(Spalte,EndSpalte,Zeile,EndZeile,VGFarbe);
                     6 : DrawVLine(Spalte,Zeile,EndZeile,EndSpalte,VGFarbe);
                     9 : DrawCPoint(Spalte,Zeile,EndZeile,VGFarbe);
                  END;
               END;
            END;
            MZeiger:=Mzeiger^.NachFolger;
         END;
      END;

   PROCEDURE TEingabeFenster.ShowFields;

      VAR   Zeiger  : TFeldZgr;
            Lauf    : BYTE;

      BEGIN
         IF (K = '_') THEN BEGIN
         END ELSE BEGIN
            Zeiger := FeldListenAnfang;
            FOR Lauf := 1 TO AnzahlFelder DO BEGIN
               WITH Zeiger^ DO BEGIN
                  IF (Feld^.MultiObj) AND ((K= '') OR (Feldkennung = K)) THEN
                     Feld^.Show('_');
                  IF (K = '') OR (FeldKennung = K) OR (Feld^.MultiObj) THEN
                    Feld^.Show(K);
               END;
               Zeiger:=Zeiger^.Nachfolger;
            END;
         END;
      END;

   PROCEDURE TEingabeFenster.Refresh;
      BEGIN
         IF Geschlossen THEN
            Show(K)
         ELSE BEGIN
            ShowConst(K);
            ShowFields(K);
         END;
      END;


   PROCEDURE TEingabeFenster.AddCPtr;
      VAR   MZeiger : TMaskZgr;

      BEGIN
         IF MaskenListe = NIL THEN
            MaskenListe:= Neu
         ELSE BEGIN
            MZeiger:=Maskenliste;
            WHILE MZeiger^.Nachfolger<>NIL DO
               MZeiger:=MZeiger^.Nachfolger;
            MZeiger^.Nachfolger := Neu;
         END;
      END;

   PROCEDURE TEingabeFenster.AddConst;
      VAR   Neu,
            MZeiger :TMaskZgr;

      BEGIN
         NEW(Neu);
         Neu^.Kennung := K;
         Neu^.Txt:=TX;
         Neu^.ExtraHoehe := 0;
         Neu^.ExtraBreite := 0;
         Neu^.TxFarbe := 255;
         Neu^.HGFarbe := 255;
         Neu^.Nachfolger := NIL;
         Neu^.Zeile := ZE;
         Neu^.Spalte:= SP;
         Neu^.EndZeile := ZE;
         Neu^.EndSpalte := SP;
         Neu^.Typ := 1;
         AddCPtr(Neu);
      END;

   PROCEDURE TEingabeFenster.AddCLong;
      VAR   Neu,
            MZeiger :TMaskZgr;

      BEGIN
         NEW(Neu);
         Neu^.Txt := '';
         Neu^.ExtraHoehe := 0;
         Neu^.ExtraBreite := 0;
         Neu^.TXFarbe := 255;
         Neu^.HGFarbe := 255;
         Neu^.Kennung := K;
         Neu^.LongVar:=@LVar;
         Neu^.Nachfolger := NIL;
         Neu^.Zeile := ZE;
         Neu^.Spalte:= SP;
         Neu^.EndZeile := Stellen;
         Neu^.EndSpalte := SP;
         Neu^.Typ := 2;
         AddCPtr(Neu);
      END;

   PROCEDURE TEingabeFenster.AddCReal;
      VAR   Neu,
            MZeiger :TMaskZgr;

      BEGIN
         NEW(Neu);
         Neu^.Txt := '';
         Neu^.ExtraHoehe := 0;
         Neu^.ExtraBreite := 0;
         Neu^.TXFarbe := 255;
         Neu^.HGFarbe := 255;
         Neu^.Kennung := K;
         Neu^.RealVar:=@RVar;
         Neu^.Nachfolger := NIL;
         Neu^.Zeile := ZE;
         Neu^.Spalte:= SP;
         Neu^.EndZeile := Stellen;
         Neu^.EndSpalte := NachK;
         Neu^.Typ := 3;
         AddCPtr(Neu);
      END;

   PROCEDURE TEingabeFenster.AddCString;
      VAR   Neu,
            MZeiger :TMaskZgr;

      BEGIN
         NEW(Neu);
         Neu^.Kennung := K;
         Neu^.Txt := '';
         Neu^.ExtraHoehe := 0;
         Neu^.ExtraBreite := 0;
         Neu^.TXFarbe := 255;
         Neu^.HGFarbe := 255;
         Neu^.StringVar:=@SVar;
         Neu^.Nachfolger := NIL;
         Neu^.Zeile := ZE;
         Neu^.Spalte:= SP;
         Neu^.EndZeile := ZE;
         Neu^.EndSpalte := SP;
         Neu^.Typ := 4;
         AddCPtr(Neu);
      END;

   PROCEDURE TEingabeFenster.ADDLine;

      VAR   Neu,
            MZeiger :TMaskZgr;

      BEGIN
         NEW(Neu);
         Neu^.Kennung := K;
         Neu^.Txt:='';
         Neu^.ExtraHoehe := 0;
         Neu^.ExtraBreite := 0;
         Neu^.TXFarbe := 255;
         Neu^.HGFarbe := 255;
         Neu^.Nachfolger := NIL;
         Neu^.Zeile := ZEA;
         Neu^.Spalte:= SPA;
         Neu^.EndZeile := ZEE;
         Neu^.EndSpalte := SPE;
         Neu^.Typ := LTyp;
         AddCPtr(Neu);
      END;

   PROCEDURE TEingabeFenster.AddCrossPoint;

      VAR   Neu,
            MZeiger :TMaskZgr;

      BEGIN
         NEW(Neu);
         Neu^.Kennung := K;
         Neu^.Txt:='';
         Neu^.ExtraHoehe := 0;
         Neu^.ExtraBreite := 0;
         Neu^.TXFarbe := 255;
         Neu^.HGFarbe := 255;
         Neu^.Nachfolger := NIL;
         Neu^.Zeile := ZE;
         Neu^.Spalte:= SP;
         Neu^.EndZeile := Typ;
         Neu^.EndSpalte := 0;
         Neu^.Typ := 9;
         AddCPtr(Neu);
      END;

   PROCEDURE TEingabeFenster.AddHLine;
      BEGIN
         IF ((YDif > 4-ZeilenAbstand) AND (YDif < 0)) OR
            ((YDif < ZeilenAbstand-4) AND (YDif >= 0)) THEN
            AddLine(K,ZE,SPA,YDif,SPE,5);
      END;


   PROCEDURE TEingabeFenster.AddVLine;
      BEGIN
         IF (ABS(XDif) <=4) THEN
            AddLine(K,ZEA,SP,ZEE,XDif,6);
      END;

   PROCEDURE TEingabeFenster.SetCColor;
      VAR MZeiger : TMaskZgr;
      BEGIN
         MZeiger := MaskenListe;
         WHILE (MZeiger <> NIL) DO BEGIN
            IF (K = '') OR (MZeiger^.Kennung = K) THEN BEGIN
               MZeiger^.TxFarbe := TXF;
               MZeiger^.HGFarbe := HGF;
            END;
            Mzeiger:= MZeiger^.Nachfolger;
         END;
      END;

   PROCEDURE TEingabeFenster.SetCHeight;
      VAR MZeiger : TMaskZgr;
      BEGIN
         MZeiger := MaskenListe;
         WHILE (MZeiger <> NIL) DO BEGIN
            IF (K = '') OR (MZeiger^.Kennung = K) THEN BEGIN
               MZeiger^.ExtraHoehe:= CH;
            END;
            Mzeiger:= MZeiger^.Nachfolger;
         END;
      END;
   PROCEDURE TEingabeFenster.SetCWidth;
      VAR MZeiger : TMaskZgr;
      BEGIN
         MZeiger := MaskenListe;
         WHILE (MZeiger <> NIL) DO BEGIN
            IF (K = '') OR (MZeiger^.Kennung = K) THEN BEGIN
               MZeiger^.ExtraBreite:= CW;
            END;
            Mzeiger:= MZeiger^.Nachfolger;
         END;
      END;

   {ษอออออออออออป
   ษออออออออออออออออออออออออออออน Bemerkung ฬอออออออออออออออออออออออออออออออป
   บ                            ศอออออออออออผ                               บ
   บ Aufgabe:     GanzzahlFeld an Fensterdialog anhngen                    บ
   บ                                                                        บ
   บ Parameter:   Feldtitel:STRING, Spalte,Zeile:BYTE,Wertevariable:INTEGER,บ
   บ              InfoDatei:STRING,Feldlnge:BYTE,Unter-,Obergrenze:INTEGER บ
   บ                                                                        บ
   บ Resultate:   Keine                                                     บ
   ศออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ}

   PROCEDURE TEingabeFenster.AddTabZeile;

      VAR   Zeiger : ^TTabZeileFeld;
            ItkF,
            ItkR   : STRING;

      BEGIN
         NEW(Zeiger);
         Zeiger^.Init(XPoint(SP),YPoint(ZE),0,InfoTx(Info),I,Extra);
         IF (Itk <> '') THEN BEGIN
            SplitString(Itk,'_',ItkF,ItKR);
            AddSubField(ItkF,ItkR,K,Zeiger,Def)
         END ELSE
            AddNew(K,Zeiger,Def);
      END;
   {ษอออออออออออป
   ษออออออออออออออออออออออออออออน Bemerkung ฬอออออออออออออออออออออออออออออออป
   บ                            ศอออออออออออผ                               บ
   บ Aufgabe:     GanzzahlFeld an Fensterdialog anhngen                    บ
   บ                                                                        บ
   บ Parameter:   Feldtitel:STRING, Spalte,Zeile:BYTE,Wertevariable:INTEGER,บ
   บ              InfoDatei:STRING,Feldlnge:BYTE,Unter-,Obergrenze:INTEGER บ
   บ                                                                        บ
   บ Resultate:   Keine                                                     บ
   ศออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ}

   PROCEDURE TEingabeFenster.AddInt;

      VAR   Zeiger : ^TIntFeld;
            ItkF,
            ItkR   : STRING;

      BEGIN
         NEW(Zeiger);
         Zeiger^.Init(XPoint(SP),YPoint(ZE),XPoint(SP+LENGTH(TX)+1)+4,YPoint(ZE),
         Len,Tx,InfoTx(Info),I,Extra,UG,OG);
         IF (Itk <> '') THEN BEGIN
            SplitString(Itk,'_',ItkF,ItKR);
            AddSubField(ItkF,ItkR,K,Zeiger,Def)
         END ELSE
            AddNew(K,Zeiger,Def);
      END;

   PROCEDURE TEingabeFenster.AddByte;

      VAR   Zeiger : ^TByteFeld;
            ItkF,
            ItkR   : STRING;

      BEGIN
         NEW(Zeiger);
         Zeiger^.Init(XPoint(SP),YPoint(ZE),XPoint(SP+LENGTH(TX)+1)+4,YPoint(ZE),
         Len,Tx,InfoTx(Info),I,Extra,UG,OG);
         IF (Itk <> '') THEN BEGIN
            SplitString(Itk,'_',ItkF,ItKR);
            AddSubField(ItkF,ItkR,K,Zeiger,Def)
         END ELSE
            AddNew(K,Zeiger,Def);
      END;

   PROCEDURE TEingabeFenster.AddWord;

      VAR   Zeiger : ^TWordFeld;
            ItkF,
            ItkR   : STRING;

      BEGIN
         NEW(Zeiger);
         Zeiger^.Init(XPoint(SP),YPoint(ZE),XPoint(SP+LENGTH(TX)+1)+4,YPoint(ZE),
         Len,Tx,InfoTx(Info),I,Extra,UG,OG);
         IF (Itk <> '') THEN BEGIN
            SplitString(Itk,'_',ItkF,ItKR);
            AddSubField(ItkF,ItkR,K,Zeiger,Def)
         END ELSE
            AddNew(K,Zeiger,Def);
      END;

   {ษอออออออออออป
   ษออออออออออออออออออออออออออออน Bemerkung ฬอออออออออออออออออออออออออออออออป
   บ                            ศอออออออออออผ                               บ
   บ Aufgabe:     Langes Ganzzahlfeld an Fensterdialog anhngen             บ
   บ                                                                        บ
   บ Parameter:   Feldtitel:STRING, Spalte,Zeile:BYTE,Wertevariable:LONGINT,บ
   บ              InfoDatei:STRING,Feldlnge:BYTE,Unter-,Obergrenze:LONGINT บ
   บ                                                                        บ
   บ Resultate:   Keine                                                     บ
   ศออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ}

   PROCEDURE TEingabeFenster.AddLongInt;

      VAR   Zeiger : ^TLongIntFeld;
            ItkF,
            ItkR   : STRING;

      BEGIN
         NEW(Zeiger);
         Zeiger^.Init(XPoint(SP),YPoint(ZE),XPoint(SP+LENGTH(TX)+1)+4,YPoint(ZE),
         Len,Tx,InfoTx(Info),I,Extra,UG,OG);
         IF (Itk <> '') THEN BEGIN
            SplitString(Itk,'_',ItkF,ItKR);
            AddSubField(ItkF,ItkR,K,Zeiger,Def)
         END ELSE
            AddNew(K,Zeiger,Def);
      END;

   {ษอออออออออออป
   ษออออออออออออออออออออออออออออน Bemerkung ฬอออออออออออออออออออออออออออออออป
   บ                            ศอออออออออออผ                               บ
   บ Aufgabe:     Textfeld an Fensterdialog anhngen                        บ
   บ                                                                        บ
   บ Parameter:   Feldtitel:STRING, Spalte,Zeile:BYTE, Wertevariable:STRING,บ
   บ              InfoDatei:STRING,Feldlnge:BYTE                           บ
   บ                                                                        บ
   บ Resultate:   Keine                                                     บ
   ศออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ}

   PROCEDURE TEingabeFenster.AddString;

      VAR   Zeiger : ^TTextFeld;
            ItkF,
            ItkR   : STRING;

      BEGIN
         NEW(Zeiger);
         Zeiger^.Init(XPoint(SP),YPoint(ZE),XPoint(SP+LENGTH(TX)+1)+4,YPoint(ZE),
         Len,Tx,InfoTx(Info),I,Extra);
         IF (Itk <> '') THEN BEGIN
            SplitString(Itk,'_',ItkF,ItKR);
            AddSubField(ItkF,ItkR,K,Zeiger,Def)
         END ELSE
            AddNew(K,Zeiger,Def);
      END;

   PROCEDURE TEingabeFenster.AddChar;

      VAR   Zeiger : ^TCharFeld;
            ItkF,
            ItkR   : STRING;

      BEGIN
         NEW(Zeiger);
         Zeiger^.Init(XPoint(SP),YPoint(ZE),XPoint(SP+LENGTH(TX)+1)+4,YPoint(ZE),
         Tx,InfoTx(Info),I,Extra);
         IF (Itk <> '') THEN BEGIN
            SplitString(Itk,'_',ItkF,ItKR);
            AddSubField(ItkF,ItkR,K,Zeiger,Def)
         END ELSE
            AddNew(K,Zeiger,Def);
      END;


   {ษอออออออออออป
   ษออออออออออออออออออออออออออออน Bemerkung ฬอออออออออออออออออออออออออออออออป
   บ                            ศอออออออออออผ                               บ
   บ Aufgabe:     Gleitkommazahlfeld an Fensterdialog anhngen              บ
   บ                                                                        บ
   บ Parameter:   Feldtitel:STRING, Spalte,Zeile:BYTE, Wertevariable:REAL,  บ
   บ              InfoDatei:STRING,Feldlnge:BYTE                           บ
   บ                                                                        บ
   บ Resultate:   Keine                                                     บ
   ศออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ}

   PROCEDURE TEingabeFenster.AddReal;

      VAR   Zeiger : ^TRealFeld;
            ItkF,
            ItkR   : STRING;

      BEGIN
         NEW(Zeiger);
         Zeiger^.Init(XPoint(SP),YPoint(ZE),XPoint(SP+LENGTH(TX)+1)+4,YPoint(ZE),
         Len,Tx,InfoTx(Info),I,Extra,Nko,UG,OG);
         IF (Itk <> '') THEN BEGIN
            SplitString(Itk,'_',ItkF,ItKR);
            AddSubField(ItkF,ItkR,K,Zeiger,Def)
         END ELSE
            AddNew(K,Zeiger,Def);
      END;



   {ษอออออออออออป
   ษออออออออออออออออออออออออออออน Bemerkung ฬอออออออออออออออออออออออออออออออป
   บ                            ศอออออออออออผ                               บ
   บ Aufgabe:     Datumsfeld an Fensterdialog anhngen                      บ
   บ                                                                        บ
   บ Parameter:   Feldtitel:STRING, Spalte,Zeile:BYTE, Wertevariable:TDatum,บ
   บ              InfoDatei:STRING                                          บ
   บ                                                                        บ
   บ Resultate:   Keine                                                     บ
   ศออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ}

   PROCEDURE TEingabeFenster.AddDate;

      VAR   Zeiger : ^TDatumFeld;
            ItkF,
            ItkR   : STRING;

      BEGIN
         NEW(Zeiger);
         Zeiger^.Init(XPoint(SP),YPoint(ZE),XPoint(SP+LENGTH(TX)+1)+4,YPoint(ZE),
         Tx,InfoTx(Info),I,Extra);
         IF (Itk <> '') THEN BEGIN
            SplitString(Itk,'_',ItkF,ItKR);
            AddSubField(ItkF,ItkR,K,Zeiger,Def)
         END ELSE
            AddNew(K,Zeiger,Def);
      END;

   {ษอออออออออออป
   ษออออออออออออออออออออออออออออน Bemerkung ฬอออออออออออออออออออออออออออออออป
   บ                            ศอออออออออออผ                               บ
   บ Aufgabe:     Zeitfeld an Fensterdialog anhngen                        บ
   บ                                                                        บ
   บ Parameter:   Feldtitel:STRING, Spalte,Zeile:BYTE, Wertevariable:TZeit, บ
   บ              InfoDatei:STRING                                          บ
   บ                                                                        บ
   บ Resultate:   Keine                                                     บ
   ศออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ}

   PROCEDURE TEingabeFenster.AddTime;

      VAR   Zeiger : ^TZeitFeld;
            ItkF,
            ItkR   : STRING;

      BEGIN
         NEW(Zeiger);
         Zeiger^.Init(XPoint(SP),YPoint(ZE),XPoint(SP+LENGTH(TX)+1)+4,YPoint(ZE),
         Tx,InfoTx(Info),I,Extra);
         IF (Itk <> '') THEN BEGIN
            SplitString(Itk,'_',ItkF,ItKR);
            AddSubField(ItkF,ItkR,K,Zeiger,Def)
         END ELSE
            AddNew(K,Zeiger,Def);
      END;

   {ษอออออออออออป
   ษออออออออออออออออออออออออออออน Bemerkung ฬอออออออออออออออออออออออออออออออป
   บ                            ศอออออออออออผ                               บ
   บ Aufgabe:     Wahrheitsfeld an Fensterdialog anhngen                   บ
   บ                                                                        บ
   บ Parameter:   Feldtitel:STRING, Spalte,Zeile:BYTE,Wertevariable:BOOLEAN,บ
   บ              InfoDatei:STRING                                          บ
   บ                                                                        บ
   บ Resultate:   Keine                                                     บ
   ศออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ}

   PROCEDURE TEingabeFenster.AddBool;

      VAR   Zeiger : ^TBoolFeld;
            ItkF,
            ItkR   : STRING;

      BEGIN
         NEW(Zeiger);
         Zeiger^.Init(XPoint(SP),YPoint(ZE),XPoint(SP+LENGTH(TX)+1)+4,YPoint(ZE),
         Tx,InfoTx(Info),I,Extra);
         IF (Itk <> '') THEN BEGIN
            SplitString(Itk,'_',ItkF,ItKR);
            AddSubField(ItkF,ItkR,K,Zeiger,Def)
         END ELSE
            AddNew(K,Zeiger,Def);
      END;
   PROCEDURE TEingabeFenster.AddSwitch;

         VAR Zeiger : ^TSwitchFeld;
             ItkF,
             ItkR   : STRING;

         BEGIN
           NEW(Zeiger);
           Zeiger^.Init(XPoint(SP),YPoint(ZE),XPoint(SP+LENGTH(TX)+1)+4,YPoint(ZE),
                        Tx,InfoTx(Info),I,Extra);
         IF (Itk <> '') THEN BEGIN
            SplitString(Itk,'_',ItkF,ItKR);
            AddSubField(ItkF,ItkR,K,Zeiger,Def)
         END ELSE
              AddNew(K,Zeiger,Def);
         END;

   {ษอออออออออออป
   ษออออออออออออออออออออออออออออน Bemerkung ฬอออออออออออออออออออออออออออออออป
   บ                            ศอออออออออออผ                               บ
   บ Aufgabe:     Rollfeld an Fensterdialog anhngen                        บ
   บ                                                                        บ
   บ Parameter:   Feldtitel:STRING, Spalte,Zeile:BYTE, Wertevariable:WORD,  บ
   บ              InfoDatei:STRING,Feldlnge,Rollfeldanzahl:BYTE,           บ
   บ              Rollfeldtextfunktion:TGetfunc                             บ
   บ                                                                        บ
   บ Resultate:   Keine                                                     บ
   ศออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ}

   PROCEDURE TEingabeFenster.AddRoll;

      VAR   Zeiger : ^TRollFeld;
            ItkF,
            ItkR   : STRING;

      BEGIN
         NEW(Zeiger);
         Zeiger^.Init(XPoint(SP),YPoint(ZE),XPoint(SP+LENGTH(TX)+1)+4,YPoint(ZE),
         Len,Tx,InfoTx(Info));
         IF (Itk <> '') THEN BEGIN
            SplitString(Itk,'_',ItkF,ItKR);
            AddSubField(ItkF,ItkR,K,Zeiger,Def)
         END ELSE
            AddNew(K,Zeiger,Def);
      END;

   {ษอออออออออออป
   ษออออออออออออออออออออออออออออน Bemerkung ฬอออออออออออออออออออออออออออออออป
   บ                            ศอออออออออออผ                               บ
   บ Aufgabe:     Auswahlfensterfeld an Fensterdialog anhngen              บ
   บ                                                                        บ
   บ Parameter:   Feldtitel:STRING, Spalte,Zeile:BYTE,Wertevariable:BYTE,   บ
   บ              InfoDatei:STRING,Feldlnge,Auswahlanzahl:BYTE,Auswahltext-บ
   บ              funktion:TGetFunc,Fenstertitel:STRING falls '' wird       บ
   บ              Feldtitel bernommen                                      บ
   บ                                                                        บ
   บ Resultate:   Keine                                                     บ
   ศออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ}

   PROCEDURE TEingabeFenster.AddChoice;

      VAR   Zeiger : ^TChoiceFeld;
            ItkF,
            ItkR   : STRING;

      BEGIN
         NEW(Zeiger);
         Zeiger^.Init(XPoint(SP),YPoint(ZE),XPoint(SP+LENGTH(TX)+1)+4,YPoint(ZE),
         Len,Tx,InfoTx(Info),FTxt);
         IF (Itk <> '') THEN BEGIN
            SplitString(Itk,'_',ItkF,ItKR);
            AddSubField(ItkF,ItkR,K,Zeiger,Def)
         END ELSE
            AddNew(K,Zeiger,Def);
      END;

   PROCEDURE TEingabeFenster.AddKw;
      VAR   Zeiger : ^TKwFeld;
            ItkF,
            ItkR   : STRING;

      BEGIN
         NEW(Zeiger);
         Zeiger^.Init(XPoint(SP),YPoint(ZE),XPoint(SP+LENGTH(TX)+1)+4,
         YPoint(ZE),Tx,TEingabeFenster.InfoTx(Info),I,Extra);
         IF (Itk <> '') THEN BEGIN
            SplitString(Itk,'_',ItkF,ItKR);
            AddSubField(ItkF,ItkR,K,Zeiger,Def)
         END ELSE
            AddNew(K,Zeiger,Def);
      END;


   PROCEDURE TEingabeFenster.AddNumText;
      VAR   Zeiger : ^TNumTextFeld;
            ItkF,
            ItkR   : STRING;

      BEGIN
         NEW(Zeiger);
         Zeiger^.Init(XPoint(SP),YPoint(ZE),XPoint(SP+LENGTH(TX)+1)+4,
         YPoint(ZE),Len,Tx,InfoTx(Info),I,Extra);
         IF (Itk <> '') THEN BEGIN
            SplitString(Itk,'_',ItkF,ItKR);
            AddSubField(ItkF,ItkR,K,Zeiger,Def)
         END ELSE
            AddNew(K,Zeiger,Def);
      END;


   PROCEDURE TEingabeFenster.AddMitNull;
      VAR   Zeiger : ^TMitNullFeld;
            ItkF,
            ItkR   : STRING;

      BEGIN
         NEW(Zeiger);
         Zeiger^.Init(XPoint(SP),YPoint(ZE),XPoint(SP+LENGTH(TX)+1)+4,
         YPoint(ZE),Len,Tx,InfoTx(Info),I,Extra,Min,Max);
         IF (Itk <> '') THEN BEGIN
            SplitString(Itk,'_',ItkF,ItKR);
            AddSubField(ItkF,ItkR,K,Zeiger,Def)
         END ELSE
            AddNew(K,Zeiger,Def);
      END;

   PROCEDURE  TEingabeFenster.AddItem;
      VAR   Lauf   : BYTE;
            Zeiger : TFeldZgr;
            ITKF,
            ITKR   : STRING;

      BEGIN
         Lauf := 1;
         Zeiger := FeldListenAnfang;
         SplitString(ItK,'_',ItkF,ItkR);
         WHILE (Lauf <= AnzahlFelder) DO BEGIN
            IF ((Zeiger^.FeldKennung = ItKF) AND Zeiger^.Feld^.ItemObj) OR
                (Zeiger^.Feld^.MultiObj) THEN
               Zeiger^.Feld^.AddItem(ItkR,TX);
            INC(Lauf);
            Zeiger := Zeiger^.Nachfolger;
         END;
      END;

   PROCEDURE  TEingabeFenster.DisableItem;
      VAR   Lauf   : BYTE;
            Zeiger : TFeldZgr;
            ITKF,
            ITKR   : STRING;

      BEGIN
         Lauf := 1;
         Zeiger := FeldListenAnfang;
         SplitString(ItK,'_',ItkF,ItkR);
         WHILE (Lauf <= AnzahlFelder) DO BEGIN
            IF ((Zeiger^.FeldKennung = ItKF) AND Zeiger^.Feld^.ItemObj) OR
                (Zeiger^.Feld^.MultiObj) THEN
               Zeiger^.Feld^.DisableItem(ItKR,It);
            INC(Lauf);
            Zeiger := Zeiger^.Nachfolger;
         END;
      END;

   FUNCTION  TEingabeFenster.ItemDisabled;
      VAR   Lauf   : BYTE;
            Zeiger : TFeldZgr;
            Ergebnis : BOOLEAN;

      BEGIN
         Lauf := 1;
         Zeiger := FeldListenAnfang;
         Ergebnis := TRUE;
         WHILE (Lauf <= AnzahlFelder) DO BEGIN
            IF (((K='') OR (Zeiger^.FeldKennung = K)) AND Zeiger^.Feld^.ItemObj) OR
                (Zeiger^.Feld^.MultiObj) THEN
                Ergebnis := Zeiger^.Feld^.ItemDisabled(K,It) AND Ergebnis;
            INC(Lauf);
            Zeiger := Zeiger^.Nachfolger;
         END;
      END;

   PROCEDURE  TEingabeFenster.EnableItem;
      VAR   Lauf   : BYTE;
            Zeiger : TFeldZgr;
            ItKF,
            ItKR   : STRING;

      BEGIN
         Lauf := 1;
         Zeiger := FeldListenAnfang;
         SplitString(ItK,'_',ItkF,ItkR);
         WHILE (Lauf <= AnzahlFelder) DO BEGIN
            IF ((Zeiger^.FeldKennung = ItKF) AND Zeiger^.Feld^.ItemObj) OR
                (Zeiger^.Feld^.MultiObj) THEN
               Zeiger^.Feld^.EnableItem(ItKR,It);
            INC(Lauf);
            Zeiger := Zeiger^.Nachfolger;
         END;
      END;

   {ษอออออออออออป
   ษออออออออออออออออออออออออออออน Bemerkung ฬอออออออออออออออออออออออออออออออป
   บ                            ศอออออออออออผ                               บ
   บ Aufgabe:     Eingabefeldbeginn auf gleiche Spalte setzen, fr          บ
   บ              unterschiedlich lange Feldtitel                           บ
   บ                                                                        บ
   บ Parameter:   Keine                                                     บ
   บ                                                                        บ
   บ Resultate:   Keine                                                     บ
   ศออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ}

   PROCEDURE TEingabeFenster.AlignFields;
      VAR   Lauf   : BYTE;
            MaxSP  : WORD;
            MaxSPEnde : WORD;
            Zeiger : TFeldZgr;
            AllAligned :  BOOLEAN;

      BEGIN
         REPEAT
            Zeiger   := FeldListenAnfang;
            MaxSP    := 0;
            MaxSPEnde := 0;
            AllAligned := TRUE;
            FOR Lauf := 1 TO AnzahlFelder DO BEGIN
               IF NOT Zeiger^.Aligned AND (NOT Zeiger^.Feld^.MultiObj) THEN BEGIN
                  IF (Zeiger^.Feld^.Left > MaxSP) AND (MaxSp <> 0) THEN BEGIN
                     AllAligned := FALSE;
                  END ELSE BEGIN
                     IF (Zeiger^.Feld^.GetInputStartPos > MaxSp) THEN
                        MaxSp := Zeiger^.Feld^.GetInputStartPos;
                  END;
               END;
               Zeiger := Zeiger^.Nachfolger;
            END;
            Zeiger := FeldListenAnfang;
            FOR Lauf := 1 TO AnzahlFelder DO BEGIN
               IF (NOT Zeiger^.Aligned) AND  NOT (Zeiger^.Feld^.MultiObj) AND
                  (Zeiger^.Feld^.Left < MaxSP) THEN BEGIN
                  Zeiger^.Feld^.SetInputStartPos(MaxSp);
                  Zeiger^.Aligned := TRUE;
               END;
               Zeiger := Zeiger^.Nachfolger;
            END;

         UNTIL AllAligned;

      END;

   {                            ษอออออออออออป
   ษออออออออออออออออออออออออออออน Bemerkung ฬอออออออออออออออออออออออออออออออป
   บ                            ศอออออออออออผ                               บ
   บ Aufgabe:     Eingabedialog in diesem Fenster aufrufen, Fenster wird    บ
   บ              aktiviert                                                 บ
   บ                                                                        บ
   บ Parameter:   Keine                                                     บ
   บ                                                                        บ
   บ Resultate:   Keine                                                     บ
   ศออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ}

   FUNCTION TEingabeFenster.Input;

      VAR   Aktuell,
            Zeiger  : TFeldZgr;
            OldEndCode : INTEGER;
            EndCode : INTEGER;
            AnzDisAbled : WORD;
            Nr        : BYTE;
            InpString :STRING;
            MoreEval,
            Check,
            Ende    : BOOLEAN;
            KeineFelder :BOOLEAN;
            MultiFeldBlock : BOOLEAN;
            AktSubFeld : BYTE;
            Lauf : BYTE;
            HFun : TActionFunc;
            Zg1 : POINTER;
            Zg2 : POINTER;
            Reaktion  : BOOLEAN;
            ReaktFNummer,
            ReaktOldFNummer : WORD;
            ReaktKey,
            ReaktOldKey     : INTEGER;

            AInputRes : BOOLEAN;
            SAnzDisabled: STRING[5];

      BEGIN
         Nr := 0;
         InpString := '';
         Show('');
         IF SwitchStatus(Zustand) THEN
            Switch;
         Ende := FALSE;
         KeineFelder:= FALSE;
         EndCode := 0;
         IF (AnzahlFelder = 0) THEN
            KeineFelder := TRUE;
         Zeiger := FeldListenAnfang;
         IF (FocusField > 1) THEN BEGIN
            FOR Lauf := 1 TO FocusField-1 DO
               Zeiger := Zeiger^.Nachfolger;
            FocusId := GetFocusID;
         END ELSE BEGIN
             IF (AnzahlFelder > 0) THEN BEGIN
                FocusField := 1;
                FocusId := GetFocusId;
             END;
         END;
         Reaktion := FALSE;
         ReaktOldFNummer:= 0;
         ReaktFNummer := 65535;
         ReaktKey :=-254;
         ReaktOldKey := -255;

         IF (NOT KeineFelder) OR (KeineFelder AND (KeyList <> NIL)) THEN BEGIN
            EndCode := 13;
            AnzDisabled := 0;
            AktSubFeld := 1;
            MultiFeldBlock := FALSE;
            REPEAT
               Reaktion := FALSE;
               IF NOT KeineFelder THEN BEGIN
                  {Es gibt aktive EingabeFelder, die Eingabe wird an die Felder weitergegeben}
                  Done := FALSE;
                  Abbruch := FALSE;
                  Ende := FALSE;
                  Aktuell := Zeiger;
                  OldEndCode := EndCode;
                  IF (NOT MultiFeldBlock) THEN BEGIN
                     { Reaktion auf den letzten Tastendruck in nach der
                       Eingabe aus einem Multifeld, Focus im nchsten
                       Multifeld entsprechend umsetzen }
                     IF ((EndCode = 13) OR (EndCode = 9)) THEN
                        AktSubFeld :=1;
                     IF (EndCode = -15) THEN
                       AktSubFeld := 255;
                  END;
                  MultiFeldBlock := Zeiger^.Feld^.MultiObj;
                  IF (NOT Zeiger^.Feld^.Disabled('')) THEN
                    { Setzen des Focusfeldes in einem Multifeldblock,
                      bei normalen Feldern keine Bedeutung }
                     Zeiger^.Feld^.SetFocusField(AktSubFeld);
                  EndCode := Zeiger^.Feld^.Input;
                  IF AInputFuncActive THEN
                     { Ausfhren der After-Input-Funktion, fr Reaktion auf
                       Vernderungen im aktuellen Feld }
                     AInputRes := AInputFunc;
                  IF Zeiger^.Feld^.KeyDone THEN BEGIN
                     { Sondertaste wurde bereits im Feld bearbeitet und
                       weiter Bearbeitung durch Fenster wird nicht erwnscht }
                     Done := TRUE;
                     Abbruch := Zeiger^.Feld^.Abbruch;
                     IF Abbruch THEN
                        Ende := TRUE;
                  END;
                  Reaktion := Done;
               END ELSE BEGIN
                  { Keine aktiven Eingabefelder, Es wird nur der Tastendruck
                    ausgewertet }
                  EndCode := KeyInput;
               END;
               IF (EndCode = 0) AND  (NOT KeineFelder) AND (Zeiger^.Feld^.Done)
                   THEN BEGIN
                  IF NOT Zeiger^.Feld^.Abbruch THEN BEGIN
                     Zeiger := FeldListenAnfang;
                     IF (FocusField > 1) THEN BEGIN
                        FOR Lauf := 1 TO FocusField-1 DO
                            Zeiger := Zeiger^.Nachfolger;
                     END;
                  END ELSE BEGIN
                     Ende := TRUE;
                     Abbruch := TRUE;
                     Done := TRUE;
                  END;
                  Reaktion := Done;
               END ELSE BEGIN
                  IF NOT KeineFelder THEN BEGIN
                     IF (EndCode = 0) THEN BEGIN
                        EndCode := OldEndCode;
                        Reaktion := TRUE;
                        INC(AnzDisabled);
                     END ELSE BEGIN
                        IF (NOT Zeiger^.Feld^.Disabled('')) AND MultiFeldBlock THEN
                           AktSubFeld :=Zeiger^.Feld^.GetFocusField;
                        AnzDisabled := 0;
                        Reaktion := TRUE;
                     END;
                     IF NOT Zeiger^.Feld^.KeyDone THEN BEGIN
                        MoreEval := FALSE;
                        Ende := KeyAction(EndCode,Nr,InpString,Abbruch,Done,Check,MoreEval);
                        IF ((NOT Ende) AND MoreEval) THEN BEGIN
                           Ende := KeyAction(EndCode,Nr,InpString,Abbruch,Done,Check,MoreEval);
                        END;
                        Reaktion:= Done;
                     END;
                  END ELSE BEGIN
                     Done := FALSE;
                     Abbruch := FALSE;
                     MoreEval := FALSE;
                     Ende := KeyAction(EndCode,Nr,InpString,Abbruch,Done,Check,MoreEval);
                     IF ((NOT Ende) AND MoreEval) THEN BEGIN
                        Ende := KeyAction(EndCode,Nr,InpString,Abbruch,Done,Check,MoreEval);
                     END;
                     Reaktion := Done;
                  END;
                  IF (NOT Done) THEN BEGIN
                     IF (EndCode = -1) AND (EndCode <> OldEndCode) THEN
                        EndCode := OldEndCode;
                     Reaktion:= TRUE;
                     CASE EndCode OF
                        13,9,-80 : BEGIN
                                      IF NOT KeineFelder THEN BEGIN
                                         Zeiger:=Zeiger^.Nachfolger;
                                         INC(FocusField);
                                         IF (Zeiger = FeldListenanfang) THEN BEGIN
                                            FocusField := 1;
                                            IF LeaveOnCr THEN BEGIN
                                               Ende := TRUE;
                                               Done := TRUE;
                                            END;
                                         END;
                                      END ELSE BEGIN
                                         IF LeaveOnCr THEN BEGIN
                                            Ende := TRUE;
                                            Done := TRUE;
                                         END;
                                      END;
                                   END;
                        -15,-72  : BEGIN
                                    {  STR(AnzDisabled,SAnzDisabled);
                                      InfoBox.Act(0,SAnzDisabled); }
                                      IF NOT KeineFelder THEN BEGIN
                                         DEC(FocusField);
                                         IF (Zeiger = FeldListenAnfang) THEN
                                            FocusField := AnzahlFelder;
                                         Zeiger:=Zeiger^.Vorgaenger;
                                      END;
                                   END;
                        27       : BEGIN
                                      IF LeaveONFKey THEN BEGIN
                                         Ende := TRUE;
                                         Abbruch := TRUE;
                                         Done := TRUE;
                                      END;
                                   END;
                        -68..-60 : BEGIN
                                      IF LeaveOnFkey THEN BEGIN
                                         Ende := TRUE;
                                         Done := TRUE;
                                      END;
                                   END;
                        -59      : BEGIN
                                      ShowKeyHelp;
                                   END;
                        ELSE       BEGIN
                                      IF (EndCode <> 0) THEN
                                         Reaktion := FALSE;
                                   END;
                     END;
                  END;
               END;
               IF ActionActive THEN
                  Ende :=  ActionFunc OR Ende;
{auskommentieren wenn Fenster zu hufig auftreten}
               IF (NOT Reaktion) THEN BEGIN
                  ReaktOldFNummer:=ReaktFNummer;
                  ReaktFNummer := FocusField;
                  ReaktOldKey := ReaktKey;
                  ReaktKey := EndCode;
                  IF (NOT Ende) AND
                     (AnzDisabled < AnzahlFelder) AND
                     (ReaktKey = ReaktOldKey) AND
                     (ReaktFNummer = ReaktOldFNummer) THEN BEGIN
                     InfoBox.Act(0,'Achtung, keine Reaktion auf Taste '+L2S(ReaktKey,0)+
                                    ' in Feld Nr. '+W2S(ReaktFNummer,0)+ '. Bitte notieren und '+
                                    ' Information weitergeben. Nchstes Feld: ');
                     IF (OldEndCode <> 13) THEN BEGIN
                        OldEndCode := 13;
                        EndCode := -1;
                        Zeiger := Zeiger^.NachFolger;
                        INC(FocusField);
                        IF (Zeiger = FeldListenanfang) THEN
                           FocusField := 1;
                     END;
                  END;
               END;
{bis hier }
            UNTIL Ende OR ((AnzDisabled >= AnzahlFelder) AND (NOT KeineFelder));
         END;
         LetzteTaste := EndCode;
         Input := EndCode;
      END;


      FUNCTION TEingabeFenster.Valid;
         VAR HFun :  TBoolFunc;

         BEGIN
            Valid := TRUE;
            IF (CheckFuncActive) THEN BEGIN
               Valid:=CheckFunc;
            END;
         END;

   {ษอออออออออออป
   ษออออออออออออออออออออออออออออน Bemerkung ฬอออออออออออออออออออออออออออออออป
   บ                            ศอออออออออออผ                               บ
   บ Aufgabe:     Gibt TastenCode an,der letztes INPUTWIN beendet hat       บ
   บ                                                                        บ
   บ Parameter:   Keine                                                     บ
   บ                                                                        บ
   บ Resultate:   Letzte Taste:INTEGER                                      บ
   ศออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ}
   
   FUNCTION TEingabeFenster.GetLastKey;

      BEGIN
         GetLastKey := LetzteTaste;
      END;

   CONSTRUCTOR TTabFenster.Init;

      BEGIN
         inherited Init(x1,y1,x2,y2,Za,Titel,Info,AnfZust);
         MaxTabZeilen := TZE;
         TabSpalten := TSP;
         TabZeilen := TZe;
         aktRow := 1;
         aktCol := 1;
         LastEdited := FeldListenAnfang;
         ScrollTab := STab;
      END;
   

   PROCEDURE TTabFenster.SetMaxRows;
      BEGIN
         IF (RNr = 0) OR (RNr >TabZeilen) THEN
            MaxTabZeilen := TabZeilen
         ELSE
            MaxTabZeilen := RNr; 
         
      END; 
   
   FUNCTION TTabFenster.Input;

      VAR   Aktuell,
            Zeiger     : TFeldZgr; 
            EndCode    : INTEGER; 
            Ende       : BOOLEAN; 
            OldCol,
            OldRow     : BYTE;
            Lauf       : BYTE; 
            
      PROCEDURE SeekNextField;
         BEGIN
            REPEAT
               Zeiger := Zeiger^.Nachfolger; 
               INC(AktCol); 
               IF AktCol = TabSpalten+1 THEN BEGIN
                  AktCol := 1;
                  INC(aktRow);
                  IF AktRow = TabZeilen+1 THEN
                     AktRow := 1;
               END;
            UNTIL NOT (Zeiger^.Feld^.Disabled(''))
         END; 
      
      PROCEDURE SeekPrevField; 
         BEGIN
            REPEAT
               Zeiger := Zeiger^.Vorgaenger; 
               DEC(AktCol); 
               IF AktCol = 0 THEN BEGIN
                  AktCol := TabSpalten;
                  DEC(aktRow); 
                  IF AktRow = 0 THEN
                     AktRow := TabZeilen;
               END; 
            UNTIL (Zeiger^.Feld^.Disabled(''))
         END; 
      
      BEGIN
         Show('');
         IF SwitchStatus(Zustand) THEN Switch;
         Ende := FALSE;
         EndCode := 0;
         Zeiger := FeldListenAnfang;
         AktRow := 1; AktCol := 1;
         IF Zeiger^.Feld^.Disabled('') THEN
            SeekNextField;
         REPEAT
            Aktuell := Zeiger;
            EndCode := Zeiger^.Feld^.Input;
            CASE EndCode OF
               13,9    : BEGIN
                            OldRow := AktRow;
                            OldCol := AktCol;
                            SeekNextField;
                            IF (AktRow < OldRow) OR
                               (AktRow > MaxTabZeilen)
                            THEN BEGIN
                               IF (MaxTabZeilen = TabZeilen) AND
                                  ScrollTab(1)
                               THEN BEGIN
                                  Zeiger := Aktuell;
                                  AktRow := OldRow;
                                  AktCol := OldCol;
                                  WHILE AktRow = OldRow DO
                                     SeekPrevField;
                                  SeekNextField;
                                  LastEdited := Zeiger;
                                  ShowFields('');
                               END
                               ELSE BEGIN
                                  WRITE(CHR(7));
                                  Zeiger:=Aktuell;
                                  AktRow := OldRow;
                                  AktCol := OldCol;
                                  LastEdited := Zeiger;
                               END;
                            END;
                         END;
               -80     : BEGIN
                            IF (aktRow = MaxTabZeilen) THEN BEGIN
                               IF (MaxTabZeilen < TabZeilen) OR
                                  (NOT ScrollTab(1))
                               THEN
                                  WRITE(CHR(7))
                               ELSE
                                  ShowFields('');
                               LastEdited := Zeiger;
                            END
                            ELSE BEGIN
                               FOR Lauf := 1 TO TabSpalten DO
                                  Zeiger := Zeiger^.Nachfolger;
                               AktRow := AktRow+1;
                            END;
                         END;
               -72     : BEGIN
                            IF (aktRow = 1) THEN BEGIN
                               IF (MaxTabZeilen < TabZeilen) OR
                                  (NOT ScrollTab(-1))
                               THEN
                                  WRITE(CHR(7))
                               ELSE
                                  ShowFields('');
                               LastEdited := Zeiger;
                            END
                            ELSE BEGIN
                               FOR Lauf := 1 TO TabSpalten DO
                                  Zeiger := Zeiger^.Vorgaenger;
                               AktRow := AktRow-1;
                            END;
                         END;
               -15       : BEGIN
                              OldRow := AktRow;
                              OldCol := AktCol;
                              SeekPrevField;
                              IF (OldRow < AktRow) THEN BEGIN
                                 IF (MaxTabZeilen = TabZeilen) AND
                                    ScrollTab(-1)
                                 THEN BEGIN
                                    Zeiger := Aktuell;
                                    AktRow := OldRow;
                                    AktCol := OldCol;
                                    WHILE AktRow = OldRow DO
                                       SeekNextField;
                                    SeekPrevField;
                                    LastEdited := Zeiger;
                                    ShowFields('');
                                 END
                                 ELSE BEGIN
                                    WRITE(CHR(7));
                                    Zeiger := Aktuell;
                                    AktRow := OldRow;
                                    AktCol := OldCol;
                                    LastEdited := Zeiger;
                                 END
                              END;
                           END;
               27       : Ende := TRUE;
               -68..-60 : Ende := TRUE;
            END;
         UNTIL Ende OR ((Zeiger^.Feld^.Disabled('')) AND (Zeiger = Aktuell));
         LetzteTaste := EndCode;
         Input := EndCode;
      END;

   CONSTRUCTOR TSearchFenster.Init;
      BEGIN
         inherited Init(x1,y1,x2,y2,Za,Titel,Info,AnfZust);
         SuchString := '';
         ValueSet := FALSE;
         HighLightPos := TRUE;
         ScrollLine := STab;
         SearchFunc := SFunc;
         MakeOutStr := OFunc;
         StartEndeFunc := SEFunc;
         AktRow := 1;
         Gestartet := FALSE;
         MaxTabZeilen := Zeilen-1;
         FirstScrollLine := 1;
         SetSeekInfo(TRUE);
      END;

   PROCEDURE TSearchFenster.SetSeekInfo;
      BEGIN
         IF VAL THEN BEGIN
            AddString('','XXX','Suchwort:',1,Zeilen,SuchString,'',Spalten-10);
            Disable('XXX');
         END ELSE BEGIN
            DisposeFields('');
         END;
      END;


   PROCEDURE TSearchFenster.SetPosShow;
      BEGIN
         HighLightPos := Val;
      END;


   FUNCTION TSearchFenster.Startup;
      VAR Msg : STRING;
          Ergebnis : BOOLEAN;
      BEGIN
        Msg := '';
        Ergebnis := TRUE;
        IF NOT Gestartet THEN BEGIN
           Ergebnis := StartEndeFunc(TRUE,Msg);
           IF NOT Ergebnis THEN
              FaultBox.Act(0,Msg)
           ELSE
              Gestartet :=TRUE

        END;
        Startup := Ergebnis;
      END;

   FUNCTION TSearchFenster.EndWin;
       VAR Msg : STRING;
           Ergebnis : BOOLEAN;
      BEGIN
        Msg :='';
        Ergebnis := TRUE;
        IF Gestartet THEN BEGIN
           Ergebnis :=StartEndeFunc(FALSE,Msg);
           IF NOT Ergebnis THEN
              FaultBox.Act(0,Msg)
           ELSE
              Gestartet := FALSE;

        END;
        EndWin:= Ergebnis;
      END;

   PROCEDURE TSearchFenster.SetFirstScrollLine;
      BEGIN
         FirstScrollLine := Zeile;
         MaxTabZeilen := (Zeilen-1) - FirstScrollLine +1;
      END;

      PROCEDURE TSearchFenster.ScrollToPosition;
           VAR Lauf,
               Laufende : BYTE;
               LetztePos : BYTE;
               AktPos :INTEGER;
           BEGIN
             Lauf := 1;
             AktPos := 1;
             WHILE (Lauf< AktTabZeilen) AND (ScrollLine(1,1) = 0) DO BEGIN
               INC(Lauf);
               DEC(AktPos);
             END;
             Lauf := 1;
             WHILE (Lauf< AktTabZeilen) AND (ScrollLine(-1,Lauf+1) = 0) DO BEGIN
                INC(Lauf);
                INC(AktPos);
             END;
             WHILE (AktPos< (AktTabZeilen DIV 2)) AND (ScrollLine(-1,AktTabZeilen) = 0) DO BEGIN
                INC(AktPos)
             END;
             AktRow := AktPos;
           END;




   FUNCTION TSearchFenster.Input;
      VAR   Ende : BOOLEAN;
            SEnde : BOOLEAN;
            NEnde : BOOLEAN;
            MoreEval : BOOLEAN;
            Check: BOOLEAN;
            Nr,
            LaufLaenge,
            Lauf  : BYTE;
            EndCode : INTEGER;

      BEGIN
         EndCode := 0;
         IF StartUp THEN BEGIN
            GetInitialState;
            Show('');
            Ende := FALSE;
            SuchString := '';
            REPEAT
               NEnde := TRUE;
               Refresh('');
               EndCode := InputKey;
               Ende := KeyAction(EndCode,Nr,Suchstring,Abbruch,Done,Check,MoreEval);
               IF NOT Done THEN BEGIN
                  IF ((EndCode >= 32) AND (Endcode <= 154)) THEN
                     SuchString:= Suchstring+UPPER(CHR(EndCode))
                  ELSE
                     Suchstring := '';
                  CASE Endcode OF
                     -59     : ShowKeyHelp;
                     -68..-60: BEGIN
                                  IF LeaveOnFkey THEN
                                     Ende:=TRUE;
                               END;
                     27      : BEGIN
                                  IF LeaveOnFKey THEN
                                     Ende := TRUE;
                               END;
                     -15,9,
                      13     : Ende := TRUE;
                     -71     : BEGIN
                                  ScrollLine(-2,1);
                                  ScrollToPosition;
                               END;
                     -79     : BEGIN
                                  ScrollLine(2,1);
                                  ScrollToPosition;
                               END;
                     32..154 : BEGIN
                                  IF SearchFunc(SuchString,1) THEN BEGIN
                                     ScrollToPosition;
                                  END;
                               END;
                     -72     : BEGIN
                                  IF (AktRow = (AktTabZeilen DIV 2)) THEN BEGIN
                                     NEnde := (ScrollLine(-1,AktTabZeilen) = 0);
                                     IF NOT NEnde  THEN BEGIN
                                        IF (AktRow > 1) THEN BEGIN
                                           DEC(AktRow);
                                           NEnde := TRUE;
                                        END;
                                     END;
                                  END ELSE BEGIN
                                     IF (AktRow > 1) THEN BEGIN
                                        DEC(AktRow);
                                     END ELSE
                                        NEnde := FALSE;
                                  END;
                               END;
                     -80     : BEGIN
                                  IF (AktRow = (AktTabZeilen DIV 2)) THEN BEGIN
                                     NEnde := (ScrollLine(1,AktTabZeilen) = 0);
                                     IF NOT NEnde THEN BEGIN
                                        IF (AktRow < AktTabZeilen) THEN BEGIN
                                           INC(AktRow);
                                           NEnde := TRUE;
                                        END;
                                     END;
                                  END ELSE BEGIN
                                     IF (AktRow < AktTabZeilen) THEN BEGIN
                                        INC(AktRow);
                                     END ELSE
                                        NEnde :=FALSE;
                                  END;
                               END;
                     -73     : BEGIN
                                  LaufLaenge := AktTabZeilen;
                                  WHILE (AktRow > (AktTabZeilen DIV 2)) DO BEGIN
                                     DEC(AktRow);
                                     DEC(LaufLaenge);
                                  END;
                                  IF (LaufLaenge > 0) THEN BEGIN
                                     SEnde := TRUE;
                                     NEnde := TRUE;
                                     FOR Lauf := 1 TO LaufLaenge DO
                                        IF Sende THEN BEGIN
                                           Sende := Sende AND (ScrollLine(-1,AktTabZeilen)= 0);
                                           IF NOT SEnde THEN BEGIN
                                              IF (AktRow > 1) THEN BEGIN
                                                 DEC(AktRow);
                                                 NEnde := TRUE;
                                              END ELSE
                                                 NEnde := FALSE;
                                           END
                                        END ELSE BEGIN
                                           IF (AktRow > 1 ) THEN BEGIN
                                              DEC(AktRow);
                                           END ELSE
                                              NEnde :=FALSE;
                                        END;
                                  END;
                               END;
                     -81     : BEGIN
                                  LaufLaenge := AktTabZeilen;
                                  WHILE (AktRow < (AktTabZeilen DIV 2)) DO BEGIN
                                     INC(AktRow);
                                     DEC(LaufLaenge);
                                  END;
                                  IF (LaufLaenge > 0) THEN BEGIN
                                     SEnde := TRUE;
                                     NEnde := TRUE;
                                     FOR Lauf := 1 TO LaufLaenge DO
                                        IF Sende THEN BEGIN
                                           Sende := Sende AND (ScrollLine(1,AktTabZeilen) = 0);
                                           IF NOT SEnde THEN BEGIN
                                              IF (AktRow < AktTabZeilen) THEN BEGIN
                                                 INC(AktRow);
                                                 NEnde := TRUE;
                                              END ELSE
                                                 NEnde := FALSE;
                                           END
                                        END ELSE BEGIN
                                           IF (AktRow < AktTabZeilen ) THEN BEGIN
                                              INC(AktRow);
                                           END ELSE
                                              NEnde :=FALSE;
                                        END;
                                  END;
                               END;
                  END;
                  IF NOT Nende THEN
                     WRITE(CHR(7));
               END;
            UNTIL Ende;
            LetzteTaste := EndCode;
            EndWin;
         END;
         Input:= EndCode;
      END;


   PROCEDURE TSearchFenster.SetValue;
      BEGIN
         AktTabZeilen := ScrollLine(0,MaxTabZeilen);
         IF SearchFunc(Eingabe,1) THEN BEGIN
            ScrollToPosition;
         END;
         ValueSet := TRUE;
      END;

   PROCEDURE TSearchFenster.GetInitialState;
      VAR   Lauf :       BYTE;
      BEGIN
         AktTabZeilen := ScrollLine(0,MaxTabZeilen);
         IF ValueSet THEN BEGIN
         END ELSE BEGIN
           ScrollLine(-2,1);
           IF (AktTabZeilen < 1) THEN
              AktTabZeilen := 1;
           IF (AktTabZeilen >= 2) THEN BEGIN
              FOR Lauf := 2 TO AktTabZeilen DO
                 ScrollLine(1,1);
              FOR Lauf := 2 TO AktTabZeilen DO
                 ScrollLine(-1,Lauf);
           END;
           AktRow := (AktTabZeilen DIV 2);
           IF AktRow < 1 THEN
              AktRow := 1;
         END;
         ValueSet := FALSE;
      END;

   PROCEDURE TSearchFenster.Refresh;
      VAR   Lauf :       BYTE;
            VG : BYTE;
            HG : BYTE;
      BEGIN
         inherited Refresh(K);
         IF (K = '') THEN BEGIN
            FOR Lauf := 1 TO AktTabZeilen DO BEGIN
               IF (Lauf = AktRow) AND HighLightPos THEN BEGIN
                  VG := 7;
                  Hg := 2;
               END
               ELSE BEGIN
                  VG := 255;
                  HG := 255;
               END;
               TSearchFenster.PlaceText(1,(Lauf+FirstScrollLine-1),
               MakeOutStr(Lauf),VG,HG,HGH,HGV);
            END;
         END;
      END;


   FUNCTION TSearchFenster.ResultRow;
      BEGIN
         ResultRow := AktRow;
      END; 

   FUNCTION TSearchFenster.GetResultText;
      BEGIN
         GetResultText := MakeOutStr(AktRow);
      END;


BEGIN
   ZustandAktiv := Aktiv; { Setzt den Initialzustand auf Aktiv }
END.
{============================
 Versionshistorie
 $Log:$
 ============================}
