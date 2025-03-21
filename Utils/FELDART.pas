{============================
 Dateiname:   $Source: $
 Version:     $Revision: $
 Autor:       $Author: $
 Gelockt von: $Locker: $
 Originaldatum : Fri Feb 25 17:56:54 GMT+01:00 2000
 Dateihistorie am Ende der Datei
 ============================}
UNIT FELDART;
{$O+}

INTERFACE

   USES ZUSAETZE,
        CASHDATA,
        PLACEWIN,
        EFELDER;

      TYPE TTabZeileFeld = OBJECT(TEFeld)
           Wert              : ^BOOLEAN;
           WertSik           : BOOLEAN;

           CONSTRUCTOR Init(ISP,IZE:WORD;Len:BYTE;InfNo:STRING;M:TModi;EA:TPrae);
           DESTRUCTOR Release;VIRTUAL;
           FUNCTION  GetFocusID:STRING;VIRTUAL;
           PROCEDURE SetFocusOnId(ItK:STRING);VIRTUAL;
           PROCEDURE SetData(DataSeg,DataOfs:WORD);VIRTUAL;
           PROCEDURE SetToggle(K:STRING;VAL:BOOLEAN);VIRTUAL;
           PROCEDURE Disable(K:STRING); VIRTUAL;
           FUNCTION  Disabled(ItK:STRING):BOOLEAN; VIRTUAL;
           PROCEDURE Enable(K:STRING); VIRTUAL;
           PROCEDURE Show(K:STRING); VIRTUAL;
           PROCEDURE ShowBar(Clear:BOOLEAN);VIRTUAL;
           FUNCTION  Input:INTEGER;VIRTUAL;
           PROCEDURE Save(K:STRING);VIRTUAL;
           PROCEDURE Restore(K:STRING);VIRTUAL;
           FUNCTION  Changed(K:STRING):BOOLEAN;VIRTUAL;
           PROCEDURE AddNew(K:STRING; Zeiger:TDialogObjPtr; VAR Def); VIRTUAL;
           PROCEDURE SetFocusField(FPos:BYTE);VIRTUAL;
           PROCEDURE AddItem(ItK:STRING; Tx:STRING); VIRTUAL;
           FUNCTION  ItemDisabled(K:STRING;It:WORD):BOOLEAN; VIRTUAL;
           PROCEDURE EnableItem(K:STRING;It:WORD); VIRTUAL;
           PROCEDURE DisableItem(K:STRING;It:WORD); VIRTUAL;
           FUNCTION  Valid(K:STRING):BOOLEAN;VIRTUAL;
           PROCEDURE  SetVal(ItK:STRING;ValString:STRING);VIRTUAL;
           FUNCTION   GetVal(ItK:STRING):STRING;VIRTUAL;
        END;



   TYPE TIntFeld = OBJECT(TEFeld)
           Wert    : ^INTEGER;
           WertSik : INTEGER;
           Min,
           Max  : INTEGER;

           CONSTRUCTOR Init(TiSP,TiZE,ISP,IZE:WORD;Len:BYTE;Txt:STRING;
                            InfNo:STRING;M:TModi;EA:TPrae;Mi,Ma:INTEGER);
           PROCEDURE SetData(DataSeg,DataOfs:WORD);VIRTUAL;
           FUNCTION MakeInpStr:STRING;VIRTUAL;
           PROCEDURE MakeVal(Eingabe:STRING);VIRTUAL;
           FUNCTION ValidKey(VAR Code:INTEGER;VAR CPos:BYTE):BOOLEAN;VIRTUAL;
           FUNCTION ValidInput(VAR Eingabe:STRING):BOOLEAN;VIRTUAL;
           PROCEDURE Save(K:STRING);VIRTUAL;
           PROCEDURE Restore(K:STRING);VIRTUAL;
           FUNCTION  Changed(K:STRING):BOOLEAN;VIRTUAL;
        END;

   TYPE TByteFeld = OBJECT(TEFeld)
           Wert    : ^BYTE;
           WertSik : BYTE;
           Min,
           Max  : BYTE;

           CONSTRUCTOR Init(TiSP,TiZE,ISP,IZE:WORD;Len:BYTE;Txt:STRING;
                            InfNo:STRING;M:TModi;EA:TPrae;Mi,Ma:BYTE);
           PROCEDURE SetData(DataSeg,DataOfs:WORD);VIRTUAL;
           FUNCTION MakeInpStr:STRING;VIRTUAL;
           PROCEDURE MakeVal(Eingabe:STRING);VIRTUAL;
           FUNCTION ValidKey(VAR Code:INTEGER;VAR CPos:BYTE):BOOLEAN;VIRTUAL;
           FUNCTION ValidInput(VAR Eingabe:STRING):BOOLEAN;VIRTUAL;
           PROCEDURE Save(K:STRING);VIRTUAL;
           PROCEDURE Restore(K:STRING);VIRTUAL;
           FUNCTION  Changed(K:STRING):BOOLEAN;VIRTUAL;
        END;

   TYPE TWordFeld = OBJECT(TEFeld)
           Wert    : ^WORD;
           WertSik : WORD;
           Min,
           Max  : WORD;

           CONSTRUCTOR Init(TiSP,TiZE,ISP,IZE:WORD;Len:BYTE;Txt:STRING;
                            InfNo:STRING;M:TModi;EA:TPrae;Mi,Ma:WORD);
           PROCEDURE SetData(DataSeg,DataOfs:WORD);VIRTUAL;
           FUNCTION MakeInpStr:STRING;VIRTUAL;
           PROCEDURE MakeVal(Eingabe:STRING);VIRTUAL;
           FUNCTION ValidKey(VAR Code:INTEGER;VAR CPos:BYTE):BOOLEAN;VIRTUAL;
           FUNCTION ValidInput(VAR Eingabe:STRING):BOOLEAN;VIRTUAL;
           PROCEDURE Save(K:STRING);VIRTUAL;
           PROCEDURE Restore(K:STRING);VIRTUAL;
           FUNCTION  Changed(K:STRING):BOOLEAN;VIRTUAL;
        END;

        TLongIntFeld = OBJECT(TEFeld)
           Wert    : ^LONGINT;
           WertSik : LONGINT;
           Min,
           Max  : LONGINT;

           CONSTRUCTOR Init(TiSP,TiZE,ISP,IZE:WORD;Len:BYTE;Txt:STRING;
                            InfNo:STRING;M:TModi;EA:TPrae;Mi,Ma:LONGINT);
           PROCEDURE SetData(DataSeg,DataOfs:WORD);VIRTUAL;
           FUNCTION MakeInpStr:STRING;VIRTUAL;
           PROCEDURE MakeVal(Eingabe:STRING);VIRTUAL;
           FUNCTION ValidKey(VAR Code:INTEGER;VAR CPos:BYTE):BOOLEAN;VIRTUAL;
           FUNCTION ValidInput(VAR Eingabe:STRING):BOOLEAN;VIRTUAL;
           PROCEDURE Save(K:STRING);VIRTUAL;
           PROCEDURE Restore(K:STRING);VIRTUAL;
           FUNCTION  Changed(K:STRING):BOOLEAN;VIRTUAL;


        END;

        TTextFeld = OBJECT(TEFeld)
           Wert    : ^STRING;
           WertSik : STRING;
           CONSTRUCTOR Init(TiSP,TiZE,ISP,IZE:WORD;Len:BYTE;Txt:STRING;
                            InfNo:STRING;M:TModi;EA:TPrae);
           PROCEDURE SetData(DataSeg,DataOfs:WORD);VIRTUAL;
           FUNCTION MakeInpStr:STRING;VIRTUAL;
           PROCEDURE MakeVal(Eingabe:STRING);VIRTUAL;
           FUNCTION ValidKey(VAR Code:INTEGER;VAR CPos:BYTE):BOOLEAN;VIRTUAL;
           FUNCTION ValidInput(VAR Eingabe:STRING):BOOLEAN;VIRTUAL;
           PROCEDURE Save(K:STRING);VIRTUAL;
           PROCEDURE Restore(K:STRING);VIRTUAL;
           FUNCTION  Changed(K:STRING):BOOLEAN;VIRTUAL;
        END;

        TCharFeld = OBJECT(TEFeld)
           Wert    : ^CHAR;
           WertSik : CHAR;
           CONSTRUCTOR Init(TiSP,TiZE,ISP,IZE:WORD;Txt:STRING;
                            InfNo:STRING;M:TModi;EA:TPrae);
           PROCEDURE SetData(DataSeg,DataOfs:WORD);VIRTUAL;
           FUNCTION MakeInpStr:STRING;VIRTUAL;
           PROCEDURE MakeVal(Eingabe:STRING);VIRTUAL;
           FUNCTION ValidKey(VAR Code:INTEGER;VAR CPos:BYTE):BOOLEAN;VIRTUAL;
           FUNCTION ValidInput(VAR Eingabe:STRING):BOOLEAN;VIRTUAL;
           PROCEDURE Save(K:STRING);VIRTUAL;
           PROCEDURE Restore(K:STRING);VIRTUAL;
           FUNCTION  Changed(K:STRING):BOOLEAN;VIRTUAL;
        END;



        TDatumFeld = OBJECT(TEFeld)
           Wert    : ^TDatum;
           WertSik : TDatum;

           CONSTRUCTOR Init(TiSP,TiZE,ISP,IZE:WORD;Txt:STRING;
                            InfNo:STRING;M:TModi;EA:TPrae);
           PROCEDURE SetData(DataSeg,DataOfs:WORD);VIRTUAL;
           FUNCTION MakeInpStr:STRING;VIRTUAL;
           PROCEDURE MakeVal(Eingabe:STRING);VIRTUAL;
           FUNCTION ValidKey(VAR Code:INTEGER;VAR CPos:BYTE):BOOLEAN;VIRTUAL;
           FUNCTION ValidInput(VAR Eingabe:STRING):BOOLEAN;VIRTUAL;
           PROCEDURE Save(K:STRING);VIRTUAL;
           PROCEDURE Restore(K:STRING);VIRTUAL;
           FUNCTION  Changed(K:STRING):BOOLEAN;VIRTUAL;
        END;


        TZeitFeld = OBJECT(TEFeld)
           Wert    : ^TZeit;
           WertSik : TZeit;
           CONSTRUCTOR Init(TiSP,TiZE,ISP,IZE:WORD;Txt:STRING;
                            InfNo:STRING;M:TModi;EA:TPrae);
           PROCEDURE SetData(DataSeg,DataOfs:WORD);VIRTUAL;
           FUNCTION MakeInpStr:STRING;VIRTUAL;
           PROCEDURE MakeVal(Eingabe:STRING);VIRTUAL;
           FUNCTION ValidKey(VAR Code:INTEGER;VAR CPos:BYTE):BOOLEAN;VIRTUAL;
           FUNCTION ValidInput(VAR Eingabe:STRING):BOOLEAN;VIRTUAL;
           PROCEDURE Save(K:STRING);VIRTUAL;
           PROCEDURE Restore(K:STRING);VIRTUAL;
           FUNCTION  Changed(K:STRING):BOOLEAN;VIRTUAL;
        END;

        TRealFeld = OBJECT(TEFeld)
           Wert    : ^REAL;
           WertSik : REAL;
           Min,
           Max  : REAL;
           Nachkommastellen : BYTE;
           CONSTRUCTOR Init(TiSP,TiZE,ISP,IZE:WORD;Len:BYTE;Txt:STRING;
                            InfNo:STRING;M:TModi;EA:TPrae;
                            NKo:BYTE;Mi,Ma:REAL);
           PROCEDURE SetData(DataSeg,DataOfs:WORD);VIRTUAL;
           FUNCTION MakeInpStr:STRING;VIRTUAL;
           PROCEDURE MakeVal(Eingabe:STRING);VIRTUAL;
           FUNCTION SpecialEval(VAR Eingabe:STRING;VAR InputCode:INTEGER;
                                VAR CPos:BYTE):BOOLEAN;VIRTUAL;
           FUNCTION ValidKey(VAR Code:INTEGER;VAR CPos:BYTE):BOOLEAN;VIRTUAL;
           FUNCTION ValidInput(VAR Eingabe:STRING):BOOLEAN;VIRTUAL;
           PROCEDURE Save(K:STRING);VIRTUAL;
           PROCEDURE Restore(K:STRING);VIRTUAL;
           FUNCTION  Changed(K:STRING):BOOLEAN;VIRTUAL;
        END;

        TBoolFeld = OBJECT(TEFeld)
           Wert    : ^BOOLEAN;
           WertSik : BOOLEAN;

           CONSTRUCTOR Init(TiSP,TiZE,ISP,IZE:WORD;Txt:STRING;
                            InfNo:STRING;M:TModi;EA:TPrae);
           PROCEDURE SetData(DataSeg,DataOfs:WORD);VIRTUAL;
           FUNCTION MakeInpStr:STRING;VIRTUAL;
           PROCEDURE MakeVal(Eingabe:STRING);VIRTUAL;
           FUNCTION ValidKey(VAR Code:INTEGER;VAR CPos:BYTE):BOOLEAN;VIRTUAL;
           FUNCTION ValidInput(VAR Eingabe:STRING):BOOLEAN;VIRTUAL;
           PROCEDURE Save(K:STRING);VIRTUAL;
           PROCEDURE Restore(K:STRING);VIRTUAL;
           FUNCTION  Changed(K:STRING):BOOLEAN;VIRTUAL;
        END;

        TSwitchFeld = OBJECT(TBoolFeld)
           FUNCTION MakeInpStr:STRING; VIRTUAL;
           PROCEDURE MakeVal(Eingabe:STRING); VIRTUAL;
           FUNCTION Input:INTEGER; VIRTUAL;
           PROCEDURE PrintInhalt(Inhalt:STRING);VIRTUAL;
           FUNCTION ValidKey(VAR Code:INTEGER; VAR CPos:BYTE):BOOLEAN; VIRTUAL;
           FUNCTION ValidInput(VAR Eingabe:STRING):BOOLEAN; VIRTUAL;
        END;


        TRItemPtr = ^TRItem;
        TRItem = RECORD
           Next,
           Last : TRItemPtr;
           Enabled : BOOLEAN;
           Txt : STRING;
        END;

        TRollFeld = OBJECT(TEFeld)
           Wert     : ^WORD;
           WertSik  : WORD;
           Anzahl   : WORD;
           AktItem  : TRitemPtr;
           ItemList : TRItemPtr;
           CONSTRUCTOR Init(TiSp,TiZe,ISP,IZE:WORD;Len:BYTE;Txt:STRING;
                            InfNo:STRING);
           PROCEDURE SetData(DataSeg,DataOfs:WORD);VIRTUAL;
           PROCEDURE AddItem(K:STRING;TX:STRING);VIRTUAL;
           FUNCTION  ItemDisabled(K:STRING;It:WORD):BOOLEAN;VIRTUAL;
           PROCEDURE EnableItem(K:STRING;It:WORD);VIRTUAL;
           PROCEDURE DisableItem(K:STRING;It:WORD);VIRTUAL;
           FUNCTION  GetInfoNo:STRING;VIRTUAL;
           FUNCTION  NextItem:WORD;VIRTUAL;
           FUNCTION  PrevItem:WORD;VIRTUAL;
           FUNCTION  MakeInpStr:STRING;VIRTUAL;
           PROCEDURE MakeVal(Eingabe:STRING);VIRTUAL;
           FUNCTION  Input:INTEGER;VIRTUAL;
           PROCEDURE GetActItem;VIRTUAL;
           PROCEDURE Save(K:STRING);VIRTUAL;
           PROCEDURE Restore(K:STRING);VIRTUAL;
           FUNCTION  Changed(K:STRING):BOOLEAN;VIRTUAL;
        END;

        TChoiceFeld = OBJECT(TEFeld)
           Wert    : ^BYTE;
           WertSik : BYTE;
           Auswahl : TAuswahlFenster;
           CONSTRUCTOR Init(TiSp,TiZe,ISP,IZE:WORD;Len:BYTE;Txt:STRING;
                            InfNo:STRING;FTxt:STRING);
           PROCEDURE SetData(DataSeg,DataOfs:WORD);VIRTUAL;
           FUNCTION  GetInfoNo:STRING;VIRTUAL;
           PROCEDURE AddItem(ItK:STRING;TX:STRING);VIRTUAL;
           FUNCTION  ItemDisabled(K:STRING;It:WORD):BOOLEAN;VIRTUAL;
           PROCEDURE EnableItem(K:STRING;It:WORD);VIRTUAL;
           PROCEDURE DisableItem(K:STRING;It:WORD);VIRTUAL;
           FUNCTION  MakeInpStr:STRING;VIRTUAL;
           PROCEDURE MakeVal(Eingabe:STRING);VIRTUAL;
           FUNCTION  Input:INTEGER;VIRTUAL;
           PROCEDURE Save(K:STRING);VIRTUAL;
           PROCEDURE Restore(K:STRING);VIRTUAL;
           FUNCTION  Changed(K:STRING):BOOLEAN;VIRTUAL;
        END;

   TYPE TKwFeld = OBJECT(TEFeld)
           Wert    : ^TWeek;
           WertSik : TWeek;

           CONSTRUCTOR Init(TiSP,TiZE,ISP,IZE:WORD;Txt:STRING;
                               InfNo:STRING;M:TModi;EA:TPrae);
           PROCEDURE SetData(DataSeg,DataOfs:WORD);VIRTUAL;
           FUNCTION MakeInpStr:STRING;VIRTUAL;
           PROCEDURE MakeVal(Eingabe:STRING);VIRTUAL;
           FUNCTION ValidKey(VAR Code:INTEGER;VAR CPos:BYTE):BOOLEAN;VIRTUAL;
           FUNCTION ValidInput(VAR Eingabe:STRING):BOOLEAN;VIRTUAL;
           PROCEDURE Save(K:STRING);VIRTUAL;
           PROCEDURE Restore(K:STRING);VIRTUAL;
           FUNCTION Changed(K:STRING):BOOLEAN;VIRTUAL;
        END;

   TYPE TNumTextFeld = OBJECT(TTextFeld)
           FUNCTION ValidKey(VAR Code:INTEGER;VAR CPos:BYTE):BOOLEAN;VIRTUAL;
           FUNCTION Changed(K:STRING):BOOLEAN;VIRTUAL;
        END;

   TYPE TMitNullFeld = OBJECT(TLongintFeld)
           FUNCTION MakeInpStr:STRING;VIRTUAL;
        END;




IMPLEMENTATION

   USES GRAPH,
        GRAPNEU;

   CONSTRUCTOR TTabZeileFeld.Init;
      BEGIN
         inherited Init(ISp,IZe,ISP,IZE,0,'',InfNo,M,EA);
         NEW(FeldListenAnfang);
         FeldListenAnfang^.Vorgaenger := FeldListenAnfang;
         FeldListenAnfang^.Nachfolger := FeldListenAnfang;
         AnzahlFelder := 0;
         FocusField := 0;
         FocusId := '';
         Enable('_');
         MultiObj := TRUE;
      END;

   PROCEDURE TTabZeileFeld.SetData;
      BEGIN
         Wert := PTR(DataSeg,DataOfs)
      END;



   PROCEDURE TTabZeileFeld.SetToggle;
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

   PROCEDURE TTabZeileFeld.Disable;
      VAR   Zeiger  : TFeldZgr;
            Lauf    : BYTE;

      BEGIN
         IF (K = '_') THEN BEGIN
            inherited Disable('');
         END ELSE BEGIN
            Zeiger := FeldListenAnfang;
            FOR Lauf := 1 TO AnzahlFelder DO BEGIN
               WITH Zeiger^ DO BEGIN
                  IF (Feld^.MultiObj) AND ((K = '') OR (K = FeldKennung))THEN
                     Feld^.Disable('_');
                  IF (K = '') OR (FeldKennung = K) OR (Feld^.MultiObj) THEN
                     Feld^.Disable(K);
               END;
               Zeiger:=Zeiger^.Nachfolger;
            END
         END;
      END;


   PROCEDURE TTabZeileFeld.Enable;
      VAR   Zeiger  : TFeldZgr;
            Lauf    : BYTE;

      BEGIN
         IF (K = '_') THEN BEGIN
            inherited Enable('');
         END ELSE BEGIN
            Zeiger := FeldListenAnfang;
            FOR Lauf := 1 TO AnzahlFelder DO BEGIN
               WITH Zeiger^ DO BEGIN
                  IF (Feld^.MultiObj) AND ((K = '') OR (K = FeldKennung))THEN
                     Feld^.Enable('_');
                  IF (K = '') OR (FeldKennung = K) OR (Feld^.MultiObj) THEN
                     Feld^.Enable(K);
               END;
               Zeiger:=Zeiger^.Nachfolger;
            END;
         END;
      END;

   FUNCTION  TTabZeileFeld.Disabled;
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


   PROCEDURE TTabZeileFeld.ShowBar;
        VAR IVGSik,
            IHGSik,
            FVGSik,
            FHGSik,
            TVGSik,
            THGSik : BYTE;
            Laenge : BYTE;
            AktFDA : BOOLEAN;
      BEGIN
         IF Clear THEN BEGIN
            BackExtra(Left,Top,(Right-Left),AktuelleFarben^.FeldHG[DI],
                      AktuelleFarben^.FeldHG[DI]);
            Show('');
         END ELSE BEGIN
            BackExtra(Left,Top,(Right-Left),AktuelleFarben^.FeldUM[I],
                      AktuelleFarben^.FeldHG[DI]);
            Show('');
         END;
      END;

   PROCEDURE TTabZeileFeld.SetFocusField;
      VAR Lauf : BYTE;
          Zgr  : TFeldZgr;
      BEGIN
         IF (FPos = 255) THEN BEGIN
            Zgr := FeldListenAnfang;
            Zgr := Zgr^.Vorgaenger;
            Lauf := AnzahlFelder;
            WHILE (Zgr^.Feld^.Disabled('')) AND (Lauf > 1) DO BEGIN
               Zgr := Zgr^.Vorgaenger;
               DEC(Lauf)
            END;
            FocusField := Lauf;
         END ELSE
            inherited SetFocusField(FPos);
      END;


   PROCEDURE TTabZeileFeld.AddNew;

      VAR   NeuerZeiger,
            HilfsZeiger : TFeldZgr;

      BEGIN
         inherited AddNew(K,Zeiger,Def);
         IF Left > Zeiger^.Left THEN
            TISpalte := Zeiger^.Left;
         IF Right < Zeiger^.Right THEN
            ISpalte := Zeiger^.Right;
      END;

   FUNCTION TTabZeileFeld.Changed;
      VAR   Ergebnis : BOOLEAN;
            Lauf : BYTE;
            Zgr  : TFeldZgr;

      BEGIN
         Ergebnis := FALSE;
         IF (K = '_') THEN BEGIN
            Ergebnis := (Wert^ <> WertSik);
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

   PROCEDURE  TTabZeileFeld.AddItem;
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


   PROCEDURE  TTabZeileFeld.DisableItem;
      VAR   Lauf   : BYTE;
           Zeiger : TFeldZgr;
           ItkF,
           ItkR   : STRING;

      BEGIN
         Lauf := 1;
         Zeiger := FeldListenAnfang;
         SplitString(K,'_',ItkF,ItkR);
         WHILE (Lauf <= AnzahlFelder) DO BEGIN
            IF ((Zeiger^.FeldKennung = ItKF) AND Zeiger^.Feld^.ItemObj) OR
                (Zeiger^.Feld^.MultiObj) THEN
               Zeiger^.Feld^.DisableItem(ItKR,It);
            INC(Lauf);
            Zeiger := Zeiger^.Nachfolger;
         END;
      END;


   PROCEDURE  TTabzeileFeld.EnableItem;
      VAR   Lauf   : BYTE;
            Zeiger : TFeldZgr;
            ItkF,
            ItkR   : STRING;

      BEGIN
         Lauf := 1;
         Zeiger := FeldListenAnfang;
         SplitString(K,'_',ItkF,ItkR);
         WHILE (Lauf <= AnzahlFelder) DO BEGIN
            IF ((Zeiger^.FeldKennung = ItKF) AND Zeiger^.Feld^.ItemObj) OR
                (Zeiger^.Feld^.MultiObj) THEN
                Zeiger^.Feld^.EnableItem(ItKR,It);
            INC(Lauf);
            Zeiger := Zeiger^.Nachfolger;
         END;
      END;

   FUNCTION  TTabzeileFeld.ItemDisabled;
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

   PROCEDURE TTabZeileFeld.Show;
      VAR   Zeiger     : TFeldZgr;
            Lauf       : BYTE;
            AktFieldDA : BOOLEAN;
            ShowAll    : BOOLEAN;
      BEGIN
         ShowAll := (K = '') OR (K = '_');
         IF ShowAll THEN
            K := '';
         IF (Wert^) THEN
            Enable('_')
         ELSE
            Disable('_');
         Zeiger := FeldListenAnfang;
         FOR Lauf := 1 TO AnzahlFelder DO BEGIN
            WITH Zeiger^ DO BEGIN
               IF (K = '') OR (FeldKennung = K) OR (Feld^.MultiObj) THEN BEGIN
                  IF (NOT Wert^) AND ShowAll THEN BEGIN
                     AktFieldDA := Feld^.Disabled('');
                     Feld^.Disable(K);
                  END;
                  IF Feld^.MultiObj THEN
                     Feld^.Show('_')
                  ELSE
                     Feld^.Show(K);
                  IF (NOT Wert^) AND (NOT AktFieldDA) AND ShowAll THEN
                     Feld^.Enable(K);
               END;
            END;
            Zeiger:=Zeiger^.Nachfolger;
         END;
      END;

   FUNCTION TTabZeileFeld.GetFocusID;
      VAR   Zeiger  : TFeldZgr;
            LAUF  : BYTE;
            HText : STRING;

      BEGIN
         Htext := '';
         IF (FocusField = 255) THEN BEGIN
            Zeiger := FeldListenAnfang;
            Zeiger := Zeiger^.Vorgaenger;
            Lauf := AnzahlFelder;
            WHILE (Lauf > 1) AND Zeiger^.Feld^.Disabled('') DO BEGIN
               Zeiger:= Zeiger^.Vorgaenger;
               DEC(Lauf);
            END;
         END ELSE BEGIN
            IF (FocusField > 0) THEN BEGIN
               Zeiger := FeldListenAnfang;
               FOR Lauf := 1 TO FocusField-1 DO
                   Zeiger := Zeiger^.Nachfolger;
            END;
         END;
         IF (FocusField > 0) THEN BEGIN
             HText:= Zeiger^.Feld^.GetFocusId;
             IF (HText <> '') THEN
                 HText := Zeiger^.Feldkennung+'_'+HText
             ELSE
                 HText := Zeiger^.FeldKennung;
         END;
         GetFocusId := HText;
      END;

   PROCEDURE TTabZeileFeld.SetFocusOnID;
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

   FUNCTION TTabZeileFeld.Input;

      VAR   Zeiger  : TFeldZgr;
            InpString : STRING;
            Nr        : BYTE;
            OuterEnde : BOOLEAN;
            OldEndCode,
            InputCode,
            EndCode : INTEGER;
            MoreEval,
            Check,
            Ende    : BOOLEAN;
            KeineFelder :BOOLEAN;
            AnzDisabled : WORD;
            LAUF  : BYTE;

      BEGIN
         InpString := '';
         Nr := 0;
         Abbruch := FALSE;
         Done := FALSE;
         Zeiger := FeldListenAnfang;
         IF (FocusField > 1) THEN BEGIN
            FOR Lauf := 1 TO FocusField-1 DO
               Zeiger := Zeiger^.Nachfolger;
            FocusId := GetFocusId;
         END ELSE BEGIN
             IF (AnzahlFelder > 0) THEN BEGIN
                FocusField := 1;
                FocusId := GetFocusID;
             END
         END;
         Show('');
         InputCode := 0;
         EndCode := 0;
         OuterEnde := FALSE;
         WHILE NOT OuterEnde DO BEGIN
            IF Disabled('') THEN BEGIN
               OuterEnde := TRUE;
               IF Toggle THEN BEGIN
                  ShowBar(FALSE);
                  REPEAT
                     OldEndCode := EndCode;
                     EndCode := InputKey;  {Hier wichtig um Toggle zu Erm�glichen}
                     Ende := KeyAction(EndCode,Nr,InpString,Abbruch,Done,Check,MoreEval);
                     KeyDone := Done AND (NOT MoreEval);
                     IF (NOT Done) THEN BEGIN
                        IF (EndCode = -1) AND (OldEndCode <> EndCode) THEN
                           EndCode := OldEndCode;
                        CASE EndCode OF
                           13,9,-80 : BEGIN
                                        Ende := TRUE;
                                        Done := TRUE;
                                      END;
                           -15      : BEGIN
                                         Ende := TRUE;
                                         Done := TRUE;
                                         FocusField := 255;
                                      END;
                           -72      : BEGIN
                                         Ende := TRUE;
                                         Done := TRUE;
                                      END;
                           27       : BEGIN
                                         Ende := TRUE;
                                         Done := TRUE;
                                         Abbruch := TRUE;
                                      END;
                           -68..-58,
                           -113..-84 : BEGIN
                                         IF (EndCode = -66) AND Toggle THEN BEGIN
                                            Wert^:= TRUE;
                                            Enable('_');
                                            FocusField := 0;
                                            OuterEnde := FALSE;
                                         END;
                                         Ende := TRUE;
                                         Done := TRUE;
                                      END;
                        END;
                        IF ActionActive THEN
                           Ende :=  ActionFunc OR Ende;
                     END ELSE BEGIN
                        IF EndCode = 0 THEN BEGIN
                           Ende := TRUE;
                           Done := TRUE;
                        END;
                     END;
                  UNTIL Ende;
                  ShowBar(TRUE);
                  InputCode := EndCode;
               END;
            END ELSE BEGIN
               Ende := FALSE;
               KeineFelder:= FALSE;
               EndCode := 0;
               OuterEnde := TRUE;
               IF (AnzahlFelder = 0) THEN
                  KeineFelder := TRUE;
               IF NOT KeineFelder THEN BEGIN
                  AnzDisabled := 0;
                  EndCode := 13;
                  REPEAT
                     OldEndCode := EndCode;
                     EndCode := Zeiger^.Feld^.Input;
                     IF (EndCode = 0) AND (Zeiger^.Feld^.Done) THEN BEGIN
                        IF NOT Zeiger^.Feld^.Abbruch THEN BEGIN
                           KeyDone := Zeiger^.Feld^.KeyDone;
                           Zeiger := FeldListenAnfang;
                           IF (FocusField > 1) THEN BEGIN
                              FOR Lauf := 1 TO FocusField-1 DO
                                  Zeiger := Zeiger^.Nachfolger;
                           END;
                           Ende := TRUE;
                           Done := TRUE;
                        END ELSE BEGIN
                           Ende := TRUE;
                           Abbruch := TRUE;
                           Done := TRUE;
                        END;
                     END ELSE BEGIN
                        IF (EndCode = 0) THEN BEGIN
                           EndCode := OldEndCode;
                           INC(AnzDisabled);
                        END ELSE
                           AnzDisabled := 0;
                        Ende := KeyAction(EndCode,Nr,InpString,Abbruch,Done,Check,MoreEval);
                        KeyDone := Done AND (NOT MoreEval);
                        IF (NOT Done) THEN BEGIN
                           IF (EndCode = -1) AND (EndCode <> OldEndCode) THEN
                              EndCode := OldEndCode;
                           CASE EndCode OF
                                13,9     : BEGIN
                                            Zeiger:=Zeiger^.Nachfolger;
                                            INC(FocusField);
                                            IF (Zeiger = FeldListenanfang) THEN BEGIN
                                               Ende := TRUE;
                                               Done := TRUE;
                                               FocusField := 1;
                                            END;
                                         END;
                             -80,-72  : BEGIN
                                           Ende := TRUE;
                                           Done := TRUE;
                                        END;
                             -15      : BEGIN
                                           DEC(FocusField);
                                           IF (Zeiger = FeldListenAnfang) THEN BEGIN
                                              Ende := TRUE;
                                              Done := TRUE;
                                              FocusField := 255;
                                           END ELSE
                                              Zeiger:=Zeiger^.Vorgaenger;
                                         END;
                             27       : BEGIN
                                           Ende := TRUE;
                                           Done := TRUE;
                                           Abbruch := TRUE;
                                        END;
                             -68..-59,
                             -113..-84 : BEGIN
                                           IF (EndCode = -66) AND Toggle THEN BEGIN
                                              Wert^:= FALSE;
                                              Disable('_');
                                              OuterEnde := FALSE;
                                           END;
                                           Ende := TRUE;
                                           Done := TRUE;
                                        END;
                          END;
                        END;
                        IF ActionActive THEN
                           Ende :=  ActionFunc OR Ende;
                     END;
                  UNTIL Ende OR (AnzDisabled >= AnzahlFelder);
               END;
               InputCode := EndCode;
            END;
         END;
         Input:= InputCode;
      END;


   PROCEDURE TTabZeileFeld.Save;
      VAR Zeiger  : TFeldZgr;
          Lauf : BYTE;
      BEGIN
         IF (K = '_') THEN BEGIN
            WertSik:= Wert^;
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

   PROCEDURE TTabZeileFeld.Restore;
      VAR Zeiger  : TFeldZgr;
          Lauf : BYTE;
      BEGIN
         IF (K = '_') THEN BEGIN
            Wert^:= WertSik;
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

   FUNCTION TTabZeileFeld.GetVal;
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

   PROCEDURE TTabZeileFeld.SetVal;
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


   FUNCTION TTabZeileFeld.Valid;
      BEGIN
         Valid :=TRUE;
      END;


   DESTRUCTOR TTabZeileFeld.Release;

      VAR   Zeiger1 : TFeldZgr;
            Lauf : BYTE;

      BEGIN
         Zeiger1 := FeldListenAnfang^.Vorgaenger;
         FOR Lauf := 1 TO AnzahlFelder-1 DO BEGIN
            DISPOSE(Zeiger1^.Feld,Release);
            Zeiger1:=Zeiger1^.Vorgaenger;
            DISPOSE(Zeiger1^.Nachfolger);
         END;
         DISPOSE(FeldListenAnfang^.Feld,Release);
         DISPOSE(FeldListenAnfang);
         inherited RELEASE;
      END;


   CONSTRUCTOR TIntFeld.Init;
      BEGIN
         inherited Init(TiSP,TiZE,ISP,IZE,Len,Txt,InfNo,M,EA);
         Min := Mi;
         Max := Ma
      END;

   PROCEDURE TIntFeld.SetData;
      BEGIN
         Wert := PTR(DataSeg,DataOfs)
      END;


   FUNCTION TIntFeld.MakeInpStr;
     VAR Txt : STRING;
        BEGIN
          IF Wert^ <> 0 THEN
            STR(Wert^,Txt)
          ELSE
            Txt :='';
          MakeInpStr:=Txt
        END;

      PROCEDURE TIntFeld.MakeVal;
        VAR C : INTEGER;
        BEGIN
          IF Eingabe = '' THEN Wert^ := 0
          ELSE VAL(Eingabe,Wert^,C);
        END;

      FUNCTION TIntFeld.ValidKey;
        BEGIN
          ValidKey:=((Code>=48) AND (Code<=57)) OR
                    ((Code = 45) AND (Min < 0));
        END;

      FUNCTION TIntFeld.ValidInput;
        VAR ZWWert  : LONGINT;
            C       : INTEGER;

        BEGIN
          VAL(Eingabe,ZWWert,C);
          IF C <> 0 THEN
            IF ZWWert < 0 THEN ZWWert := MAXINT*(-1)
            ELSE ZWWert:= MAXINT;
          IF ((ZWWert < Min) OR ( ZWWert > Max)) AND (Eingabe <> '') THEN BEGIN
            IF (ZWWert > Max) THEN ZWWert := Max
            ELSE ZWWert := Min;
            STR(ZWWert,Eingabe);
            ValidInput := FALSE
          END ELSE ValidInput := TRUE;
        END;

      PROCEDURE TIntFeld.Save;
        BEGIN
          WertSik:= Wert^;
        END;

      PROCEDURE TIntFeld.Restore;
        BEGIN
          Wert^ := WertSik;
        END;

      FUNCTION TIntFeld.Changed;
        BEGIN
          Changed := (Wert^ <> WertSik);
        END;

      CONSTRUCTOR TByteFeld.Init;
        BEGIN
          inherited Init(TiSP,TiZE,ISP,IZE,Len,Txt,InfNo,M,EA);
          Min := Mi;
          Max := Ma
        END;

      PROCEDURE TByteFeld.SetData;
        BEGIN
          Wert := PTR(DataSeg,DataOfs)
        END;


      FUNCTION TByteFeld.MakeInpStr;
        VAR Txt : STRING;
        BEGIN
          IF Wert^ <> 0 THEN
            STR(Wert^,Txt)
          ELSE
            Txt :='';
          MakeInpStr:=Txt
        END;

      PROCEDURE TByteFeld.MakeVal;
        VAR C : INTEGER;
        BEGIN
          IF Eingabe = '' THEN Wert^ := 0
          ELSE VAL(Eingabe,Wert^,C);
        END;

      FUNCTION TByteFeld.ValidKey;
        BEGIN
          ValidKey:=((Code>=48) AND (Code<=57));
        END;

      FUNCTION TByteFeld.ValidInput;
        VAR ZWWert  : LONGINT;
            C       : INTEGER;

        BEGIN
          VAL(Eingabe,ZWWert,C);
          IF C <> 0 THEN
            IF ZWWert < 0 THEN ZWWert := 0
            ELSE ZWWert:= 255;
          IF ((ZWWert < Min) OR ( ZWWert > Max)) AND (Eingabe <> '') THEN BEGIN
            IF (ZWWert > Max) THEN ZWWert := Max
            ELSE ZWWert := Min;
            STR(ZWWert,Eingabe);
            ValidInput := FALSE
          END ELSE ValidInput := TRUE;
        END;

      PROCEDURE TByteFeld.Save;
        BEGIN
          WertSik:= Wert^;
        END;

      PROCEDURE TByteFeld.Restore;
        BEGIN
          Wert^ := WertSik;
        END;

      FUNCTION TByteFeld.Changed;
        BEGIN
          Changed := (Wert^ <> WertSik);
        END;

      CONSTRUCTOR TWordFeld.Init;
        BEGIN
          inherited Init(TiSP,TiZE,ISP,IZE,Len,Txt,InfNo,M,EA);
          Min := Mi;
          Max := Ma
        END;

      PROCEDURE TWordFeld.SetData;
        BEGIN
          Wert := PTR(DataSeg,DataOfs)
        END;

      FUNCTION TWordFeld.MakeInpStr;
        VAR Txt : STRING;
        BEGIN
          IF Wert^ <> 0 THEN
            STR(Wert^,Txt)
          ELSE
            Txt :='';
          MakeInpStr:=Txt
        END;

      PROCEDURE TWordFeld.MakeVal;
        VAR C : INTEGER;
        BEGIN
          IF Eingabe = '' THEN Wert^ := 0
          ELSE VAL(Eingabe,Wert^,C);
        END;

      FUNCTION TWordFeld.ValidKey;
        BEGIN
          ValidKey:=((Code>=48) AND (Code<=57));
        END;

      FUNCTION TWordFeld.ValidInput;
        VAR ZWWert  : LONGINT;
            C       : INTEGER;

        BEGIN
          VAL(Eingabe,ZWWert,C);
          IF C <> 0 THEN
            IF ZWWert < 0 THEN ZWWert := 0
            ELSE ZWWert:= 65535;
          IF ((ZWWert < Min) OR ( ZWWert > Max)) AND (Eingabe <> '') THEN BEGIN
            IF (ZWWert > Max) THEN ZWWert := Max
            ELSE ZWWert := Min;
            STR(ZWWert,Eingabe);
            ValidInput := FALSE
          END ELSE ValidInput := TRUE;
        END;

      PROCEDURE TWordFeld.Save;
        BEGIN
          WertSik:= Wert^;
        END;

      PROCEDURE TWordFeld.Restore;
        BEGIN
          Wert^ := WertSik;
        END;

      FUNCTION TWordFeld.Changed;
        BEGIN
          Changed := (Wert^ <> WertSik);
        END;

      CONSTRUCTOR TLongIntFeld.Init;

        BEGIN
          inherited Init(TiSP,TiZE,ISP,IZE,Len,Txt,InfNo,M,EA);
          Min := Mi;
          Max := Ma
        END;

      PROCEDURE TLongIntFeld.SetData;

        BEGIN
          Wert := PTR(DataSeg,DataOfs)
        END;


      FUNCTION TLongIntFeld.MakeInpStr;

        VAR Txt : STRING;

        BEGIN
          IF Wert^ <> 0 THEN
            STR(Wert^,Txt)
          ELSE
            Txt :='';
          MakeInpStr:=Txt
        END;

      PROCEDURE TLongIntFeld.MakeVal;

        VAR C : INTEGER;

        BEGIN
          IF Eingabe = '' THEN Wert^ := 0
          ELSE VAL(Eingabe,Wert^,C);
        END;

      FUNCTION TLongIntFeld.ValidKey;

        BEGIN
          ValidKey:=((Code>=48) AND (Code<=57)) OR
                    ((Code = 45) AND (Min < 0));
        END;

      FUNCTION TLongIntFeld.ValidInput;

        VAR ZWWert  : LONGINT;
            C       : INTEGER;

        BEGIN
          VAL(Eingabe,ZWWert,C);
          IF C <> 0 THEN
            IF ZWWert < 0 THEN ZWWert := MAXINT*(-1)
            ELSE ZWWert:= MAXINT;
          IF ((ZWWert < Min) OR ( ZWWert > Max)) AND (Eingabe <> '') THEN BEGIN
            IF (ZWWert > Max) THEN ZWWert := Max
            ELSE ZWWert := Min;
            STR(ZWWert,Eingabe);
            ValidInput := FALSE
          END ELSE ValidInput := TRUE;
        END;

      PROCEDURE TLongIntFeld.Save;
        BEGIN
          WertSik:= Wert^;
        END;

      PROCEDURE TLongIntFeld.Restore;
        BEGIN
          Wert^ := WertSik;
        END;

      FUNCTION TLongIntFeld.Changed;
        BEGIN
          Changed := (Wert^<> WertSik);
        END;


      CONSTRUCTOR TTextFeld.Init;

        BEGIN
          inherited Init(TiSP,TiZE,ISP,IZE,Len,Txt,InfNo,M,EA);
        END;

   PROCEDURE TTextFeld.SetData;
        BEGIN
          Wert := PTR(DataSeg,DataOfs)
        END;

      FUNCTION TTextFeld.MakeInpStr;

        BEGIN
            MakeInpStr:=Wert^
        END;

      PROCEDURE TTextFeld.MakeVal;

        BEGIN
          Wert^ := Eingabe
        END;

      FUNCTION TTextFeld.ValidKey;

        BEGIN
          ValidKey:= ((Code>=32) AND (Code<=125)) OR (Code=129) OR (Code=132)
                  OR (Code=142) OR (Code=148) OR (Code=153) OR (Code=154)
        END;

      FUNCTION TTextFeld.ValidInput;

        BEGIN
          ValidInput := TRUE
        END;

      PROCEDURE TTextFeld.Save;
        BEGIN
          WertSik := Wert^;
        END;

      PROCEDURE TTextFeld.Restore;
        BEGIN
          Wert^  := WertSik;
        END;

      FUNCTION TTextFeld.Changed;
        BEGIN
          Changed := (Wert^ <> WertSik);
        END;

   CONSTRUCTOR TCharFeld.Init;

        BEGIN
          inherited Init(TiSP,TiZE,ISP,IZE,1,Txt,InfNo,M,EA);
        END;

   PROCEDURE TCharFeld.SetData;
        BEGIN
          Wert := PTR(DataSeg,DataOfs)
        END;

      FUNCTION TCharFeld.MakeInpStr;

        BEGIN
            MakeInpStr:=Wert^
        END;

      PROCEDURE TCharFeld.MakeVal;

        BEGIN
          Wert^ := Eingabe[1];
        END;

      FUNCTION TCharFeld.ValidKey;

        BEGIN
          ValidKey:= ((Code>=32) AND (Code<=125)) OR (Code=129) OR (Code=132)
                  OR (Code=142) OR (Code=148) OR (Code=153) OR (Code=154)
        END;

      FUNCTION TCharFeld.ValidInput;

        BEGIN
          ValidInput := TRUE
        END;

      PROCEDURE TCharFeld.Save;
        BEGIN
          WertSik := Wert^;
        END;

      PROCEDURE TCharFeld.Restore;
        BEGIN
          Wert^  := WertSik;
        END;

      FUNCTION TCharFeld.Changed;
        BEGIN
          Changed := (Wert^ <> WertSik);
        END;

      CONSTRUCTOR TDatumFeld.Init;

        BEGIN
          inherited Init(TiSP,TiZE,ISP,IZE,8,Txt,InfNo,M,EA);
          Maske:= '  .  .  ';
        END;

      PROCEDURE TDatumFeld.SetData;

        BEGIN
          Wert := PTR(DataSeg,DataOfs)
        END;

      FUNCTION TDatumFeld.MakeInpStr;

        BEGIN
          IF Wert^ ='' THEN
            MakeInpStr := Maske
          ELSE
            MakeInpStr := Wert^
        END;

      PROCEDURE TDatumFeld.MakeVal;

        VAR Tag,
            Monat,
            Jahr   :STRING[2];

        BEGIN
          IF Eingabe = MASKE THEN
             Wert^:=''
          ELSE BEGIN
            Tag := COPY(Eingabe,1,2);
            Monat := COPY(Eingabe,4,2);
            Jahr := COPY(Eingabe,7,2);
            Wert^:=FillUp('0',Tag,2,2)+'.'+FillUp('0',Monat,2,2)+'.'+
                   FillUp('0',Jahr,2,2);
          END;
        END;

      FUNCTION TDatumFeld.ValidKey;

        BEGIN
          ValidKey:= ((Code>=48) AND (Code<=57)) OR (Code = 32)
        END;

      FUNCTION TDatumFeld.ValidInput;

        VAR BTag,
            BMonat,
            BJahr        : BYTE;
            Tag,
            Monat,
            Jahr         : STRING[2];
            C            : INTEGER;

        BEGIN
          Tag := COPY(Eingabe,1,2);
          Monat := COPY(Eingabe,4,2);
          Jahr  := COPY(Eingabe,7,2);
          VAL(Tag,BTag,C);
          VAL(Monat,BMonat,C);
          VAL(Jahr,BJahr,C);
          ValidInput:=((BTag >= 1) AND (BTag <= 31) AND (BMonat >= 1) AND (BMonat <=12)
                       AND (Jahr <> '  ')) OR (Eingabe = Maske);
        END;


      PROCEDURE TDatumFeld.Save;
        BEGIN
          WertSik:= Wert^;
        END;

      PROCEDURE TDatumFeld.Restore;
        BEGIN
          Wert^ := WertSik;
        END;

      FUNCTION TDatumFeld.Changed;
        BEGIN
          Changed := (Wert^<> WertSik);
        END;

      CONSTRUCTOR TZeitFeld.Init;

        BEGIN
          inherited Init(TiSP,TiZE,ISP,IZE,5,Txt,InfNo,M,EA);
          Maske := '  :  ';
        END;

      PROCEDURE TZeitFeld.SetData;

        BEGIN
          Wert := PTR(DataSeg,DataOfs)
        END;

      FUNCTION TZeitFeld.MakeInpStr;

        BEGIN
          IF Wert^ = '' THEN
            MakeInpStr := Maske
          ELSE
            MakeInpStr := Wert^
        END;

      PROCEDURE TZeitFeld.MakeVal;

        BEGIN
          IF (Eingabe = Maske) THEN
             Wert^:= ''
          ELSE
             Wert^ := Eingabe;
        END;

      FUNCTION TZeitFeld.ValidKey;

        BEGIN
          ValidKey:= ((Code>=48) AND (Code<=57)) OR (Code = 32)
        END;

      FUNCTION TZeitFeld.ValidInput;

        VAR Stunde,
            Minuten      : STRING[5];
            BStunde,
            BMinuten     : BYTE;
            C            : INTEGER;

        BEGIN
          Stunde  := COPY(Eingabe,1,2);
          Minuten := COPY(Eingabe,4,2);
          VAL(Stunde,BStunde,C);
          VAL(Minuten,BMinuten,C);
          ValidInput:=((BStunde>= 0) AND (BStunde <= 23) AND (BMinuten >= 0) AND (BMinuten <= 59)) OR
                      (Eingabe = Maske);
        END;

      PROCEDURE TZeitFeld.Save;
        BEGIN
          WertSik:= Wert^;
        END;

      PROCEDURE TZeitFeld.Restore;
        BEGIN
          Wert^ := WertSik;
        END;

      FUNCTION TZeitFeld.Changed;
        BEGIN
          Changed := (Wert^<> WertSik);
        END;

      CONSTRUCTOR TRealFeld.Init;

        BEGIN
          inherited Init(TiSP,TiZE,ISP,IZE,Len,Txt,InfNo,M,EA);
          Min := Mi;
          Max := Ma;
          Nachkommastellen := NKo;
          IF Nachkommastellen > Len+1 THEN
              Nachkommastellen := 0;
          IF NachkommaStellen <> 0 THEN
             Maske := REPLICATE(' ',Len-1-Nko)+'.'+REPLICATE(' ',Nko);
        END;

      PROCEDURE TRealFeld.SetData;

        BEGIN
          Wert := PTR(DataSeg,DataOfs)
        END;

      FUNCTION TRealFeld.MakeInpStr;

        VAR Txt : STRING;

        BEGIN
          IF Wert^ <> 0 THEN
            STR(Wert^:FeldLaenge:Nachkommastellen,Txt)
          ELSE
            Txt :=Maske;
          MakeInpStr:=Txt
        END;

      PROCEDURE TRealFeld.MakeVal;

        VAR C : INTEGER;
            PointPos:BYTE;
            Vor,
            Nach : REAL;
        BEGIN
          IF (Eingabe = '') OR (Eingabe = Maske) THEN
             Wert^ := 0
          ELSE
             VAL(RemoveAllSpaces(Eingabe),Wert^,C)
        END;


   FUNCTION TRealFeld.SpecialEval;
      VAR PointPos : BYTE;
          T : STRING;
          Ergebnis : BOOLEAN;
      BEGIN
         Ergebnis:= FALSE;
         IF SpecialEvalActive THEN BEGIN
            Ergebnis := SpecialEvalFunc(Eingabe,InputCode,CPos);
         END;
         IF NOT Ergebnis THEN BEGIN
            CASE InputCode OF
               44,46 : BEGIN
                          PointPos := POS('.',Maske);
                          IF CPos < PointPos THEN BEGIN
                             Eingabe := COPY(Eingabe,1,CPos-1)+
                                         Replicate(' ',PointPos-CPos)+
                                         COPY(Eingabe,PointPos,FeldLaenge-PointPos+1);
                             CPos := PointPos+1;
                             Ergebnis:= TRUE;
                          END;
                       END;
               45    : IF (Min < 0) THEN BEGIN
                          T := COPY(Eingabe,1,CPos-1);
                          IF FirstMaskChar(T) = 0 THEN BEGIN
                             Eingabe[1] := '-';
                             IF Cpos = 1 THEN
                                INC(CPos);
                             Ergebnis:= TRUE;
                          END;
                       END;
              48..57 : IF ((CPos = 1) AND (Eingabe[1]='-')) THEN BEGIN
                          Eingabe[1] := CHR(Inputcode);
                          CPos := Cpos +1;
                          Ergebnis := TRUE;
                       END;

            END;
         END;
         SpecialEval := Ergebnis;
      END;

      FUNCTION TRealFeld.ValidKey;

        BEGIN
          ValidKey:= (Code>=48) AND (Code<=57);
        END;

      FUNCTION TRealFeld.ValidInput;

        VAR ZWWert  : REAL;
            C       : INTEGER;

        BEGIN
          VAL(Eingabe,ZWWert,C);
          IF (ZWWert < Min) OR ( ZWWert > Max) THEN BEGIN
            IF (ZWWert > Max) THEN ZWWert := Max
            ELSE ZWWert := Min;
            STR(ZWWert:FeldLaenge:Nachkommastellen,Eingabe);
            ValidInput := FALSE
          END ELSE ValidInput := TRUE;
        END;

      PROCEDURE TRealFeld.Save;
        BEGIN
          WertSik:= Wert^;
        END;

      PROCEDURE TRealFeld.Restore;
        BEGIN
          Wert^ := WertSik;
        END;

      FUNCTION TRealFeld.Changed;
        BEGIN
          Changed := (Wert^<> WertSik);
        END;

      CONSTRUCTOR TBoolFeld.Init;

        BEGIN
          inherited Init(TiSP,TiZE,ISP,IZE,1,Txt,InfNo,M,EA);
        END;

      PROCEDURE TBoolFeld.SetData;

        BEGIN
          Wert := PTR(DataSeg,DataOfs)
        END;


      FUNCTION TBoolFeld.MakeInpStr;

        BEGIN
          IF Wert^ THEN MakeInpStr := 'J'
          ELSE MakeInpStr := 'N'
        END;

      PROCEDURE TBoolFeld.MakeVal;

        BEGIN
          IF (Eingabe = 'J') OR (Eingabe = 'j')
          THEN Wert^ := TRUE
          ELSE Wert^ := FALSE
        END;

      FUNCTION TBoolFeld.ValidKey;

        BEGIN
          CASE Code OF
              43,106 : Code :=74;
              45,110 : Code :=78;
          END;
          ValidKey:= (Code=74) OR
                     (Code=78) ;
        END;

      FUNCTION TBoolFeld.ValidInput;

        BEGIN
          ValidInput := TRUE
        END;


      PROCEDURE TBoolFeld.Save;
        BEGIN
          WertSik:= Wert^;
        END;

      PROCEDURE TBoolFeld.Restore;
        BEGIN
          Wert^ := WertSik;
        END;

      FUNCTION TBoolFeld.Changed;
        BEGIN
          Changed := (Wert^<> WertSik);
        END;

   FUNCTION TSwitchFeld.MakeInpStr;

      BEGIN
         IF Wert^ THEN
            MakeInpStr := 'X'
         ELSE
            MakeInpStr := ' '
      END;


   PROCEDURE TSwitchFeld.MakeVal;

      BEGIN
         IF Eingabe = 'X' THEN
           Wert^ := TRUE
         ELSE
            Wert^:= FALSE;
      END;

   FUNCTION TSwitchFeld.ValidKey;

      BEGIN
         ValidKey:= (Code = 74) OR
                    (Code = 106) OR
                    (Code = 78) OR
                    (Code = 110) OR
                    (Code = 32) OR
                    (Code = 88) OR
                    (Code = 120) OR
                    (Code = 43) OR
                    (Code = 45);
      END;

   FUNCTION TSwitchFeld.ValidInput;

      BEGIN
         ValidInput := TRUE
      END;

   PROCEDURE TSwitchFeld.PrintInhalt;

      BEGIN
         IF (FeldModus IN [DI,EN,DE]) THEN BEGIN
            SETCOLOR (AktuelleFarben^.FeldHG[FeldModus]);
            RECTANGLE(ISpalte-4,IZeile-3,ISpalte+10,IZeile+9);
         END;
         SETCOLOR (AktuelleFarben^.FeldUM[I]);
         RECTANGLE(ISpalte-3,IZeile-2,ISpalte+9,IZeile+8);
         IF NOT (FeldModus  IN [DI,EN,DE]) THEN BEGIN
           LINE(ISpalte-3,IZeile-3,ISpalte+9,IZeile-3);
           LINE(ISpalte-4,IZeile-2,ISpalte-4,IZeile+8);
           LINE(ISpalte-3,IZeile+9,ISpalte+9,IZeile+9);
           LINE(ISpalte+10,IZeile-2,ISpalte+10,IZeile+8);
         END;
         Inhalt:=COPY(Inhalt,1,FeldLaenge);
         SETCOLOR(AktuelleFarben^.InhaltVG[FeldModus]);
         SETFILLSTYLE(1,AktuelleFarben^.InhaltHG[FeldModus]);
         BAR(ISpalte-1,IZeile-1,ISpalte+LENGTH(Inhalt)*8,IZeile+7);
         OUTTEXTXY(ISpalte,IZeile,Inhalt);
         SETFILLSTYLE(1,AktuelleFarben^.FeldHG[FeldModus]);
         IF (FeldLaenge > LENGTH(Inhalt)) THEN
            BAR(ISpalte+LENGTH(Inhalt)*8+2,IZeile-4
            ,ISpalte+FeldLaenge*8,IZeile+10);
      END;

   FUNCTION TSwitchFeld.Input;

      VAR   Ende:        BOOLEAN;
            MoreEval,
            Check:       BOOLEAN;
            MText,
            Eingabe:     STRING;
            Nr,
            CursorPos:   BYTE;
            InputCode:   INTEGER;


      PROCEDURE Cashback(Full:BOOLEAN; CursorPos:BYTE; Text:STRING);

         BEGIN
            IF Full THEN BEGIN
               PrintTitel
            END;
            PrintInhalt(Text);
         END;

      { Anfang von Input HauptProgramm }

      BEGIN
         Abbruch := FALSE;
         Done  := FALSE;
         InputCode := 0;
         IF NOT Disabled('') THEN BEGIN
            SetActive;
            Eingabe := MakeInpStr;
            Ende   := FALSE;
            MText  := Eingabe;
            CursorPos     := 1;
            CASHBACK(TRUE,CursorPos,Eingabe);
            REPEAT
               CASHBACK(FALSE,CursorPos,Eingabe);
               MText:=Eingabe;
               InputCode:=KeyInput;
               Check:=TRUE;
               Ende := KeyAction(InputCode,Nr,Eingabe,Abbruch,Done,Check,MoreEval);
               KeyDone := Done AND (NOT MoreEval);
               IF NOT Done THEN BEGIN
                  IF (FeldModus = a) THEN BEGIN
                     IF ValidKey(InputCode,CursorPos) THEN BEGIN
                        CursorPos:=1;
                        Eingabe:=MakeClear
                     END;
                     FeldModus:=i;
                     CASHBACK(FALSE,CursorPos,Eingabe)
                  END;
                  CASE InputCode OF
                      -15,-72,
                      -80,
                     9,13      : BEGIN
                                    Ende := TRUE;
                                    Done := TRUE;
                                 END;
                     27        : BEGIN
                                    Ende := TRUE;
                                    Done := TRUE;
                                    Abbruch := TRUE;
                                 END;
                     -68..-59,
                     -113..-84 : BEGIN
                                    Ende := TRUE;
                                    Done := TRUE;
                                 END;
                     ELSE
                        IF NOT SpecialEval(Eingabe,InputCode,Cursorpos) THEN
                           IF ValidKey(InputCode,CursorPos) THEN BEGIN
                              CASE InputCode OF
                                 74,45,88,
                                 120,106 : Eingabe := 'X';
                                 32      : IF Mtext = 'X' THEN
                                              Eingabe := ' '
                                           ELSE
                                              Eingabe := 'X';
                                 43,78,
                                 110     : Eingabe := ' ';
                              END;
                           END ELSE
                               WRITE(CHR(7));
                  END;
                  IF Ende THEN BEGIN
                     IF Abbruch THEN
                        Eingabe := MakeInpStr;
                     IF NOT ValidInput(Eingabe) THEN BEGIN
                        WRITE(CHR(7));
                        Ende :=FALSE;
                     END ELSE
                        MakeVal(Eingabe);
                  END
               END;
               IF Done THEN BEGIN
                  CursorPos := 1;
               END;
            UNTIL Ende;
            Enable('');
            CashBack (TRUE,CursorPos,MakeInpStr);
         END;
         Input := InputCode;
      END;

      CONSTRUCTOR TRollFeld.Init;

        BEGIN
          inherited Init(TiSP,TiZE,ISP,IZE,Len,Txt,InfNo,I,Extra);
          Anzahl :=0;
          ItemObj := TRUE;
          ItemList:= NIL;
        END;

      PROCEDURE TRollFeld.SetData;
         BEGIN
           Wert := PTR(DataSeg,DataOfs)
         END;



      FUNCTION TRollFeld.GetInfoNo;
         BEGIN
            IF Disabled('') THEN
               GetInfoNo := InfoKennung
            ELSE
               GetInfoNo := InfoKennung + W2S(Wert^,0);
         END;

      PROCEDURE TRollFeld.AddItem;
        VAR Lauf : WORD;
            Neu,
            Zgr  : TRItemPtr;
        BEGIN
           IF MaxAvail>Sizeof(TRItem) THEN BEGIN
              NEW(Neu);
              Lauf := 1;
              IF Anzahl = 0 THEN
                 Itemlist := Neu;
              Zgr := ItemList;
              WHILE Lauf < Anzahl DO BEGIN
                Zgr := Zgr^.Next;
                INC(Lauf);
              END;
              Neu^.Next := ItemList;
              Neu^.Last := Zgr;
              Neu^.Enabled := TRUE;
              Neu^.Txt := TX;
              Zgr^.Next := Neu;
              ItemList^.Last  :=  Neu;
              INC(Anzahl);
           END;
        END;

      FUNCTION TRollFeld.MakeInpStr;
        VAR Lauf : WORD;
            Zgr  : TRItemPtr;
        BEGIN
          Zgr := ItemList;
          Lauf := 1;
          WHILE Lauf < Wert^ DO BEGIN
            Zgr := Zgr^.Next;
            INC(Lauf);
          END;
          MakeInpStr := Zgr^.Txt
        END;

      PROCEDURE TRollFeld.MakeVal;
        VAR Lauf : WORD;
            Zgr  : TRItemPtr;
        BEGIN
          Zgr := ItemList;
          Lauf := 1;
          WHILE (Zgr^.Txt <> Eingabe) DO BEGIN
            Zgr := Zgr^.Next;
            INC(Lauf);
          END;
          IF (Lauf <= Anzahl) THEN
             Wert^:= Lauf
          ELSE
             Wert^:=1;
        END;

      PROCEDURE TRollFeld.GetActItem;
        VAR Lauf :WORD;
            Zgr : TRItemPtr;
        BEGIN
          Zgr := ItemList;
          Lauf := 1;
          WHILE Lauf < Wert^ DO BEGIN
            Zgr := Zgr^.Next;
            INC(Lauf);
          END;
          AktItem := Zgr;
        END;


      PROCEDURE TRollFeld.EnableItem;
        VAR Lauf : WORD;
            Zgr  : TRItemPtr;
        BEGIN
           IF (It <= Anzahl) THEN BEGIN
              Zgr := ItemList;
              Lauf := 1;
              WHILE Lauf < It DO BEGIN
                Zgr := Zgr^.Next;
                INC(Lauf);
              END;
              Zgr^.Enabled := TRUE;
           END;
        END;

      PROCEDURE TRollFeld.DisableItem;
        VAR Lauf : WORD;
            Zgr  : TRItemPtr;
        BEGIN
           IF (It <= Anzahl) THEN BEGIN
              Zgr := ItemList;
              Lauf := 1;
              WHILE Lauf < It DO BEGIN
                Zgr := Zgr^.Next;
                INC(Lauf);
              END;
              Zgr^.Enabled := FALSE;
           END;
        END;

      FUNCTION TRollFeld.ItemDisabled;
        VAR Lauf : WORD;
            Zgr  : TRItemPtr;
        BEGIN
           ItemDisabled := FALSE;
           IF (It <= Anzahl) THEN BEGIN
              Zgr := ItemList;
              Lauf := 1;
              WHILE Lauf < It DO BEGIN
                Zgr := Zgr^.Next;
                INC(Lauf);
              END;
              ItemDisabled :=(NOT Zgr^.Enabled);
           END;
        END;

      FUNCTION TRollFeld.PrevItem;

        BEGIN
           REPEAT
             IF Wert^ = 1 THEN Wert^:=Anzahl
                ELSE DEC(Wert^);
             AktItem := AktItem^.Last;
           UNTIL AktItem^.Enabled;
        END;

      FUNCTION TRollFeld.NextItem;

        BEGIN
           REPEAT
             IF Wert^ = Anzahl THEN Wert^:=1
             ELSE INC(Wert^);
             AktItem := AktItem^.Next;
           UNTIL AktItem^.Enabled;
        END;

      FUNCTION TRollFeld.Input;

        Var MPos      : WORD;
            Ende      : BOOLEAN;
            MoreEval  : BOOLEAN;
            InputCode : INTEGER;
            Eingabe   : STRING;
            Nr        : BYTE;
            CPos      : BYTE;
            Check     : BOOLEAN;

        BEGIN
          InputCode := 0;
          Nr := 0;
          Done := FALSE;
          Abbruch := FALSE;
          IF Not Disabled('') THEN BEGIN
             SetActive;
             MPos:=Wert^;
             IF (Wert^> Anzahl) THEN
                Wert ^:= 1;
             GetActItem;
             IF NOT AktItem^.Enabled THEN
                NextItem;
             Ende := FALSE;
             BackInput;
             PrintTitel;
             REPEAT
                Eingabe := MakeInpStr;
                PrintInhalt(Eingabe);
                InputCode := KeyInput;
                Ende := KeyAction(InputCode,Nr,Eingabe,Abbruch,Done,Check,MoreEval);
                KeyDone := Done AND (NOT MoreEval);
                IF NOT Done THEN BEGIN
                   CASE InputCode OF
                      -72,-75,
                           45  : PrevItem;
                      -77,-80,
                      32,43    : NextItem;
                      -71      : BEGIN
                                    Wert^:=1;
                                    GetActItem;
                                    IF (NOT AktItem^.Enabled) THEN
                                       NextItem;
                                 END;
                      -79      : BEGIN
                                    Wert^:=Anzahl;
                                    GetActItem;
                                    IF (NOT AktItem^.Enabled) THEN
                                       PrevItem;
                                 END;
                      27       : BEGIN
                                    Ende := TRUE;
                                    Done := TRUE;
                                    Abbruch := TRUE;
                                 END;
                      13,9,-15,
                      -68..-59,
                      -113..-84: BEGIN
                                    ENDE := TRUE;
                                    Done := TRUE;
                                 END;
                      ELSE WRITE(CHR(7));
                   END;
                   Eingabe := MakeInpStr;
                   SpecialEval(Eingabe,InputCode,CPos);
                END;
             UNTIL Ende;
             IF Abbruch THEN
                Wert^:= Mpos;
             Enable('');
             BackInput;
             Show('');
          END;
          Input:=InputCode;
        END;

      PROCEDURE TRollFeld.Save;
        BEGIN
          WertSik:= Wert^;
        END;

      PROCEDURE TRollFeld.Restore;
        BEGIN
          Wert^ := WertSik;
        END;

      FUNCTION TRollFeld.Changed;
        BEGIN
          Changed := (Wert^<> WertSik);
        END;

   CONSTRUCTOR TChoiceFeld.Init;

      VAR CurColor : BYTE;
          FTitel   : STRING;

      BEGIN
         inherited Init(TiSP,TiZE,ISP,IZE,Len,Txt,InfNo,I,Extra);
         CurColor := GETCOLOR;
         ItemObj := TRUE;
         IF FTxt <> '' THEN
            FTitel := FTxt
         ELSE
            FTitel := Titel;
         Auswahl.Init(15,7,2,FTitel,InfNo,7,CurColor);
      END;

   FUNCTION TChoiceFeld.GetInfoNo;
      BEGIN
         IF Disabled('') THEN
            GetInfoNo := InfoKennung
         ELSE
            GetInfoNo := InfoKennung + W2S(Wert^,0);
      END;

   PROCEDURE TChoiceFeld.AddItem;
      BEGIN
         Auswahl.AddItem('',TX);
      END;

   FUNCTION TChoiceFeld.ItemDisabled;
      BEGIN
         ItemDisabled :=Auswahl.ItemDisabled('',It);
      END;

   PROCEDURE TChoiceFeld.EnableItem;
      BEGIN
         Auswahl.EnableItem('',It);
      END;


   PROCEDURE TChoiceFeld.DisableItem;
      BEGIN
         Auswahl.DisableItem('',It);
      END;

   PROCEDURE TChoiceFeld.SetData;
       BEGIN
          Wert := PTR(DataSeg,DataOfs)
        END;

   FUNCTION TChoiceFeld.MakeInpStr;
      BEGIN
         MakeInpStr := Auswahl.GetData(Wert^)
      END;

   PROCEDURE TChoiceFeld.MakeVal;
      VAR Lauf : BYTE;
      BEGIN
         Lauf:= 1;
         WHILE (Auswahl.GetData(Lauf) <> Eingabe) DO
            INC(Lauf);
         IF (Lauf <= Auswahl.Anzahl) THEN
            Wert^:= Lauf
         ELSE
            Wert^:= 1;
      END;

   FUNCTION TChoiceFeld.Input;
      VAR InputCode : INTEGER;

      BEGIN
         InputCode := 0;
         Abbruch :=FALSE;
         Done := FALSE;
         IF NOT Disabled('') THEN BEGIN
            SetActive;
            Auswahl.SetPos(Wert^);
            InputCode := Auswahl.MakeChoice(FALSE);
            KeyDone := Auswahl.KeyDone;
            Done := Auswahl.Done;
            Abbruch := Auswahl.Abbruch;
            Auswahl.Hide;
            IF Abbruch THEN
               InputCode := -1
            ELSE
               Wert^ := Auswahl.GetPos;
            Enable('');
            Show('');
         END;
         Input:=InputCode;
      END;

   PROCEDURE TChoiceFeld.Save;
      BEGIN
         WertSik:= Wert^;
      END;

   PROCEDURE TChoiceFeld.Restore;
      BEGIN
         Wert^ := WertSik;
      END;

   FUNCTION TChoiceFeld.Changed;
      BEGIN
         Changed := (Wert^<> WertSik);
      END;

   CONSTRUCTOR TKwFeld.Init;
      BEGIN
         inherited Init(TiSP,TiZE,ISP,IZE,5,Txt,InfNo,M,EA);
         Maske :='  /  ';
      END;

   PROCEDURE TKwFeld.SetData;
      BEGIN
         Wert := PTR(DataSeg,DataOfs)
      END;



   FUNCTION TKwFeld.MakeInpStr;
      BEGIN
         IF Wert^ = '' THEN
            MakeInpStr := Maske
         ELSE
            MakeInpStr:=FillUp('0',COPY(Wert^,1,2),2,2)+'/'+
                           FillUp('0',COPY(Wert^,3,2),2,2);
      END;

   PROCEDURE TKwFeld.MakeVal;
      BEGIN
         IF (Eingabe = Maske) THEN
            Wert^:=''
         ELSE
            Wert^ := COPY(Eingabe,1,2)+COPY(Eingabe,4,2);
      END;

   FUNCTION TKwFeld.ValidKey;
      BEGIN
         ValidKey := (Code>=48) AND (Code<=57);
      END;

   FUNCTION TKwFeld.ValidInput;
      VAR Woche : BYTE;
          Jahr  : STRING[2];
          c     : INTEGER;

      BEGIN
         VAL(COPY(Eingabe,1,2),Woche,c);
         Jahr := COPY(Eingabe,4,2);
         ValidInput := ((Woche > 0) AND ( Woche <= 52) AND (Jahr >'.')) OR
                          (Eingabe = Maske);
      END;

   PROCEDURE TKwFeld.Save;
      BEGIN
         WertSik:= Wert^;
      END;

   PROCEDURE TKwFeld.Restore;
      BEGIN
         Wert^ := WertSik;
      END;

   FUNCTION TKwFeld.Changed;
      BEGIN
         Changed := (Wert^<> WertSik);
      END;

   FUNCTION TNumTextFeld.ValidKey;
      BEGIN
         ValidKey := (Code>=48) AND (Code<=57);
      END;

   FUNCTION TNumTextFeld.Changed;
      BEGIN
         Changed := (S2L(Wert^)<> S2L(WertSik));
      END;

   FUNCTION TMitNullFeld.MakeInpStr;
      VAR T : STRING;

      BEGIN
         STR(Wert^,T);
         WHILE LENGTH(T) < FeldLaenge DO
            T:= '0'+T;
         MakeInpStr := T;
      END;



BEGIN
END.
{============================
 Versionshistorie
 $Log:$
 ============================}
