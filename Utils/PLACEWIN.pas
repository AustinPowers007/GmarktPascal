{============================
 Dateiname:   $Source: $
 Version:     $Revision: $
 Autor:       $Author: $
 Gelockt von: $Locker: $
 Originaldatum : Fri Jan 07 18:04:26 GMT+01:00 2000
 Dateihistorie am Ende der Datei
 ============================}
                               {浜様様様様様�
   浜様様様様様様様様様様様様様郵 Bemerkung 麺様様様様様様様様様様様様様様様�
   �                            藩様様様様様�                               �
   � Aufgabe:     Stellt Fenster im EBS Layout mit verschiedenen Attributen �
   �              zur Verf�gung                                             �
   藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様夕}

UNIT PLACEWIN;
{$O+}


INTERFACE
    USES CASHDATA;


                               {浜様様様様様�
   浜様様様様様様様様様様様様様郵 Bemerkung 麺様様様様様様様様様様様様様様様�
   �                            藩様様様様様�                               �
   � Aufgabe:     Stellt Standardfenster ohne besondere Attribute           �
   �              Ausgabe von Text, Wahl von Farben, variieren von Z-Abst.  �
   藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様夕}

    TYPE   TFensterPtr = ^TFenster;
           TFenster = OBJECT(TDialogObj)
           Links,
           Oben,
           Rechts,
           Unten,
           ErsteSpalte,
           ErsteZeile,
           HGH,HGV      : WORD;
           FensterTitel : STRING;
           Zeilenabstand,
           Zeilen,
           Spalten,
           FensterFarbe,
           Schattenfarbe,
           Titeltext,
           Titelfarbe   : BYTE;
           MitSchatten,
           AllesErzeugt : BOOLEAN;
           CONSTRUCTOR Init(x1,y1,x2,y2:WORD;Za:BYTE;Farbe1,Farbe2:BYTE;
                              Titel:STRING);
           PROCEDURE  SetTitel(TTxt:STRING);VIRTUAL;
           PROCEDURE  PrintWin;VIRTUAL;
           PROCEDURE  PlaceText(Spalte:INTEGER;Zeile:BYTE;Txt:STRING;
                                 TxFarbe,HgFarbe:BYTE;EW,EH:BYTE);VIRTUAL;
           PROCEDURE  DrawHLine(SPA,SPE,ZE,YDif:INTEGER;Farbe:BYTE);VIRTUAL;
           PROCEDURE  DrawVLine(SP,ZEA,ZEE,XDif:INTEGER;Farbe:BYTE);VIRTUAL;
           PROCEDURE  DrawCPoint(SP,ZE:INTEGER;Typ,Farbe:BYTE);VIRTUAL;
           PROCEDURE  Show(K:STRING);VIRTUAL;
           FUNCTION   Left:WORD;VIRTUAL;
           FUNCTION   Right:WORD;VIRTUAL;
           FUNCTION   Top:WORD;VIRTUAL;
           FUNCTION   Bottom:WORD;VIRTUAL;
           FUNCTION   MaxLine:BYTE;VIRTUAL;
           FUNCTION   MaxCol:BYTE;VIRTUAL;
           PROCEDURE  ClearBackground;VIRTUAL;
           PROCEDURE  DisableShadow;VIRTUAL;
           PROCEDURE  EnableShadow;VIRTUAL;
           DESTRUCTOR Release;VIRTUAL;
        END;

                               {浜様様様様様�
   浜様様様様様様様様様様様様様郵 Bemerkung 麺様様様様様様様様様様様様様様様�
   �                            藩様様様様様�                               �
   � Aufgabe:     Stellt Fenster mit Hintergrundmerkf�higkeit, sonst wie    �
   �              TFenster                                                  �
   藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様夕}
        TBrainFensterPtr = ^TBrainFenster;
        TBrainFenster = OBJECT(TFenster)
           AlterBildschirm : ARRAY[1..4] OF POINTER;
           AnzPointer      : BYTE;
           Groesse         : ARRAY[1..4] OF WORD;
           GesamtGroesse   : LONGINT;
           Geschlossen     : BOOLEAN;

           CONSTRUCTOR Init(x1,y1,x2,y2:WORD;Za:BYTE;Farbe1,Farbe2:BYTE;
                              Titel:STRING);
           PROCEDURE CalculateSize;VIRTUAL;
           PROCEDURE CalculateEdge(Part:BYTE;VAR x1,y1,x2,y2:WORD);
           PROCEDURE Show(K:STRING);VIRTUAL;
           PROCEDURE Hide;VIRTUAL;
           PROCEDURE Brain;VIRTUAL;
           PROCEDURE Remember;VIRTUAL;
           PROCEDURE ClearBackground;VIRTUAL;
           DESTRUCTOR Release;VIRTUAL;
           PROCEDURE DisableShadow;VIRTUAL;
           PROCEDURE EnableShadow;VIRTUAL;

        END;

       TResizeAbleBrainFensterPtr = ^TResizeAbleBrainFenster;
       TResizeAbleBrainFenster  = OBJECT(TBrainFenster)

       CONSTRUCTOR Init(x1,y1,x2,y2:WORD;Za:BYTE;Farbe1,Farbe2:BYTE;
                              Titel:STRING);
           PROCEDURE NewCenter(NX,NY:WORD);
           PROCEDURE WinEdge(x1,y1,x2,y2:WORD);
           PROCEDURE ResizeWin(NS,NZ:BYTE);VIRTUAL;
           PROCEDURE SetDefaultSize;
           PROCEDURE SetMaxSize(x1,y1,x2,y2:WORD);
           PRIVATE
              DefL,
              DefO,
              DefR,
              DefU,
              DefCenterX,
              DefCenterY,
              MaxL,
              MaxO,
              MaxR,
              MaxU,
              MaxCenterX,
              MaxCenterY : WORD;
              DefZ,
              DefS,
              MaxZ,
              MaxS :BYTE;
        END;

        TMessageBoxFensterPtr = ^TMessageBoxFenster;
        TMessageBoxFenster = OBJECT(TResizeAbleBrainFenster)
            MeldungsZeile : ARRAY [1..6] OF STRING[50];
            MeldungsZeilen: BYTE;
            CONSTRUCTOR Init(x1,y1,x2,y2:WORD;Za:BYTE;Farbe1,Farbe2:BYTE;
                              Titel:STRING);
            FUNCTION SplitMessage(Meldung:STRING):BYTE;
        END;

        TMessageBoxTimeFensterPtr = ^TMessageBoxTimeFenster;
        TMessageBoxTimeFenster = OBJECT(TMessageBoxFenster)
           WaitTime  : WORD;
           CONSTRUCTOR Init(x1,y1,x2,y2:WORD;Za:BYTE;Farbe1,Farbe2:BYTE;
                              Titel:STRING);
           FUNCTION    KeyInput:INTEGER;VIRTUAL;
        END;

        TRequestPtr= ^TRequest;
        TRequest = OBJECT(TMessageBoxFenster)
           CONSTRUCTOR Init;
           FUNCTION  Act(Meldung : STRING):BOOLEAN;VIRTUAL;
        END;

        TTripleQuestPtr= ^TTripleQuest;
        TTripleQuest = OBJECT(TMessageBoxFenster)
           Text1,
           Text2,
           Text3 : STRING[9];
           CONSTRUCTOR Init;
           PROCEDURE SetChoice(TN1,TN2,TN3:STRING);VIRTUAL;
           FUNCTION  Act(Meldung : STRING):INTEGER;VIRTUAL;
        END;

        TInfoBoxPtr= ^TInfoBox;
        TInfoBox = OBJECT(TMessageBoxTimeFenster)
           CONSTRUCTOR Init;
           PROCEDURE Act(Zeit:WORD;Meldung:STRING);VIRTUAL;
        END;

        TStatusBoxPtr= ^TStatusBox;
        TStatusBox = OBJECT(TBrainFenster)
           AlteZeile1,
           AlteZeile2 : STRING;

           CONSTRUCTOR Init;
           PROCEDURE Act(Zeile1,Zeile2:STRING);VIRTUAL;
           PROCEDURE Hide;VIRTUAL;
        END;

        TFaultBoxPtr= ^TFaultBox;
        TFaultBox = OBJECT(TMessageBoxTimeFenster)
           CONSTRUCTOR Init;
           PROCEDURE Act(Zeit:WORD;Meldung:STRING);VIRTUAL;
        END;


                               {浜様様様様様�
   浜様様様様様様様様様様様様様郵 Bemerkung 麺様様様様様様様様様様様様様様様�
   �                            藩様様様様様�                               �
   � Aufgabe:     Erzeugt Fenster mit Auswahl, memoriert Hintergrund, wird  �
   �              von TChoiceFeld aus FELDART verwendet.                    �
   藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様夕}
        TAItemPtr = ^TAuswahlItem;

        TAuswahlItem = RECORD
           Text : STRING;
           FeldKennung : STRING;
           Enabled : BOOLEAN;
           FUNC : TMenuFunc;
        END;

        TAuswahlFensterPtr= ^TAuswahlFenster;
        TAuswahlFenster = OBJECT(TResizeAbleBrainFenster)
           FensterInfo : STRING[8];
           Anzahl,
           Wert,
           Laenge,
           ESp,
           BarVG,
           BarHG   : BYTE;
           ItemList: ARRAY[1..24] OF TAItemPtr;

           CONSTRUCTOR Init(za,Farbe1,Farbe2:BYTE;Titel,Info:STRING;
                              BVG,BHG:BYTE);
           PROCEDURE AddItem(K:STRING;TX:STRING);VIRTUAL;
           FUNCTION  ItemDisabled(K:STRING;It:WORD):BOOLEAN;VIRTUAL;
           PROCEDURE DisableItem(K:STRING;It:WORD);VIRTUAL;
           PROCEDURE EnableItem(K:STRING;It:WORD);VIRTUAL;
           PROCEDURE Disable(K:STRING); VIRTUAL;
           PROCEDURE Enable(K:STRING); VIRTUAL;
           PROCEDURE GiveChoice;VIRTUAL;
           PROCEDURE ChangeBar(OldPos,NewPos:BYTE);VIRTUAL;
           FUNCTION  MakeText(Nr:BYTE):STRING;VIRTUAL;
           FUNCTION  GetData(Nr:BYTE):STRING;VIRTUAL;
           PROCEDURE ShowBar(Pos:BYTE);VIRTUAL;
           PROCEDURE SetPos(Pos:BYTE);VIRTUAL;
           FUNCTION GetPos:BYTE;VIRTUAL;
           FUNCTION MakeChoice(Angezeigt:BOOLEAN):INTEGER;VIRTUAL;
           PROCEDURE SetFKeyQuit(Val:BOOLEAN);VIRTUAL;
           PRIVATE  LEER:STRING;
        END;




   VAR FaultBox   : TFaultBox;
       Request    : TRequest;
       TripleQuest: TTripleQuest;
       InfoBox    : TInfoBox;
       StatusBox  : TStatusBox;

   FUNCTION QUIT(Nr:BYTE;VAR Taste:INTEGER):BOOLEAN;

IMPLEMENTATION

   USES DOS,
        GMCRT,
        GRAPH,
        GRAPNEU,
        ZUSAETZE;

   VAR t : text;


   FUNCTION QUIT;

      BEGIN
         Quit:= FALSE;
      END;
                               {浜様様様様様�
   浜様様様様様様様様様様様様様郵 Bemerkung 麺様様様様様様様様様様様様様様様�
   �                            藩様様様様様�                               �
   � Aufgabe:     Fensterwerte initialisieren, errechnet Zeilen und Spalten �
   �                                                                        �
   � Parameter:   Links,Oben,Rechts,Unten:WORD, Zeilenabstand,              �
   �              FensterFarbe,TitelFarbe:BYTE, FensterTitel:STRING         �
   �                                                                        �
   � Resultate:   Keine                                                     �
   藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様夕}

   CONSTRUCTOR TFenster.Init;
      VAR XWidth : LONGINT;
          YWidth : LONGINT;
          Xrand,
          YRand  : LONGINT;

      BEGIN
         inherited Init;
         Links := x1;
         Oben := y1;
         Rechts := x2;
         Unten := y2;
         IF ((Za >= 1) AND (Za <= 14)) THEN
            Zeilenabstand := Za+7
         ELSE
            Zeilenabstand := 15;
         YRand:= 17;
         IF (FensterTitel <> '') THEN
            YRand := YRand +8;
         XRand := 10;
         YWidth := y2;
         Ywidth :=YWidth-y1-(2*YRand);
         IF Ywidth > 0 THEN
            Zeilen := YWidth DIV Zeilenabstand
         ELSE
            Zeilen := 0;
         XWidth := x2;
         Xwidth := XWidth-x1-(2*XRand);
         IF Xwidth > 8 THEN
            Spalten := (XWidth DIV 8)-1
         ELSE
            Spalten := 0;
         ErsteSpalte := Links+10;
         ErsteZeile := Oben+17;
         IF (FensterTitel <> '') THEN
            ErsteZeile := ErsteZeile+8;
         HGH:= 0;
         HGV := 0;
         FensterFarbe := Farbe1;
         Schattenfarbe  := Farbe2;
         Titeltext := 0;
         Titelfarbe := 3;
         SetTitel(Titel);
         AllesErzeugt := TRUE;
         MitSchatten := TRUE;
      END;
    FUNCTION TFenster.Left;
       BEGIN
          Left := Links;
       END;
    FUNCTION TFenster.Right;
       BEGIN
          Right := Rechts;
       END;
    FUNCTION TFenster.Top;
       BEGIN
          Top := Oben;
       END;

    FUNCTION TFenster.Bottom;
       BEGIN
          Bottom := Unten;
       END;

   PROCEDURE TFenster.SetTitel;
      BEGIN
         FensterTitel := TTxt;
         ErsteZeile := Oben+17;
         IF (FensterTitel <> '') THEN
            ErsteZeile := ErsteZeile+8;
      END;

      PROCEDURE TFenster.PrintWin;
         BEGIN
            DrawWin(Links,Oben,Rechts,Unten,FensterFarbe,TitelFarbe,
                    TitelText,Schattenfarbe,MitSchatten,FensterTitel);
         END;


                               {浜様様様様様�
   浜様様様様様様様様様様様様様郵 Bemerkung 麺様様様様様様様様様様様様様様様�
   �                            藩様様様様様�                               �
   � Aufgabe:     Gibt Fensterrahmen auf den Bildschirm aus                 �
   �                                                                        �
   � Parameter:   Keine                                                     �
   �                                                                        �
   � Resultate:   Keine                                                     �
   藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様夕}

   PROCEDURE TFenster.Show;

      BEGIN
         inherited Show(K);
         PrintWin;
         SETFILLSTYLE(1,FensterFarbe);
      END;

                               {浜様様様様様�
   浜様様様様様様様様様様様様様郵 Bemerkung 麺様様様様様様様様様様様様様様様�
   �                            藩様様様様様�                               �
   � Aufgabe:     L�scht Fenster vom Bildschirm mit Hintergrundfarbe        �
   �                                                                        �
   � Parameter:   Keine                                                     �
   �                                                                        �
   � Resultate:   Keine                                                     �
   藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様夕}

   PROCEDURE TFenster.ClearBackGround;
      BEGIN
         ClearWin(Links,Oben,Rechts,Unten,MitSchatten);
      END;

   PROCEDURE TFenster.EnableShadow;

      BEGIN
         MitSchatten := TRUE;
      END;

   PROCEDURE TFenster.DisableShadow;

      BEGIN
         MitSchatten := FALSE;
      END;

                               {浜様様様様様�
   浜様様様様様様様様様様様様様郵 Bemerkung 麺様様様様様様様様様様様様様様様�
   �                            藩様様様様様�                               �
   � Aufgabe:     L�scht Fenster vom Bildschirm mit Hintergrundfarbe        �
   �                 und vergi�t Objekt                                     �
   � Parameter:   Keine                                                     �
   �                                                                        �
   � Resultate:   Keine                                                     �
   藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様夕}

   DESTRUCTOR TFenster.Release;

      BEGIN
         ClearBackGround;
      END;

                               {浜様様様様様�
   浜様様様様様様様様様様様様様郵 Bemerkung 麺様様様様様様様様様様様様様様様�
   �                            藩様様様様様�                               �
   � Aufgabe:     Gibt Text in Fenster aus                                  �
   �                                                                        �
   � Parameter:   Spalte,Zeile:BYTE, Text:STRING, TextFarbe,HG-Farbe:BYTE   �
   �              F�r Farbwert = 255 wird momentan gesetzte Farbe genommen  �
   �                                                                        �
   � Resultate:   Keine                                                     �
   藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様夕}

   PROCEDURE TFenster.PlaceText;

      BEGIN
         IF Spalte = -1 THEN
            Spalte := (Spalten-LENGTH(Txt)) DIV 2+1;

         IF (Zeile <= Zeilen+1) AND (Spalte+LENGTH(Txt) <= Spalten+1)
            AND (Zeile >= 0) AND (Spalte >= 0) THEN BEGIN
            IF HgFarbe = 255 THEN HgFarbe := Fensterfarbe;
            IF TxFarbe = 255 THEN TxFarbe := Titeltext;
            DisplayText((ErsteSpalte+Spalte*8),(ErsteZeile+Zeile*ZeilenAbstand)
                        ,EW,EH,HGFarbe,TxFarbe,Txt);

         END
      END;

   PROCEDURE TFenster.DrawHLine;
      VAR X1,X2,Y1,Y2 : WORD;

      BEGIN
         X1 :=ErsteSpalte+Spa*8;
         X2 :=ErsteSpalte+Spe*8+8;
         Y1 :=ErsteZeile+4+YDif+Ze*ZeilenAbstand;
         Y2:= Y1;
         DrawLine(X1,Y1,X2,Y2,Farbe);
      END;

   PROCEDURE TFenster.DrawVLine;

      VAR X1,X2,Y1,Y2 : WORD;

      BEGIN
         X1 :=ErsteSpalte+Sp*8+4+XDif;
         X2 :=X1;
         Y1 :=ErsteZeile+Zea*ZeilenAbstand;
         Y2 :=ErsteZeile+8+Zee*ZeilenAbstand;
         DrawLine(X1,Y1,X2,Y2,Farbe);
      END;

   PROCEDURE TFenster.DrawCPoint;
      VAR X1,Y1 : WORD;
      BEGIN
         X1 :=ErsteSpalte+Sp*8;
         Y1 :=ErsteZeile+Ze*ZeilenAbstand;
         DrawCrossPoint(X1,Y1,ZeilenAbstand,Typ,Farbe);
      END;

                               {浜様様様様様�
   浜様様様様様様様様様様様様様郵 Bemerkung 麺様様様様様様様様様様様様様様様�
   �                            藩様様様様様�                               �
   � Aufgabe:     Gibt die Zeilenzahl an                                    �
   �                                                                        �
   � Parameter:   Keine                                                     �
   �                                                                        �
   � Resultate:   Zeilen:BYTE                                               �
   藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様夕}

   FUNCTION TFenster.MaxLine;

      BEGIN
         MaxLine:=Zeilen;
      END;

                               {浜様様様様様�
   浜様様様様様様様様様様様様様郵 Bemerkung 麺様様様様様様様様様様様様様様様�
   �                            藩様様様様様�                               �
   � Aufgabe:     Gibt die Spaltenzahl an                                   �
   �                                                                        �
   � Parameter:   Keine                                                     �
   �                                                                        �
   � Resultate:   Spalten:BYTE                                              �
   藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様夕}

   FUNCTION TFenster.MaxCol;

      BEGIN
         MaxCol:=Spalten;
      END;


                               {浜様様様様様�
   浜様様様様様様様様様様様様様郵 Bemerkung 麺様様様様様様様様様様様様様様様�
   �                            藩様様様様様�                               �
   � Aufgabe:     Fenster initialisieren, Hintergrund merken falls Speicher �
   �              ausreicht                                                 �
   �                                                                        �
   � Parameter:   siehe TFenster                                            �
   �                                                                        �
   � Resultate:   Keine                                                     �
   藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様夕}

   CONSTRUCTOR TBrainFenster.Init;
      VAR Lauf : BYTE;
      BEGIN
         inherited Init(x1,y1,x2,y2,za,Farbe1,Farbe2,Titel);
         AnzPointer := 1;
         FOR Lauf := 1 TO 4 DO BEGIN
            Groesse[Lauf] := 0;
            AlterBildschirm[Lauf] := NIL;
         END;
         CalculateSize;
         AllesErzeugt := FALSE;
         Geschlossen := TRUE;
      END;

   PROCEDURE TBrainFenster.CalculateEdge;
      VAR SchattenGroesse : BYTE;
          LRechts,
          LUnten,
          VertMitte,
          HorizMitte     : WORD;
      BEGIN
         IF MitSchatten THEN
            SchattenGroesse := 10
         ELSE
            SchattenGroesse := 0;
         LRechts := Rechts+SchattenGroesse;
         LUnten  := Unten+SchattenGroesse;
         VertMitte := LUnten;
         HorizMitte := LRechts;
         IF (AnzPointer <> 1) THEN
            VertMitte :=Oben +(LUnten-Oben) DIV 2;
         IF (AnzPointer = 4) THEN
            HorizMitte :=Links +(LRechts-Links) DIV 2;
         CASE Part OF
            1: BEGIN
                 x1 := Links;x2 := HorizMitte;
                 y1 := Oben; y2:= VertMitte;
               END;
            2: BEGIN
                 x1 := Links;x2 := HorizMitte;
                 y1 := VertMitte+1; y2:= LUnten;
               END;
            3: BEGIN
                 x1 := HorizMitte+1;x2 := LRechts;
                 y1 := Oben; y2:= VertMitte;
               END;
            4: BEGIN
                 x1 := HorizMitte+1;x2 := LRechts;
                 y1 := VertMitte+1; y2:= LUnten;
               END;
         END;
         IF x1 < 0  THEN x1 := 0;
         IF y1 < 0  THEN y1 := 0;
         IF x2 >639 THEN x2 := 639;
         IF y2 > 479 THEN y2 := 479;
      END;

   PROCEDURE TBrainFenster.CalculateSize;
      VAR Lauf : BYTE;
          x1,x2,
          y1,y2 : WORD;

      BEGIN
         AnzPointer := 1;
         CalculateEdge(1,x1,y1,x2,y2);
         Groesse[1] := IMAGESIZE(x1,y1,x2,y2);
         IF (Groesse[1] = 0) THEN BEGIN
            AnzPointer := 2;
            FOR Lauf := 1 TO AnzPointer DO BEGIN
               CalculateEdge(Lauf,x1,y1,x2,y2);
               Groesse[Lauf] := IMAGESIZE(x1,y1,x2,y2);
            END;
            IF (Groesse[1] = 0) OR (Groesse[2] = 0) THEN BEGIN
               AnzPointer := 4;
               FOR Lauf := 1 TO AnzPointer DO BEGIN
                  CalculateEdge(Lauf,x1,y1,x2,y2);
                  Groesse[Lauf] := IMAGESIZE(x1,y1,x2,y2);
               END;
            END;
         END;
         GesamtGroesse := 0;
         FOR Lauf:= 1 TO AnzPointer DO
           GesamtGroesse := GesamtGroesse + Groesse[Lauf];
      END;

   PROCEDURE TBrainFenster.EnableShadow;
      VAR GRes : INTEGER;

      BEGIN
         IF NOT MitSchatten THEN BEGIN
            MitSchatten := TRUE;
            CalculateSize;
         END;
      END;

   PROCEDURE TBrainFenster.DisableShadow;
      VAR GRes : INTEGER;

      BEGIN
         IF MitSchatten THEN BEGIN
            MitSchatten := FALSE;
            CalculateSize;
         END;
      END;

                               {浜様様様様様�
   浜様様様様様様様様様様様様様郵 Bemerkung 麺様様様様様様様様様様様様様様様�
   �                            藩様様様様様�                               �
   � Aufgabe:     Fenster anzeigen, Hintergrund merken falls Speicher reicht�
   �                                                                        �
   � Parameter:   Keine                                                     �
   �                                                                        �
   � Resultate:   Keine                                                     �
   藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様夕}

    PROCEDURE TBrainFenster.Show;

       BEGIN
          IF Geschlossen THEN BEGIN
             Brain;
             inherited Show(K);
             Geschlossen := FALSE;
          END;
       END;

    PROCEDURE TBrainFenster.Brain;
       VAR Lauf : BYTE;
           x1,x2,
           y1,y2 : WORD;
       BEGIN
          IF (AlterBildschirm[1] <> NIL) THEN BEGIN
             FOR Lauf := 1 TO AnzPointer DO BEGIN
                FREEMEM(AlterBildschirm[Lauf],Groesse[Lauf]);
                AlterBildschirm[Lauf] := NIL;
             END;
          END;
          IF (MAXAVAIL > GesamtGroesse) THEN BEGIN
             FOR Lauf := 1 TO AnzPointer DO BEGIN
                GETMEM(AlterBildschirm[Lauf],Groesse[Lauf]);
                CalculateEdge(Lauf,x1,y1,x2,y2);
                GETIMAGE(x1,y1,x2,y2,AlterBildschirm[Lauf]^);
             END;
          END;
       END;

    PROCEDURE TBrainFenster.Remember;
       VAR Lauf  : BYTE;
           x1,x2,
           y1,y2 : WORD;

       BEGIN
          IF (AlterBildSchirm[1] <> NIL) THEN BEGIN
             FOR Lauf := 1 TO AnzPointer DO BEGIN
                CalculateEdge(Lauf,x1,y1,x2,y2);
                PUTIMAGE(x1,y1,AlterBildschirm[Lauf]^,NormalPut);
                FREEMEM(AlterBildschirm[Lauf],Groesse[Lauf]);
                AlterBildschirm[Lauf] := NIL;
             END;
          END;
       END;

                               {浜様様様様様�
   浜様様様様様様様様様様様様様郵 Bemerkung 麺様様様様様様様様様様様様様様様�
   �                            藩様様様様様�                               �
   � Aufgabe:     Fenster vom Bildschirm entfernen, aber nicht l�schen      �
   �              altes Bild wiederherstellen wenn m�glich                  �
   �                                                                        �
   � Parameter:   Keine                                                     �
   �                                                                        �
   � Resultate:   Keine                                                     �
   藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様夕}

   PROCEDURE TBrainFenster.Hide;

      BEGIN
         IF NOT Geschlossen THEN BEGIN
            IF (AlterBildschirm[1] <> NIL) THEN BEGIN
               Remember;
            END ELSE
               ClearBackGround;
            Geschlossen := TRUE;
         END;
      END;

   PROCEDURE TBrainFenster.ClearBackground;
      BEGIN
         IF NOT Geschlossen THEN BEGIN
            inherited ClearBackground;
            Geschlossen := TRUE;
         END;
      END;

                               {浜様様様様様�
   浜様様様様様様様様様様様様様郵 Bemerkung 麺様様様様様様様様様様様様様様様�
   �                            藩様様様様様�                               �
   � Aufgabe:     Fenster l�schen, Speicherplatz f�r Hintergrund freigeben  �
   �                                                                        �
   � Parameter:   Keine                                                     �
   �                                                                        �
   � Resultate:   Keine                                                     �
   藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様夕}

   DESTRUCTOR TBrainFenster.Release;

      BEGIN
         Hide;
      END;

   CONSTRUCTOR TResizeAbleBrainFenster.Init;
      BEGIN
         inherited Init(x1,y1,x2,y2,za,Farbe1,Farbe2,Titel);
         DefZ := Zeilen;
         DefS := Spalten;
         WinEdge(x1,y1,x2,y2);
         MaxL := x1;
         MaxO := y1;
         MaxR := x2;
         MaxU := y2;
         MaxZ := Zeilen;
         MaxS := Spalten;
         MaxCenterX:= DefCenterX;
         MaxCenterY:= DefCenterY;
      END;

   PROCEDURE TResizeableBrainFenster.WinEdge;
      BEGIN
         DefL := x1;
         DefO := y1;
         DefR := x2;
         DefU := y2;
         DefCenterX := x1+(x2-x1) DIV 2;
         DefCenterY := y1+(y2-y1) DIV 2;
      END;

   PROCEDURE TResizeAbleBrainFenster.ResizeWin;
      VAR Center : WORD;
          YRand,
          XRand : LONGINT;
      BEGIN
         XRand := 10;
         YRand := 17;
         IF (FensterTitel <> '') THEN
            YRand := YRand+8;
         IF (Nz > MaxZ) THEN
             Nz := MaxZ;
         IF (NS > MaxS) THEN
             NS := MaxS;
         IF (NS < DefS) THEN
             NS := DefS;
         IF (NZ < DefZ) THEN
             NZ := DefZ;
         IF (NZ <> Zeilen) THEN BEGIN
           IF (NZ = DefZ) THEN BEGIN
              Oben := DefO;
              Unten := DefU;
           END ELSE IF (NZ = MaxZ) THEN BEGIN
              Oben := MaxO;
              Unten := MaxU;
           END ELSE BEGIN
              Center := ROUND((MaxCenterY-DefCenterY)/(MaxZ-DefZ)*(NZ-DefZ)/
                              (MaxZ-DefZ)+DefCenterY);
              Oben := Center-((ZeilenAbstand*NZ+(YRand*2)) DIV 2);
              Unten := Oben+ZeilenAbstand*NZ+(YRand*2);
            END;
            Zeilen := NZ
         END;
         IF (NS <> Spalten) THEN BEGIN
           IF (NS = DefS) THEN BEGIN
              Links:= DefL;
              Rechts:= DefR;
           END ELSE IF (NS = MaxS) THEN BEGIN
              Links:= MaxL;
              Rechts:= MaxR;
           END ELSE BEGIN
              Center := ROUND((MaxCenterX-DefCenterX)/(MaxS-DefS)*(NS-DefS)/
                              (MaxS-DefS)+DefCenterX);
              Links := Center-((8*NS+(XRand*2)+8) DIV 2);
              Rechts := Links+8*NS+(XRand*2)+8;
           END;
           Spalten := NS;
         END;
         CalculateSize;
         ErsteSpalte := Links+10;
         ErsteZeile := Oben+17;
         IF (FensterTitel <> '') THEN
            ErsteZeile := ErsteZeile+8;
      END;

   PROCEDURE TResizeAbleBrainFenster.NewCenter;
      VAR Center : WORD;
          XOfs  : LONGINT;
          YOfs  : LONGINT;
      BEGIN
         XOfs := (Links+(Rechts-Links) DIV 2);
         XOfs := XOfs-NX;
         YOfs := (Oben+(Unten-Oben) DIV 2);
         YOfs := YOfs-NY;
         Links := Links-XOfs;
         Rechts:= Rechts-XOfs;
         Oben  := Oben-YOfs;
         Unten:= Unten-YOfs;
         WinEdge(Links,Oben,Rechts,Unten);
         CalculateSize;
         ErsteSpalte := Links+10;
         ErsteZeile := Oben+17;
         IF (FensterTitel <> '') THEN
            ErsteZeile := ErsteZeile+8;
      END;


   PROCEDURE TResizeAbleBrainFenster.SetDefaultSize;
      BEGIN
         Links := DefL;
         Oben := DefO;
         Rechts := DefR;
         Unten := DefU;
         Zeilen := DefZ;
         Spalten := DefS;
         CalculateSize;
         ErsteSpalte := Links+10;
         ErsteZeile := Oben+17;
         IF (FensterTitel <> '') THEN
            ErsteZeile := ErsteZeile+8;
      END;

   PROCEDURE TResizeAbleBrainFenster.SetMaxSize;
      VAR XWidth,
          YWidth : LONGINT;
      BEGIN
         IF (x1 < DefL) AND (x1 > 0) THEN
            MaxL := x1;
         IF (y1 < DefO) AND (y1 > 0) THEN
            MaxO := y1;
         IF (x2 > DefR) AND (x2 < 640) THEN
            MaxR := x2;
         IF (y2 > DefU) AND (y2 < 640) THEN
            MaxU := y2;
         YWidth :=MaxU;
         YWidth := YWidth-MaxO-50;
         XWidth :=MaxR;
         XWidth := XWidth-MaxL-36;
         IF (YWidth > 0) THEN
            MaxZ := YWidth DIV ZeilenAbstand
         ELSE
            MaxZ := 0;
         IF (XWidth > 0) THEN
            MaxS := XWidth DIV 8
         ELSE
            MaxS := 0;
         MaxCenterX := MaxL+(MaxR-MaxL) DIV 2;
         MaxCenterY := MaxO+(MaxU-MaxO) DIV 2;
      END;

   CONSTRUCTOR TMessageBoxFenster.Init;
      VAR Lauf: BYTE;
      BEGIN
         inherited Init(x1,y1,x2,y2,Za,Farbe1,Farbe2,Titel);
         FOR Lauf := 1 TO 6 DO
            MeldungsZeile[Lauf] := '';
         MeldungsZeilen := 1;
      END;

   FUNCTION TMessageBoxFenster.SplitMessage;
      VAR HelpEndPos,
          CutPos : BYTE;
          ArbeitsString : STRING;
          MaxLen,
          SwPos,
          Lauf : BYTE;

      BEGIN
         MeldungsZeilen := 1;
         MaxLen :=0;
         IF (LENGTH(Meldung) > 40 ) THEN BEGIN
            ArbeitsString := Meldung;
            REPEAT
               CutPos := MidSpace(Arbeitsstring);
               IF (CutPos <= 50) AND (CutPos > 0) THEN BEGIN
                  MeldungsZeile[MeldungsZeilen]:= LeftStr(ArbeitsString,CutPos-1);
                  INC(MeldungsZeilen);
                  IF (MaxLen < (CutPos-1)) THEN
                     MaxLen := CutPos-1;
                  ArbeitsString :=RightStr(ArbeitsString,CutPos+1);
               END ELSE BEGIN
                  HelpEndPos := CutPos-1;
                  WHILE (CutPos > 50) DO BEGIN
                     CutPos := MidSpace(LeftStr(ArbeitsString,HelpEndPos));
                     HelpEndPos := CutPos-1;
                  END;
                  IF (CutPos = 0) THEN BEGIN
                    ArbeitsString := LeftStr(ArbeitsString,49);
                  END ELSE BEGIN
                     MeldungsZeile[MeldungsZeilen] := LeftStr(ArbeitsString,CutPos-1);
                     IF (MaxLen < (CutPos-1)) THEN
                        MaxLen := CutPos-1;
                     INC(MeldungsZeilen);
                     ArbeitsString := RightStr(ArbeitsString,CutPos+1);
                  END;
               END;
            UNTIL (LENGTH(ArbeitsString) < 50) OR  (MeldungsZeilen = 6);
            MeldungsZeile[MeldungsZeilen]:=ArbeitsString;
            IF (LENGTH(ArbeitsString) > MAXLEN) THEN
               MaxLen := LENGTH(ArbeitsString);
         END ELSE BEGIN
            MeldungsZeile[1]:= Meldung;
            MaxLen := LENGTH(Meldung);
         END;
         SplitMessage  := MaxLen;
      END;

   CONSTRUCTOR TMessageBoxTimeFenster.Init;
      BEGIN
         inherited Init(x1,y1,x2,y2,Za,Farbe1,Farbe2,Titel);
         WaitTime := 0;
      END;

   FUNCTION TMessageBoxTimeFenster.KeyInput;
      VAR Ende : BOOLEAN;
          AnfZeit,EndZeit,Stuff,Seconds : WORD;
          EndCode :INTEGER;
      BEGIN
         EndCode := 0;
         Ende := FALSE;
         IF KEYPRESSED THEN
            INPUTKEY;
         GETTIME(Stuff,Stuff,AnfZeit,Stuff);
         IF WaitTime > 0 THEN
            EndZeit := (AnfZeit+WaitTime) MOD 60
         ELSE
            EndZeit := 61;
         REPEAT
            IF KeyPress(EndCode) THEN
               Ende := TRUE;
            GETTIME(Stuff,Stuff,Seconds,Stuff);
            IF (Seconds>= EndZeit) THEN
               Ende := TRUE;
            IF (ActionActive) THEN BEGIN
               Ende :=ActionFunc;
            END;
         UNTIL Ende;
         KeyInput :=EndCode;
      END;
                               {浜様様様様様�
   浜様様様様様様様様様様様様様郵 Bemerkung 麺様様様様様様様様様様様様様様様�
   �                            藩様様様様様�                               �
   � Aufgabe:     Auswahlfenster mit Vorgaben belegen                       �
   �                                                                        �
   � Parameter:   Zeilenabstand,Fensterfarbe,Titelfarbe:BYTE,               �
   �              Funktion f�r Auswahldaten:TGetFunc,Feldl�nge,Anzahl Felder�
   �              Textfabe Balken,BalkenHintergrund:BYTE                    �
   �                                                                        �
   � Resultate:   Keine                                                     �
   藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様夕}

   CONSTRUCTOR TAuswahlFenster.Init;

      VAR Lauf : BYTE;
          Breite,
          Hoehe,
          x1,
          y1,
          x2,
          y2      : WORD;
          NewLength : WORD;

      BEGIN
         IF Za < 11 THEN
            Za := 11;
         inherited Init(310,215,330,265,Za,Farbe1,Farbe2,Titel);
         ItemObj := TRUE;
         LeaveOnFKey := TRUE;
         SetMaxSize(40,40,600,440);
         Laenge := 0;
         ESp := 1;
         NewLength := Length(Titel);
         ResizeWin(NewLength,0);
         HGH := 2;
         HGV := 2;
         Anzahl := 0;
         Wert := 0;
         FensterInfo:=Info;
         BarVg  := BVG;
         BarHg  := BHG;
         LEER := '';
         FOR Lauf := 1 TO 24 DO
            ItemList[Lauf] := NIL;
      END;

   PROCEDURE TAuswahlFenster.Enable;
      VAR Lauf    : BYTE;

      BEGIN
         IF (K = '_') THEN BEGIN
         END ELSE BEGIN
            IF (Anzahl > 0) THEN BEGIN
               FOR Lauf := 1 TO Anzahl DO BEGIN
                  IF (ItemList[Lauf]^.FeldKennung = K) THEN
                     ItemList[Lauf]^.Enabled:= TRUE;
               END;
            END;
         END;
      END;

   PROCEDURE TAuswahlFenster.Disable;
      VAR Lauf    : BYTE;

      BEGIN
         IF (K = '_') THEN BEGIN
         END ELSE BEGIN
            IF (Anzahl > 0) THEN BEGIN
               FOR Lauf := 1 TO Anzahl DO BEGIN
                  IF (ItemList[Lauf]^.FeldKennung = K) THEN
                     ItemList[Lauf]^.Enabled:= FALSE;
               END;
            END;
         END;
      END;

   PROCEDURE TAuswahlFenster.AddItem;
      VAR Nzeilen : BYTE;
          NLaenge : BYTE;
          Neu: TAItemPtr;
      BEGIN
         IF (Anzahl < 24) THEN BEGIN
            Hide;
            NZeilen := Zeilen +1;
            NLaenge := LENGTH(Tx)+3;
            IF NLaenge > Laenge THEN BEGIN
               Laenge := NLaenge;
               LEER := REPLICATE(' ',Laenge);
            END;
            ResizeWin(Laenge+1,NZeilen);
            NEW(Neu);
            Neu^.Text := Tx;
            Neu^.Enabled := TRUE;
            Neu^.FeldKennung := K;
            INC(Anzahl);
            ItemList[Anzahl] := Neu;
         END;
      END;


   FUNCTION  TAuswahlFenster.ItemDisabled;
      BEGIN
         ItemDisabled :=  NOT (ItemList[It]^.Enabled);
      END;

   PROCEDURE TAuswahlFenster.EnableItem;
        BEGIN
           IF (It <= Anzahl) THEN BEGIN
              ItemList[It]^.Enabled := TRUE;
           END;
        END;

   PROCEDURE TAuswahlFenster.DisableItem;
        VAR Lauf : BYTE;
            Zgr  : TAItemPtr;
        BEGIN
           IF (It <= Anzahl) THEN BEGIN
              ItemList[It]^.Enabled := FALSE;
           END;
        END;

   FUNCTION TAuswahlFenster.GetData;
      VAR Zgr : TAItemPtr;
          Lauf :BYTE;
      BEGIN
         GetData := ItemList[Nr]^.Text;
      END;

                               {浜様様様様様�
   浜様様様様様様様様様様様様様郵 Bemerkung 麺様様様様様様様様様様様様様様様�
   �                            藩様様様様様�                               �
   � Aufgabe:     Text zu einer Position in Feldl�nge erstellen             �
   �                                                                        �
   � Parameter:   Position:BYTE                                             �
   �                                                                        �
   � Resultate:   Positionsnummer+Positionstext:STRING                      �
   藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様夕}
   FUNCTION  TAuswahlFenster.MakeText;


      VAR Txt,
          HText : STRING;

      BEGIN
         HText := ItemList[Nr]^.Text;
         CASE Nr OF
            1..9   : Nr := Nr +48;
            10     : Nr := 48;
            11..24 : Nr := Nr +54;
         END;
         Txt := CHR(Nr)+') ';
         Txt := Txt+HText;
         IF LENGTH(Txt) >= Laenge THEN
            MakeText:=COPY(Txt,1,Laenge)
         ELSE
            MakeText := Txt+COPY(LEER,1,Laenge-LENGTH(Txt));
      END;

                               {浜様様様様様�
   浜様様様様様様様様様様様様様郵 Bemerkung 麺様様様様様様様様様様様様様様様�
   �                            藩様様様様様�                               �
   � Aufgabe:     Alle Auswahlfensterpositionen ausgeben                    �
   �                                                                        �
   � Parameter:   Keine                                                     �
   �                                                                        �
   � Resultate:   Keine                                                     �
   藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様夕}

   PROCEDURE TAuswahlFenster.GiveChoice;

      VAR Lauf  : BYTE;

      BEGIN
         Show('');
         FOR Lauf := 1 TO Anzahl DO
            IF ItemDisabled('',Lauf) THEN
               PlaceText(ESp,Lauf,MakeText(Lauf),3,255,HGH,HGV)
            ELSE
               PlaceText(ESp,Lauf,MakeText(Lauf),255,255,HGH,HGV);
      END;

                               {浜様様様様様�
   浜様様様様様様様様様様様様様郵 Bemerkung 麺様様様様様様様様様様様様様様様�
   �                            藩様様様様様�                               �
   � Aufgabe:     Auswahlbalken auf Position anzeigen                       �
   �                                                                        �
   � Parameter:   Position : BYTE                                           �
   �                                                                        �
   � Resultate:   Keine                                                     �
   藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様夕}

   PROCEDURE TAuswahlFenster.ShowBar;

      BEGIN
         PLACETEXT(ESp,Pos,MakeText(Pos),BarVG,BarHG,HGH,HGV)
      END;

                               {浜様様様様様�
   浜様様様様様様様様様様様様様郵 Bemerkung 麺様様様様様様様様様様様様様様様�
   �                            藩様様様様様�                               �
   � Aufgabe:     Auswahlbalken auf andere Position setzen                  �
   �                                                                        �
   � Parameter:   Alte Position, Neue Position : BYTE                       �
   �                                                                        �
   � Resultate:   Keine                                                     �
   藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様夕}

   PROCEDURE TAuswahlFenster.ChangeBar;

      BEGIN
         PLACETEXT(ESp,OldPos,MakeText(OldPos),255,255,HGH,HGV);
         ShowBar(NewPos)
      END;

   PROCEDURE TAuswahlFenster.Setpos;

      BEGIN
         Wert := POS;
      END;

   FUNCTION TAuswahlFenster.GetPos;

      BEGIN
         GetPos:=Wert;
      END;


   PROCEDURE TAuswahlFenster.SetFKeyQuit;
      BEGIN
         LeaveOnFKey := VAL;
      END;

   FUNCTION TAuswahlFenster.MakeChoice;

      VAR MPos,
          OldVal    : BYTE;
          Ende      : BOOLEAN;
          MoreEval  : BOOLEAN;
          InputCode : INTEGER;
          Eingabe : STRING;
          NoLoop      : BOOLEAN;
          Check : BOOLEAN;

      BEGIN
         Eingabe := '';
         Done := FALSE;
         Abbruch :=FALSE;
         MPos := Wert;
         NoLoop := TRUE;
         WHILE ItemDisabled('',Wert) AND NoLoop DO BEGIN
            INC(Wert);
            IF (Wert > Anzahl) THEN
               Wert := 1;
            IF (Wert = MPOS) THEN
               NoLoop:= FALSE;
         END;
         IF NoLoop THEN BEGIN
            Ende := FALSE;
            IF NOT Angezeigt THEN BEGIN
               GiveChoice;
               ShowBar(Wert);
            END;
            REPEAT
               InputCode:=KeyInput;
               OldVal := Wert;
               Ende := KeyAction(InputCode,Wert,Eingabe,Abbruch,Done,Check,MoreEval);
               KeyDone := Done AND (NOT MoreEval);
               IF NOT Done THEN BEGIN
                  CASE InputCode OF
                     -72,-75,
                          45  : REPEAT
                                   DEC(Wert);
                                   IF (Wert = 0) THEN
                                      Wert:= Anzahl;
                                UNTIL NOT ItemDisabled('',Wert);
                     -77,-80,
                       32,43  : REPEAT
                                   INC(Wert);
                                   IF (Wert > Anzahl) THEN
                                      Wert:= 1;
                                UNTIL NOT ItemDisabled('',Wert);
                     49..57   : IF (Anzahl+48 >= InputCode) AND
                                   (NOT ItemDisAbled('',InputCode-48)) THEN BEGIN
                                   ENDE := TRUE;
                                   Done := TRUE;
                                   Wert:= InputCode-48;
                                   InputCode := 13;
                                 END ELSE
                                    WRITE(CHR(7));
                     48       : IF (Anzahl >= 10) AND (NOT ItemDisAbled('',10)) THEN BEGIN
                                   ENDE := TRUE;
                                   Done := TRUE;
                                   Wert:=10;
                                   InputCode := 13;
                                END ELSE
                                    WRITE(CHR(7));
                     65..78   : IF (Anzahl+54 >= InputCode) AND
                                   (NOT ItemDisabled('',InputCode-54)) THEN BEGIN
                                   ENDE := TRUE;
                                   Done := TRUE;
                                   Wert:= InputCode-54;
                                   InputCode := 13;
                                END ELSE
                                   WRITE(CHR(7));
                     97..110  : IF (Anzahl+86 >= InputCode) AND
                                   (NOT ItemDisabled('',InputCode-86)) THEN BEGIN
                                   ENDE := TRUE;
                                   Done := TRUE;
                                   Wert:= InputCode-86;
                                   InputCode := 13;
                                END ELSE
                                   WRITE(CHR(7));
                     -71      : BEGIN
                                   Wert:=1;
                                   WHILE ItemDisabled('',Wert) DO
                                      INC(Wert);
                                END;
                     -79      : BEGIN
                                   Wert:=Anzahl;
                                   WHILE ItemDisabled('',Wert) DO
                                      DEC(Wert);
                                END;
                     27       : BEGIN
                                   IF LeaveOnFkey THEN BEGIN
                                      Ende := TRUE;
                                      Done := TRUE;
                                      Abbruch := TRUE;
                                   END;
                                END;
                      13,9,
                       -15    : BEGIN
                                   ENDE := TRUE;
                                   Done := TRUE;
                                END;
                     -68..-60 : BEGIN
                                   IF LeaveOnFKey THEN BEGIN
                                      ENDE := TRUE;
                                      Done := TRUE;
                                   END;
                                END;
                     ELSE       WRITE(CHR(7));
                  END;
                  IF OldVal <> Wert THEN
                     ChangeBar(OldVal,Wert);
               END;
            UNTIL Ende;
            IF Abbruch THEN
               Wert:= Mpos;
         END ELSE
            InputCode := 27;
         MakeChoice:=InputCode;
      END;

   CONSTRUCTOR TRequest.Init;

      BEGIN
         inherited Init(170,200,470,280,0,7,2,'Best�tigung');
         SetMaxSize(100,160,540,320);
      END;


   FUNCTION TRequest.Act;
      VAR Ende,
          Zustand : BOOLEAN;
          EndCode : INTEGER;
          JPos,NPos,
          MaxLen,
          SwPos,
          Lauf : BYTE;
      BEGIN
         Zustand := FALSE;
         MaxLen :=SplitMessage(Meldung);
         SWPos := MeldungsZeilen+1;
         ResizeWin(MaxLen,SwPos+1);
         Show('');
         IF (Spalten >= 21) THEN BEGIN
            JPos := 5;
            NPos := Spalten-9;
         END ELSE BEGIN
            JPos := 1;
            NPos := Spalten-6;
         END;
         FOR Lauf := 1 TO MeldungsZeilen DO
           Placetext(-1,Lauf,MeldungsZeile[Lauf],255,255,HGH,HGV);
         Ende := FALSE;
         REPEAT
            IF Zustand THEN BEGIN
               PlaceText(JPos,SwPos,'  Ja  ',7,2,HGH,HGV);
               PlaceText(NPos,SwPos,' Nein ',255,255,HGH,HGV);
            END ELSE BEGIN
               PlaceText(JPos,SwPos,'  Ja  ',255,255,HGH,HGV);
               PlaceText(NPos,SwPos,' Nein ',7,2,HGH,HGV);
            END;
            EndCode := KeyInput;
            CASE EndCode OF
               27,
               78,
               110 : BEGIN
                        Ende   := TRUE;
                        Zustand:= FALSE;
                     END;
               74,
               106 : BEGIN
                        Ende := TRUE;
                        Zustand:= TRUE;
                     END;
               13  : Ende := TRUE;

               -72,
               -75,
               -77,
               -80,
               -15,
               9   : Zustand := NOT Zustand;
            END;
         UNTIL Ende;
         Act := Zustand;
         Hide;
      END;

   CONSTRUCTOR TTripleQuest.Init;
      BEGIN
         inherited Init(170,200,470,280,0,7,2,'Best�tigung');
         Text1 :='   Ja    ';
         Text2 :='  Nein   ';
         Text3:= ' Abbruch ';
         SetMaxSize(100,160,540,320);
      END;

   FUNCTION TTripleQuest.Act;
      VAR Ende    : BOOLEAN;
          Zustand : INTEGER;
          EndCode : INTEGER;
          JPos,NPos,APos,
          MaxLen,
          SwPos,
          Lauf : BYTE;
      BEGIN
         Zustand := 1;
         MaxLen :=SplitMessage(Meldung);
         SWPos := MeldungsZeilen+1;
         ResizeWin(MaxLen,SwPos+1);
         Show('');
         JPos := 3;
         APos := Spalten-10;
         NPos := (Spalten DIV 2) - 4;
         FOR Lauf := 1 TO MeldungsZeilen DO
           Placetext(-1,Lauf,MeldungsZeile[Lauf],255,255,HGH,HGV);
         Ende := FALSE;
         REPEAT
            CASE Zustand OF
               1 : BEGIN
                      PlaceText(JPos,SwPos,Text1,7,2,HGH,HGV);
                      PlaceText(NPos,SwPos,Text2,255,255,HGH,HGV);
                      PlaceText(APos,SwPos,Text3,255,255,HGH,HGV);
                   END;
               0 : BEGIN
                      PlaceText(JPos,SwPos,Text1,255,255,HGH,HGV);
                      PlaceText(NPos,SwPos,Text2,7,2,HGH,HGV);
                      PlaceText(APos,SwPos,Text3,255,255,HGH,HGV);
                   END;
               -1: BEGIN
                      PlaceText(JPos,SwPos,Text1,255,255,HGH,HGV);
                      PlaceText(NPos,SwPos,Text2,255,255,HGH,HGV);
                      PlaceText(APos,SWPos,Text3,7,2,HGH,HGV);
                   END;
            END;
            EndCode := KeyInput;
            CASE EndCode OF
               27,
               65,
               97  : BEGIN
                        Ende   := TRUE;
                        Zustand:= -1;
                     END;
               78,
               110 : BEGIN
                        Ende   := TRUE;
                        Zustand:= 0;
                     END;
               74,
               106 : BEGIN
                        Ende := TRUE;
                        Zustand:= 1;
                     END;
               13  : Ende := TRUE;
               -72,
               -75,
               -15 : BEGIN
                        INC(Zustand);
                        IF (Zustand = 2) THEN
                           Zustand := -1;
                     END;
               -77,
               -80,
               9   : BEGIN
                        DEC(Zustand);
                        IF (Zustand = -2) THEN
                           Zustand := 1;
                     END;
            END;
         UNTIL Ende;
         Act := Zustand;
         Hide;
      END;

   PROCEDURE TTripleQuest.SetChoice;
      BEGIN
         Text1:= TN1;
         Text2:= TN2;
         Text3:= TN3;
      END;


   CONSTRUCTOR TInfoBox.Init;

      BEGIN
         inherited Init(170,200,470,280,0,6,2,'Information');
         SetMaxSize(100,170,540,310);
      END;

   PROCEDURE TInfoBox.Act;

      VAR MaxLen,
          SwPos,
          Lauf : BYTE;
          Taste : INTEGER;

      BEGIN
         MeldungsZeilen := 1;
         MaxLen :=SplitMessage(Meldung);
         SWPos := MeldungsZeilen+2;
         ResizeWin(MaxLen,SwPos);
         Show('');
         FOR Lauf := 1 TO MeldungsZeilen DO
           Placetext(-1,Lauf,MeldungsZeile[Lauf],255,255,HGH,HGV);
         WRITE(chr(7));
         Placetext(1,Zeilen,'Taste...',255,255,HGH,HGV);
         Show('');
         WaitTime := Zeit;
         Taste := KeyInput;
         Hide;
      END;

   CONSTRUCTOR TStatusBox.Init;

      BEGIN
         inherited Init(170,200,470,280,0,7,2,'Status');
         AlteZeile1:='';AlteZeile2:='';
      END;

   PROCEDURE TStatusBox.Act;

      BEGIN
         Show('');
         IF Zeile1 <> AlteZeile1 THEN BEGIN
            Placetext(-1,1,REPLICATE(' ',LENGTH(AlteZeile1)),255,255,HGH,HGV);
            Placetext(-1,1,Zeile1,255,255,HGH,HGV);
            AlteZeile1 := Zeile1;
         END;
         IF Zeile2 <> AlteZeile2 THEN BEGIN
            Placetext(-1,2,REPLICATE(' ',LENGTH(AlteZeile2)),255,255,HGH,HGV);
            Placetext(-1,2,Zeile2,255,255,HGH,HGV);
            AlteZeile2 := Zeile2;
         END;
      END;

   PROCEDURE TStatusBox.Hide;

      BEGIN
         AlteZeile1:='';AlteZeile2:='';
         inherited Hide;
      END;

   CONSTRUCTOR TFaultBox.Init;

      BEGIN
         TMessageBoxTimeFenster.Init(170,200,470,280,0,3,6,'Fehler');
         SetMaxSize(100,170,540,310);
         Titelfarbe := 4;
      END;

   PROCEDURE TFaultBox.Act;
      VAR Taste : INTEGER;
          MaxLen,
          SwPos,
          Lauf : BYTE;

      BEGIN
         MeldungsZeilen := 1;
         MaxLen :=SplitMessage(Meldung);
         SWPos := MeldungsZeilen+2;
         ResizeWin(MaxLen,SwPos);
         Show('');
         FOR Lauf := 1 TO MeldungsZeilen DO
           Placetext(-1,Lauf,MeldungsZeile[Lauf],255,255,HGH,HGV);
         WRITE(chr(7));
         Placetext(1,Zeilen,'Taste...',255,255,HGH,HGV);
         WaitTime := Zeit;
         Taste := KeyInput;
         Hide;
      END;


BEGIN
   OPENGRAPH;
   SETCOLOR(0);
   SETPALETTEALL;
   SETPALETTEWORK;
   SETFILLSTYLE(1,1);
   BAR(0,0,639,479);
   TripleQuest.Init;
   FaultBox.Init;
   InfoBox.Init;
   Request.Init;
   StatusBox.Init;
END.
{============================
 Versionshistorie
 $Log:$
 ============================}
