{============================
 Dateiname:   $Source: $
 Version:     $Revision: $
 Autor:       $Author: $
 Gelockt von: $Locker: $
 Originaldatum : Fri May 09 06:55:28 GMT+02:00 1997
 Dateihistorie am Ende der Datei
 ============================}
UNIT CASHDATA;
{$O+}

{
    Basisunit fr alle Dialogobjekte enth„lt die Typdefinitionen fr die
    verwendeten Funktionen und das Basisobjekt #TDialog#

}

INTERFACE

   USES ZUSAETZE;

   TYPE  TFkeyActionPtr = ^TFkeyAction; { Zeiger vom Typ #TFkeyAction# }
         TFKeyAction = FUNCTION(VAR Taste:INTEGER;VAR Nr:BYTE;VAR InpString:STRING;
                                VAR Break,Check,MoreEval:BOOLEAN):BOOLEAN;
         { TFKeyAction ist der Funktionstyp aller Sondertastenfunktionen. Wenn an ein
           in einem #TDialog#-Objekt Sondertasten vom Typ #TFkey# definiert sind wird nach einem
           Tastendruck geprft ob der Tastencode eine Sondertastenfunktion hat. Wenn dies so ist
           dann  wird zugeh”rige Funktion aufgerufen.Alle Funktionsparameter
           werden bei Žnderungen im Objekt ver„ndert weiterbearbeitet.

              Die Funktionsparameter haben folgende Bedeutung:

              Taste     : Wert der bergebenen Taste
              Nr        : Nr des Feldes in einem Auswahlmen, sonst noch ohne
                          Bedeutung
              InpString : Bei Eingabefelder aktueller Inhalt des Feldes

              Break, Check und MoreEval sind nur Rckgabewerte

              Break     : Liefert Abbruchkriterium, falls Break auf wahr gesetzt wird
                          wird das aktuelle Objekt verlassen und die Kontrolle an das
                          n„chsth”rere  Dialogobjekt bergeben (z.B. Eingabefeld gibt
                          Kontrolle an Fenster ab ) Break setzt in dem DialogObjekt den
                          Wert Abbruch der dann zum Beispiel bei einem Eingabefenster
                          als Endbedingung fr die aufrufende Schleife genutzt werden
                          kann

              Check     : Falls Check gesetzt wird die Standardprfung fr das Feld
                          ausgefhrt, sonst nicht.
              MoreEval  : Falls MoreEval gesetzt wird, wird der Taste zus„tzlich
                          durch das Objekt ausgewertet. Das kann dazu benutzt werden um
                          um Beispiel Tastenbelegungen umzudefinieren. Beispiel hierfr
                          sind die Funktionen DN2Tab in GMLIEFED. Hier wird ein Druck der
                          Taste Runter in ein Tab umgewandelt.

              Der Ergebniswert der Funktion gibt an ob eine Auswertung der Taste durchgefhrt
              wurde. Wenn keine Auswertung durchgefhrt wurde. Dann wird die Taste durch das Objekt
              normal ausgewertet.
              }



         TFKeyDispPtr = ^TFKeyDisp;
         TFKeyDisp = RECORD {  verwendet fr die Buttonleiste }
                  Z1,
                  Z2 :STRING[20];
         END;

         TFKeyPtr = ^TFKey; { Zeiger auf Funktionstastentyp #TFKey# }
         TFKey = RECORD { Funktionstastentyp, wird genutzt um Sondertasten an
                          #TDialog#-Objekte anzuh„ngen }
                  Key     : INTEGER; { Funktionstastenwert }
                  Action  : TFKeyAction; { zugeh”rige Funktion }{#X TFkeyaction}
                  PreCheck: BOOLEAN; { falls PreCheck gesetzt ist, wird die
                                       Prffunktion  des #TDialog#-Objektes vor dem Aufruf
                                       von #Action# gestartet und }
                  InfoTxt : STRING[50];{ Text fr kontextabh„ngige Hilfe }
                  NextKey : TFKeyPtr; { nur fr Listenverwaltung der Sondertasten}
                END;

   TYPE TFKeyInfoPtr = ^TFKeyInfo; { Zeiger auf #TFkeyInfo#}
        TFKeyInfo = RECORD { Datenstruktur fr Generierung der Hilfefenster }
             Key   : INTEGER;
             Info  : STRING[50];
             Next  : TFKeyInfoPtr;
        END;

   TYPE TBoolFuncPtr = ^TBoolFunc;{ Zeiger auf #TBoolFunc#}
        TBoolFunc = FUNCTION:BOOLEAN;

   TYPE TMenuFunc = FUNCTION(Nr:BYTE;VAR Taste:INTEGER):BOOLEAN;
      { Funktion fr Auswahlmens, es wird die aktuelle Cursorposition
        bergeben und die letzte gedrckte Taste. Der Ergebniswert der
        Funktion gibt an ob das Auswahlmen geschlossen werden soll }



   TYPE TSpecialEvalFuncPtr  = ^TSpecialEvalFunc;{ Zeiger auf #TSpecialEvalFunc#}
        TSpecialEvalFunc = FUNCTION(VAR Eingabe:STRING;VAR InputCode:INTEGER;
                                    VAR Cpos:BYTE):BOOLEAN;
        { Typ fr spezielle Eingabebehandlung nach einem Tastendruck im
           EingabeFeld.Es k”nnen hier der aktuelle Feldinhalt, die Taste und die
           Cursorposition ver„ndert werden.


           Beim Rollfeld hat diese Funktion ein andere Bedeutung. Da
           im Rollfeld die Eingabe nicht direkt beeinfluát wird,
           wird hier nach Auswertung des Tastendrucks der Eingabetext
           neu erzeugt und diese Funktion mit dem neuen Wert aufgerufen.
           Kann zur Behandlung dynamischer Ver„nderung w„hrend der Bearbeitung
           eines Rollfelds genutzt werden.
        }





   TYPE TActionFuncPtr = ^TActionFunc;{ Zeiger auf #TActionFunc#}
        TActionFunc = FUNCTION:BOOLEAN;
        { Typ fr Funktion die in allen Felder und Fenster w„hrend dem
          warten auf den n„chsten Tastendruck ausgefhrt wird. Achtung bei
          dieser Funktion sie wird in einer Schleife aufgerufen die nur diese
          Funktion ausfhrt und auf einen Tastendruck wartet.
          Zeitintensive Funktionen k”nnen damit so wirken als ob das Programm
          abgestrtzt ist.}

   TYPE TGetFuncPtr = ^TGetFunc;{ Zeiger auf #TGetFunc#}
        TGetFunc = FUNCTION(Nr:WORD):STRING;
        {  Typ fr Datenbernahme fr TChoiceFeld und TRollFeld,
             Funktionstyp liefert Text zu bergebener Position.      }


   TYPE TFensterZustand = (Aktiv,
                           NichtAktiv);
        { Gibt den Anfangszustand fr Fenster an und erm”glicht
            somit aktivieren und deaktivieren von Fenster ber
            SwitchStatus aus CASHNEU }

        TModi       = (i,o,a,en,di,de);
        { Modustypen der Eingabefelder
          i  = Insert           FeldModus bei Fokus
          o  = Overwrite           "      "
          a  = Overwrite All       "      "
          en = Enabled          FeldModus ohne Focus
          di = Disabled            "      "
          de = Default          Feldmodus fr zus„tzliche Farbpalette}

        TModiFarben = ARRAY[i..de] OF BYTE;
          {Typ fr Farbdefinition in den einzelnden Modi }

        TPrae       = (Normal,Extra);
           {TYP fr Darstellungsart der Eingabefelder
            Normal = Normale Darstellung Kasten
            Extra  =Darstellung runde Enden   }

        TSchalter   = (On,Off);
           { TYP fr Cursorstatus }


   TYPE  TFeldZgr = ^TFelder; { Zeiger auf #TFelder# }
         TDialogObjPtr  = ^TDialogObj; { Zeiger auf #TDialogObj# }
         TFelder = RECORD { Datenstruktur fr eine Feldliste
                             in einem Dialogobjekt mit mehreren
                             Unterelementen, zum Beispiel Eingabefenster
                             Feldgruppen. }
                      Vorgaenger, { Zeiger auf vorhergehendes Feld
                                    beim ersten Feld Zeiger auf letztes
                                    Feld }

                      Nachfolger : TFeldZgr; { Zeiger auf n„chstes Feld
                                               beim letzten Feld Zeiger
                                               auf erstes Feld }
                      Aligned    : BOOLEAN; { Dieser Wert gibt an ob das
                                              Eingabefeld schon positioniert
                                              wurde. Bei der Positionierung
                                              werden alle noch nicht positionierten
                                              Felder berprft und die Eingabefeld-
                                              startposition angeglichen. Die Titel
                                              der Felder werden davon nicht
                                              beeinfluát. }
                      FeldKennung: STRING; { Die Feldkennung ist ein wesentlicher
                                             Bestandteil der Liste. Die Feldkennung
                                             l„sst Selektionen bei Aktionen auf dem
                                             Dialogobjekten zu. N„heres hierzu
                                             in der Beschreibung von #TDialogObj#
                                             Die Feldkennung sollte im allgemeinen
                                             eine Zahl sein, es drfen aber auch
                                             Buchstaben sein. Bei der Kennung darf
                                             nur nicht das Zeichen '_' verwendet
                                             werden ,da dieses Zeichen als Trennzeichen
                                             in der Hierarchie der Dialogobjekte verwendet
                                             wird.}
                      Feld       : TDialogObjPtr; { Zeiger auf das DialogObj }
                   END;
        TDialogObj  = OBJECT
           { Das DialogObjekt ist das Basisobjekt fr alle Dialogelemente.
             Aus dem Dialogobjekt werden sowohl Eingabefenster als auch
             Auswahlmens und Eingabefelder entwickelt. Die allermeisten Methoden
             der Objekte werden hier definiert. Wesentliche Bestandteile des
             Dialogobjektes sind die Sonderfunktionen und die Funktionstastenliste.
             Das Dialogobjekt enth„lt auch Informationen die nicht bei allen
             Nachfahren wirklichen Sinn ergeben, die aber notwendig sind um
             einen rekrusiven Informationsaufbau bei den Mehoden zu erm”glichen.
             Ein gutes Beispiel hierfr sind die Methoden #Enable# und #Disable#.
             Bei diesen Methoden wird wie bei fast allen Methoden eine Feldkennung
             mit bergeben. Bei den einfachen Eingabefelder spielt der Inhalt
             dieser Feldkennung keine Rolle, das diese Felder entweder nur
             editierbar oder nicht sind. Aber durch diesen Aufbau wird eine
             allgemeine Behandlung der Methoden erm”glicht.

           }
           SpecialEvalActive :BOOLEAN; {Wird gesetzt wenn #SpecialEvalFunc#
                                        belegt ist }
           SpecialEvalFunc : TSpecialEvalFunc; { Funktion die nach Tastendruck
                                                 ausgefhrt wird }
           KeyList    : TFKeyPtr; { Liste der Sondfertasten mit ihren Funktionen }
           FocusID    :STRING;  { Kennung fr das aktuelle Fokusfeld gibt
                                  die aktuelle Position in einem Dialogobjekt
                                  mit mehren Unterobjekten an}
                                {#X FocusField}
           FocusField : BYTE;   { Wie #FocusID#, aber hier die Position in der
                                  Feldliste }
           ActionActive :BOOLEAN; { Wird gesetzt wenn #ActionFunc# belegt ist }
           ActionFunc : TActionFunc;
           ItemObj,
           MultiObj,
           Toggle,
           Abbruch,
           KeyDone,
           Done,
           Default    : BOOLEAN;
           LeaveOnFKey,
           LeaveOnCR         : BOOLEAN;
           FeldListenAnfang  :TFeldZgr;
           AnzahlFelder : BYTE;
           CONSTRUCTOR Init;
           DESTRUCTOR Release;VIRTUAL;
           PROCEDURE  SetData(DataSeg,DataOfs:WORD);VIRTUAL;
           PROCEDURE  AddNew(K:STRING; Zeiger:TDialogObjPtr; VAR Def); VIRTUAL;
           PROCEDURE  AddField(ItK:STRING;K:STRING; Zeiger:TDialogObjPtr; VAR Def); VIRTUAL;
           PROCEDURE  AddSubField(SubFK:STRING;Itk:STRING;K:STRING;Zeiger:TDialogObjPtr;Var Def);VIRTUAL;
           PROCEDURE  SetActionFunc(K:STRING;Func:TActionFunc);VIRTUAL;
           PROCEDURE  SetSEvalFunc(K:STRING;Func:TSpecialEvalFunc);VIRTUAL;
           PROCEDURE  AddActionKey(K:STRING;CKey:INTEGER;Action:TFKeyAction;PreCheck:BOOLEAN;
                      InfoTxt:STRING);VIRTUAL;
           PROCEDURE  ClearActionKey(K:STRING;CKey:INTEGER);VIRTUAL;
           PROCEDURE  Show(K:STRING);VIRTUAL;
           PROCEDURE  Refresh(K:STRING);VIRTUAL;
           PROCEDURE  SetDefault(K:STRING);VIRTUAL;
           PROCEDURE  ClearDefault(K:STRING);VIRTUAL;
           FUNCTION   IsDefault(K:STRING):BOOLEAN;VIRTUAL;
           FUNCTION   GetKeyInfoList(ItK:STRING):TFKeyInfoPtr;VIRTUAL;
           PROCEDURE  DisposeKeyInfoList(FirstKey:TFKeyInfoPtr);VIRTUAL;
           PROCEDURE  StripKeyInfoList(VAR FirstKey:TFKeyInfoPtr);VIRTUAL;
           PROCEDURE  SetFocusField(FPos:BYTE);VIRTUAL;
           FUNCTION   GetFocusField:BYTE;VIRTUAL;
           FUNCTION   GetFocusId:STRING;VIRTUAL;
           PROCEDURE  SetFocusOnId(ItK:STRING);VIRTUAL;
           PROCEDURE  AddItem(ItK:STRING;TX:STRING);VIRTUAL;
           FUNCTION   ItemDisabled(K:STRING;It:WORD):BOOLEAN;VIRTUAL;
           PROCEDURE  DisableItem(K:STRING;It:WORD);VIRTUAL;
           PROCEDURE  EnableItem(K:STRING;It:WORD);VIRTUAL;
           PROCEDURE  SetToggle(K:STRING;Val:BOOLEAN);VIRTUAL;
           PROCEDURE  Enable(K:STRING);VIRTUAL;
           PROCEDURE  Disable(K:STRING);VIRTUAL;
           FUNCTION   Disabled(ItK:STRING):BOOLEAN;VIRTUAL;
           FUNCTION   Left:WORD;VIRTUAL;
           FUNCTION   Right:WORD;VIRTUAL;
           FUNCTION   Top:WORD;VIRTUAL;
           FUNCTION   Bottom:WORD;VIRTUAL;
           FUNCTION   Input:INTEGER;VIRTUAL;
           PROCEDURE  Save(K:STRING);VIRTUAL;
           PROCEDURE  Restore(K:STRING);VIRTUAL;
           FUNCTION   Changed(K:STRING):BOOLEAN;VIRTUAL;
           FUNCTION   KeyInput:INTEGER;VIRTUAL;
           FUNCTION   Valid(K:STRING):BOOLEAN;VIRTUAL;
           PROCEDURE  SetVal(ItK:STRING;ValString:STRING);VIRTUAL;
           FUNCTION   GetVal(ItK:STRING):STRING;VIRTUAL;
           FUNCTION   GetInputStartPos:WORD;VIRTUAL;
           PROCEDURE  SetInputStartPos(ColPos:WORD);VIRTUAL;
           PROCEDURE  SetCRQuit(Val:BOOLEAN);VIRTUAL;
           PROCEDURE  SetFKeyQuit(Val:BOOLEAN);VIRTUAL;
           FUNCTION   KeyAction(VAR Taste:INTEGER;VAR Nr:BYTE;VAR InpString:STRING;
                                VAR Break,ActionDone,Check,MoreEval:BOOLEAN):BOOLEAN;VIRTUAL;
        END;
        { Basistyp fr alle Dialogobjekte }


        TFKeyLine = ARRAY[1..6] OF TFKeyDisp;


   TYPE TBottomline = OBJECT(TDialogObj)
           FTasten    : TFKeyLine;
           FKeyAction : ARRAY [1..6] OF TFKey;
           Hidden     : BOOLEAN;
           ButtonChanged : ARRAY[1..6] OF BOOLEAN;
           ButtonErased  : ARRAY[1..6] OF BOOLEAN;
           CONSTRUCTOR Init;
           DESTRUCTOR Release;VIRTUAL;
           PROCEDURE SetAll(Inhalt:TFkeyLine);
           PROCEDURE SetButton(Nr:BYTE;Inhalt:TFkeyDisp);
           PROCEDURE SetButtonAct(Nr:BYTE;Action:TFkey);
           PROCEDURE EraseAll;
           PROCEDURE EraseButton(Nr:BYTE);
           PROCEDURE EraseButtonAct(Nr:BYTE);
           PROCEDURE Hide;VIRTUAL;
           PROCEDURE Show(K:STRING);VIRTUAL;
           FUNCTION  Changed(K:STRING):BOOLEAN;VIRTUAL;
           FUNCTION  KeyAction(VAR Taste:INTEGER;VAR Nr:BYTE;VAR InpString:STRING;
                               VAR Break,ActionDone,Check,MoreEval:BOOLEAN):BOOLEAN;VIRTUAL;
        END;

   FUNCTION ButtonMoreEval(VAR Taste:INTEGER;VAR Nr:BYTE;VAR InpString:STRING;VAR Break,Check,MoreEval:BOOLEAN):BOOLEAN;FAR;
   FUNCTION ButtonQuit(VAR Taste:INTEGER;VAR Nr:BYTE;VAR InpString:STRING;VAR Break,Check,MoreEval:BOOLEAN):BOOLEAN;FAR;
   FUNCTION ButtonQuit0(VAR Taste:INTEGER;VAR Nr:BYTE;VAR InpString:STRING;VAR Break,Check,MoreEval:BOOLEAN):BOOLEAN;FAR;
   FUNCTION ButtonBreak(VAR Taste:INTEGER;VAR Nr:BYTE;VAR InpString:STRING;VAR Break,Check,MoreEval:BOOLEAN):BOOLEAN;FAR;
   FUNCTION ButtonBreak0(VAR Taste:INTEGER;VAR Nr:BYTE;VAR InpString:STRING;VAR Break,Check,MoreEval:BOOLEAN):BOOLEAN;FAR;
   FUNCTION DoNothing(VAR Taste:INTEGER;VAR Nr:BYTE;VAR InpString:STRING;VAR Break,Check,MoreEval:BOOLEAN):BOOLEAN;FAR;

   CONST Leer         : TFKeyDisp = (Z1 : ''   ;Z2 : '');

   VAR   ActionTMod: TMODI;
         ActionText: STRING;

IMPLEMENTATION
    USES GRAPNEU;

    CONSTRUCTOR TDialogObj.Init;
      BEGIN
         LeaveOnFkey := TRUE;
         LeaveOnCR := TRUE;
         FeldListenAnfang := NIL;
         KeyList := NIL;
         Done := FALSE;
         KeyDone := FALSE;
         Abbruch := FALSE;
         ActionActive := FALSE;
         SpecialEvalActive := FALSE;
         Default := FALSE;
         FocusField := 0;
         FocusID    := '';
         AnzahlFelder := 0;
         MultiObj := FALSE;
         ItemObj := FALSE;
         Toggle := FALSE;
      END;


   PROCEDURE TDialogObj.AddNew;

      VAR   NeuerZeiger,
            HilfsZeiger : TFeldZgr;

      BEGIN
         IF AnzahlFelder > 0 THEN BEGIN
            HilfsZeiger := FeldListenAnfang^.Vorgaenger;
            NEW(NeuerZeiger);
            NeuerZeiger^.NachFolger:=FeldListenAnfang;
            NeuerZeiger^.Vorgaenger:=HilfsZeiger;
            HilfsZeiger^.NachFolger:=NeuerZeiger;
            FeldListenAnfang^.Vorgaenger:=NeuerZeiger;
         END
         ELSE BEGIN
            NeuerZeiger:=FeldListenAnfang;
            FocusField := 1;
            FocusId:=K;
         END;
         INC(AnzahlFelder);
         NeuerZeiger^.FeldKennung := K;
         NeuerZeiger^.Feld := Zeiger;
         NeuerZeiger^.Aligned := FALSE;
         NeuerZeiger^.Feld^.SetData(SEG(DEF),OFS(DEF));
      END;

   PROCEDURE TDialogObj.SetData;
      BEGIN
        RUNERROR(211)
      END;

   PROCEDURE TDialogObj.AddField;
      VAR ItkF,ItkR : STRING;

      BEGIN
         IF MultiObj THEN BEGIN
            IF (Itk<> '') THEN BEGIN
               SplitString(Itk,'_',ItKF,ItKR);
               AddSubField(ItkF,ItkR,K,Zeiger,Def);
            END ELSE BEGIN
               AddNew(K,Zeiger,Def);
            END
         END ELSE BEGIN
            RUNERROR(211);
         END;
      END;

   PROCEDURE TDialogObj.AddSubField;
      VAR Lauf: BYTE;
          LZeiger : TFeldZgr;
      BEGIN
         IF MultiObj THEN BEGIN
            LZeiger := FeldListenAnfang;
            FOR Lauf := 1 TO AnzahlFelder DO BEGIN
               WITH LZeiger^ DO BEGIN
                  IF (FeldKennung = SubFK) AND (Feld^.MultiObj) THEN BEGIN
                     Feld^.AddField(Itk,K,Zeiger,Def);
                     Lauf := AnzahlFelder;
                  END;
               END;
               LZeiger:=LZeiger^.Nachfolger;
            END;
         END ELSE BEGIN
            RUNERROR(211);
         END;
      END;
   PROCEDURE TDialogObj.AddActionKey;
      VAR   Zeiger  : TFeldZgr;
            Lauf    : BYTE;
      VAR   ItF,ITR : STRING;
            OnlyMulti : BOOLEAN;
      VAR   NewKey : TFKey;

      PROCEDURE NewKeyProc;
         VAR NewKeyPtr : TFkeyPtr;
             Klauf,
             Dlauf : TFkeyPtr;
         BEGIN
            IF (MaxAvail > SIZEOF(TFkey)) THEN BEGIN
               NEW(NewKeyPtr);
               NewKeyPtr^:= NewKey;
               NewKeyPtr^.NextKey:=KeyList;
               KeyList := NewKeyPtr;
               KLauf := KeyList;
               WHILE (Klauf <> NIL) DO BEGIN
                  DLauf := Klauf;
                  Klauf := Klauf^.NextKey;
                  IF (Klauf <> NIL) AND (KLauf^.Key =NewKey.Key) THEN BEGIN
                     Dlauf^.NextKey := Klauf^.NextKey;
                     DISPOSE(KLauf);
                     Klauf:= DLauf^.NextKey;
                  END;
               END;
            END;
         END;

      BEGIN
         IF (NOT MultiOBJ) OR (K = '_') THEN BEGIN
            NewKey.Key :=CKey;
            NewKey.Action := Action;
            NewKey.PreCheck := Precheck;
            NewKey.InfoTxt := InfoTxt;
            NewKey.NextKey :=NIL;
            NewKeyProc;
         END ELSE BEGIN
            SplitString(K,'_',ITF,ITR);
            OnlyMulti := (ItF <> '') AND ((ITR <> '') OR (POS('_',K) <> 0));
            Zeiger := FeldListenAnfang;
            FOR Lauf := 1 TO AnzahlFelder DO BEGIN
               WITH Zeiger^ DO BEGIN
                  IF (Feld^.MultiObj) AND ((ITF= '') OR ((FeldKennung = ITF) AND (ITR= ''))) THEN
                     Feld^.AddActionKey('_',CKey,Action,PreCheck,InfoTxt);
                  IF ((ITF = '') OR (FeldKennung = ITF) OR (Feld^.MultiObj)) AND
                     (NOT OnlyMulti) THEN BEGIN
                     Feld^.AddActionKey(ITF,CKey,Action,PreCheck,InfoTxt);
                  END;
                  IF OnlyMulti AND Feld^.MultiObj AND
                     ((ITF = '') OR (FeldKennung = ITF)) THEN BEGIN
                     Feld^.AddActionKey(ITR,CKey,Action,PreCheck,InfoTxt)
                  END;
               END;
               Zeiger:=Zeiger^.Nachfolger;
            END
         END;
      END;

   PROCEDURE TDialogObj.ClearActionKey;
      VAR   Zeiger  : TFeldZgr;
            Lauf    : BYTE;
      VAR   ItF,ITR : STRING;
            OnlyMulti : BOOLEAN;

      PROCEDURE ClearKeyProc;
         VAR HPtr,
             KLauf : TFKeyPtr;
             Ende  : BOOLEAN;

         BEGIN
            Klauf := Keylist;
            HPtr  := Klauf;
            IF (KLauf <> NIL) THEN BEGIN
               IF (KLauf^.Key = CKey) THEN BEGIN
                  KeyList := KLauf^.NextKey;
               END ELSE BEGIN
                 Ende := FALSE;
                 WHILE (KLauf <> NIL) AND (NOT Ende) DO BEGIN
                    HPtr := KLauf;
                    KLauf := KLauf^.NextKey;
                    IF (Klauf <> NIL) AND (KLauf^.Key = CKey) THEN BEGIN
                       HPtr^.NextKey := KLauf^.NextKey;
                       DISPOSE(Klauf);
                       Ende :=TRUE;
                    END;
                 END;
              END;
           END;
        END;

      BEGIN
         IF (NOT MultiObj) OR (K = '_') THEN BEGIN
            ClearKeyProc;
         END ELSE BEGIN
            SplitString(K,'_',ITF,ITR);
            OnlyMulti := (ItF <> '') AND ((ITR <> '') OR (POS('_',K) <> 0));
            Zeiger := FeldListenAnfang;
            FOR Lauf := 1 TO AnzahlFelder DO BEGIN
               WITH Zeiger^ DO BEGIN
                  IF (Feld^.MultiObj) AND ((ITF= '') OR ((FeldKennung = ITF) AND (ITR= ''))) THEN
                     Feld^.ClearActionKey('_',CKey);
                  IF ((ITF = '') OR (FeldKennung = ITF) OR (Feld^.MultiObj)) AND
                     (NOT OnlyMulti) THEN BEGIN
                     Feld^.ClearActionKey(ITF,CKey);
                  END;
                  IF OnlyMulti AND Feld^.MultiObj AND
                     ((ITF = '') OR (FeldKennung = ITF)) THEN BEGIN
                     Feld^.ClearActionKey(ITR,CKey)
                  END;
               END;
               Zeiger:=Zeiger^.Nachfolger;
            END
         END;
      END;

   PROCEDURE TDialogObj.SetToggle;
      BEGIN
         Toggle:= Val;
      END;

   PROCEDURE TDialogObj.SetFocusField;
      BEGIN
         IF (FPos <= AnzahlFelder) THEN
            FocusField :=FPos
         ELSE
            FocusField := AnzahlFelder;
      END;

   FUNCTION TDialogObj.GetFocusField;
      BEGIN
         GetFocusField := FocusField;
      END;

   PROCEDURE TDialogObj.SetFocusOnId;
      BEGIN
      END;

    PROCEDURE TDialogObj.SetActionFunc;
       VAR   Zeiger  : TFeldZgr;
             Lauf    : BYTE;
       VAR   ItF,ITR : STRING;
             OnlyMulti : BOOLEAN;
       BEGIN
         IF (NOT MultiObj) OR (K = '_') THEN BEGIN
            ActionActive := TRUE;
            ActionFunc :=Func;
         END ELSE BEGIN
            Zeiger := FeldListenAnfang;
            SplitString(K,'_',ITF,ITR);
            OnlyMulti := (ItF <> '') AND ((ITR <> '') OR (POS('_',K) <> 0));
            FOR Lauf := 1 TO AnzahlFelder DO BEGIN
               WITH Zeiger^ DO BEGIN
                  IF (Feld^.MultiObj) AND ((ITF= '') OR ((FeldKennung = ITF) AND (ITR= ''))) THEN
                     Feld^.SetActionFunc('_',Func);
                  IF ((ITF = '') OR (FeldKennung = ITF) OR (Feld^.MultiObj)) AND
                     (NOT OnlyMulti) THEN BEGIN
                     Feld^.SetActionFunc(ITF,Func);
                  END;
                  IF OnlyMulti AND Feld^.MultiObj AND
                     ((ITF = '') OR (FeldKennung = ITF)) THEN BEGIN
                     Feld^.SetActionFunc(ITR,Func)
                  END;
               END;
               Zeiger:=Zeiger^.Nachfolger;
            END
         END;
      END;

    PROCEDURE TDialogObj.SetSEvalFunc;
       VAR   Zeiger  : TFeldZgr;
             Lauf    : BYTE;
       VAR   ItF,ITR : STRING;
             OnlyMulti : BOOLEAN;
       BEGIN
         IF (NOT MultiObj) OR (K = '_') THEN BEGIN
            SpecialEvalActive := TRUE;
            SpecialEvalFunc :=Func;
         END ELSE BEGIN
            Zeiger := FeldListenAnfang;
            SplitString(K,'_',ITF,ITR);
            OnlyMulti := (ItF <> '') AND ((ITR <> '') OR (POS('_',K) <> 0));
            FOR Lauf := 1 TO AnzahlFelder DO BEGIN
               WITH Zeiger^ DO BEGIN
                  IF ((ITF = '') OR (FeldKennung = ITF) OR (Feld^.MultiObj)) AND
                     (NOT OnlyMulti) THEN BEGIN
                     Feld^.SetSEvalFunc(ITF,Func);
                  END;
                  IF OnlyMulti AND Feld^.MultiObj AND
                     ((ITF = '') OR (FeldKennung = ITF)) THEN BEGIN
                     Feld^.SetSEvalFunc(ITR,Func)
                  END;
               END;
               Zeiger:=Zeiger^.Nachfolger;
            END
         END;
       END;

   PROCEDURE TDialogObj.SetDefault;
      VAR   Zeiger  : TFeldZgr;
            Lauf    : BYTE;
      VAR   ItF,ITR : STRING;
            OnlyMulti : BOOLEAN;

      BEGIN
         IF (NOT MultiObj) OR (K = '_') THEN BEGIN
            Default := TRUE;
         END ELSE BEGIN
            Zeiger := FeldListenAnfang;
            SplitString(K,'_',ITF,ITR);
            OnlyMulti := (ItF <> '') AND ((ITR <> '') OR (POS('_',K) <> 0));
            FOR Lauf := 1 TO AnzahlFelder DO BEGIN
               WITH Zeiger^ DO BEGIN
                  IF (Feld^.MultiObj) AND ((ITF= '') OR ((FeldKennung = ITF) AND (ITR= ''))) THEN
                     Feld^.SetDefault('_');
                  IF ((ITF = '') OR (FeldKennung = ITF) OR (Feld^.MultiObj)) AND
                     (NOT OnlyMulti) THEN BEGIN
                     Feld^.SetDefault(ITF);
                  END;
                  IF OnlyMulti AND Feld^.MultiObj AND
                     ((ITF = '') OR (FeldKennung = ITF)) THEN BEGIN
                     Feld^.SetDefault(ITR)
                  END;
               END;
               Zeiger:=Zeiger^.Nachfolger;
            END
         END;
      END;

   PROCEDURE TDialogObj.ClearDefault;
      VAR   Zeiger  : TFeldZgr;
            Lauf    : BYTE;
      VAR   ItF,ITR : STRING;
            OnlyMulti : BOOLEAN;

      BEGIN
         IF (NOT MultiObj) OR (K = '_') THEN BEGIN
            Default := FALSE;
         END ELSE BEGIN
            Zeiger := FeldListenAnfang;
            SplitString(K,'_',ITF,ITR);
            OnlyMulti := (ItF <> '') AND ((ITR <> '') OR (POS('_',K) <> 0));
            FOR Lauf := 1 TO AnzahlFelder DO BEGIN
               WITH Zeiger^ DO BEGIN
                  IF (Feld^.MultiObj) AND ((ITF= '') OR ((FeldKennung = ITF) AND (ITR= ''))) THEN
                     Feld^.ClearDefault('_');
                  IF ((ITF = '') OR (FeldKennung = ITF) OR (Feld^.MultiObj)) AND
                     (NOT OnlyMulti) THEN BEGIN
                     Feld^.ClearDefault(ITF);
                  END;
                  IF OnlyMulti AND Feld^.MultiObj AND
                     ((ITF = '') OR (FeldKennung = ITF)) THEN BEGIN
                     Feld^.ClearDefault(ITR)
                  END;
               END;
               Zeiger:=Zeiger^.Nachfolger;
            END
         END;
      END;

   PROCEDURE TDialogObj.DisposeKeyInfoList;
      VAR LPtr,
          DPtr : TFKeyInfoPtr;
      BEGIN
         LPtr :=FirstKey;
         WHILE (LPtr <> NIL) DO BEGIN
            DPtr :=LPtr;
            LPtr:= LPtr^.Next;
            DISPOSE(DPtr);
         END;
         FirstKey:= NIL;
      END;

   PROCEDURE TDialogObj.StripKeyInfoList;
      VAR LPtr,
          DPtr : TFKeyInfoPtr;
      BEGIN
         IF (FirstKey <> NIL) THEN BEGIN
            IF (FirstKey^.Info ='') THEN BEGIN
               REPEAT
                  LPtr := FirstKey^.Next;
                  DISPOSE(FirstKey);
                  FirstKey := Lptr;
               UNTIL (FirstKey = NIL) OR (FirstKey^.Info <> '');
            END;
            IF (FirstKey <> NIL) THEN BEGIN
               DPtr := FirstKey;
               LPtr := FirstKey^.Next;
               WHILE (LPtr <> NIL) DO BEGIN
                  IF (LPtr^.Info = '') THEN BEGIN
                     DPtr^.Next:= LPtr^.Next;
                     DISPOSE(LPtr);
                     LPtr:= DPtr^.Next;
                  END ELSE BEGIN
                     DPtr := LPtr;
                     LPtr := LPtr^.Next;
                  END;
               END;
            END;
         END;
      END;

   FUNCTION TDialogObj.GetKeyInfoList;
      VAR   Zeiger  : TFeldZgr;
            Lauf    : BYTE;
      VAR   ItF,ITR : STRING;
            Ende : BOOLEAN;
            FirstKey     : TFKeyInfoPtr;
            NewKey       : TFKeyInfo;
            KLauf,
            HKInfoList   : TFKeyInfoPtr;
            FKLauf : TFKeyPtr;

      PROCEDURE ADDKeyInfo(NKey:TFkeyInfo;VAR IListe:TFKeyInfoPtr);
         VAR NKPtr,
             LPTr,
             OLPtr: TFKeyInfoPtr;
         BEGIN
            IF (MaxAvail > SIZEOF(TFKeyInfo)) THEN BEGIN
               NEW(NKPtr);
               NKPtr^ := NKey;
               IF (IListe = NIL) THEN BEGIN
                  IListe := NKPtr;
               END ELSE BEGIN
                  IF (NKPTr^.Key < IListe^.Key) THEN BEGIN
                     NKPtr^.Next := Iliste;
                     IListe := NkPtr;
                  END ELSE BEGIN
                     LPtr := IListe;
                     OLPtr := IListe;
                     WHILE (Lptr <> NIL) AND (LPtr^.Key < NKPtr^.Key) DO BEGIN
                        OLPtr := Lptr;
                        LPtr := LPtr^.Next;
                     END;
                     IF (LPtr = NIL) OR (LPtr^.Key > NkPtr^.Key) THEN BEGIN
                        OLPtr^.Next := NKPtr;
                        NKPtr^.Next := LPtr;
                     END ELSE
                        DISPOSE(NkPtr);
                  END;
               END;
            END;
         END;


      BEGIN
         IF (NOT MultiObj) OR (ItK = '') THEN BEGIN
            FirstKey := NIL;
            FKLauf := KeyList;
            WHILE (FKLAuf <> NIL) DO BEGIN
               NewKey.Key := FKlauf^.Key;
               NewKey.Info := Fklauf^.InfoTxt;
               NewKey.Next := NIL;
               AddKeyInfo(NewKey,FirstKey);
               FKLauf := FKLauf^.NextKey;
            END;
         END ELSE BEGIN
            Zeiger := FeldListenAnfang;
            SplitString(Itk,'_',ITF,ITR);
            Lauf := 1;
            FirstKey := NIL;
            Ende := FALSE;
            WHILE (Lauf <= AnzahlFelder) AND (NOT Ende) DO BEGIN
               WITH Zeiger^ DO BEGIN
                  IF (ITR <> '') THEN BEGIN
                     IF (Feld^.MultiObj AND (FeldKennung = ITF)) THEN BEGIN
                        FirstKey:= Feld^.GetKeyInfoList(ITR);
                        HKInfoList := GetKeyInfoList('');
                        KLauf := HKInfoList;
                        WHILE (KLauf <> NIL) DO BEGIN
                           NewKey:= KLauf^;
                           NewKey.Next := NIL;
                           AddKeyInfo(NewKey,FirstKey);
                           KLauf := Klauf^.Next;
                        END;
                        DisposeKeyInfoList(HKInfoList);
                        Ende := TRUE;
                     END
                  END ELSE BEGIN
                      IF (Feldkennung = ITF) THEN BEGIN
                         FirstKey:= Feld^.GetKeyInfoList(ITR);
                         HKInfoList := GetKeyInfoList('');
                         KLauf := HKInfoList;
                         WHILE (KLauf <> NIL) DO BEGIN
                            NewKey:= KLauf^;
                            NewKey.Next := NIL;
                            AddKeyInfo(NewKey,FirstKey);
                            KLauf := Klauf^.Next;
                         END;
                         DisposeKeyInfoList(HKInfoList);
                         Ende := TRUE;
                      END;

                  END;
               END;
               Zeiger:=Zeiger^.Nachfolger;
               INC(Lauf);
            END
         END;
         GetKeyInfoList := FirstKey;
      END;


    FUNCTION TDialogObj.IsDefault;
        BEGIN
           IsDefault := DEFAULT;
        END;

    PROCEDURE TDialogObj.AddItem;
        BEGIN
          RUNERROR(211);
        END;

    FUNCTION TDialogObj.ItemDisabled;
        BEGIN
           ItemDisabled:= FALSE;
        END;

    PROCEDURE TDialogObj.DisableItem;
       BEGIN
          RUNERROR(211);
       END;

    PROCEDURE TDialogObj.EnableItem;
       BEGIN
          RUNERROR(211);
       END;

    PROCEDURE TDialogObj.Enable;
       BEGIN
       END;

    PROCEDURE TDialogObj.Disable;
       BEGIN
       END;

    FUNCTION TDialogObj.Disabled;
       BEGIN
          Disabled :=FALSE;
       END;

    PROCEDURE TDialogObj.Show;
       BEGIN
       END;

    FUNCTION TDialogObj.Changed;
       BEGIN
          Changed := FALSE;
       END;

    PROCEDURE TDialogObj.Save;
       BEGIN
       END;

    PROCEDURE TDialogObj.Restore;
       BEGIN
       END;

    PROCEDURE TDialogObj.Refresh;
       BEGIN
          IF Changed(K) THEN
             Show(K);
       END;

    FUNCTION TDialogObj.Input;
       BEGIN
          RUNERROR(211);
       END;
    FUNCTION TDialogObj.Left;
       BEGIN
          RUNERROR(211);
       END;
    FUNCTION TDialogObj.Right;
       BEGIN
          RUNERROR(211);
       END;
    FUNCTION TDialogObj.Top;
       BEGIN
          RUNERROR(211);
       END;

    FUNCTION TDialogObj.Bottom;
       BEGIN
          RUNERROR(211);
       END;

    FUNCTION TDialogObj.GetFocusId;
       BEGIN
          GetFocusId:= FocusId;
       END;


   FUNCTION TDialogObj.Valid;
       BEGIN
          Valid:=TRUE;
       END;

   FUNCTION TDialogObj.GetVal;
       BEGIN
          GetVal := '';
       END;

   PROCEDURE TDialogObj.SetVal;
       BEGIN
          RUNERROR(211);
       END;

   FUNCTION TDialogObj.KeyAction;
      VAR KLauf: TFKeyPtr;
          Ende : BOOLEAN;
          ValStrSave: STRING;
          Ergebnis : BOOLEAN;
          DOFunc : BOOLEAN;
      BEGIN
         ActionDone := FALSE;
         Ende := FALSE;
         KLauf := KeyList;
         WHILE (Klauf <> NIL) AND (NOT Ende) DO BEGIN
           IF (KLauf^.Key = Taste) THEN BEGIN
              DOFunc :=TRUE;
              IF KLauf^.PreCheck THEN BEGIN
                 ValStrSave:=GetVal('');
                 SetVal('',InpString);
                 DOFunc := Valid('');
                 IF DoFunc THEN BEGIN
                    InpString := GetVal('');
                 END ELSE BEGIN
                    SetVal('',ValStrSave);
                 END;
              END;
              IF DoFunc THEN BEGIN
                Ergebnis := Klauf^.Action(Taste,Nr,InpString,Break,Check,MoreEval);
                ActionDone := TRUE;
              END;
              Ende := TRUE;
           END;
           KLauf := Klauf^.NextKey;
         END;
         IF NOT ActionDone THEN BEGIN
            Ergebnis := FALSE;
            Break := FALSE;
         END;
         KeyAction := Ergebnis;
      END;

    FUNCTION TDialogObj.GetInputStartPos;
      BEGIN
         GetInputStartPos := 0;
      END;

    PROCEDURE TDialogObj.SetInputStartPos;
      BEGIN
         RUNERROR(211);
      END;

    PROCEDURE TDialogObj.SetCrQuit;
      BEGIN
         RUNERROR(211);
      END;

    PROCEDURE TDialogObj.SetFKeyQuit;
      BEGIN
         RUNERROR(211);
      END;

    FUNCTION TDialogObj.KeyInput;
      VAR Ende : BOOLEAN;
          EndCode :INTEGER;
          HFun :TActionFunc;
          L:WORD;
      BEGIN
         EndCode := 0;
         Ende := FALSE;
         L:=0;
         REPEAT
            INC(L);
            IF KeyPress(EndCode) THEN
               Ende := TRUE;
            IF (L=10000) THEN BEGIN
              L:=0;
              IF ActionActive THEN
                 ActionFunc;
            END;
         UNTIL Ende;
         KeyInput:=EndCode;
      END;

   DESTRUCTOR TDialogObj.Release;
      VAR KLauf,
          Hptr   :TFkeyPtr;
      BEGIN
         KLauf := KeyList;
         WHILE (Klauf <> NIL) DO BEGIN
            HPtr := KLauf;
            KLauf := Klauf^.NextKey;
            DISPOSE(HPtr);
         END;
         KeyList := NIL
      END;

   CONSTRUCTOR TBottomLine.Init;
      VAR Lauf : BYTE;

      BEGIN
         inherited Init;
         EraseAll;
         Hidden:=TRUE;
         FOR Lauf := 1 TO 6 DO
            ButtonChanged[Lauf] :=TRUE;
      END;

   PROCEDURE TBottomline.SetAll;
      VAR Lauf : BYTE;

      BEGIN
         FOR LAUF := 1 TO 6 DO
            SetButton(Lauf,Inhalt[Lauf]);
      END;


   PROCEDURE TBottomLine.SetButton;

      BEGIN
         IF (Inhalt.Z1 <> FTasten[Nr].Z1) OR
            (Inhalt.Z2 <> FTasten[Nr].Z2) THEN
            ButtonChanged[Nr] := TRUE;
         IF (Inhalt.Z1 = '') AND (Inhalt.Z2 = '') THEN
            ButtonErased[Nr] := TRUE
         ELSE
            ButtonErased[Nr] := FALSE;
         FTasten[Nr]:=Inhalt;
      END;

   PROCEDURE TBottomLine.SetButtonAct;

      BEGIN
         FKeyAction[Nr]:=Action;
      END;

   PROCEDURE TBottomLine.EraseButtonAct;
      CONST Leer : TFKey = (Key:0;Action:DoNothing;PreCheck:FALSE;InfoTxt:'';NextKey:NIL);
      BEGIN
         FKeyAction[Nr]:=Leer;
      END;

   PROCEDURE TBottomLine.EraseAll;
      VAR Lauf : BYTE;

      BEGIN
         FOR Lauf := 1 TO 6 DO
            EraseButton(Lauf);
      END;

   PROCEDURE TBottomLine.EraseButton;
      CONST Leer  : TFKeyDisp = (Z1 : '';Z2 : '');

      BEGIN
         SetButton(Nr,Leer);
      END;

   FUNCTION TBottomLine.KeyAction;
      VAR Lauf : BYTE;
          Ergebnis : BOOLEAN;
      BEGIN
         ActionDone := FALSE;
         FOR Lauf := 1 TO 6 DO
           IF (FKeyAction[Lauf].Key = Taste) AND NOT ActionDone THEN BEGIN
              Ergebnis := FKeyAction[Lauf].Action(Taste,Nr,InpString,Break,Check,MoreEval);
              ActionDone := TRUE;
           END;
         IF NOT ActionDone THEN BEGIN
            Ergebnis := FALSE;
            Break := FALSE;
         END;
         KeyAction := Ergebnis;
      END;

   FUNCTION ButtonQuit;
      BEGIN
         MoreEval := FALSE;
         Break:= FALSE;
         Check := TRUE;
         ButtonQuit := TRUE;
      END;

   FUNCTION ButtonMoreEval;
      BEGIN
         MoreEval := TRUE;
         Break:= FALSE;
         Check := TRUE;
         ButtonMoreEval := TRUE;
      END;

   FUNCTION ButtonQuit0;
      BEGIN
         MoreEval := FALSE;
         Break:= FALSE;
         Check := TRUE;
         Taste := 0;
         ButtonQuit0 := TRUE;
      END;

   FUNCTION DONothing;
      BEGIN
         MoreEval := FALSE;
         Break:= FALSE;
         Check := TRUE;
         DoNothing := FALSE;
      END;

   FUNCTION ButtonBreak;
      BEGIN
         MoreEval := FALSE;
         Check := FALSE;
         Break:= TRUE;
         ButtonBreak := TRUE;
      END;

   FUNCTION ButtonBreak0;
      BEGIN
         MoreEval := FALSE;
         Break:= TRUE;
         Check := TRUE;
         Taste := 0;
         ButtonBreak0 := TRUE;
      END;

   PROCEDURE TBottomLine.Hide;

      BEGIN
         clear_buttons;
         Hidden:=TRUE;
      END;

   FUNCTION TBottomLine.Changed;
      VAR Lauf : BYTE;
          Ergebnis : BOOLEAN;
      BEGIN
         Ergebnis := FALSE;
         FOR Lauf := 1 TO 6 DO
            Ergebnis := Ergebnis OR ButtonChanged[Lauf];
         Changed := Ergebnis;
      END;

   PROCEDURE TBottomLine.Show;

      VAR Xa,Xe,         { X-Anfang u. X-Ende Button               }
          Tx,Ty:WORD;    { Text-X u. Text-Y Anfang                 }
          Nr,            { "Nr" ist die Nummer des Buttons         }
          BZ:   BYTE;    { "BZ" ist die Nummer der Zeile im Button }
          Linkslassen,
          rechtslassen :BOOLEAN;
      BEGIN
         IF Hidden THEN
           FOR  Nr := 1 TO 6 DO
              ButtonChanged[Nr] := TRUE;
         Hidden := FALSE;
         FOR Nr:=1 TO 6 DO BEGIN                    { Sechs Buttons }
            IF ButtonErased[Nr] THEN BEGIN
               Linkslassen := (Nr > 1) AND (Not ButtonErased[Nr-1]);
               Rechtslassen := (Nr < 5) AND (Not ButtonErased[Nr+1]);
               erase_button(Nr,linkslassen,rechtslassen);
            END ELSE BEGIN                           { Button       }
               IF ButtonChanged[Nr] THEN BEGIN
                  ButtonChanged[Nr] :=FALSE;
                  set_button(Nr,FTasten[Nr].Z1,FTasten[Nr].Z2);
               END
            END       { Ende Button vorhanden                       }
         END;        { Ende FOR                                    }
      END;            { Ende der Procedure                          }

   DESTRUCTOR TBottomLine.Release;
      BEGIN
         EraseAll;
         Hide;
         inherited Release;
      END;

END.
{============================
 Versionshistorie
 $Log:$
 ============================}
