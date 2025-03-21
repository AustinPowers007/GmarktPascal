{============================
 Dateiname:   $Source: $
 Version:     $Revision: $
 Autor:       $Author: $
 Gelockt von: $Locker: $
 Originaldatum : Fri Oct 11 15:41:36 GMT+02:00 1996
 Dateihistorie am Ende der Datei
 ============================}
                               {�����������ͻ
   ����������������������������͹ Bemerkung �������������������������������ͻ
   �                            �����������ͼ                               �
   � Aufgabe:     Stellt Objekt f�r Farbverwaltung bez�glich Eingabemodi    �
   ������������������������������������������������������������������������ͼ}

UNIT FARBEN;
{$O+}

INTERFACE

   USES CASHDATA;

                               {�����������ͻ
   ����������������������������͹ Bemerkung �������������������������������ͻ
   �                            �����������ͼ                               �
   � Aufgabe:     Objekt mit Farbkombination eines Fenster, ben�tigt in     �
   �              CASHNEU                                                   �
   ������������������������������������������������������������������������ͼ}


   TYPE TFarben = OBJECT
           FeldUm,                   { Farbe der Umrandung des Feldes   }
           FeldHG,                   { Farbe des Feldhintergrunds       }
           InhaltVG,                 { Farbe des Feldinhalts            }
           InhaltHG,                 { Farbe des Hintergrund Feldinhalt }
           TitelVG,                  { Farbe des Feldtitels             }
           TitelHG,                  { Farbe des Hintergrund Feldtitel  }
           CursorVG,                 { Farbe des Cursors Vordergrund    }
           CursorHG,                 { Farbe des Cursors Hintergrund    }
           CursorInhVG : TModiFarben;{ Farbe des Inhalts Cursorfeld     }

           PROCEDURE SetLayout(UMp,        { Farben der Umrandung           }
                               HGp:STRING);{   "    des Hintergr.   im Feld }

           PROCEDURE SetText  (IVp,        { Farben des Feldinhaltes        }
                               IHp:STRING);{   "    Hintergrunds Feldinhalt }

           PROCEDURE SetCursor(CVp,        { Farben des Cursors Vordergund  }
                               CHp,        {   "     "  Cursors Hintergrund }
                               CTp:STRING);{   "     "  Cursorinh. Vordergr.}

           PROCEDURE SetTitel (TVp,          { Farben des Titels Vordergrund}
                               THp:STRING);  {   "     "    "    Hintergrund}

        END;

IMPLEMENTATION

                               {�����������ͻ
   ����������������������������͹ Bemerkung �������������������������������ͻ
   �                            �����������ͼ                               �
   � Aufgabe:     LayoutFarben f�r Feldumrandung und Feldhintergrund setzen �
   �                                                                        �
   � Parameter:   Keine                                                     �
   �                                                                        �
   � Resultate:   Keine                                                     �
   ������������������������������������������������������������������������ͼ}

   PROCEDURE TFarben.SetLayout;
      VAR Lauf,C: INTEGER;
          Modus : TModi;

      BEGIN
         Lauf:=1;
         FOR Modus:=i TO de DO BEGIN
            VAL(COPY(UMp,Lauf,2),FeldUM[Modus],C);
            VAL(COPY(HGp,Lauf,2),FeldHG[Modus],C);
            INC(Lauf,3)
         END;
      END;

                               {�����������ͻ
   ����������������������������͹ Bemerkung �������������������������������ͻ
   �                            �����������ͼ                               �
   � Aufgabe:     Inhaltsfarben setzen                                      �
   �                                                                        �
   � Parameter:   Keine                                                     �
   �                                                                        �
   � Resultate:   Keine                                                     �
   ������������������������������������������������������������������������ͼ}

   PROCEDURE TFarben.SetText;
      VAR Lauf,C: INTEGER;
          Modus : TModi;

      BEGIN
         Lauf:=1;
         FOR Modus:=i TO de DO BEGIN
            VAL(COPY(IVp,Lauf,2),InhaltVG[Modus],C);
            VAL(COPY(IHp,Lauf,2),InhaltHG[Modus],C);
            INC(Lauf,3)
         END
      END;

                               {�����������ͻ
   ����������������������������͹ Bemerkung �������������������������������ͻ
   �                            �����������ͼ                               �
   � Aufgabe:     Cursorfarben setzen                                       �
   �                                                                        �
   � Parameter:   Keine                                                     �
   �                                                                        �
   � Resultate:   Keine                                                     �
   ������������������������������������������������������������������������ͼ}

   PROCEDURE TFarben.SetCursor;
      VAR Lauf,C: INTEGER;
          Modus : TModi;

      BEGIN
         Lauf:=1;
         FOR Modus:=i TO de DO BEGIN
            VAL(COPY(CVp,Lauf,2),CursorVG[Modus],C);
            VAL(COPY(CHp,Lauf,2),CursorHG[Modus],C);
            VAL(COPY(CTp,Lauf,2),CursorInhVG[Modus],C);
            INC(Lauf,3)
         END
      END;

                               {�����������ͻ
   ����������������������������͹ Bemerkung �������������������������������ͻ
   �                            �����������ͼ                               �
   � Aufgabe:     Titelfarben setzen                                        �
   �                                                                        �
   � Parameter:   Keine                                                     �
   �                                                                        �
   � Resultate:   Keine                                                     �
   ������������������������������������������������������������������������ͼ}

   PROCEDURE TFarben.SetTitel;
      VAR Lauf,C: INTEGER;
          Modus : TModi;

      BEGIN
         Lauf:=1;
         FOR Modus:=i TO de DO BEGIN
            VAL(COPY(TVp,Lauf,2),TitelVG[Modus],C);
            VAL(COPY(THp,Lauf,2),TitelHG[Modus],C);
            INC(Lauf,3)
         END
      END;

END.
{============================
 Versionshistorie
 $Log:$
 ============================}
