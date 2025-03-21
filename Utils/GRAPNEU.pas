{============================
 Dateiname:   $Source: $
 Version:     $Revision: $
 Autor:       $Author: $
 Gelockt von: $Locker: $
 Originaldatum : Fri Jan 07 18:04:12 GMT+01:00 2000
 Dateihistorie am Ende der Datei
 ============================}
UNIT GRAPNEU;
{$O+}

INTERFACE

   USES  GRAPH;

   PROCEDURE OPENGRAPH;
   PROCEDURE SetPaletteAll;
   PROCEDURE SetPaletteWork;
   PROCEDURE SetPaletteTape;
   PROCEDURE SetPaletteInaktiv;
   PROCEDURE ResetPalette;
   PROCEDURE Switch;

   PROCEDURE DrawWin(Links,Oben,Rechts,Unten:WORD;FensterFarbe,
               TitelFarbe,TitelText,Schattenfarbe:BYTE;MitSchatten:BOOLEAN;
               FensterTitel:STRING);
   PROCEDURE DrawLine(X1,Y1,X2,Y2:WORD;ActColor:BYTE);
   PROCEDURE DrawCrossPoint(X1,Y1,ZHeight:WORD;Typ,ActColor:BYTE);
   PROCEDURE DisplayText(X,Y,HGH,HGV,HGFarbe,TxFarbe:WORD;Txt:STRING);
   PROCEDURE BackExtra(ISpalte,IZeile,Laenge:WORD;RandFarbe,HGFarbe:BYTE);

   PROCEDURE SetFStyle(FillType:BYTE);
   PROCEDURE ClearWin(Links,Oben,Rechts,Unten:WORD;MitSchatten:BOOLEAN);
   PROCEDURE set_button(Nr:WORD;txt1,txt2:STRING);
   PROCEDURE erase_button(Nr:WORD;left_set,right_set:BOOLEAN);
   PROCEDURE clear_buttons;

   VAR  StdPalette,
        Palette: PaletteType;

IMPLEMENTATION

   USES  GMCRT;


   PROCEDURE EGAVGADriver;external;
   {$L VGADRV.OBJ}

   PROCEDURE SWITCH;

      BEGIN
         GETPALETTE(Palette);
         SETPALETTE(8,Palette.colors[12]);
         SETPALETTE(12,Palette.colors[8]);
      END;

      { Pallette gesamt setzten }

   PROCEDURE SetPaletteAll;

      BEGIN
         SETALLPALETTE(StdPalette);
         SETPALETTE(0,StdPalette.colors[Black]);
         SETPALETTE(1,StdPalette.colors[LightBlue]);
         SETPALETTE(2,StdPalette.colors[Blue]);
         SETPALETTE(3,StdPalette.colors[Red]);
         SETPALETTE(4,StdPalette.colors[Yellow]);
         SETPALETTE(5,StdPalette.colors[LightGray]);
         SETPALETTE(6,StdPalette.colors[DarkGray]);
         SETPALETTE(7,StdPalette.colors[White])
      END;

   PROCEDURE ResetPalette;

      BEGIN
         SETALLPALETTE(StdPalette);
      END;

   PROCEDURE SetPaletteWork;

      BEGIN
         GETPALETTE(Palette);
         SETPALETTE( 8,Palette.colors[7]);
         SETPALETTE( 9,Palette.colors[2]);
         SETPALETTE(10,Palette.colors[7]);
         SETPALETTE(11,Palette.colors[5]);
         SETPALETTE(12,Palette.colors[6]);
         SETPALETTE(13,Palette.colors[5]);
         SETPALETTE(14,Palette.colors[5]);
         SETPALETTE(15,Palette.colors[6])
      END;

   PROCEDURE SetPaletteTape;

      BEGIN
         GETPALETTE(Palette);
         SETPALETTE( 8,PALETTE.colors[6]);
         SETPALETTE( 9,PALETTE.colors[5]);
         SETPALETTE(10,PALETTE.colors[5]);
         SETPALETTE(11,PALETTE.colors[6]);
         SETPALETTE(12,PALETTE.colors[7]);
         SETPALETTE(13,PALETTE.colors[2]);
         SETPALETTE(14,PALETTE.colors[7]);
         SETPALETTE(15,PALETTE.colors[5])
      END;

   PROCEDURE SetPaletteInaktiv;

      BEGIN
         GETPALETTE(PALETTE);
         SETPALETTE( 8,PALETTE.colors[6]);
         SETPALETTE( 9,PALETTE.colors[5]);
         SETPALETTE(10,PALETTE.colors[5]);
         SETPALETTE(11,PALETTE.colors[6]);
         SETPALETTE(12,PALETTE.colors[6]);
         SETPALETTE(13,PALETTE.colors[5]);
         SETPALETTE(14,PALETTE.colors[5]);
         SETPALETTE(15,PALETTE.colors[6])
      END;

      { Allgemeines }

   PROCEDURE OPENGRAPH;
      VAR GraphMode,GraphDriver,X:INTEGER;

      BEGIN
         X:= RegisterBGIDriver(@EgaVgaDriver);
         GraphMode:=2;
         GraphDriver:=DETECT;
         INITGRAPH(GraphDriver, GraphMode,'');
         GetPalette(StdPalette);
      END;

  PROCEDURE set_button_back(c2,c1:BYTE;x1,y1,x2:WORD);

      BEGIN
         SETCOLOR       (0);
         SETFSTYLE(c2);
         PIESLICE(x1+ 5,y1+ 25,180,270,8); PIESLICE(x2- 1,y1+  5,  0, 90,8);
         PIESLICE(x1+ 5,y1+  5, 90,180,8); PIESLICE(x2- 1,y1+ 25,270,360,8);
         BAR(x1+5,y1-3,x2-1,y1+33);        BAR(x1-3,y1+5,x2+7,y1+25);
         LINE(x1+5,y1-3,x2-1,y1-3);        LINE(x1+5,y1+33,x2-1,y1+33);
         LINE(x1-3,y1+3,x1-3,y1+25);       LINE(x2+7,y1+3,x2+7,y1+25);
         SETFSTYLE(c1);
         PIESLICE(x1+ 8,y1+18,180,270, 8); PIESLICE(x2- 8,y1+ 8,  0, 90, 8);
         PIESLICE(x1+ 8,y1+ 8, 90,180, 8); PIESLICE(x2- 8,y1+18,270,360, 8);
         BAR(x1+8,y1,x2-8,y1+26);          BAR(x1,y1+8,x2,y1+18);
         LINE(x1+8,y1,x2-8,y1);            LINE(x1+8,y1+26,x2-8,y1+26);
         LINE(x1,y1+8,x1,y1+18);           LINE(x2,y1+8,x2,y1+18)
      END;

   FUNCTION  CalculateXA(Nr:WORD):WORD;
      BEGIN
            CalculateXa:= Nr * 100 - 86 + (Nr DIV 6) * 24;
      END;

   FUNCTION  CalculateXE(Nr:WORD):WORD;
      VAR XA : WORD;
      BEGIN
            XA:= Nr * 100 - 86 + (Nr DIV 6) * 24;
            CalculateXe:= Xa +  90;
      END;



  PROCEDURE set_button;
      VAR Xa,XE : WORD;
          Tx,TY : WORD;
          Bz : BYTE;
      BEGIN
         XA := CalculateXA(Nr);
         XE := CalculateXE(Nr);
         set_button_back(5,7,Xa,440,Xe);
                  { Ausgabe der beiden Textzeilen in den Button "NB" }

         SETCOLOR(0);
         Tx := Xa+(Xe-Xa-LENGTH(Txt1)*8+1) DIV 2+1;
         Ty := 445;
         OUTTEXTXY(Tx,Ty,Txt1);
         Tx := Xa+(Xe-Xa-LENGTH(Txt2)*8+1) DIV 2+1;
         Ty := 455;
         OUTTEXTXY(Tx,Ty,Txt2)
      END;

  PROCEDURE erase_button;
      Var Linkslassen,
          Rechtslassen : WORD;
      BEGIN
         SETFSTYLE(1);
         Linkslassen := 0;
         Rechtslassen  := 0;
         IF left_set THEN
             Linkslassen := 1;
         IF right_set THEN
            Rechtslassen := 1;
         BAR(CalculateXa(Nr)-3+Linkslassen,435,
             CalculateXe(Nr)+7-Rechtslassen,474)
      END;


   PROCEDURE clear_buttons;
      BEGIN
         SETFSTYLE(1);
         BAR(0,435,639,474);
      END;

   PROCEDURE DrawWin;
         VAR   FillInfo : FILLSETTINGSTYPE;
               CurColor : BYTE;
               di  : INTEGER;
               xa,xe   : WORD;
         VAR   VPlus : BYTE;
         BEGIN
            IF (FensterTitel <> '') THEN
               VPlus := 8
            ELSE
               VPlus := 0;
            GetFillSettings(FillInfo);
            CurColor := GetColor;
            { Schatten }
            IF MitSchatten THEN BEGIN
               SETCOLOR      (Schattenfarbe);
               SETFSTYLE(Schattenfarbe);
               PIESLICE(Links+32,Unten+1,180,270,9);
               PIESLICE(Rechts+1,Oben+32+Vplus, 0, 90,9);
               PIESLICE(Rechts-5,Unten-5,270,360,15);
               BAR(Links+33,Unten-15,Rechts- 5,Unten+10);
               BAR(Rechts- 2,Unten- 5,Rechts+10,Oben+32+VPlus);
            END;
           { Fenster }
            SETCOLOR(0);
            SETFSTYLE(Fensterfarbe);
            PIESLICE(Links+15,Unten-15,180,270,15); LINE(Links+15,Unten,Rechts-15,Unten);
            PIESLICE(Rechts-15,Oben+15+Vplus,0,90,15); LINE(Links+15,Oben+Vplus,Rechts-15,Oben+ Vplus);
            PIESLICE(Links+15,Oben+15+Vplus,90,180,15); LINE(Links,Oben+15+Vplus,Links,Unten-15);
            PIESLICE(Rechts-15,Unten-15,270,360,15);LINE(Rechts,Oben+15+Vplus,Rechts,Unten-15);
            BAR(Links+1,Oben+15+Vplus,Rechts-1,Unten-15);
            BAR(Links+15,Oben+1+Vplus,Rechts-15,Oben+15+Vplus);
            BAR(Links+15,Unten-1,Rechts-15,Unten-15);

           { Titel }
            IF (FensterTitel <> '') THEN BEGIN
               di:=Rechts-Links;
               di := di-(LENGTH(Fenstertitel)*8)-29;
               di := di DIV 2;
               xe:=Links+di;
               xa:=Rechts-di;
               SETFSTYLE(Titelfarbe);
               PIESLICE(xe+8,Oben+8,180,270,8);LINE(xe+8,Oben,xa-8,Oben);
               PIESLICE(xa-8,Oben+8,0,90,8);LINE(xe+8,Oben+16,xa-8,Oben+16);
               PIESLICE(xe+8,Oben+8,90,180,8);
               PIESLICE(xa-8,Oben+8,270,360,8);BAR(xe+8,Oben+1,xa-8,Oben+15);
               SETCOLOR(Titelfarbe);
               LINE(xe+1,Oben+8,xa-1,Oben+8);
               SETCOLOR(Titeltext); SETTEXTSTYLE(0,0,0);
               OUTTEXTXY(xe+16,Oben+5,Fenstertitel);
               SETCOLOR(CurColor);
               SETFSTYLE(FillInfo.Color)
            END;
         END;

   PROCEDURE SetFStyle;
      BEGIN
         SETFILLStyle(1,FillType);
      END;

   PROCEDURE ClearWin;
      VAR PlusSchatten : BYTE;
      BEGIN
         SETFSTYLE(1);
         IF MitSchatten THEN
            PlusSchatten := 10
         ELSE
            PlusSchatten := 0;
         BAR(Links,Oben,Rechts+PlusSchatten,Unten+PlusSchatten)
      END;

   PROCEDURE DisplayText;
      VAR FillInfo : FILLSETTINGSTYPE;
          CurColor : BYTE;

      BEGIN
         IF (Txt <> '') THEN BEGIN
            GetFillSettings(FillInfo);
            CurColor := GetColor;
            SETFSTYLE(HgFarbe);
            SETCOLOR(TxFarbe);
            BAR(X-HGH,Y-2-HGV,X+HGH+(LENGTH(Txt))*8,Y+HGV+8);
            OUTTEXTXY(X,Y,Txt);
            SETCOLOR(CurColor);
            SETFILLSTYLE(1,FillInfo.Color)
         END;
      END;

   PROCEDURE BackExtra;
        VAR Xe: WORD;

        BEGIN
           Xe:=ISpalte+Laenge;
           SETCOLOR      (Randfarbe);
           SETFILLSTYLE(1,HgFarbe);
             PIESLICE(ISpalte,IZeile+ 3,  180,  270, 8);
             LINE    (ISpalte,IZeile- 5,Xe   ,IZeile- 5);
             PIESLICE(Xe,IZeile+ 3,    0,   90, 8);
             LINE    (ISpalte,IZeile+11,Xe   ,IZeile+11);
             PIESLICE(ISpalte,IZeile+ 3,   90,  180, 8);
             PIESLICE(Xe,IZeile+ 3,  270,  360, 8);
             BAR     (ISpalte,IZeile- 4,Xe   ,IZeile+10);
             SETCOLOR      (HgFarbe);
             LINE    (ISpalte-7,IZeile+3,Xe+7,IZeile+3)
        END;

   PROCEDURE DrawLine;
      VAR CurColor : BYTE;

      BEGIN
         CurColor := GETCOLOR;
         SETCOLOR(ActColor);
         LINE(X1,Y1,X2,Y2);
         SETCOLOR(CurColor);
      END;

   PROCEDURE DrawCrossPoint;
      BEGIN
         {Typen : 1 = �, 2 = �, 3 = �, 4 = �, 5 = �,

                  6 = � ,7 = �, 8 = �, 9 = � }

         {Zeichne horizontal Linie}
         CASE Typ OF
           1,2,3 : DrawLine(x1,y1+4,x1+8,y1+4,ActColor);
           5,7,9 : DrawLine(x1,y1+4,x1+4,y1+4,ActColor);
           4,6,8 : DrawLine(x1+4,y1+4,x1+8,y1+4,ActColor);
         END;
         {Zeichne vertikale Linie}

         CASE Typ OF
            1,4,5 : DrawLine(x1+4,y1+8-ZHeight,x1+4,y1+ZHeight,ActColor);
            2,8,9 : DrawLine(x1+4,y1+8-ZHeight,x1+4,y1+4,ActColor);
            3,6,7 : DrawLine(x1+4,y1+4,x1+4,y1+ZHeight,ActColor);
         END;
      END;





END.
{============================
 Versionshistorie
 $Log:$
 ============================}
