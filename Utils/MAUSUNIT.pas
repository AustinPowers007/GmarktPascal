{============================
 Dateiname:   $Source: $
 Version:     $Revision: $
 Autor:       $Author: $
 Gelockt von: $Locker: $
 Originaldatum : Sat Jan 08 15:37:04 GMT+01:00 2000
 Dateihistorie am Ende der Datei
 ============================}
{$N-}
Unit Mausunit;
{
*****************************************************************************
*                                                                           *
*                    Microsoft Mouse Interface Routines                     *
*                            Unit version 1.0                               *
*                                                                           *
*                        Copyright   (C) 1988 by                            *
*                          Dr. Bernhard StÅtz                               *
*                                                                           *
*****************************************************************************
}

interface
uses gmcrt,dos,graph;
type
  MouseMenuFlags = Array[1..20] of boolean;
  MaskType       = Array [0..1,0..15] of word;
  MaskPointer    = ^MaskRecord;
  MaskRecord     = record
                     mask : masktype;
                     x    : word;
                     y    : word;
                   end;

  procedure ResetMouse;
  procedure ShowMouse;
  procedure HideMouse;
  procedure MousePos(var X,Y : word);
  function  LeftButton : boolean;
  function  RightButton : boolean;
  procedure PutMouse(X,Y : word);
  function  ButtonPressInfo(Button : byte;
                            var PressCount,X,Y : word):boolean;
  function  ButtonReleaseInfo(Button : word;
                            var PressCount,X,Y : word):boolean;
  procedure SetHorizontalRange(Min,Max : word);
  procedure SetVerticalRange(Min,Max : word);
  function  MouseInBox(x1,y1,x2,y2 : word) : boolean;
  procedure SetGraphicsCursor(var maskp : MaskRecord);
  procedure SetTextCursor(sel,start,stop : word);
  procedure MouseRatio(X,Y : word);
  procedure MotionCounters(var X,Y : integer);
  procedure LightPenEmulation(f : boolean);
  procedure ConditionalOff(ux,uy,lx,ly : word);
  procedure DoublespeedThreshold(mickey : word);
  procedure SetMouseProcedure(m : word;p: pointer);
  procedure CursorMirror(var S,D : MaskRecord;F : byte);
  function  MouseMenu(s : string;c : byte; var  f : MouseMenuFlags):byte;

(****************************************************************************)

Implementation
var
  RightarrowCursor,DownarrowCursor,InvertedCursor : MaskRecord;
const
  LeftB   : byte=0;
  RightB  : byte=1;

  StandardCursor   : MaskRecord
       = (mask:(($3fff,$1fff,$fff, $7ff, $3ff, $1ff, $ff,  $7f,
                 $3f,  $1f,  $1ff, $10ff,$30ff,$f87f,$f87f,$fc3f),
                ($0,   $4000,$6000,$7000,$7800,$7c00,$7e00,$7f00,
                 $7f80,$7fc0,$7c00,$4600,$600, $300, $300, $180));
                x:0;y:0);

  UpArrowCursor    : MaskRecord
       = (mask:(($f9ff,$f0ff,$e07f,$e07f,$c03f,$c03f,$801f,$801f,
                 $f,   $f,   $f0ff,$f0ff,$f0ff,$f0ff,$f0ff,$f0ff),
                ($0,   $600, $f00, $f00, $1f80,$1f80,$3fc0,$3fc0,
                 $7fe0,$600, $600, $600, $600, $600, $600, $600));
                x:5;y:0);

  LeftArrowCursor  : MaskRecord
       = (mask:(($fe1f,$f01f,$0,   $0,   $0,   $f01f,$fe1f,$ffff,
                 $ffff,$ffff,$ffff,$ffff,$ffff,$ffff,$ffff,$ffff),
                ($0,   $c0,  $7c0, $7ffe,$7c0, $c0,  $0,   $0,
                 $0,   $0,   $0,   $0,   $0,   $0,   $0,   $0));
                x:0;y:3);

  CheckMarkCursor  : MaskRecord
       = (mask:(($fff0,$ffe0,$ffc0,$ff81,$ff03,$607, $f,   $1f,
                 $c03f,$f07f,$ffff,$ffff,$ffff,$ffff,$ffff,$ffff),
                ($0,   $6,   $c,   $18,  $30,  $60,  $70c0,$1d80,
                 $700, $0,   $0,   $0,   $0,   $0,   $0,   $0));
                x:6;y:7);

  PointingHandCursor  : MaskRecord
       = (mask:(($e1ff,$e1ff,$e1ff,$e1ff,$e1ff,$e000,$e000,$e000,
                 $0,   $0,   $0,   $0,   $0,   $0,   $0,   $0),
                ($1e00,$1200,$1200,$1200,$1200,$13ff,$1249,$1249,
                 $f249,$9001,$9001,$9001,$8001,$8001,$8001,$ffff));
                x:5;y:0);

  DiagonalCrossCursor  : MaskRecord
       = (mask:(($7e0, $180, $0,   $c003,$f00f,$c003,$0,   $180,
                 $7e0, $ffff,$ffff,$ffff,$ffff,$ffff,$ffff,$ffff),
                ($0,   $700e,$1c38,$660, $3c0, $660, $1c38,$700e,
                 $0,   $0,   $0,   $0,   $0,   $0,   $0,   $0));
                x:7;y:4);

  RectangularCrossCursor  : MaskRecord
       = (mask:(($fc3f,$fc3f,$fc3f,$0,   $0,   $0,   $fc3f,$fc3f,
                 $fc3f,$ffff,$ffff,$ffff,$ffff,$ffff,$ffff,$ffff),
                ($0,   $180, $180, $180, $7ffe,$180, $180, $180,
                 $0,   $0,   $0,   $0,   $0,   $0,   $0,   $0));
                x:7;y:4);

  HourglassCursor  : MaskRecord
       = (mask:(($0,   $0,   $0,   $0,   $8001,$c003,$e007,$f00f,
                 $e007,$c003,$8001,$0,   $0,   $0,   $0,   $ffff),
                ($0,   $7ffe,$6006,$300c,$1818,$c30, $660, $3c0,
                 $660, $c30, $1998,$33cc,$67e6,$7ffe,$0,   $0));
                x:7;y:7);

var
  CrtMode : byte absolute $40:$49;
  MouseHandler : pointer;
  ExitSave : pointer;
  Int1bSave : pointer;
  OldCursor : pointer;

const
  M1 : word = 0;
  M2 : word = 0;
  M3 : word = 0;
  M4 : word = 0;
  M5 : word = 0;
  M6 : word = 0;
  MaxX : integer = 639;    { EGA F-MODE }
  MaxY : integer = 349;
  SegPointer : word = $ffff;
  GDriver    : integer = Detect;
  GMode      : integer = 0;
  AboFlag    : Boolean = false;
  Hercules   : Boolean = false;

function BitSet(TestByte : byte;BitNumber : byte): boolean;
{ testet Bit BitNumber in byte TestByte }
begin
  TestByte := TestByte and(1 shl BitNumber);
  BitSet   := (TestByte >0);
end;

procedure CheckPos(var X,Y : word);
{ Limitiert die Variablen X und Y auf maximale Bildschirmgrî·e }
begin
  if Y > MaxY then Y := MaxY;
  if X > MaxX then X := MaxX;
end;

procedure Mouse;
{ Ruft den Maustreiber Åber Interrupt 51d }
var
  regs : registers;
begin
  Regs.ax := M1;           { CPU-Register fÅr Mauscall setzen }
   Regs.bx := M2;
  Regs.cx := M3;
  Regs.dx := M4;
  Regs.si := M5;
  Regs.di := M6;
  Regs.es := SegPointer;
  Intr(51,Regs);           { Maustreiber aufrufen }
  M1 := Regs.ax;           { Ergebnisse in Variablen kopieren }
  M2 := Regs.bx;
  M3 := Regs.cx;
  M4 := Regs.dx;
end;

procedure ShowMouse;
{ schaltet Mauszeiger ein }
begin
  M1 := 1;
  Mouse;
end;

procedure HideMouse;
{ schaltet Mauszeiger aus }
begin
  M1 := 2;
  Mouse;
end;

procedure MousePos(var X,Y : word);
{ ermittelt position des Mauszeigers }
begin
  M1 := 3;
  Mouse;
  X := M3;
  Y := M4;
  if AboFlag then
  begin
    CloseGraph;
    {!!!NoSound;}
    writeln('Abbruch');
    halt(1);
  end;
end;

function LeftButton : boolean;
{ testet Status des linken Mausknopfes }
begin
  M1 := 3;
  Mouse;
  if BitSet(M2,LeftB) then LeftButton := true
                      else LeftButton := false;
end;

function RightButton : boolean;
{ testet Status des rechten Mausknopfes }
begin
  M1 := 3;
  Mouse;
  if BitSet(M2,RightB) then RightButton := true
                      else RightButton := false;
end;

procedure PutMouse(X,Y : word);
{ Setzt Maustreiber auf bestimmte Position }
begin
  CheckPos(X,Y);
  M1 := 4;
  M3 := X;
  M4 := Y;
  Mouse;
end;

function  ButtonPressInfo(Button : byte;
                            var PressCount,X,Y : word):boolean;
{ testet Anzahl der MausknopfbetÑtigungen seit letztem Aufruf }
{ Button :       0=leftb, 1=rightb }
begin
  M1 := 5;
  M2 := Button;
  Mouse;
  ButtonPressInfo := BitSet(M1,Button);
  Presscount := M2;       { KnopfbetÑtigungen seit letzten Aufruf }
  X := M3;                { CursorPosition seit letzter BetÑtigung }
  Y := M4;                {        dito                            }
end;

function  ButtonReleaseInfo(Button : word;
                            var PressCount,X,Y : word):boolean;
{ testet Anzahl der Mausknopfloslassungen seit letztem Aufruf }
{ Button :       0=leftb, 1=rightb }
begin
  M1 := 6;
  M2 := Button;
  Mouse;
  ButtonReleaseInfo := not BitSet(M1,Button);
  Presscount := M2;       { Loslassungen seit letzten Aufruf }
  X := M3;                { CursorPosition seit letztem Loslassen }
  Y := M4;                {        dito                            }
end;

procedure SetHorizontalRange(Min,Max : word);
{ setzt horizontalen Mauszeigerbereich }
var Dummy : word;
begin
  CheckPos(min,dummy);
  CheckPos(max,dummy);
  M1 := 7;
  M3 := min;
  M4 := max;
  Mouse;
end;

procedure SetVerticalRange(Min,Max : word);
{ setzt vertikalen Mauszeigerbereich }
var Dummy : word;
begin
  CheckPos(dummy,min);
  CheckPos(dummy,max);
  M1 := 8;
  M3 := min;
  M4 := max;
  Mouse;
end;

function  MouseInBox(x1,y1,x2,y2 : word) : boolean;
{ testet, ob sich Mauszeiger in bestimmtem Bildschirmbereich befindet }
var x,y : word;
begin
  if x2<x1 then begin x:=x1; x1:=x2; x2:=x; end;
  if y2<y1 then begin y:=y1; y1:=y2; y2:=y; end;
  MousePos(x,y);
  MouseInBox:=( ((x>=x1) and (x<x2)) and
                ((y>=y1) and (y<y2))  );
end;

procedure SetGraphicsCursor(var maskp : MaskRecord);
{ setzt neue Form des Grafik-Mauszeigers }
begin
  if OldCursor <> @maskp then begin
     OldCursor := @maskp;
     with Maskp do begin
       M1 := 9;
       M2 := X;
       M3 := Y;
       M4 := ofs(Mask);
       SegPointer := seg(Mask);
     end;
   end;
   Mouse;
 end;

procedure SetTextCursor(sel,start,stop : word);
{ setzt Grî·e und Art des Mauszeigers im Textmodus }
begin
  M1 := 10;
  M2 := sel;      { Hardware- oder Softwarecursor }
  M3 := start;    { scanline oder Bildschirmmaske }
  M4 := stop;     { scanline oder Zeigermaske     }
  Mouse;
end;

procedure MouseRatio(X,Y : word);
{ setzt Empfindlichkeit des Mauszeigers }
begin
  M1 := 15;
  M3 := X;
  M4 := Y;
  Mouse;
end;

procedure MotionCounters(var X,Y : integer);
{ gibt Distanz in Mauseinheitenseit letztem Aufruf zurÅck }
{ 1 Mickey = 1/200 inch }
var
  x1,y1 : longint;
begin
  M1 := 11;
  Mouse;
  if M3 > $fff then x1:=m3-65536 else x1:=m3;
  if M4 > $fff then y1:=m4-65536 else y1:=m4;
  X := x1;
  Y := y1;
end;

procedure LightpenEmulation(f : boolean);
{ schaltet Lightpen Emulation an/aus }
{ Nur der VollstÑndigkeit halber }
begin
  if f then M1 := 13 else M1 := 14;
  Mouse;
end;

procedure conditionalOff(ux,uy,lx,ly : word);
{ schaltet Mauszeiger aus, wenn er sich im angegebenen Bereich befindet }
begin
  M1 := 16;
  if ux < lx
  then begin
    M3 := ux;
    M5 := lx;
  end
  else begin
    M3 := lx;
    M5 := ux;
  end;
  if uy < ly
  then begin
    M4 := uy;
    M6 := ly;
  end
  else begin
    M4 := ly;
    M6 := uy;
  end;
  Mouse;
end;

procedure DoubleSpeedThreshold(mickey : word);
{ Legt Mausgeschwindigkeit fest, bei der sich die Mausempfindlichkeit verdoppelt }
begin
  M1 := 19;
  M4 := Mickey;     { Schwelle in mickeys/Sekunde }
  Mouse;
end;

procedure MouseCallExit;
{ Exitprozedur des Maustreiberaufrufs }
{ Nîtig, weil Interrupt Procedure mit RETI statt mit RET FAR beendet wird.
  Maustreiber erwartet RET FAR. }
begin
  inline ($5d/$58/$58/$89/$ec/$5d/$07/$1f/$5f/$5e/$5a/$59/$5b/$58/$cb);
  { pop bp ; pop ax ; pop ax ; mov sp,bp ; pop es ; pop ds ;
    pop di ; pop si ; pop dx ; pop cx ; pop bx ; pop ax ;
    ret far}
end;

procedure UserHandlerCall(Mask,Button,X,Y : word);
{ ruft User Maus Interrupt Prozedur }
{ Nîtig, weil TurboPascal V4 keine prozeduralen Parameter erlaubt. }
Inline($ff/$1e/MouseHandler);      { call far [MouseHandler] }

procedure MouseInterrupt(Flags,CS,IP,AX,BX,CX,DX,SI,DI,DS,ES,BP : word);
{ Diese Prozedur wird vom Maustreiber bei in Mask festgelegten
  Bedingungen gerufen. Von ihr wird der User Handler aufgerufen. }
Interrupt;
begin
  UserHandlerCall(AX,BX,CX,DX);
  MouseCallExit;
end;

procedure SetMouseProcedure(m : word;p : pointer);
{ Teilt Maustreiberadresse von MouseInterrupt und Event-Maske mit }
begin
  M1 := 12;
  M3 := m;
  M4 := Ofs(MouseInterrupt);
  SegPointer := Seg(MouseInterrupt);
  MouseHandler := p;           { Adresse des UserHandlers }
  Mouse;
end;


Function MouseMenu(s:string;c:byte;var f:MouseMenuFlags):byte;
{ MenÅzeile am unteren Bildrand einschalten, angeklickte
  Auswahl als Funktionswert zurÅckgeben, Flags setzen  }
var
  test,d:boolean;
  color,ncolor,i,k,p1,p2,p3:byte;
  boxl,count,x,y,y1:word;
  code:integer;
  text:string;
  wbar:string;
begin
  SetViewPort(0,0,GetMaxX,GetMaxY,ClipOn);
  if Length(s) > 0 then d:=true else d:=false;
  p1:=1;
  boxl:=trunc(GetMaxX/c);
  MouseMenu:=0;
  MousePOs(x,y);
  if y>GetMaxY-25 then
  begin
    test:=buttonpressinfo(LeftB,count,x,y1);
    setgraphicscursor(PointinghandCursor);
  end;
  while (y>GetMaxY-25) or d do
  begin
    test:=buttonpressinfo(LeftB,count,x,y1);
    ShowMouse;
    if (count >0) or d then begin
      HideMouse;
      if d then begin
        SetViewPort(0,GetMaxY-25,GetMaxX,GetMaxY,ClipOn);
        ClearViewPort;
        SetViewPort(0,0,GetMaxX,GetMaxY,ClipOn);
      end;

      for i:=1 to c do begin
        if MouseInBox(boxl*(i-1),GetMaxY-20,boxl*i-5,GetMaxY)
                 and (count > 0) then begin
          MouseMenu:=i;         { Funktionswert zurÅckgeben        }
          f[i]:=not f[i];       { zugehîriges Flag komplementieren }
          {!!!Sound(2000);}          { akustische RÅckmeldung           }
          {!!!delay(8);}
          {!!!nosound;}
          MousePos(x,y);
          SetViewPort(0,0,GetMaxX,GetMaxY-25,ClipOn);
          exit;
        end;
        if d then begin
          color:=GetColor;      { MenÅleiste zeichnen }
          SetColor(GetMaxColor);
          Rectangle(boxl*(i-1),GetMaxY-20,boxl*i-5,GetMaxY);
          if f[i] then begin
            p1:=pos(':',copy(s,p1,length(s)))+p1;
            p2:=pos(';',copy(s,p1,length(s)));
            p3:=p2;
          end else begin
            p2:=pos(':',copy(s,p1,length(s)));
            p3:=pos(';',copy(s,p1,length(s)));
          end;
          if p2 = 0 then p2:=length(s);
          text:=copy(s,p1,p2-1);
          val(copy(text,1,2),ncolor,code);
          if copy(text,1,2) = '00' then begin
            SetColor(GetMaxColor);
            text:=copy(text,3,length(text));
            wbar:=' ';
            for k:=1 to length(text)+1 do wbar:=wbar+'€';
            OutTextXY(boxl*(i-1)+3,GetMaxY-10,wbar);
            SetColor(0);
          end;
          if ncolor > 0 then begin
            if ncolor > GetMaxColor then ncolor:=GetMaxColor;
            SetColor(ncolor);
            text:=copy(text,3,length(text));
          end;
          p1:=p3+p1;
          OutTextXY(boxl*(i-1)+5,GetMaxY-10,text);
          SetColor(color);
        end;
      end;
      ShowMouse;
    end;
    MousePos(x,y);
    d:=false;
  end;
  SetViewPort(0,0,GetMaxX,GetMaxY-25,ClipOn);
end;


function BitInvert(b:word):word;
{ Invertiert Bitfolge in wort b : fedcba9876543210 -> 0123456789abcdef }
inline ($58/$b9/$10/$00/$33/$db/$d1/$d0/$d1/$db/$e2/$fa/$8b/$c3);
{ pop ax; mov cx,16; xor bx,bx; rcl ax,1; rcr bx,1; loop; mov ax,bx }

procedure CursorMirror(var S,D:MaskRecord;F:byte);
{ Kopiert und spiegelt Mauszeigermasken }
var
  i,k:byte;
begin
  for i:=0 to 1 do begin
    for k:=0 to 15 do begin
      if (F and 1) > 0 then D.mask[i,k]:=S.mask[i,15-k]
                       else D.mask[i,k]:=S.mask[i,k];
      if (F and 2) > 0 then D.mask[i,k]:=BitInvert(D.mask[i,k]);
    end;
  end;
  if (F and 1) > 0 then D.y:=15-S.y else D.y:=S.y;
  if (F and 2) > 0 then D.x:=15-S.x else D.x:=S.x;
end;

procedure ResetMouse;
{ Initialisiert Maustreiber, testet Bildschirmmodus, stzt bei
  Herkuleskarte richtigen Bildschirmmodus und ermittelt maximalen
  Mauszeigerbereich    }
var
  size:word;
  save:boolean;
begin
  DetectGraph(GDriver,GMode);
{ Mit diesem Trick versteht der Maustreiber die Herkuleskarte }
  if GDriver = HercMono then begin
    CrtMode := 6;
    Hercules := true;
  end;

  M1:=0;
  Mouse;
  MaxX:=GetMaxX;
  MaxY:=GetMaxY;
  SetHorizontalRange(0,MaxX);
  SetVerticalRange(0,MaxY);
end;

procedure CallOld1B;
  inline ($9c/$ff/$1e/Int1BSave);
  { push ; call far Int1BSave }
  { rufe alten Interrupt 1B Handler }
{$F+} procedure Int1B; Interrupt; {$F-}
{ setzt Abbruchflag, wenn Break gedrÅckt wird.
  Der Abbruch erfolgt, sobald MousePos aufgerufen wird }
begin
  SetIntVec($1B,Int1BSave);
  CallOld1B;
  AboFlag:=true;
end;

{$F+} procedure MouseExit; {$F-}
{ Mouse Unit Exit Handler }
{ Setzt Bildschirmmodus auf Textmodus }
begin
  M1:=0;
  Mouse;
  {!!!nosound;}
  if hercules then CrtMode := 7;
  ExitProc:=ExitSave;
end;

{ Unit Initialisierung }
begin
  M1:=0;
  Mouse;
  if not (M1=65535) then begin    { Testen, ob Maustreiber vorhanden }
    CloseGraph;
    {!!!Clrscr;}
    {!!!Gotoxy(20,12);}
    write('Wo ist denn mein Mauskumpel, der Treiber ?! ');
    Halt(1);
  end;
  MaxX:=79;
  MaxY:=24;
  {!!!clrscr; }

  CursorMirror(LeftArrowCursor,RightarrowCursor,2);
  CursorMirror(UpArrowCursor,DownArrowCursor,1);
  CursorMirror(StandardCursor,InvertedCursor,3);
  ExitSave:=ExitProc;
  ExitProc:=@MouseExit;
  GetIntVec($1B,Int1BSave);
  SetIntVec($1B,@Int1B);
end.
{============================
 Versionshistorie
 $Log:$
 ============================}
