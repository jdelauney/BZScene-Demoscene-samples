Unit uMainForm;

{$mode objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs,
  BZMath, BZVectorMath,
  BZColors, BZGraphic, BZBitmap, {%H-}BZBitmapIO,
  BZCadencer, BZStopWatch;

Const
  cFormCaption: String = 'BZScene Real Fire Demo';
  cMaxFireSize: Word   = 760;

Const
  MaxX  = 975;
  MaxY  = 767;
  Trainee = 90;  // longueur de la trainée
  R     = 160;  // Rayon du cercle
  RXmax = 170 + R;
  RXmin = 150 - R;  // clipping
  RYmax = 130 + R;
  RYmin = 1;
  vitesse = 3; //  vitesse de rotation
  PP    = 10; // Précision du cercle

Type

  { TMainForm }

  TMainForm = Class(TForm)
    Procedure FormCreate(Sender: TObject);
    Procedure FormCloseQuery(Sender: TObject; Var CanClose: Boolean);
    Procedure FormDestroy(Sender: TObject);
    Procedure FormShow(Sender: TObject);
  Private
    FCadencer:      TBZCadencer;
    FStopWatch:     TBZStopWatch;
    FDisplayBuffer: TBZBitmap;

    //-------- Scene vars ----------
    //FSkullBitmap:     TBZBitmap;
    //FSKullX, FSkullY: Integer;
    //FZoomRect, FSkullRect: TBZRect;
    //FCurrentZoomStep, FZoomStep: Word;
    //FDeltaSkullAlpha: Single;
    //FDeltaZoomX, FDeltaZoomY: Single;
    //FSkullAlpha:      Single;


    FFirePalette: Array Of TBZColor;  // On aurait pu directement utiliser la palette intégrée au TBZBitmap
    FFireBuffer:  Array Of Array Of Integer;
    FFireMaxY, FFireMaxX: Integer;

    FFlameSize: Integer;

    FRedPos:    Integer;
    FYellowPos: Integer;
    FBlackPos:  Integer;
    FWhitePos:  Integer;
    FColdPos:   Integer;
    FFadePos:   Integer;
    FRedDiv:    Integer;

    Procedure CadencerProgress(Sender: TObject; Const deltaTime, newTime: Double);
  Public
    CS, SS: Array [1..720] Of Integer;
    points, coor: Array [1..50, 1..3] Of Integer;
    XC, YC, NX, NY, A, AO: Integer;

    Procedure InitEngine;
    Procedure StartEngine;
    Procedure StopEngine;
    Procedure DoneEngine;

    Procedure InitScene;
    Procedure InitColorMap;
    Procedure FireLine(X1, Y1, X2, Y2, C: Integer);
    Procedure DrawFireFigure;
    Procedure RenderScene(DeltaTime: Double);
    Procedure DoneScene;

    Procedure UpdateFire(DeltaTime: Double; Const AOxygen: Boolean = True);
  End;

Var
  MainForm: TMainForm;

Implementation

{$R *.lfm}

{ TMainForm }
Uses
  Math;

Procedure TMainForm.FireLine(X1, Y1, X2, Y2, C: Integer);
Var
  Xdiff, Ydiff, Xincr, Yincr, Erreur, X, Y, I: Integer;
Begin
  Erreur := 0;
  XDIFF  := X2 - X1;
  YDIFF  := Y2 - Y1;
  X      := X1;
  Y      := Y1;
  Xincr  := 1;
  Yincr  := 1;
  If (XDIFF < 0) Then
  Begin
    XINCR := -XINCR;
    XDIFF := -XDIFF;
  End;
  If YDIFF < 0 Then
  Begin
    YINCR := -YINCR;
    YDIFF := -YDIFF;
  End;

  If (Abs(XDIFF) > Abs(YDIFF)) Then
  Begin
    For I := 1 To XDIFF + 1 Do
    Begin
      FFireBuffer[X, Y] := C;
      X      := X + XINCR;
      ERREUR := ERREUR + YDIFF;
      If ERREUR > XDIFF Then
      Begin
        Y      := Y + YINCR;
        ERREUR := ERREUR - XDIFF;
      End;
    End;
  End
  Else
  Begin
    For I := 1 To YDIFF + 1 Do
    Begin
      FFireBuffer[X, Y] := C;
      Y      := Y + YINCR;
      ERREUR := ERREUR + XDIFF;
      If ERREUR > YDIFF Then
      Begin
        X      := X + XINCR;
        ERREUR := ERREUR - YDIFF;
      End;
    End;
  End;
End;

Procedure TMainForm.DrawFireFigure;
Var
  //tmp: Integer;
  i, x1, y1, y2, z1, j: Integer;
Begin

  A := A + vitesse; (* on fait tourner ... le logo*)
  A := (A - 1) mod 720 + 1;(* on fait un modulo sur l'angle*)
  For I := 1 To 5 * PP Do
  Begin
    // rotation selon l'axe Z
    X1 := (coor[I, 1] * CS[A] - coor[I, 2] * SS[A]) div 256;
    Y1 := (coor[I, 1] * SS[A] + coor[I, 2] * CS[A]) div 256;
    // rotation selon l'axe Y
    Z1 := (coor[I, 3] * CS[A] - X1 * SS[A]) div 256;
    X1 := (coor[I, 3] * SS[A] + X1 * CS[A]) div 256;
    // rotation selon l'axe X
    Y2 := Y1;
    Y1 := (Z1 * SS[A] + Y1 * CS[A]) div 256;
    Z1 := (Z1 * CS[A] - Y2 * SS[A]) div 256;
    // projection + gestion de la profondeur + centrage
    Points[I, 1] := XC + round(X1 * (1 + Z1 / 256));
    Points[I, 2] := YC + round(Y1 * (1 + Z1 / 256));
  End;

  // on trace les branches du pentacle
  FireLine(Points[PP, 1], Points[PP, 2], Points[3 * PP, 1], Points[3 * PP, 2], random(4) * 255);
  FireLine(Points[3 * PP, 1], Points[3 * PP, 2], Points[5 * PP, 1], Points[5 * PP, 2], random(4) * 255);
  FireLine(Points[2 * PP, 1], Points[2 * PP, 2], Points[4 * PP, 1], Points[4 * PP, 2], random(4) * 255);
  FireLine(Points[5 * PP, 1], Points[5 * PP, 2], Points[2 * PP, 1], Points[2 * PP, 2], random(4) * 255);
  FireLine(Points[4 * PP, 1], Points[4 * PP, 2], Points[PP, 1], Points[PP, 2], random(4) * 255);

  // on trace le cercle du pentacle
  For J := 2 To 5 * PP Do
    FireLine(Points[J, 1], Points[J, 2], Points[J - 1, 1], Points[J - 1, 2], random(7) * 255);//+random(4)*255);
  //FireLine(Points[5*PP,1],Points[5*PP,2],Points[1,1],Points[1,2],random(6)*255); //+random(4)*255);


  // Calcul du feu
  //for y:=rymin to rymax do
  //begin
  //  for x:=rxmin to rxmax do
  //  begin
  //  // on fait la moyenne des intensités des pixels (blur)
  //    If (Tab[X,Y]>0) and (random(100)>Trainee) then Tab[X,Y]:=Tab[X,Y]div 2;
  //         tmp:=Tab[X,Y] shr 1+(Tab[X-1,Y]+Tab[X+1,Y]) shr 2;
  //         Tab[X,Y-1]:=tmp;
  //  // si la couleur<>0 on affiche
  //     if tmp<>0 then dxasmpixel16(x,y-1,ctable[tmp]);
  //  end;
  //end;

End;

Procedure TMainForm.FormCreate(Sender: TObject);
Begin
  InitEngine;
  InitScene;
End;

Procedure TMainForm.FormCloseQuery(Sender: TObject; Var CanClose: Boolean);
Begin
  StopEngine;
  CanClose := True;
End;

Procedure TMainForm.FormDestroy(Sender: TObject);
Begin
  DoneScene;
  DoneEngine;
End;

Procedure TMainForm.FormShow(Sender: TObject);
Begin
  StartEngine;
End;

Procedure TMainForm.CadencerProgress(Sender: TObject; Const deltaTime, newTime: Double);
Begin
  RenderScene(DeltaTime);
  FDisplayBuffer.DrawToCanvas(Canvas, ClientRect);
  Caption := cFormCaption + ' : ' + Format('%.*f FPS', [3, FStopWatch.getFPS]);
End;

Procedure TMainForm.InitEngine;
Begin
  FCadencer      := TBZCadencer.Create(self);
  FCadencer.Enabled := False;
  FCadencer.OnProgress := @CadencerProgress;
  FStopWatch     := TBZStopWatch.Create(self);
  FDisplayBuffer := TBZBitmap.Create(Width, Height);
End;

Procedure TMainForm.StartEngine;
Begin
  DoubleBuffered := True;
  FStopWatch.Start;
  FCadencer.Enabled := True;
End;

Procedure TMainForm.StopEngine;
Begin
  FStopWatch.Stop;
  FCadencer.Enabled := False;
End;

Procedure TMainForm.DoneEngine;
Begin
  FreeAndNil(FStopWatch);
  FreeAndNil(FCadencer);
  FreeAndNil(FDisplayBuffer);
End;

Procedure TMainForm.InitScene;
// Initalisation des données de la scene
Var
  I: Integer;
Begin

  //FSkullBitmap := TBZBitmap.Create;
  //FSkullBitmap.LoadFromFile('..\..\..\..\media\images\skull01.png');
  //FSkullX := FDisplayBuffer.CenterX - (FSkullBitmap.CenterX+1);
  //FSkullY := 100;
  //FSkullRect.Create(FSkullX, FSkullY, FSkullX + FSkullBitmap.Width, FSkullY + FSkullBitmap.Height);
  //FZoomRect.Create(FDisplayBuffer.CenterX-1, FDisplayBuffer.CenterY-1,FDisplayBuffer.CenterX+1, FDisplayBuffer.CenterY+1);
  //FZoomStep := 1000;   // environ 10sec
  //FCurrentZoomStep := 0;
  //FDeltaZoomX := FSkullBitmap.Width/FZoomStep;
  //FDeltaZoomY := FSkullBitmap.Height/FZoomStep;
  //FSkullAlpha := 0;
  //FDeltaSkullAlpha := 192 / FZoomStep;

  Randomize;

  FFlameSize := Min(FDisplayBuffer.Height, cMaxFireSize);

  FBlackPos  := 0;
  FRedPos    := FFlameSize div 7 * 3;
  FYellowPos := FRedPos + (FFlameSize - FRedPos) div 4;
  FWhitePos  := FFlameSize - 1;
  FColdPos   := FFlameSize div 6;
  FFadePos   := Round(FFlameSize / 2.95);
  FRedDiv    := FRedPos div 16;

  FFireMaxY := FDisplayBuffer.MaxHeight;
  FFireMaxX := FDisplayBuffer.MaxWidth;

  SetLength(FFireBuffer, FDisplayBuffer.Height);
  For I := 0 To FFireMaxY Do
    SetLength(FFireBuffer[I], FDisplayBuffer.Width);

  For I := 1 To 720 Do
  Begin
    CS[I] := round(256 * cos(c2Pi * I / 720));
    SS[I] := round(256 * sin(c2Pi * I / 720));
  End;

  For I := 1 To 5 * PP Do
  Begin
    coor[I, 1] := round(R * cos((2 * I / PP) * cPi / 5));
    coor[I, 2] := round(R * sin((2 * I / PP) * cPi / 5));
    coor[I, 3] := 0;
  End;
  XC := 340;
  YC := 511;
  A  := 0;

  DrawFireFigure;

  InitColorMap;
End;

Procedure TMainForm.InitColorMap;
// Initalisation de la palette de couleur, si utilisée
Var
  Index: Integer;
  BlackColor, WhiteColor, YellowColor, RedColor: TBZColor;
Begin

  BlackColor.Create(0, 0, 0, 0);
  WhiteColor.Create(252, 252, 252, 232);
  YellowColor.Create(32, 192, 255, 232);
  RedColor.Create(24, 92, 208, 232);
  //  YellowColor.Create(252, 208, 64, 232);
  //RedColor.Create(255, 92, 40, 232);
  SetLength(FFirePalette, FWhitePos + 1);

  For Index := FBlackPos To FRedPos Do
    FFirePalette[Index] := BlackColor.Lerp(RedColor, 1 / (FRedPos - FBlackPos) * (Index - FBlackPos), 0, itTan, True);

  For Index := FRedPos To FYellowPos Do
    FFirePalette[Index] := RedColor.Lerp(YellowColor, 1 / (FYellowPos - FRedPos) * (Index - FRedPos), 0, itSinAlt, True);

  For Index := FYellowPos To FWhitePos Do
    FFirePalette[Index] := YellowColor.Lerp(WhiteColor, 1 / (FWhitePos - FYellowPos) * (Index - FYellowPos), 0, itSin, True);

End;

Procedure TMainForm.RenderScene(DeltaTime: Double);
// Coeur du rendu de la scene
Begin
  FDisplayBuffer.Clear(clrBlack);
  UpdateFire(DeltaTime, True);
  //if (FCurrentZoomStep < FZoomStep) then
  //begin

  //  if (FZoomRect.Top > FSkullRect.Top) then FZoomRect.Top := Round(FZoomRect.Top - FDeltaZoomY);
  //  if (FZoomRect.Bottom < FSkullRect.Bottom) then FZoomRect.Bottom := Round(FZoomRect.Bottom + FDeltaZoomY);
  //  if (FZoomRect.Left > FSkullRect.Left) then FZoomRect.Left := Round(FZoomRect.Left - FDeltaZoomX);
  //  if (FZoomRect.Right < FSkullRect.Right) then FZoomRect.Right := Round(FZoomRect.Right + FDeltaZoomX);
  //  if FSkullAlpha < 192 then FSkullAlpha := FSkullAlpha + FDeltaSkullAlpha else FSkullAlpha := 192;
  //  Inc(FCurrentZoomStep);
  //end;

  //FDisplayBuffer.PutImageStretch(FSkullBitmap,FZoomRect,Round(FSkullAlpha));

End;

Procedure TMainForm.DoneScene;
// Finalisation de la scene
Var
  I: Integer;
Begin
  {$IFDEF WINDOWS}
  For I := 0 To FFlameSize - 1 Do
    SetLength(FFireBuffer[I], 0);
{$ENDIF}
  SetLength(FFireBuffer, 0);
  SetLength(FFirePalette, 0);
  //FreeAndNil(FSkullBitmap);
End;

Procedure TMainForm.UpdateFire(DeltaTime: Double; Const AOxygen: Boolean = True);
Var
  X, XX, Y, StartY, ColorIndex:  Integer;
  c1, c2, c3, c4, OutColorIndex: Integer;

  Procedure UpdateHotPoints;
  Var
    CX: Integer;
  Begin
    For CX := 1 To FFireMaxX - 1 Do
    Begin
      ColorIndex := FBlackPos;
      If (AOxygen and (Random(10) in [0 .. 6])) Then
      Begin
        Case Random(100) + 1 Of
          30 .. 59: ColorIndex := FRedPos;
          0  .. 29: ColorIndex := FYellowPos;
        Else
          ColorIndex := FWhitePos;
        End;
      End;
      FFireBuffer[FFireMaxY, CX] := ColorIndex;
    End;
  End;

Begin
  DrawFireFigure;
  UpdateHotPoints;
  StartY := FDisplayBuffer.MaxHeight - cMaxFireSize;
  For XX := 1 To FFireMaxX - 1 Do
  Begin
    If Odd(Round(DeltaTime * 1000)) Then
      X := XX
    Else
      X := FFireMaxX - XX;

    For Y := FFireMaxY Downto StartY Do
    Begin
      C1 := FFireBuffer[Y, X];
      C2 := FFireBuffer[Y, X - 1];
      C3 := FFireBuffer[Y, X + 1];
      //C4       := FFireBuffer[Y-1, X] + 4;

      //if y<640 then OutColorIndex := Min(( C1 + C2 + C3 + C4) Shr 2, High(FFirePalette))
      //else
      OutColorIndex := Min((C1 + C1 + C2 + C3) shr 2, High(FFirePalette));

      If (FFireMaxY - Y < FColdPos) and ((OutColorIndex >= 1) and (OutColorIndex <= FFlameSize shr 3)) Then
        OutColorIndex := OutColorIndex div FRedDiv
      Else If (OutColorIndex >= FRedPos) and (OutColorIndex <= FYellowPos) Then
        Inc(OutColorIndex, Ord(Random(4) = 2));

      If (OutColorIndex > 0) and (OutColorIndex < FFadePos) Then
        Dec(OutColorIndex);

      FFireBuffer[Y - 1, X] := OutColorIndex;
      If OutColorIndex > 0 Then
        FDisplayBuffer.setPixel(X, Y - 1, FFirePalette[OutColorIndex]);
    End;
  End;
End;


End.
