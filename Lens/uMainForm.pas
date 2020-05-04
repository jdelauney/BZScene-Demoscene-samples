unit uMainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs,
  BZColors, BZGraphic, BZBitmap, BZBitmapIO, BZCadencer, BZStopWatch, Types;

Const
 cMinLensRadius : byte = 4;
 cMaxLensRadius : byte = 240;
 cLensDefaultRadius : byte = 80;
 {Distance}
 cMinLensDist : Integer = 1; // plus c'est grand et plus c'est petit
 cMaxLensDist : Integer = 150;
 cLensDefaultDist : Integer = 20;

type

  { TMainForm }

  TMainForm = class(TForm)
    procedure FormMouseMove(Sender : TObject; {%H-}Shift : TShiftState; X, Y : Integer);
    procedure FormCloseQuery(Sender : TObject; var CanClose : boolean);
    procedure FormDestroy(Sender : TObject);
    procedure FormShow(Sender : TObject);
    procedure FormMouseWheelDown(Sender : TObject; {%H-}Shift : TShiftState; {%H-}MousePos : TPoint; var {%H-}Handled : Boolean);
    procedure FormKeyPress(Sender : TObject; var Key : char);
    procedure FormMouseWheelUp(Sender : TObject; {%H-}Shift : TShiftState; {%H-}MousePos : TPoint; var {%H-}Handled : Boolean);
  private
    FCadencer : TBZCadencer;
    FStopWatch : TBZStopWatch;
    FSwapBuffer, FBackground, FDisplayBuffer : TBZBitmap;
    FLensMode : Byte;
    FExtraParams : Single;
    DisplayMaxWidth, DisplayMaxHeight : Integer;

    procedure CadencerProgress(Sender : TObject; const deltaTime, newTime : Double);
  public
    MousePos : TPoint;
    px, py, px2, py2, px3, py3 : Integer;
    dpx,dpy, dpx2,dpy2, dpx3,dpy3 : Integer;
    CurrentLensRadius, CurrentLensDist, A, AD : Integer;
    DisplayHelp : Boolean;
    procedure InitScene;
    procedure RenderLens(xp, yp, LensRadius, LensDist : Integer; Tint : TBZColor);
    procedure RenderScene(DeltaTime : Double);
  end;

var
  MainForm : TMainForm;

implementation

{$R *.lfm}

uses BZMath, BZLogger;

{ TMainForm }

procedure TMainForm.FormMouseMove(Sender : TObject; Shift : TShiftState; X, Y : Integer);
begin
  MousePos.Create(x,y);
end;

procedure TMainForm.FormCloseQuery(Sender : TObject; var CanClose : boolean);
begin
  FCadencer.Enabled := False;
  FStopWatch.Stop;
  CanClose := True;
end;

procedure TMainForm.FormDestroy(Sender : TObject);
begin
  FreeAndNil(FSwapBuffer);
  FreeAndNil(FBackground);
  FreeAndNil(FDisplayBuffer);
  FreeAndNil(FStopWatch);
  FreeAndNil(FCadencer);
end;

procedure TMainForm.FormShow(Sender : TObject);
begin
  InitScene;
  DoubleBuffered:=true;
  FStopWatch.Start;
  FCadencer.Enabled := True;
end;

procedure TMainForm.FormMouseWheelDown(Sender : TObject; Shift : TShiftState; MousePos : TPoint; var Handled : Boolean);
begin
  CurrentLensRadius := CurrentLensRadius - 1;
  if CurrentLensRadius<cMinLensRadius then CurrentLensRadius := cMinLensRadius;
end;

procedure TMainForm.FormKeyPress(Sender : TObject; var Key : char);
begin
  if Key in ['0'..'4'] then
  begin
    FLensMode := StrToInt(Key);
  end
  else if Key = '+' then
  begin
    CurrentLensDist := CurrentLensDist - 1;
    if CurrentLensDist<cMinLensDist then CurrentLensDist := cMinLensDist;
  end
  else if Key = '-' then
  begin
    CurrentLensDist := CurrentLensDist + 1;
    if CurrentLensDist>cMaxLensDist then CurrentLensDist := cMaxLensDist;
  end
  else if UpCase(Key) = 'Z' then
  begin
    FExtraParams := FExtraParams + 1;
  end
  else if UpCase(Key) = 'A' then
  begin
    FExtraParams := FExtraParams - 1;
  end
  else if upCase(Key)='H' then DisplayHelp := not(DisplayHelp)
  else if key =#27 then close;
end;

procedure TMainForm.FormMouseWheelUp(Sender : TObject; Shift : TShiftState; MousePos : TPoint; var Handled : Boolean);
begin
  CurrentLensRadius := CurrentLensRadius + 1;
  if CurrentLensRadius>cMaxLensRadius then CurrentLensRadius := cMaxLensRadius;
end;

procedure TMainForm.CadencerProgress(Sender : TObject; const deltaTime, newTime : Double);
begin
  RenderScene(NewTime-DeltaTime);
  FDisplayBuffer.DrawToCanvas(Canvas, ClientRect);
  Caption:='BZScene - Lens Demo : '+Format('%.*f FPS', [3, FStopWatch.getFPS]);
end;


procedure TMainForm.InitScene;
begin
  FCadencer := TBZCadencer.Create(self);
  FCadencer.Enabled := False;
  FCadencer.OnProgress := @CadencerProgress;

  FStopWatch := TBZStopWatch.Create(Self);

  FDisplayBuffer := TBZBitmap.Create(Width,Height);

  FDisplayBuffer.Clear(clrBlack);
  FBackground := TBZBitmap.Create;
  FBackGround.LoadFromFile('..\..\..\..\media\images\clonemea.png');
  FSwapBuffer := TBZBitmap.Create(FBackground.Width, FBackground.Height); //FBackGround.CreateClone;

  DisplayMaxWidth := FDisplayBuffer.Width;
  DisplayMaxHeight := FDisplayBuffer.Height;

  CurrentLensRadius := 60;
  CurrentLensDist := 20;
  MousePos.X := (Width div 2) -1;
  MousePos.y := (Height div 2) -1;
  Mouse.CursorPos := MousePos;
  Screen.Cursor := crNone;
  px := 30;
  py := 30;
  px2 := 512;
  py2 := 375;
  dpx := 3;
  dpy := -1;
  dpx2 := -2;
  dpy2 := 3;
  px3 := FDisplayBuffer.Width - 60;
  py3 := FDisplayBuffer.Height - 45;
  dpx3 := -2;
  dpy3 := -2;
  A := 1;
  Ad := 1;
  DisplayHelp := True;
  FLensMode := 0;
  FExtraParams := 10;

end;

procedure TMainForm.RenderLens(xp, yp, LensRadius, LensDist : Integer; Tint : TBZColor);
Var
 xx,yy,dx,dy : Integer;
 srx, sry: integer;
 u,v, ux, uy : integer;
 MaxLensDiameter, LensDiameter : Integer;
 RadSqr, DistSqr,  z, angle, Rayon : Single;
 DistLens : Single;
 OutColor : TBZColor;
 {$CODEALIGN VARMIN=16}
 //c1,c2,c3,c4, cd : TBZColorVector;
 //c5,c6,c7,c8, c9 : TBZColorVector;
 {$CODEALIGN VARMIN=4}
Begin
  u := 0;
  v := 0;
  LensDiameter := LensRadius + LensRadius;
  MaxLensDiameter := LensDiameter-1;
  //If FLensMode < 5 then
  RadSqr := LensRadius*LensRadius;
  //else RadSqr := (LensRadius * 100) * LensDist / 100;
  for YY := 0 to MaxLensDiameter do
  begin
    dy := yy + yp;
    uy := yy - LensRadius;
    sry := uy * uy;
    for xx := 0 to MaxLensDiameter-1 do
    begin
      dx := xx + xp;
      ux := xx - LensRadius;
      srx := ux * ux;
      DistSqr := srx+sry;
      if (DistSqr <= RadSqr) then
      begin
        Case FLensMode of
          0 :  // Reflexion spherique
          begin
            {$IFDEF CPU64}
            z:= System.sqrt(RadSqr-srx-sry);
            {$ELSE}
            z:= System.sqrt(RadSqr-srx-sry) + cEpsilon;
            {$ENDIF}
            DistLens := LensDist-z;
            {$IFDEF CPU64}
            z := (1.0/z);  // Erreur SIGFPE en 32 bits
            // Note  round(DistLens*(ux/z)) div FBackground.Width
            // et round(DistLens*(uy/z)) mod FBackground.Height peuvent être précalculer dans une LUT pour gagner des FPS
            u:= dx + round(DistLens*(ux*z)) div FBackground.Width;
            v:= dy + round(DistLens*(uy*z)) mod FBackground.Height;
            {$ELSE}
            //z := 1.0/z;
            u:= dx + round(DistLens*(ux / z)) div FBackground.Width;
            v:= dy + round(DistLens*(uy / z)) mod FBackground.Height;
            {$ENDIF}
          end;
          1 : // Simple Zoom
          begin
            u := dx + Round(-ux * FExtraParams / LensDist) ;
            v := dy + Round(-uy * FExtraParams / LensDist) ;
          end;
          2 : // Pinch
          begin
            {$IFDEF CPU64}
            z:= System.sqrt(RadSqr-srx-sry);
            {$ELSE}
            z:= System.sqrt(RadSqr-srx-sry) + cEpsilon;
            {$ENDIF}
            DistLens := LensDist-z;

            angle:=arctan2(uy,ux);
            rayon := (LensDiameter + LensDiameter) - LensRadius; //(LensRadius * 0.5);
            //rayon := (DistLens + DistLens) - LensRadius;
            u := dx + Round(LensDist * rayon * cos(angle) / 300)  div FBackground.Width;
            v := dy + Round(LensDist * rayon * sin(angle) / 300)  mod FBackground.Height;
          end;
          3 : // Twirl
          begin
            angle := arctan2(uy,ux);
            {$IFDEF CPU64}
            z:= System.sqrt(RadSqr-srx-sry);
            {$ELSE}
            z:= System.sqrt(RadSqr-srx-sry) + cEpsilon;
            {$ENDIF}
            DistLens := LensDist-z;
            //(DistLens - LensRadius)
            u := dx + round(LensRadius * cos(angle - ((DistLens - LensRadius) * FExtraParams)/LensDist)-ux)  div FBackground.Width;
            v := dy + round(LensRadius * sin(angle - ((DistLens - LensRadius) * FExtraParams)/LensDist)-uy) mod FBackground.Height;
          end;
          4 : // Water ripple
          begin
            angle := arctan2(uy,ux);
            {$IFDEF CPU64}
            z:= System.sqrt(RadSqr-srx-sry);
            {$ELSE}
            z:= System.sqrt(RadSqr-srx-sry) + cEpsilon;
            {$ENDIF}
            DistLens := LensDist-z;
            rayon := DistLens - LensRadius;
            z:= sin(rayon * FExtraParams/DistLens) * LensDist;
            u := dx + Round(cos(angle)*z)  div FBackground.Width;
            v := dy + Round(sin(angle)*z)  mod FBackground.Height;
          end;
        end;
      end
      else
      begin
        u:= dx;
        v:= dy;
      end;

      if (RadSqr >= DistSqr) then
      begin

        //FDisplayBuffer.setPixel(dx,dy, cd.AsRGBA);
        u:= Clamp(u,1,FSwapBuffer.MaxWidth - 1);
        v:= Clamp(v,1,FSwapBuffer.MaxHeight - 1);
        //FDisplayBuffer.setPixel(dx,dy, FBackground.GetPixel(u,v));
        //OutColor := FSwapBuffer.GetPixel(u,v);
        //OutColor := FSwapBuffer.GetPixelNeighbour(u,v);
        OutColor := FSwapBuffer.GetPixelBilinear(u,v);
        //OutColor := FSwapBuffer.GetPixelBicubic(u,v);
        //OutColor := FSwapBuffer.GetPixelMean(u,v);
        //OutColor.Create(cd);
        if Tint<>clrTransparent then OutColor := OutColor.Mix(Tint,0.2);
        FDisplayBuffer.setPixel(dx,dy,OutColor);
      end;
    end;
  end;
  FSwapBuffer.FastCopy(FDisplayBuffer);
end;

procedure TMainForm.RenderScene(DeltaTime : Double);
begin
  //FDisplayBuffer.Assign(FBackground);
  //FDisplayBuffer.PutImage(FBackground,0,0);   // Plus rapide que assign
  FDisplayBuffer.FastCopy(FBackground); // encore plus rapide, ne copie que les donnée;
  //FSwapBuffer.Assign(FBackground);
  FSwapBuffer.PutImage(FBackground,0,0);

  // Do the Lens effect
  CurrentLensRadius := 30+Round(15*Sin(DeltaTime*10));
  CurrentLensDist := 20+Round(15*Cos(DeltaTime*10));
  RenderLens(px-CurrentLensRadius+cLensDefaultRadius,py-cLensDefaultRadius-cLensDefaultRadius,cLensDefaultRadius+CurrentLensRadius,CurrentLensDist-5, clrRed);
  RenderLens(px2-cLensDefaultRadius-40,py2-cLensDefaultRadius-60,cLensDefaultRadius+40,CurrentLensDist+20,clrTransparent );
  RenderLens(MousePos.x-CurrentLensRadius,MousePos.y-CurrentLensRadius,CurrentLensRadius, CurrentLensDist, clrBlue);

  //RenderLens(px3-CurrentLensRadius+30,py2-CurrentLensRadius-30,CurrentLensRadius+30,  CurrentLensDist+20, clrBlue);
  //RenderLens(px2-CurrentLensRadius+10,py3-CurrentLensRadius+10,CurrentLensRadius+10,CurrentLensDist-10, clrGreen);

  // Move the lens

  px := px +  dpx;
  py := py +  dpy;
  if (px<0) or (px> FDisplayBuffer.MaxWidth) then dpx := -dpx;
  if (py<0) or (py> FDisplayBuffer.MaxHeight)then dpy := -dpy;

  px2 := px2 +  dpx2;
  py2 := py2 +  dpy2;
  if (px2<0) or (px2> FDisplayBuffer.MaxWidth) then dpx2 := -dpx2;
  if (py2<0) or (py2> FDisplayBuffer.MaxHeight)then dpy2 := -dpy2;

  px3 := px3 +  dpx3;
  py3 := py3 +  dpy3;
  if (px3<0) or (px3> FDisplayBuffer.MaxWidth) then dpx3 := -dpx3;
  if (py3<0) or (py3> FDisplayBuffer.MaxHeight)then dpy3 := -dpy3;

  if (A>240) then AD := -AD;
  if (A<60) then AD := -AD;
  A := A + AD;

  if DisplayHelp then
  begin
    With FDisplayBuffer.Canvas do
    begin
      Brush.Color := clrWhite;
      Brush.Color.Alpha := 92;
      Brush.Style := bsSolid;
      Pen.Color := clrBlack;
      Pen.Style := ssSolid;
      DrawMode.AlphaMode := amAlpha;

      Rectangle(4,4,580,100);
      Font.Size := 12;
      Font.Bold := True;
      DrawMode.AlphaMode := amNone;
      TextOut(10,20,'Utilisez la souris pour déplacer la lentille');
      TextOut(10,34,'Utilisez la molette pour agrandir ou réduire la lentille');
      TextOut(10,48,'Utilisez les touche + et - pour controler le facteur de zoom de la lentille');
      TextOut(10,62,'Tapez 0, 1, 2, 3, 4 pour changer de mode');
      TextOut(10,78,'Utilisez les touche A et Z pour controler le facteur supplémentaire (Mode 1, 3 et 4)');
      TextOut(10,92,'Tapez H pour afficher/cacher cette aide');
    end;
  end;
end;

end.

