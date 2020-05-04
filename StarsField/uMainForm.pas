unit uMainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs,
  BZMath, BZVectorMath, BZColors, BZGraphic, BZBitmap, BZCadencer, BZStopWatch;

Const
  cFormCaption : String = 'BZScene Stars Field Demo';
  cMaxStars : Integer = 499;
  cStarFieldZFactor : Integer = 1;

type

  { TMainForm }
  TStarRec = record
    x,y,z : Integer;
  end;

  TStarFieldMode = (sfmVertical, sfmHorizontal, sfm3D, sfmWormHole, sfmWormHoleMoving, sfmPlane, sfmPlaneUp,sfmPerspectivePlane, sfmDoublePlane, sfmSinusPlane, sfmDoubleSinusPlane, sfmPlaneDown);

  TMainForm = class(TForm)
    procedure FormCloseQuery(Sender : TObject; var CanClose : boolean);
    procedure FormCreate(Sender : TObject);
    procedure FormShow(Sender : TObject);
    procedure FormDestroy(Sender : TObject);
    procedure FormKeyPress(Sender : TObject; var Key : char);
  private
    FCadencer : TBZCadencer;
    FStopWatch : TBZStopWatch;
    FDisplayBuffer : TBZBitmap;

    FStarsPos : array[0..499] of TStarRec;
    FStarsSpeed : array[0..499] of Word;
    FFrameCounter : DWord;
    FStarFieldMode : TStarFieldMode;
    FAngle, FBaseLine, FThresOld, FDeltaAngle, FDeltaBaseLine : Integer;

    procedure InitStarsField3D;
    Procedure RenderStarsField3D(Angle : Integer; BaseLine : Integer; ThresOld : Integer);
    procedure DisplayHelp;
    procedure CadencerProgress(Sender : TObject; const deltaTime, newTime : Double);
  public
    procedure InitEngine;
    procedure StartEngine;
    procedure StopEngine;
    procedure DoneEngine;
    procedure InitScene;
    procedure InitColorMap;
    procedure RenderScene;
    procedure DoneScene;
  end;

var
  MainForm : TMainForm;

implementation

{$R *.lfm}

{ TMainForm }
Uses
  BZLogger;

procedure TMainForm.FormCloseQuery(Sender : TObject; var CanClose : boolean);
begin
  StopEngine;
  CanClose := True;
end;

procedure TMainForm.FormCreate(Sender : TObject);
begin
  InitEngine;
  InitScene;
end;

procedure TMainForm.FormShow(Sender : TObject);
begin
  StartEngine;
end;

procedure TMainForm.FormDestroy(Sender : TObject);
begin
  DoneScene;
  DoneEngine;
end;

procedure TMainForm.FormKeyPress(Sender : TObject; var Key : char);
begin
  Case Key of
    '0' : FStarFieldMode := sfm3D;
    '1' : FStarFieldMode := sfmWormHole;
    '2' : FStarFieldMode := sfmWormHoleMoving;
    '3' : FStarFieldMode := sfmPlane;
    '4' : FStarFieldMode := sfmPerspectivePlane;
    '5' : FStarFieldMode := sfmPlaneUp;
    '6' : FStarFieldMode := sfmSinusPlane;
    '7' : FStarFieldMode := sfmDoublePlane;
    '8' : FStarFieldMode := sfmDoubleSinusPlane;
    #27 : Close;
//    '9' : FStarFieldMode := sfmDown;
  end;
 // FDisplayBuffer.Clear(clrBlack);
  InitStarsField3D;
end;

procedure TMainForm.InitStarsField3D;
Var
  i : Word;
begin
  Randomize;
  For i:=0 to cMaxStars do
  begin
     Case FStarFieldMode of
       sfmVertical :;
       sfmHorizontal :;
       sfm3D, sfmWormHole, sfmWormHoleMoving :
       begin
         // 3D Hole
         FStarsPos[i].x := Random((FDisplayBuffer.CenterX+1)) - ((FDisplayBuffer.CenterX+1) shr 1);
         FStarsPos[i].y := Random((FDisplayBuffer.CenterY+1)) - ((FDisplayBuffer.CenterY+1) shr 1);
         FStarsPos[i].z := Random(100) + cStarFieldZFactor;
       end;
       sfmPlane, sfmPerspectivePlane, sfmDoublePlane, sfmSinusPlane, sfmDoubleSinusPlane, sfmPlaneUp :
       begin
         // 3D Plane
         FStarsPos[i].x := Random((FDisplayBuffer.Width-20)) - (FDisplayBuffer.CenterX-20);
         FStarsPos[i].y := 85; //(FDisplayBuffer.Height-10);
         FStarsPos[i].z := Random(100) + cStarFieldZFactor;  //100;
       end;
     end;
     FStarsSpeed[i] := Random(2)+1;
  end;

end;

procedure TMainForm.RenderStarsField3D(Angle : Integer; BaseLine : Integer; ThresOld : Integer);
Const
  Dist = 200;
  cStarsDensity = 180;
Var
  x,y, i, j, ColLevel, radius, dst : Integer;
  Col : TBZColor;

  procedure NewStarA(num : Integer; aBaseLine : Integer);
  Var a : Single;
  begin
    Case FStarFieldMode of
      sfm3D :
      begin
        // 3D  Center
        FStarsPos[Num].x := Random((FDisplayBuffer.CenterX+1)) - ((FDisplayBuffer.CenterX+1) shr 1);
        FStarsPos[Num].y := Random((FDisplayBuffer.CenterY+1)) - ((FDisplayBuffer.CenterY+1) shr 1);
        FStarsPos[Num].z := Random(200) + cStarFieldZFactor; // 100
        // 3D Center 2
        //FStarsPos[Num].x := Random((FDisplayBuffer.Width)) - Dist;
        //FStarsPos[Num].y := Random((FDisplayBuffer.Height)) - Dist;
      end;
      sfmWormHole :
      begin
        // 3d WormHole fixe
        a:=random*6.283;
        FStarsPos[Num].X:=trunc(50*cos(a));
        FStarsPos[Num].Y:=trunc(50*sin(a));
        FStarsPos[Num].z := Random(200) + cStarFieldZFactor; // 100
      end;
      sfmWormHoleMoving :
      begin
        // 3D Moving WormHole
        a:=random*6.283;
        FStarsPos[Num].X:=trunc(50*cos(a)+30*cos(FFrameCounter/256));
        FStarsPos[Num].Y:=trunc(50*sin(a)+30*sin(FFrameCounter/64));
        FStarsPos[Num].z := Random(200) + cStarFieldZFactor; // 100
      end;
      sfmPlane :
      begin
        // 3D Plane Down to top full line
        FStarsPos[Num].x := Random((FDisplayBuffer.Width-20)) - (FDisplayBuffer.CenterX-20);
        FStarsPos[Num].y := aBaseLine;
        FStarsPos[Num].z := Random(200) + cStarFieldZFactor; // 100
      end;
      sfmPerspectivePlane :
      begin
        // 3D Plane up to down perspective
        FStarsPos[Num].x := 20 + (Random((FDisplayBuffer.CenterX-20)) - (FDisplayBuffer.CenterX shr 1)-20);
        FStarsPos[Num].y := aBaseLine; // Copris entre -100 et 100. 0 = milieu de l'ecran
        FStarsPos[Num].z := Random(200) + cStarFieldZFactor; // 100
      end;
      sfmPlaneUp :
      begin
        // 3D Plane Up
        FStarsPos[Num].x := (Random((FDisplayBuffer.Width-20) - BaseLine * 4) - ((FDisplayBuffer.CenterX -20 ) - (aBaseLine * 2)));
        FStarsPos[Num].y := aBaseLine; // Copris entre -100 et 100. 0 = milieu de l'ecran
        FStarsPos[Num].z := Random((BaseLine Shr 1)-10) + ((BaseLine Shr 1)-10) + 100;
      end;
      sfmDoublePlane :
      begin
        // 3D Double Plane
        FStarsPos[Num].x := Random((FDisplayBuffer.Width-20)) - (FDisplayBuffer.CenterX-20);
        FStarsPos[Num].y:= ((FStarsPos[Num].x*Angle) div FDisplayBuffer.CenterY) + 20;
        FStarsPos[Num].z := Random(200) + cStarFieldZFactor; // 100
      end;
      sfmSinusPlane, sfmDoubleSinusPlane :
      begin
        // 3D Sinus Plane
        FStarsPos[Num].x := Random((FDisplayBuffer.Width-20)) - (FDisplayBuffer.CenterX-20);
        FStarsPos[Num].y:=trunc(aBaseLine*Sin((Angle/cStarsDensity))*Cos((FStarsPos[Num].x/cStarsDensity)+1)+aBaseline); //85 = Haut
        FStarsPos[Num].z := Random(200) + cStarFieldZFactor; // 100
      end;
      sfmPlaneDown :
      begin

      end;

    end;
  end;


  procedure NewStarB(num : Integer; aBaseLine : Integer);
  begin
    Case FStarFieldMode of
      sfmPerspectivePlane :
      begin
        // 3D Plane up to down perspective
        FStarsPos[Num].x := 20 + (Random((FDisplayBuffer.CenterX-20)) - (FDisplayBuffer.CenterX shr 1)-20);
        FStarsPos[Num].y := aBaseLine; // Copris entre -100 et 100. 0 = milieu de l'ecran
        FStarsPos[Num].z := Random(200) + cStarFieldZFactor; // 100
      end;
      sfmPlaneUp :
      begin
        // 3D Plane Up
        FStarsPos[Num].x := (Random((FDisplayBuffer.Width-20) - BaseLine * 4) - ((FDisplayBuffer.CenterX -20 ) - (aBaseLine * 2)));
        FStarsPos[Num].y := aBaseLine; // Copris entre -100 et 100. 0 = milieu de l'ecran
        FStarsPos[Num].z := Random((BaseLine Shr 1)-10) + ((BaseLine Shr 1)-10) + 100;
      end;
      sfmDoublePlane :
      begin
        // 3D Double Plane
        FStarsPos[Num].x := Random((FDisplayBuffer.Width-20)) - (FDisplayBuffer.CenterX-20);
        FStarsPos[Num].y:= ((FStarsPos[Num].x*Angle) div FDisplayBuffer.CenterY) - 20;
        FStarsPos[Num].z := Random(200) + cStarFieldZFactor; // 100
      end;
      sfmDoubleSinusPlane :
      begin
        // 3D Sinus Plane
        FStarsPos[Num].x := Random((FDisplayBuffer.Width-20)) - (FDisplayBuffer.CenterX-20);
        FStarsPos[Num].y:=trunc(-aBaseLine*Sin((FFrameCounter/cStarsDensity))*Cos((FStarsPos[Num].x/cStarsDensity)+1)-aBaseLine); //85 = Haut
        FStarsPos[Num].z := Random(200) + cStarFieldZFactor; // 100
      end;

      sfmPlaneDown :
      begin

      end;

    end;
  end;


begin
  //for j:= 0 to 31 do
  //begin
  for I := 0 to cMaxStars do
  begin
    //inc(FStarsPos[I].Z,FStarsSpeed[I]);
    FStarsPos[I].Z := FStarsPos[I].Z + FStarsSpeed[I];
    //if FStarsPos[I].Z > 400 then NewStarA(I, BaseLine);
    //FStarsSpeed[I] := 1; //(1+(FStarsPos[I].Z div 25))*(1+(5-(abs(FStarsPos[I].X*FStarsPos[I].Y) div 50)));
    dst := Dist  - FStarsPos[I].Z;
    if (Dst <> 0) then
    begin
      x := Dist*FStarsPos[I].X div Dst;
      y := Dist*FStarsPos[I].Y div Dst;
    end
    else NewStarA(I, BaseLine);

    if (FStarFieldMode = sfmDoublePlane) or (FStarFieldMode = sfmDoubleSinusPlane) then
    begin
      // DoublePlane
      if (X < -FDisplayBuffer.CenterX) or (Y < -FDisplayBuffer.CenterY)  then
      begin
        NewStarA(I, BaseLine);
      end;
      if (X > FDisplayBuffer.CenterX) or (Y > FDisplayBuffer.CenterY)  then
      begin
        NewStarB(I, BaseLine);
      end;
      X := X + FDisplayBuffer.CenterX;
      Y := Y + FDisplayBuffer.CenterY;
      ColLevel :=  FStarsPos[I].Z;
      if ColLevel < 8 then ColLevel := 8;
      Col.Create(ColLevel,ColLevel,ColLevel);
      Radius := ColLevel div 32;
      if (Radius <= 8) and (Radius > 0) then
      begin
        With FDisplayBuffer.Canvas do
        begin
          Pen.Style := ssClear;
          Brush.Color := Col;
          Brush.Style := bsSolid;
          Circle(x,y,Radius);
        end;
      end;
      //FDisplayBuffer.SetPixel(x,y,col);
    end
    else if (FStarFieldMode = sfmPlaneUp) then
    begin
      // PlaneUp
      if (X < -FDisplayBuffer.CenterX) or (X > FDisplayBuffer.CenterX) or
         (Y < -FDisplayBuffer.CenterY) or (Y > FDisplayBuffer.CenterY) then NewStarA(I, BaseLine);

      if (X > -FDisplayBuffer.CenterX) or (X < FDisplayBuffer.CenterX) or
         (Y > -FDisplayBuffer.CenterY) or (Y < FDisplayBuffer.CenterY) then
      begin
        X := X + FDisplayBuffer.CenterX;
        Y := Y + FDisplayBuffer.CenterY;
        ColLevel :=  FStarsPos[I].Z;
        if ColLevel < 8 then ColLevel := 8;
        Col.Create(ColLevel,ColLevel,ColLevel);
        Radius := ColLevel div 32;
        if (Radius <= 8) and (Radius > 0) then
        begin
          With FDisplayBuffer.Canvas do
          begin
            Pen.Style := ssClear;
            Brush.Color := Col;
            Brush.Style := bsSolid;
            Circle(x,y,Radius);
          end;
        end;
        //FDisplayBuffer.SetPixel(x,y,col);
      end;
    end
    else if (FStarFieldMode = sfmPlaneDown) then
    begin
      // Descente
      //if (xe>-159) and (xe<159) and (ye>-99) and (ye<99)
      //                  then AffichePixel(seg(video^),xe,ye,z[i])
      //                  else if i<maxsame then Nouvelle(i)
      //                                    else begin
      //                                              z[i]:=z[i]+2;
      //                                              y[i]:=y[i]+1;
      //                                         end;
    end
    else
    begin
      X := X + FDisplayBuffer.CenterX;
      Y := Y + FDisplayBuffer.CenterY;
      ColLevel :=  FStarsPos[I].Z;
      if ColLevel < 8 then ColLevel := 8;
      Col.Create(ColLevel,ColLevel,ColLevel);
      Radius := ColLevel div 32;
      if (Radius <= 8) and (Radius > 0) then
      begin
        With FDisplayBuffer.Canvas do
        begin
          Pen.Style := ssClear;
          Brush.Color := Col;
          Brush.Style := bsSolid;
          Circle(x,y,Radius);
        end;
      end;
      //FDisplayBuffer.SetPixel(x,y,col);
    end;
  end;

  //end;
end;

procedure TMainForm.DisplayHelp;
begin
  With FDisplayBuffer.Canvas do
  begin
    Font.Color := clrWhite;
    Font.Size := 12;
    TextOut(10,20,'Tap 0,1,2,3,4,5,6,7,8 Key to change StarField Mode');
  end;
end;

procedure TMainForm.CadencerProgress(Sender : TObject; const deltaTime, newTime : Double);
begin
  RenderScene;
  DisplayHelp;
  FDisplayBuffer.DrawToCanvas(Canvas, ClientRect);
  Caption:=cFormCaption + ' : '+Format('%.*f FPS', [3, FStopWatch.getFPS]);
end;

procedure TMainForm.InitEngine;
begin
  FCadencer := TBZCadencer.Create(self);
  FCadencer.Enabled := False;
  FCadencer.OnProgress := @CadencerProgress;
  FStopWatch := TBZStopWatch.Create(self);
  FDisplayBuffer := TBZBitmap.Create(Width, Height);
  FDisplayBuffer.Clear(clrBlack);
end;

procedure TMainForm.StartEngine;
begin
  DoubleBuffered:=true;
  FStopWatch.Start;
  FCadencer.Enabled := True;
end;

procedure TMainForm.StopEngine;
begin
  FStopWatch.Stop;
  FCadencer.Enabled := False;
end;

procedure TMainForm.DoneEngine;
begin
  FreeAndNil(FStopWatch);
  FreeAndNil(FCadencer);
  FreeAndNil(FDisplayBuffer);
end;

procedure TMainForm.InitScene;
// Initalisation des données de la scene
begin
  InitStarsField3D;
  FFrameCounter := 0;
  FStarFieldMode := sfm3D;
  FAngle := 360;
  FBaseLine := 100;
  FThresOld := cMaxStars - 300;
  FDeltaAngle := 1;
  FDeltaBaseLine := 10;
  InitColorMap;
end;

procedure TMainForm.InitColorMap;
// Initalisation de la palette de couleur, si utilisée
begin
 // Vide
end;

procedure TMainForm.RenderScene;
// Coeur du rendu de la scene
begin
  //if ((FFrameCounter Mod 60) = 0) then
  FDisplayBuffer.Clear(clrBlack);
  Case FStarFieldMode of
    sfmDoublePlane, sfmDoubleSinusPlane, sfmSinusPlane :
    begin
      FBaseLine := 85;
      FAngle := FAngle + FDeltaAngle;
      if (FAngle > 720) or (FAngle < -720) then FDeltaAngle := - FDeltaAngle;
    end;
    sfmPlane, sfmPerspectivePlane, sfmPlaneUp :
    begin
      if FStarFieldMode =sfmPlaneUp then FBaseLine := 25;
      //if (FFrameCounter Mod 35) = 0 then FBaseLine := FBaseLine + FDeltaBaseLine;
      if (FBaseLine > 95) or (FBaseLine < -95) then FDeltaBaseLine := - FDeltaBaseLine;
    end;
  end;
  RenderStarsField3D(FAngle, FBaseLine, FThresOld);
  Inc(FFrameCounter);
end;

procedure TMainForm.DoneScene;
// Finalisation de la scene
begin
  // Vide
end;

end.

