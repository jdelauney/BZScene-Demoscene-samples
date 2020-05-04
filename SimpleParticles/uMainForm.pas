unit uMainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs,
  BZMath, BZVectorMath, BZColors, BZGraphic, BZBitmap, BZCadencer, BZStopWatch;

Const
  cFormCaption : String = 'BZScene Simple particles Demo';
  cPARTICLE_AMOUNT = 499;
  cDARK_PARTICLES = 50;

type
  TSimpleParticle=record
    X,Y,
    SpeedX,SpeedY,
    Angle,MaxAngle,
    GravityX,
    GravityY,
    VelocityX,
    VelocityY:single;
  end;

type

  { TMainForm }

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

    FParticles : Array[0..cPARTICLE_AMOUNT] of TSimpleParticle;
    FDirection : integer;
    FBLUR_DECAY,FBLUR_SPREAD : integer;
    FPARTICLE_EFFECT : integer;
    FX1, FY1, FAngle : Integer;
    FSpeedX, FSpeedY : Integer;
    FStop:Boolean;
    procedure CadencerProgress(Sender : TObject; const deltaTime, newTime : Double);
  public
    procedure InitEngine;
    procedure StartEngine;
    procedure StopEngine;
    procedure DoneEngine;
    procedure InitScene;
    procedure InitColorMap;
    procedure RenderScene(NewTime : Double);
    procedure DoneScene;
  end;

var
  MainForm : TMainForm;

implementation

{$R *.lfm}

{ TMainForm }

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
  if key = #27 then Close;
  if Key = '1' then FPARTICLE_EFFECT:=1;
  if Key = '2' then FPARTICLE_EFFECT:=2;
  if Key = '3' then FPARTICLE_EFFECT:=3;
  if Key = '4' then FPARTICLE_EFFECT:=4;
  if Key = '5' then FPARTICLE_EFFECT:=5;
  if Key = '6' then FPARTICLE_EFFECT:=6;
  if Key = '7' then FPARTICLE_EFFECT:=7;
  if Key = '8' then FPARTICLE_EFFECT:=8;
  if Key = '9' then FPARTICLE_EFFECT:=9;
  if Key = '0' then FPARTICLE_EFFECT:=0;
  if key = ' ' then
  begin
    FStop:=Not(FStop);
    if not(FStop) then
    begin
      //BZStopWatch1.Start;
     // FrameCounter:=0;
      //DoTheParticles;
    end;
  end
end;

procedure TMainForm.CadencerProgress(Sender : TObject; const deltaTime, newTime : Double);
Var
  Effect : String;
begin
  RenderScene(NewTime);
  With FDisplayBuffer.Canvas do
  begin
   Font.Color:=clrWhite;
   Font.Size := 12;
   Font.Bold := False;
   TextOut(FDisplayBuffer.CenterX-200,FDisplayBuffer.MaxHeight - 24,'Touches, 0,1,2,3,4,5,6,7,8,9 pour changer d''effets - ESC pour quitter');
   Case FParticle_effect of
     0: effect:='Weird "twirl"';
     1: effect:='Water Bubble';
     2: effect:='Followers';
     3: effect:='Distort Flight';
     4: effect:='Rotating';
     5: effect:='Christmas powder';
     6: effect:='Random walk';
     7: effect:='Nova Blast';
     8: effect:='Scotish Square';
     9: effect:='Smoke';
   end;
   Font.Color:=clrYellow;
   Font.Size := 16;
   Font.Bold := true;
   TextOut(FDisplayBuffer.CenterX-50,24,effect);
  end;
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
Var
  i:integer;
begin
  Randomize;


  FDisplayBuffer.UsePalette := True;
  With FdisplayBuffer.ColorManager do
  begin
   CreateGradientColor(BZColor(0,0,0,0),0.0);
   CreateGradientColor(BZColor(0,0,128,255),0.25);
   CreateGradientColor(BZColor(160,64,0,255),0.50);
   CreateGradientColor(BZColor(192,192,128,255),0.75);
   CreateGradientColor(BZColor(255,255,255,255),1.0);
  End;
  FDisplayBuffer.ColorManager.MakeGradientPalette();

  FX1 := FDisplayBuffer.CenterX;
  FY1 := FDisplayBuffer.CenterY;
  FBLUR_DECAY := 1;
  FBLUR_SPREAD := 4;
  FPARTICLE_EFFECT := 0;
  FAngle := 2;
  FSpeedX := 1;
  FSpeedY := 1;

  For i:=0 to cPARTICLE_AMOUNT do
  begin
    With FParticles[I] do
    begin
     X := FX1;
     Y := FY1;
     SpeedX := 10;
     SpeedY := 10;
     Angle := 5;
     MaxAngle := 360;
     GravityX := 0;
     GravityY := 0;
     VelocityX := 4;
     VelocityY := 4;
    End;
  end;

  InitColorMap;
end;

procedure TMainForm.InitColorMap;
begin
  // Initalisation de la palette de couleur, si utilisée
end;

procedure TMainForm.RenderScene(NewTime : Double);
// Coeur du rendu de la scene
var
    j,k:integer;
    RX,RY:Integer;
    currentcolor:integer;
    Row1 : Integer;
    //Row2A,Row2B,Row2C:Integer;

    c : Integer;
begin

  FDisplayBuffer.Clear(0);

  if FPARTICLE_EFFECT=9 then
  begin
    FX1 := FDisplayBuffer.CenterX;
    FY1 := FDisplayBuffer.MaxHeight;
  end
  else
  begin
    FX1 := FDisplayBuffer.CenterX;// + Round(Sin(NewTime*c2PI*150)*45);
    FY1 := FDisplayBuffer.CenterY;// + Round(Cos(NewTime*c2PI*100)*25);
  end;

  For j:=0 to cPARTICLE_AMOUNT do
  begin
    With FParticles[j] do
    begin
       X := X + SpeedX;
       Y := Y + SpeedY;
       Case FPARTICLE_EFFECT of
          0:
          begin  /// Weird "twirl" effect
            SpeedX := SpeedX + (SpeedY/50);
            SpeedY := SpeedY - (SpeedX/FX1);
          end;
          1:
          begin  /// Water bubbles
            If random(100)<20 then
            begin
              SpeedX := (X-FX1)/10;
              SpeedY := (Y-FY1)/10;
            end;
            If random(100)<10 then
            begin
              X := random(FDisplayBuffer.MaxWidth);
              Y := random(FDisplayBuffer.MaxHeight);
              SpeedX := (X-FX1)/10;
              SpeedY := (Y-FY1)/10;
            end;
          end;
          2:
          begin  /// Followers
            If X>FX1 then SpeedX := SpeedX-0.1*ABS(X-FX1)/(ABS(X-FX1)+ABS(Y-FY1));
            If X<FX1 then SpeedX := SpeedX+0.1*ABS(X-FX1)/(ABS(X-FX1)+ABS(Y-FY1));
            If Y>FY1 then SpeedY := SpeedY-0.1*ABS(Y-FY1)/(ABS(X-FX1)+ABS(Y-FY1));
            If Y<FY1 then SpeedY := SpeedY+0.1*ABS(Y-FY1)/(ABS(X-FX1)+ABS(Y-FY1));
          end;
          3:
          begin  /// Distorted Flight
            X := X + Sin((45*Y)/c180DivPi);
            Y := Y + Cos((45*X)/c180DivPi);
            SpeedX :=((X-(FDisplayBuffer.CenterX))/40)+((random(401)-100)/50);
            SpeedY :=((Y-(FDisplayBuffer.CenterY))/40)+((random(401)-100)/50);
          end;
          4:
          begin  /// Rotating
            X:= x + Sin((15*Y)/c180DivPi);
            Y:= y + Cos((15*X)/c180DivPi);
          end;
          5:
          begin  /// Christmas powder
            If random(200)<1 then SpeedX := (SpeedX/8);
            If random(200)<1 then SpeedY := (SpeedY/8);
          end;
          6:
          begin  /// Random walk
            If random(200)<1 then SpeedX := -SpeedX;
            If random(200)<1 then SpeedY := -SpeedY;
          end;
          7:
          begin  /// Nova blast
            SpeedX := SpeedX + (SpeedX/150);
            SpeedY := SpeedY + 0.00666; // (1/150);
          end;
          8:
          begin  /// Scotish Square
            X := X + Cos((45*X)/c180DivPi);
            Y := Y + Sin((45*Y)/c180DivPi);
            SpeedX := ((X - FDisplayBuffer.CenterX) / 40) + ((random(401) - 200) / 50);
            SpeedY := ((Y - FDisplayBuffer.CenterY) / 40) + ((random(401) - 200) / 50);
          end;
          9:
          begin  /// Smoke
            X := x + Sin((5*Y)/c180DivPi) + random(4);
            Y := y + Cos((5*X)/c180DivPi) - random(6);
          end;

        end;

        If (X<1) or (X>Width-2) or (Y<1) or (Y>Height-2) or (RANDOM(200)<1) then
        begin
          X := FX1;
          Y := FY1;
          SpeedX := (random(100)/100)+0.01;
          If random(101)<50 then SpeedX := -SpeedX;
          SpeedY := (random(100)/100)+0.01;
          If random(101)<50 then SpeedY := -SpeedY;
        end;
        CurrentColor := 255;//Random(255);
        if CurrentColor> 0 then FDisplayBuffer.Colors[round(x),round(y)] := CurrentColor;
      end;
    end;

  For j:=0 to cDARK_PARTICLES do
  begin
    Rx := random(FDisplayBuffer.MaxWidth);
    Ry := random(FDisplayBuffer.MaxHeight);
    CurrentColor:= FDisplayBuffer.Colors[Rx,Ry];
    if CurrentColor>0 then CurrentColor := CurrentColor-random(10);
    If CurrentColor<0 then CurrentColor:=0;
    FDisplayBuffer.Colors[Rx,Ry]:=CurrentColor; // on bloucle dans la palette
  end;

  If FDirection>1 then FDirection:=-1;

  For j:=1 to FDisplayBuffer.Height-2 do
  begin
    Row1 := j + FDirection;
    //Row2A := j - 1;
    //Row2B := j;
    //Row2C := j + 1;

    For k := 1 to FDisplayBuffer.Width-2 do
    begin
      c := FDisplayBuffer.Colors[k,j];
      //CurrentColor := ClampByte(ROUND((FDisplayBuffer.Colors[k-1,Row2A] +
      //                                 FDisplayBuffer.Colors[k,Row2A] +
      //                                 FDisplayBuffer.Colors[k+1,Row2A] +
      //                                 FDisplayBuffer.Colors[k-1,Row2B] +
      //                                 (FBLUR_SPREAD*FDisplayBuffer.Colors[k,Row2B]) +
      //                                 FDisplayBuffer.Colors[k+1,Row2B] +
      //                                 FDisplayBuffer.Colors[k-1,Row2C] +
      //                                 FDisplayBuffer.Colors[k,Row2C] +
      //                                 FDisplayBuffer.Colors[k+1,Row2C]) / (8+FBLUR_SPREAD)));

      //CurrentColor := ClampByte(ROUND(FDisplayBuffer.Colors[k,j]+FDisplayBuffer.Colors[k+1,j]+FDisplayBuffer.Colors[k-1,j]+FDisplayBuffer.Colors[k,j+1] / FBLUR_SPREAD));
      if c>0 then
      begin
        CurrentColor := ClampByte(ROUND((FBLUR_SPREAD*c+FDisplayBuffer.Colors[k+1,j]+FDisplayBuffer.Colors[k-1,j]+FDisplayBuffer.Colors[k,j+1]) / (4+FBLUR_SPREAD)));
        // Fade out pixel
        If CurrentColor>FBLUR_DECAY then Dec(CurrentColor,FBLUR_DECAY) else CurrentColor:=0;
        //if currentcolor<0 then CurrentColor:=0;
        if currentcolor>0 then FDisplayBuffer.Colors[k,Row1] := CurrentColor;
      end;
    end;
  end;

end;

procedure TMainForm.DoneScene;
begin
  // Finalisation de la scene
end;

end.

