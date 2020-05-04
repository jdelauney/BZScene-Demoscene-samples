unit uMainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs,
  BZMath, BZVectorMath, BZColors, BZGraphic, BZBitmap, BZCadencer, BZStopWatch, {%H-}BZBitmapIO,
  BZScreenMode;

Const
  cFormCaption : String = 'BZScene Demo';
  cDistLUTSize = 512;

type

  { TMainForm }

  TMainForm = class(TForm)
    BZScreenMode : TBZScreenMode;
    procedure FormCloseQuery(Sender : TObject; var CanClose : boolean);
    procedure FormCreate(Sender : TObject);
    procedure FormShow(Sender : TObject);
    procedure FormDestroy(Sender : TObject);
    procedure FormKeyPress(Sender : TObject; var Key : char);
    procedure BZScreenModeAfterRestoreVideoMode(Sender : TObject);
    procedure BZScreenModeBeforeChangeVideoMode(Sender : TObject; {%H-}ScreenWidth, {%H-}ScreenHeight, {%H-}ScreenDepth, {%H-}ScreenFrequency : Integer; {%H-}VideoModeIndex : TBZScreenModeResolution);
  private
    FCadencer : TBZCadencer;
    FStopWatch : TBZStopWatch;
    FDisplayBuffer : TBZBitmap;
    //FFullScreen : Boolean;
    FTextureMap : TBZBitmap;

    FPos : TBZPoint;
    FSpeedStep : Word;
    FBias, FAmp : Single;
    FDistortMethod : Byte;

    procedure DistortX(Source: TBZBitmap; px,py : Integer; Step, Bias, Amp : Single);
    procedure DistortY(Source: TBZBitmap; px,py : Integer; Step, Bias, Amp : Single);
    procedure DistortXY(Source: TBZBitmap; px,py : Integer; StepX, BiasX, AmpX,StepY, BiasY, AmpY : Single);

    procedure CadencerProgress(Sender : TObject; const {%H-}deltaTime, newTime : Double);
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
  BZScreenMode.RestoreVideoMode;
  //WindowState:=wsNormal;
  //Position:=poDesigned;
  //FormStyle:=fsNormal;
  //BorderStyle:=bsSizeable;
  //RestoreDefaultScreenMode;
  //FFullScreen:=False;
  CanClose := True;
end;

procedure TMainForm.FormCreate(Sender : TObject);
begin
  InitEngine;
  InitScene;
end;

procedure TMainForm.FormShow(Sender : TObject);
//var
//  idx : Integer;
begin
  StartEngine;
  BZScreenMode.ChangeVideoMode;
  //FFullScreen:=False;
  //idx := GetIndexFromResolution(1024,768,32);
  //if idx<>0 then
  //begin
  //  if SetFullscreenMode(idx,60) then
  //  begin
  //     BorderStyle:=bsNone;
  //     FormStyle:=fsSystemStayOnTop;
  //     WindowState:=wsMaximized; //wsFullScreen;
  //     FFullScreen:=True;
  //  end;
  //end;
end;

procedure TMainForm.FormDestroy(Sender : TObject);
begin
  DoneScene;
  DoneEngine;
end;

procedure TMainForm.FormKeyPress(Sender : TObject; var Key : char);
begin
  if Key='2' then FSpeedStep := FSpeedStep + 10;
  if Key='1' then FSpeedStep := FSpeedStep - 10;
  if Key='*' then FBias := FBias + 0.5;
  if Key='/' then FBias := FBias - 0.5;
  if Key='+' then FAmp := FAmp + 0.5;
  if Key='-' then FAmp := FAmp - 0.5;
  if UpCase(Key)='A' then FDistortMethod := 0;
  if UpCase(Key)='B' then FDistortMethod := 1;
  if UpCase(Key)='C' then FDistortMethod := 2;
  if Key=#27 then close;
end;

procedure TMainForm.BZScreenModeAfterRestoreVideoMode(Sender : TObject);
begin
  BZScreenMode.ShowMouseCursor;
end;

procedure TMainForm.BZScreenModeBeforeChangeVideoMode(Sender : TObject; ScreenWidth, ScreenHeight, ScreenDepth, ScreenFrequency : Integer; VideoModeIndex : TBZScreenModeResolution);
begin
  BZScreenMode.HideMouseCursor;
end;

procedure TMainForm.DistortX(Source : TBZBitmap; px, py : Integer; Step, Bias, Amp : Single);
Var
  y : Integer;
begin
  for y:= 0 to Source.MaxHeight do
  begin
    FDisplayBuffer.CopyBlock(Source,0, y, Source.Width, 1, Round(px+Sin(cPi*(y+Step)/Bias)*Amp) , py+y, True);
  end;
end;

procedure TMainForm.DistortY(Source : TBZBitmap; px, py : Integer; Step, Bias, Amp : Single);
Var
  x : Integer;
begin
  for x:= 0 to Source.MaxWidth do
  begin
    FDisplayBuffer.CopyBlock(Source,x, 0, 1,Source.Height, px+x,Round(py+Sin(cPi*(x+Step)/Bias)*Amp), True);
  end;
end;

procedure TMainForm.DistortXY(Source : TBZBitmap; px, py : Integer; StepX, BiasX, AmpX,StepY, BiasY, AmpY  : Single);
Var
  x,y, Dif : Integer;
  TempBuffer : TBZBitmap;
begin
  Dif := Round(AmpX);
  TempBuffer := TBZBitmap.Create(Round(Source.Width + Dif+Dif), Source.Height);
  TempBuffer.Clear(clrTransparent);
  for y:= 0 to Source.MaxHeight do
  begin
    TempBuffer.CopyBlock(Source,0, y, Source.Width, 1, Dif+Round(Sin(cPi*(y+StepX)/BiasX)*AmpX) , y, True);
  end;

  px := px - Dif;
  for x:= 0 to TempBuffer.MaxWidth do
  begin
    FDisplayBuffer.CopyBlock(TempBuffer,x, 0, 1,Source.Height, px+x,Round(py+Cos(cPi*(x+StepY)/BiasY)*AmpY), True);
  end;

  FreeAndNil(TempBuffer);

end;

procedure TMainForm.CadencerProgress(Sender : TObject; const deltaTime, newTime : Double);
var
  dm: string;
begin
  RenderScene(NewTime);

  if BZScreenMode.FullScreenMode then
  begin
    FDisplayBuffer.Canvas.TextOut(12,96,'FrameRate : '+Format('%.*f FPS', [3, FStopWatch.getFPS]));
  end
  else Caption:=cFormCaption + ' : '+Format('%.*f FPS', [3, FStopWatch.getFPS]);

  Case FDistortMethod of
    0 : DM:='X';
    1 : DM:='Y';
    2 : DM:='XY';
  end;
  With FDisplayBuffer.Canvas do
  begin
    Font.Size := 12;
    Font.Color := clrWhite;
    TextOut(12,16,'Use + and - to change Amplitude : '+FAmp.ToString);
    TextOut(12,32,'Use * and / to change Bias Divisor : '+ FBias.ToString);
    TextOut(12,48,'Use 1 and 2 to change Speed : '+FSpeedStep.ToString);
    TextOut(12,64,'Use A, B and C to Choose Distortion method : '+DM);
    TextOut(12,80,'ESC to exit');
  end;

  FDisplayBuffer.DrawToCanvas(Canvas, ClientRect);
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
  FreeAndNil(FTextureMap);
end;

procedure TMainForm.InitScene;
// Initalisation des données de la scene

begin

  FTextureMap := TBZBitmap.Create;
  FTextureMap.LoadFromFile('..\..\..\..\media\images\mangaka.png');

  FPos.X := FDisplayBuffer.CenterX - FTextureMap.CenterX;
  FPos.Y := FDisplayBuffer.CenterY - FTextureMap.CenterY;


  FSpeedStep := 100;
  FBias := 64;
  FAmp := 12;
  FDistortMethod :=0;

  InitColorMap;
end;

procedure TMainForm.InitColorMap;
begin
  // Initalisation de la palette de couleur, si utilisée
end;

procedure TMainForm.RenderScene(NewTime:Double);
begin
  // Coeur du rendu de la scene
  FDisplayBuffer.Clear(ClrBlack);
  Case FDistortMethod of
    0 :
    begin
      DistortX(FTextureMap, FPos.X, FPos.Y, (NewTime*FSpeedStep), FBias, FAmp);
    end;
    1 :
    begin
      DistortY(FTextureMap, FPos.X, FPos.Y, (NewTime*FSpeedStep), FBias, FAmp);
    end;
    2 :
    begin
      DistortXY(FTextureMap, FPos.X, FPos.Y, (NewTime*FSpeedStep), FBias, FAmp,(NewTime*(FSpeedStep/2)), FBias/2, FAmp/2);
    end;
  end;


end;

procedure TMainForm.DoneScene;
begin
  // Finalisation de la scene
end;

end.

