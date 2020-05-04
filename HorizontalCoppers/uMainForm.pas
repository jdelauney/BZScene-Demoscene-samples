unit uMainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs,
  BZMath, BZVectorMath, BZColors, BZGraphic, BZBitmap, BZCadencer, BZStopWatch;

Const
  cFormCaption : String = 'BZScene Horizontal Coppers Demo';

Type
  TCopper = record
    Y : Integer;
    Height : Integer;
    ColorTop : TBZColor;
    ColorMiddle : TBZColor;
    ColorBottom : TBZColor;
  end;
type

  { TMainForm }

  TMainForm = class(TForm)
    procedure FormCloseQuery(Sender : TObject; var CanClose : boolean);
    procedure FormCreate(Sender : TObject);
    procedure FormShow(Sender : TObject);
    procedure FormDestroy(Sender : TObject);
  private
    FCadencer : TBZCadencer;
    FStopWatch : TBZStopWatch;
    FDisplayBuffer : TBZBitmap;

    FCoppers : Array[0..9] Of TCopper;
    FAnimAng : Single;

    procedure RenderCopper(y, h : Integer; ColorTop, ColorMiddle, ColorBottom : TBZColor);

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

procedure TMainForm.CadencerProgress(Sender : TObject; const deltaTime, newTime : Double);
begin
  RenderScene;
  FDisplayBuffer.DrawToCanvas(Canvas, ClientRect);
  Caption:=cFormCaption + ' : '+Format('%.*f FPS', [3, FStopWatch.getFPS]);
end;

procedure TMainForm.RenderCopper(y, h : Integer; ColorTop, ColorMiddle, ColorBottom : TBZColor);
Var
  x, dh,py, sy, MaxH : Integer;
  OutColor, InColor : TBZColor;
  Delta, Step : Single;
  PixPtr : PBZColor;

begin
  dh := (H div 2)-1;
  sy := y;
  MaxH := sy + dh;
  Delta := 0;
  Step := 1.0 / (H * 0.5);
  //FDisplayBuffer.Canvas.DrawMode.PixelMode := dmSet;
  //FDisplayBuffer.Canvas.DrawMode.AlphaMode := amAlpha;

  For py := sy to MaxH do
  begin
    if (py>=0) and (py< FDisplayBuffer.Height) then
    begin
      OutColor := ColorTop.Lerp(ColorMiddle, Delta,0, itLinear,True);
      PixPtr := FDisplayBuffer.GetScanLine(py);
      //FillDWord(PixPtr^,FDisplayBuffer.Width, OutColor.AsInteger);
      For x := 0 to FDisplayBuffer.MaxWidth do
      begin
        PixPtr^:= PixPtr^.AlphaBlend(OutColor);
        Inc(PixPtr);
        //FDisplayBuffer.SetPixelAlphaBlend(x,py,OutColor);
        //FDisplayBuffer.Canvas.PutPixel(x,yy,OutColor);
      end;
    end;
    Delta := Delta + Step;
  end;

  sy := MaxH + 1;
  MaxH := sy + dh;
  Delta := 0;
  For py := sy to MaxH do
  begin
    if (py>=0) and (py< FDisplayBuffer.Height) then
    begin
      OutColor := ColorMiddle.Lerp(ColorBottom, Delta,0, itLinear,True);
      PixPtr := FDisplayBuffer.GetScanLine(py);
      For x := 0 to FDisplayBuffer.MaxWidth do
      begin
        PixPtr^:= PixPtr^.AlphaBlend(OutColor);
        Inc(PixPtr);
      end;
    end;
    Delta := Delta + Step;
  end;

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
  H,I : Integer;

begin
  Randomize;

  For I := 0 to 9 do
  begin
    H := 40+Random(40);
    FCoppers[I].Height := H;
    FCoppers[I].Y := H + Random(FDisplayBuffer.MaxHeight - H);
    Case I of
      6,2:
      Begin
        FCoppers[i].ColorTop := clrRed;
        FCoppers[i].ColorTop.Alpha := 32;
        FCoppers[i].ColorMiddle := clrFuchsia;
        FCoppers[i].ColorBottom := clrRed;
        FCoppers[i].ColorBottom.Alpha := 32;
      end;
      7,3 :
      Begin
        FCoppers[i].ColorTop := clrBlue;
        FCoppers[i].ColorTop.Alpha := 32;
        FCoppers[i].ColorMiddle := clrCyan;
        FCoppers[i].ColorBottom := clrBlue;
        FCoppers[i].ColorBottom.Alpha := 32;
      end;
      1,5 :
      Begin
        FCoppers[i].ColorTop := clrGreen;
        FCoppers[i].ColorTop.Alpha := 32;
        FCoppers[i].ColorMiddle := clrLime;
        FCoppers[i].ColorBottom := clrGreen;
        FCoppers[i].ColorBottom.Alpha := 32;
      end;
      4,0 :
      Begin
        FCoppers[i].ColorTop := clrGray;
        FCoppers[i].ColorTop.Alpha := 32;
        FCoppers[i].ColorMiddle := clrWhite;
        FCoppers[i].ColorBottom := clrGray;
        FCoppers[i].ColorBottom.Alpha := 32;
      end;
    end;
  end;
  FAnimAng := 0;
  InitColorMap;
end;

procedure TMainForm.InitColorMap;
// Initalisation de la palette de couleur, si utilisée
begin

end;

procedure TMainForm.RenderScene;
// Coeur du rendu de la scene
Var
  I : Integer;
begin
  FDisplayBuffer.Clear(clrBlack);
  For I := 0 to 9 do
  Begin
    FCoppers[i].Y := round(sin((FAnimAng+(20*I))*cPIDiv180) * FDisplayBuffer.CenterY + FDisplayBuffer.CenterY); //(FDisplayBuffer.MaxHeight - ((FCoppers[i].Height+1) * 2)));
    FAnimAng:= FAnimAng + 0.1;
    RenderCopper(FCoppers[I].Y, FCoppers[i].Height, FCoppers[i].ColorTop, FCoppers[i].ColorMiddle, FCoppers[i].ColorBottom);
  end;
end;

procedure TMainForm.DoneScene;
// Finalisation de la scene
begin
  // Ne fait rien
end;

end.

