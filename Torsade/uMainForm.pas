unit uMainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs,
  BZMath, BZVectorMath, BZColors, BZGraphic, BZBitmap, BZCadencer, BZStopWatch, BZBitmapIO;

Const
  cFormCaption : String = 'BZScene Torsade Demo';

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
    FTextureMapRecto : TBZBitmap;
    FTextureMapVerso : TBZBitmap;
    FSinLUT : Array[0..1023] of Integer;
    FAngle : Integer;
    FAdjustFactorHT : Single;
    FLeftPart, FRightPart : Integer;
    FXD, FYD : Integer;
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
  i : integer;
begin
  FTextureMapRecto := TBZBitmap.Create;
  FTextureMapRecto.LoadFromFile('..\..\..\..\media\images\LooneyBoy.png');
  FTextureMapVerso := TBZBitmap.Create;
  FTextureMapVerso.LoadFromFile('..\..\..\..\media\images\LooneyBoy02.png');
  for i := 0 to 1023 do
  begin
    FSinLUT[i] := Round(System.Sin(i*(360/1024)*cPiDiv180) * FTextureMapRecto.Width);
  end;
  FAdjustFactorHT := FTextureMapRecto.Height / (FDisplayBuffer.Height);
  //FAdjustFactorHT := FTextureMapRecto.Height / 392;
  FAngle := 0;
  FXD := FDisplayBuffer.CenterX - FTextureMapRecto.CenterX;
  FYD := FDisplayBuffer.CenterY - FTextureMapRecto.CenterY;
  FLeftPart := 0;
  FRightPart := FTextureMapRecto.MaxWidth;
  InitColorMap;
end;

procedure TMainForm.InitColorMap;
begin
  // Initalisation de la palette de couleur, si utilisée
end;

procedure TMainForm.RenderScene;
// Coeur du rendu de la scene
VAr
  y, lg,i,x : Integer;
  rp, irp  : single;
  DstPtr : PBZColor;
  OutColor : TBZColor;
begin
  FDisplayBuffer.Clear(clrBlack);
  for y := 0 to FTextureMapRecto.MaxHeight do
  begin
    lg := FSinLUT[(FAngle+Round(y*FAdjustFactorHT)) and 1023];
    if (lg > 0) then
    begin
      rp := (FTextureMapRecto.Width) / lg;
      DstPtr := FDisplayBuffer.GetPixelPtr(FXD + FTextureMapRecto.CenterX - ((LG-1) shr 1) ,FYD+Y);
      irp :=0;
      x :=FLeftPart;
      for i := 0 to lg-1 do
      begin
        OutColor :=  FTextureMapRecto.GetPixel(x+(Round(irp)),y); //SrcPtr^;
        if OutColor.Alpha > 0 then DstPtr^:= OutColor;
        irp := irp + rp;
        Inc(DstPtr);
      end;
    end
    else if (lg<0) then
    begin
      lg := -lg; //Abs(lg);
      rp := (FTextureMapRecto.Width) / lg;
      DstPtr := FDisplayBuffer.GetPixelPtr(FXD + FTextureMapRecto.CenterX - ((LG-1) shr 1),FYD+Y);
      irp :=0;
      x := FRightPart;
      for i := 0 to lg-1 do
      begin
        OutColor :=  FTextureMapVerso.GetPixel(x-(Round(irp) ),y);
        if OutColor.Alpha > 0 then DstPtr^:= OutColor;
        irp := irp + rp;
        inc(DstPtr);
      end;
    end;

  end;
  Inc(FAngle,2);
end;

procedure TMainForm.DoneScene;
begin
  // Finalisation de la scene
  FreeAndNil(FtextureMapVerso);
  FreeAndNil(FtextureMapRecto);
end;

end.

