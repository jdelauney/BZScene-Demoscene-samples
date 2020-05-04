unit uMainForm;

{$mode objfpc}{$H+}

// Sample from Asphyre / plx library https://github.com/ultibohub/Asphyre
interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs,
  BZColors, BZGraphic, BZBitmap, BZStopWatch, BZCadencer, BZImageFilePNG;

type

  { TMainForm }

  TMainForm = class(TForm)
    procedure FormClose(Sender : TObject; var CloseAction : TCloseAction);
    procedure FormCloseQuery(Sender : TObject; var CanClose : boolean);
    procedure FormCreate(Sender : TObject);
    procedure FormShow(Sender : TObject);
    procedure FormKeyPress(Sender : TObject; var Key : char);
  private
    FCadencer : TBZCadencer;
    FStopWatch : TBZStopWatch;
    //ColorMap: array[0..255] of TBZColor;

  protected
    SinLookup: array[0..255] of Word;
    CosLookup: array[0..255] of Word;
    Buffer, PlasmaSurface, ImageScanLine : TBZBitmap;
    FrameCounter :Integer;
    ShiftX: Integer;
    ShiftY: Integer;
    PaletteIndex: Integer;

    procedure CadencerProgress(Sender : TObject; const {%H-}deltaTime, newTime : Double);

  public
    procedure InitScene;
    //procedure FillColorMap(i1,i2:Integer;c1,c2:TBZColor);
    //procedure SetPal(idx:integer;r,g,b:byte);
    //procedure GetPal(idx:integer;var r,g,b:byte);
    procedure InitColorMap;
    procedure DoThePlasma;

    procedure PreparePlasma(const aShiftX, aShiftY: Integer);
  end;

var
  MainForm : TMainForm;

implementation

{$R *.lfm}

{ TMainForm }

procedure TMainForm.InitScene;
var
  i:integer;
begin
  Randomize;

  for I := 0 to 255 do
  begin
    SinLookup[I] := (Trunc(Sin(2.0 * Pi * I / 256.0) * 128.0) + 256) and $FF;
    CosLookup[I] := (Trunc(Cos(2.0 * Pi * I / 256.0) * 128.0) + 255) and $FF;
  end;

  ShiftX := 0;
  ShiftY := 0;
  PaletteIndex := 0;
 // InitColorMap;

  Buffer:= TBZBitmap.Create(ClientWidth,ClientHeight);
  Buffer.Clear(clrBlack);

  ImageScanLine:= TBZBitmap.Create;
  ImageScanLine.LoadFromFile('..\..\..\..\media\images\TVScanLine.png');

  //PlasmaSurface:= TBZBitmap.Create(256,256);
  PlasmaSurface:= TBZBitmap.Create(Width,Height);
  PlasmaSurface.Clear(clrBlack);

  InitColorMap;

  FrameCounter:=0;
end;

procedure TMainForm.InitColorMap;
var
  i, j :integer;
begin
//  PlasmaSurface.UsePalette := True;
  for i:=0 to 63 do PlasmaSurface.ColorManager.CreateColor(BZColor(i * 4, i * 2, i * 2));
  for i:=64 to 127 do
  Begin
    j := i - 64;
    PlasmaSurface.ColorManager.CreateColor(BZColor(j * 4, j * 2, j * 2));
  end;
  for i:= 128 to 191 do
  Begin
    j := i - 128;
    PlasmaSurface.ColorManager.CreateColor(BZColor(j, j * 4, j));
  end;
  for i:= 192 to 255 do
  Begin
    j := i - 192;
    PlasmaSurface.ColorManager.CreateColor(BZColor(j * 4, j * 4, j * 4));
  end;
end;

procedure TMainForm.DoThePlasma;
var
  J, I: Integer;
begin
  PreparePlasma(ShiftX, ShiftY);
  Buffer.FastCopy(PlasmaSurface);
  //Buffer.RenderFilter.TileTexture(256,256,PlasmaSurface);
  Buffer.RenderFilter.TileTexture(64,64,ImageScanLine,dmSet, amAlpha);
end;

procedure TMainForm.PreparePlasma(const aShiftX, aShiftY : Integer);
var
  I, J, Xadd, Cadd: Integer;
  DestPixel: PBZColor;
  Index: Integer;
begin
  for J := 0 to PlasmaSurface.MaxHeight do
  begin
    DestPixel := PlasmaSurface.GetScanLine(J);

    Xadd := SinLookup[((J shl 1) + aShiftX) and $FF];
    Cadd := CosLookup[((J shl 1) + aShiftY) and $FF];

    for I := 0 to PlasmaSurface.MaxWidth do
    begin
      Index := (SinLookup[((I shl 1) + Xadd) and $FF] + Cadd + (PaletteIndex * 2)) and $FF;
      if Index > 127 then Index := 255 - Index;

      DestPixel^ := PlasmaSurface.ColorManager.Palette.Colors[((Index div 2 ) + PaletteIndex) and $FF].Value;
      //ColorMap[((Index div 2 ) + PaletteIndex) and $FF];
      Inc(DestPixel);
    end;
  end;
end;

procedure TMainForm.FormClose(Sender : TObject; var CloseAction : TCloseAction);
begin
  FreeAndNil(FStopWatch);
  FreeAndNil(FCadencer);
  FreeAndNil(PlasmaSurface);
  FreeAndNil(ImageScanLine);
  FreeAndNil(Buffer);
end;

procedure TMainForm.FormCloseQuery(Sender : TObject; var CanClose : boolean);
begin
  FStopWatch.Stop;
  FCadencer.Enabled := False;
end;

procedure TMainForm.FormCreate(Sender : TObject);
begin
  FCadencer := TBZCadencer.Create(self);
  FCadencer.Enabled := False;
  FCadencer.OnProgress := @CadencerProgress;
  FStopWatch := TBZStopWatch.Create(self);
  InitScene;
end;

procedure TMainForm.FormShow(Sender : TObject);
begin
  DoubleBuffered:=true;
  FStopWatch.Start;
  FCadencer.Enabled:=true;
end;

procedure TMainForm.FormKeyPress(Sender : TObject; var Key : char);
begin
  if Key = #27 then Close;
end;

procedure TMainForm.CadencerProgress(Sender : TObject; const deltaTime, newTime : Double);
begin
  DoThePlasma;
  Buffer.DrawToCanvas(Canvas, ClientRect);
  Inc(ShiftX);
  Dec(ShiftY);
  Inc(PaletteIndex);
  Caption:='BZScene PlasmaTV Demo : '+Format('%.*f FPS', [3,FStopWatch.getFPS]);
end;

end.

