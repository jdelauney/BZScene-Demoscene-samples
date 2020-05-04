unit uMainForm;

{$mode objfpc}{$H+}

//https://lodev.org/cgtutor/plasma.html
//https://www.bidouille.org/prog/plasma
interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Math,
  BZMath, BZVectorMath, BZColors, BZGraphic, BZBitmap, BZCadencer, BZStopWatch;

Const
  cFormCaption : String = 'BZScene Demo - Another plasma : ';

type

  { TMainForm }
    FractalColorMapProc =  procedure(Bmp : TBZBitmap; Offset : DWord; v : Single);
    FractalMapFunc = Function(x,y : Integer; DeltaTime : Double) : Single;

  TMainForm = class(TForm)
    Timer1: TTimer;
    procedure FormCloseQuery(Sender : TObject; var CanClose : boolean);
    procedure FormCreate(Sender : TObject);
    procedure FormShow(Sender : TObject);
    procedure FormDestroy(Sender : TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    FCadencer : TBZCadencer;
    FStopWatch : TBZStopWatch;
    FDisplayBuffer : TBZBitmap;

    FPlasmaLUT1, FPLasmaLUT2, FPLasmaLUT3, FPLasmaLUT4 : Array of Integer;
    FPalette : TBZPaletteEntries;
    FAngle : Single;

    procedure CadencerProgress(Sender : TObject; const deltaTime, newTime : Double);
  public
    procedure InitEngine;
    procedure StartEngine;
    procedure StopEngine;
    procedure DoneEngine;
    procedure InitScene;
    procedure InitColorMap;
    procedure RenderScene(DeltaTime : Double);

    procedure FractalColorMap1(Bmp : TBZBitmap; Offset : DWord; v : Single);
    Function  FractalMap1(x,y : Integer; DeltaTime : Double) : Single;

    procedure RenderPlasmaFrame(Bmp : TBZBitmap; DeltaTime : Double);
      //Const aFractalMapFunc : FractalMapFunc; Const aColorMapProc : FractalColorMapProc);

    procedure RenderPrecalcPlasmaFrame(Bmp : TBZBitmap; DeltaTime : Double);

    procedure DoneScene;
  end;

var
  MainForm : TMainForm;

implementation

{$R *.lfm}

//uses
//  BZLogger;
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

procedure TMainForm.Timer1Timer(Sender: TObject);
Var
  NewTime : Double;
begin
  NewTime := GetTickCount64 / 1000;
  RenderScene(NewTime);
  FDisplayBuffer.DrawToCanvas(Canvas, ClientRect);
  Caption:=cFormCaption + ' : '+Format('%.*f FPS', [3, FStopWatch.getFPS]);
end;

procedure TMainForm.CadencerProgress(Sender : TObject; const deltaTime, newTime : Double);
begin
  RenderScene(NewTime);
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
  //Timer1.Enabled := True;
end;

procedure TMainForm.StopEngine;
begin
  FStopWatch.Stop;
  FCadencer.Enabled := False;
  //Timer1.Enabled := False;
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
  i,j, d : integer;
  cx,cy, t : single;
begin
  // Creation de 4 tampons precalculés du double de la taille d'affichage pour nos fonctions Sin/Cos du plasma
  SetLength(FPlasmaLUT1, (width*2) * (Height*2) );
  SetLength(FPlasmaLUT2, (width*2) * (Height*2) );
  SetLength(FPlasmaLUT3, (width*2) * (Height*2) );
  SetLength(FPlasmaLUT4, (width*2) * (Height*2) );


  d:=0;
  t := 0;
  for j := 0 to (Height*2)-1 do
  begin
    d := j * (width*2) ;

    for i := 0 to (width*2)-1 do
    begin

      // Changer ces deux fonction pour obtenir des effets différents
      FPlasmaLUT1[d+i] := Round(128 + (127*System.Sin(i/64)));

      FPlasmaLUT2[d+i] := Round(128 + (127 *  System.Sin(System.Sin((i+j)/64))));
      //GlobalLogger.LogNotice('PLASMALUT2 = '+FPlasmaLUT2[d+i].ToString);
      //cx := i + (width/4)*Sin(t/50);
      //cy := j + (height/4)*Cos(t/30);
      FPlasmaLUT3[d+i] := Round(128 + (127 * System.Sin(System.Sqrt(100*(i*i+j*j)+1)/512))); //

      FPlasmaLUT4[d+i] := Round(128 + (127 * System.Sin(27+i/271)) *  System.Cos(32+j/57));
      t:=t+1;

      //FPlasmaLUT1[d+i] :=  Round(64 + (63 * Sin(2 * cPI*i /128)/32));
      //FPlasmaLUT1[d+i] := Round(64 + (63 * Sin(d*Sqrt((i*i+j*j)+1)+d)/16));
      //FPlasmaLUT1[d+i] := Round(64 + (63 * Sin(hypot( (Height/2)-j, (Width/2)-i )/16 )));
      //FPlasmaLUT2[d+i] :=  Round(64 + (63 * Sin(( (Width)-i)/(40*Cos(i/2048)))* Cos(((Height)-j)/(30*sin(i/2048)))));


    end;
  end;
  FAngle := 0;
  InitColorMap;
end;

procedure TMainForm.InitColorMap;
// Initalisation de la palette de couleur, si utilisée
Var
  i : Integer;
  r,g,b : Byte;
begin
  For i := 0 to 255 do
  begin
    R := Round(255*(0.5+0.5*Sin(2*cPI*i/255)));
    G := Round(255*(0.5+0.5*Cos(2*cPI*i/255)));
    B := 0;
    FPalette[i] := BZColor(R,G,B);
  end;

end;

procedure TMainForm.RenderScene(DeltaTime : Double);
// Coeur du rendu de la scene
begin
  FDisplayBuffer.Clear(clrBlack);
  //RenderPlasmaFrame(FDisplayBuffer, DeltaTime);
  RenderPrecalcPlasmaFrame(FDisplayBuffer, DeltaTime*100);
end;

{  function colorMap1(px, offset, v)
     px[offset  ] = 255*(.5+.5*Math.sin(Math.PI*v));
     px[offset+1] = 255*(.5+.5*Math.cos(Math.PI*v));
     px[offset+2] = 0;
     px[offset+3] = 255;


   function colorMap2(px, offset, v)
     px[offset  ] = 255;
     px[offset+1] = 255*(.5+.5*Math.cos(Math.PI*v));
     px[offset+2] = 255*(.5+.5*Math.sin(Math.PI*v));
     px[offset+3] = 255;


   function colorMap3(px, offset, v)
     px[offset  ] = 255*(.5+.5*Math.sin(Math.PI*v));
     px[offset+1] = 255*(.5+.5*Math.sin(Math.PI*v+2*Math.PI/3));
     px[offset+2] = 255*(.5+.5*Math.sin(Math.PI*v+4*Math.PI/3));
     px[offset+3] = 255;


   function colorMap4(px, offset, v)
     var c = .5+.5*Math.sin(Math.PI*v*5);
     px[offset  ] = 255*c;
     px[offset+1] = 255*c;
     px[offset+2] = 255*c;
     px[offset+3] = 255;


   function colorMapGrey(px, offset, v)
     var c = 255*(.5+.5*v*.8);
     px[offset  ] = c;
     px[offset+1] = c;
     px[offset+2] = c;
     px[offset+3] = 255;
   }
procedure TMainForm.FractalColorMap1(Bmp : TBZBitmap; Offset : DWord; v : Single);
Var
  OutColor : TBZColor;
begin
  OutColor.Red   := Round(255 * (0.5 + 0.5 * Sin(cPI * v)));
  OutColor.Green := Round(255 * (0.5 + 0.5 * Sin(cPI * v + 2 * cPIDiv3)));
  OutColor.Blue  := Round(255 * (0.5 + 0.5 * Sin(cPI * v + 4 * cPIDiv3)));
  OutColor.Alpha := 255;
  Bmp.SetPixelOffset(Offset,OutColor);
end;

function TMainForm.FractalMap1(x, y : Integer; DeltaTime : Double) : Single;
VAr
  cx, cy, v: Single;
begin
  v := 0;
  v := v + Sin((x*10+DeltaTime));
  v := v + Sin((y*10+DeltaTime) * 0.5);
  v := v + Sin((x*10+y*10+DeltaTime) * 0.5);
  cx := x + 0.5 * Sin(DeltaTime/5.0);
  cy := y + 0.5 * Cos(DeltaTime/3.0);
  v := v + Sin(Sqrt(100*(cx*cx+cy*cy)+1)+DeltaTime);
  v := v * 0.5;
  result := v;
end;

procedure TMainForm.RenderPlasmaFrame(Bmp : TBZBitmap; DeltaTime : Double);
Var
  x,y : DWord;
  Delta, yy, xx, v, cx, cy : Double;
  OutColor : TBZColor;
begin
  Delta := bmp.Width / bmp.Height;
  For y := 0 to bmp.MaxHeight do
  begin
    yy := y / bmp.Height*0.5;
    v := 0;
    v := v + Sin((yy*10+DeltaTime) * 0.5) ;
    For x := 0 to Bmp.MaxWidth do
    begin
      xx := ((Delta * x) / bmp.Width) - (Delta * 0.5);
      //v := aFractalMapFunc(Round(xx),Round(yy),DeltaTime);

      v := v + Sin((xx*10+DeltaTime));

      v := v + Sin((xx*10+yy*10+DeltaTime) * 0.5 );
      v := v + Sin(10*(xx*Sin(DeltaTime * 0.5) + yy * Cos(DeltaTime / 3)) + DeltaTime);

      cx := xx + 0.5 * Sin(DeltaTime/5.0);
      cy := yy + 0.5 * Cos(DeltaTime/3.0);
      v := v + Sin(Sqrt(45*(cx*cx+cy*cy)+1)+DeltaTime);
      v := v * 0.5;

      //GlobalLogger.LogNotice('V = '+v.ToString);
      //aColorMapProc(Bmp, (y*Bmp.Width+x), v);
      //OutColor.Red   := Round(255 * (0.5 + 0.5 * Sin(cPI * v)));
      //OutColor.Green := Round(255 * (0.5 + 0.5 * Sin(cPI * v + 2 * cPIDiv3)));
      //OutColor.Blue  := Round(255 * (0.5 + 0.5 * Sin(cPI * v + 4 * cPIDiv3)));

      OutColor.Red   := Round(255*(0.5 + 0.5 * Sin(cPI*v)));
      OutColor.Green := Round(255*(0.5 + 0.5 * Cos(cPI*v)));
      OutColor.Blue  := 0;
      OutColor.Alpha := 255;
      //Bmp.SetPixelOffset(Offset,OutColor);
      //Bmp.SetPixel(Round(xx),Round(yy),OutColor);
       Bmp.SetPixel(x,y,OutColor);

    end;
  end;
end;

procedure TMainForm.RenderPrecalcPlasmaFrame(Bmp : TBZBitmap; DeltaTime : Double);
Var
  w, x1,y1,x2,y2,x3,y3,x4,y4, offs1, offs2, offs3, offs4, cx, cy, Rotated : Integer;
  An,ry,rx : Single;
  ColIdx : Byte;
  PixPtr : PBZColor;
begin
  w := Bmp.Width*2;
  cx := Bmp.CenterX;
  cy := Bmp.CenterY;
  //An := DegToRadian(fmodf(FAngle,360));

  // On bouge les fenêtre des fonctions du plasma avec quleques fonctions Sinus supplémentaire
  x1 := cx + Round(cX * Cos(DeltaTime/97 ));
  x2 := cx + Round(cX * Sin(-DeltaTime/114 ));
  x3 := cx + Round(cX * Sin(DeltaTime/87 ));
  x4 := cx + Round(cX * Sin(-DeltaTime/256 ));
  y1 := cy + Round(cY * Sin(DeltaTime/63 ));
  y2 := cy + Round(cY * Cos(-DeltaTime/75 ));
  y3 := cy + Round(cY * Cos(DeltaTime/108 ));
  y4 := cy + Round(cY * Cos(-DeltaTime/168 ));

  // On calcul les offsets pour nos tables précaculcée
  Offs1 := y1*w+x1;
  Offs2 := y2*w+x2;
  Offs3 := y3*w+x3;
  Offs4 := y4*w+x4;

  // On dessine le plasma, on fait la somme de nos 4 fonctions
  PixPtr := Bmp.GetScanLine(0);
  for cy := 0 to Bmp.MaxHeight do
  begin
    //ry := cy*0.05*BZMath.Cos(An);
    for cx := 0 to Bmp.MaxWidth do
    begin
      //rx := cx*0.05*BZMath.Sin(An);
      ColIdx := byte(FPlasmaLUT1[Offs1] + FPlasmaLUT2[Offs2] + FPlasmaLUT3[Offs3] + FPlasmaLUT4[Offs4]);
      //round(FPlasmaLUT2[Offs2]+DeltaTime * 0.00125)
      //ColIdx := byte(FPlasmaLUT4[Offs1]);
      //Rotated := Round(128 + (127 *  System.Sin((cx*15+cy*15+DeltaTime)/512)));
      //Rotated := Round(128 + (127 * BZMath.Sin((cx*0.05*BZMath.Sin(An)+cy*0.05*BZMath.Cos(An))+(DeltaTime*0.00125))));
      //rotated := Round(128 + (127 *BZMath.Sin((rx + ry)+DeltaTime * 0.00125)));
      //ColIdx := byte(FPlasmaLUT1[Offs1] + Rotated + FPlasmaLUT3[Offs3] + FPlasmaLUT4[Offs4]);
      //ColIdx := Byte(Rotated);
      PixPtr^ := FPalette[ColIdx];

      Inc(Offs1);
      Inc(Offs2);
      Inc(Offs3);
      Inc(Offs4);
      Inc(PixPtr);
    end;
    Inc(Offs1,Bmp.Width);
    Inc(Offs2,Bmp.Width);
    Inc(Offs3,Bmp.Width);
    Inc(Offs4,Bmp.Width);
  end;
  //FAngle := FAngle + 0.3;
end;

procedure TMainForm.DoneScene;
// Finalisation de la scene
begin
  SetLength(FPlasmaLUT4, 0 );
  FPlasmaLUT4 := Nil;
  SetLength(FPlasmaLUT3, 0);
  FPlasmaLUT3 := Nil;
  SetLength(FPlasmaLUT2, 0 );
  FPlasmaLUT2 := nil;
  SetLength(FPlasmaLUT1, 0);
  FPlasmaLUT1 := nil;
end;

end.

