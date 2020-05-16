unit uMainForm;

{$mode objfpc}{$H+}

// ALIGNEMENT
{$ifdef cpu64}
  {$ALIGN 16}

  {$CODEALIGN CONSTMIN=16}
  {$CODEALIGN LOCALMIN=16}
  {$CODEALIGN VARMIN=16}
{$endif}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs,
  BZVectorMath, BZColors, BZGraphic, BZBitmap, BZStopWatch, BZCadencer, BZParallelThread, BZBitmapIO;

type

  { TMainForm }

  TMainForm = class(TForm)
    procedure FormDestroy(Sender : TObject);
    procedure FormKeyPress(Sender : TObject; var Key : char);
    procedure FormShow(Sender : TObject);
    procedure FormCloseQuery(Sender : TObject; var CanClose : boolean);
  private
    FBitmapBuffer : TBZBitmap;
    FCadencer : TBZCadencer;
    FStopWatch : TBZStopWatch;

  protected
    FTexture : TBZBitmap;

    FUVMapLUT : TBZVector2f2DMap;
    {$CODEALIGN RECORDMIN=16}

     //FUVMapLut : Array[0..1023,0..767] of TBZVector2f;
     pt0, {pt1,} ptc, pts, ptb : TBZVector2f;
    {$CODEALIGN RECORDMIN=4}
     dc, AlphaScale : Single;


    FrameCounter :Integer;
    FCurrentFX : Byte;

    procedure ComputePixel(Sender: TObject; Index: Integer ; Data : Pointer);

    procedure CadencerProgress(Sender : TObject; const deltaTime, newTime : Double);

    procedure ComputeUVMapLUT(idx : byte);

  public
    Procedure RenderScene(iTime:Double);
    Procedure InitScene;
    Procedure InitColorMap;
    procedure SceneDone;
  end;

var
  MainForm : TMainForm;

implementation

{$R *.lfm}

uses BZMath, BZTypesHelpers, BZLogger, Math;

{ TMainForm }

procedure TMainForm.FormDestroy(Sender : TObject);
begin
  SceneDone;
end;

procedure TMainForm.FormShow(Sender : TObject);
begin
  InitScene;
  DoubleBuffered:=true;
  FStopWatch.Start;
  FCadencer.Enabled := True;
end;

procedure TMainForm.FormCloseQuery(Sender : TObject; var CanClose : boolean);
begin
  FStopWatch.Stop;
  FCadencer.Enabled := False;
end;

procedure TMainForm.FormKeyPress(Sender : TObject; var Key : char);
var
  k:Char;
begin
  if key = #27 then Close;
  k := Key.ToUpper;
  if k in['0','1','2','3','4','5','6','7','8','9','A','B','C'] then
  begin
    FStopWatch.Stop;
    FCadencer.Enabled:=False;
    //Sleep(100);
    if  k = 'A' then FCurrentFX := 10
    else if k = 'B' then FCurrentFX := 11
    else if k = 'C' then FCurrentFX := 12
    else
      FCurrentFX := StrToInt(K);

    ComputeUVMapLUT(FCurrentFX);

    FrameCounter := 0;
    FStopWatch.Start;
    FCadencer.Enabled := true;
  end;
end;

procedure TMainForm.CadencerProgress(Sender : TObject; const deltaTime, newTime : Double);
begin
  RenderScene(NewTime-DeltaTime);
  With FBitmapBuffer.Canvas do
  begin
    Font.Size := 10;
    Font.Color := clrWhite;
    TextOut(10,16,'Tap key : 0,1,2,3,4,5,6,7,8,9,A,B,C');
  End;
  FBitmapBuffer.DrawToCanvas(Canvas, ClientRect);
  //Inc(FrameCounter);
  Caption:='BZScene Demo - Planar DeformationZ : '+Format('%.*f FPS', [3, FStopWatch.getFPS]);
end;

procedure TMainForm.ComputeUVMapLUT(idx : byte);
{$CODEALIGN CONSTMIN=16}
Const
  cOne2f : TBZVector2f = (x:1.0;y:1.0);
{$CODEALIGN CONSTMIN=4}
var
  i,j : integer; //,n,m
  a,r : single;
  {$CODEALIGN VARMIN=16}
  P, P1, UV : TBZVector2f; //, T
  {$CODEALIGN VARMIN=4}
begin
  //FCadencer.Enabled:=False;
  FBitmapBuffer.Clear(clrBlack);

  if assigned(FUVMapLUT) then FreeAndNil(FUVMapLUT);
  FUVMapLUT := TBZVector2f2DMap.Create(FBitmapBuffer.Width,FBitmapBuffer.Height);
  P.Create(0,0);

  Case Idx of
    0:FTexture.LoadFromFile('../../../../media/images/tunnel7.bmp');
    1:FTexture.LoadFromFile('../../../../media/images/tunnel6.bmp');
    2:FTexture.LoadFromFile('../../../../media/images/tunnel5.bmp');
    3:FTexture.LoadFromFile('../../../../media/images/tunnel4.bmp');
    4:FTexture.LoadFromFile('../../../../media/images/tunnel11.bmp');
    5:FTexture.LoadFromFile('../../../../media/images/tunnel2.bmp');
    6:FTexture.LoadFromFile('../../../../media/images/tunnel3.bmp');
    7:FTexture.LoadFromFile('../../../../media/images/tunnel6.bmp');
    8:FTexture.LoadFromFile('../../../../media/images/tunnel1.bmp');
    9:FTexture.LoadFromFile('../../../../media/images/tunnel2.bmp');
    10:FTexture.LoadFromFile('../../../../media/images/tunnel10.bmp');
    11:FTexture.LoadFromFile('../../../../media/images/tunnel1.bmp');
    12:FTexture.LoadFromFile('../../../../media/images/tunnel2.bmp');
  end;

  for j:=0 to FBitmapBuffer.MaxHeight do
  begin
    for i:=0 to FBitmapBuffer.MaxWidth do
    begin
       P.x := -1.0+2.0*i/FBitmapBuffer.Width;
       P.y := -1.0+2.0*j/FBitmapBuffer.Height;

       //p = (-iResolution.xy + 2.0*fragCoord)/iResolution.y;
       //n:=j+1;
       //m:=i+1;
       //p.x := (-FBitmapBuffer.Width + 2.0 * m)/FBitmapBuffer.Width;
       //p.y := (-FBitmapBuffer.Height + 2.0 * n)/FBitmapBuffer.Height;

       a := Math.ArcTan2( P.y, P.x );
       //if a = 0 then a := 1;

       //r := System.Sqrt(sqr(p.x) + sqr (p.y)); //
       r := P.LengthSquare +cEpsilon;
       //r := P.Length; //Cylindrical mapping
       //r := Math.power( Math.power(p.x*p.x,4.0) + Math.power(p.y*p.y,4.0), 1.0/8.0 ); // Box Mapping

       //float e = iMouse.x / iResolution.x*8.;
       //float r = pow( pow(p.x*p.x,e) + pow(p.y*p.y,e), 1.0/(2.*e) );

       // t := P.Abs;
       // r := max(t.x, t.y);

       // magic formulas here :
       Case Idx of
         0:
         begin
           uv.x := p.x;
           uv.y := abs(p.y)+cEpsilon;
           uv.x := Fract((uv.x/uv.y));
           uv.y := (1.0/uv.y);
         end;
         1:
         begin
           uv.x :=r;
           uv.y := a * cInvPI;
         end;
         2:
         begin
           uv.x :=(System.cos(i) * a - System.sin(j));
           uv.y := a * cInvPI;
         end;
         3:
         begin
           uv.x := System.cos(a)/r;
           uv.y := System.sin(a)/r;
           //uv.x := r*System.cos(a+r);
           //uv.y := r*System.sin(a+r);
         end;
         4:
         begin
           { Provoque une cassure au mileu
           uv.x := 0.5*a +0.5 / r;
           uv.y := 1.0*a / c2Pi;
           }

           //uv.x := 0.3/r;
           //uv.y := a/cPi;

           uv.x := a/c2Pi;
           uv.y := 0.5/r;

           //uv.x :=a /c2Pi;
           //uv.y := 1 / r + 0.5;
           // uv.x := 1 / r + 0.5;
           // uv.y := a /c2Pi;
         end;
         5:
         begin
           uv.x := 0.3 / (r + 0.5 * i);
           uv.y := 3 * a / cPi;
         end;
         6:
         begin
           //P1.x :=-1.0+2.0*i/FBitmapBuffer.Width;
           //P1.y := -1.0+2.0*j/FBitmapBuffer.Height;
           //P1 := P1 - 0.5;
           P1.Create(FBitmapBuffer.Height/FBitmapBuffer.Width,1);
           //P1 := P1 * P;
           UV := P;
           UV.x := 0.5*r+P1.X+System.cos(arctan(uv.X/uv.y)) / cPI;
           UV.Y := 0.3*r+P1.Y-System.Sin(P.Length*a);
           UV := UV +0.1;
           //uv.x := (5*i*cos(0.5*r)) - (3*j*sin(r));
           //uv.y := (5*j*cos(r)) + (3*i*sin(0.5*r));

         end;
         7:
         begin
           uv.x := 0.02*j+0.03*System.cos(a*3)/r;
           uv.y := 0.02*i+0.03*System.sin(a*3)/r;
         end;
         8:
         begin
           uv.x := 0.5 * a / cPi;
           uv.y := sin(7*r);
         end;
         9:
         begin
          uv.x := r*System.cos(a+r);
          uv.y := r*System.sin(a+r);
         end;
         10:
         begin
           uv.x := 1/ ( r + 0.5 + 0.5 * System.sin(5 * a));
           uv.y := a * 3 / cPi;
         end;
         11:
         begin
           uv.x := -3.0*a + 3.0*r;
           uv.y := 3.0*i+ System.sin(a*3)/r*cInvPI;
         end;
         12:
         begin
          uv.x := i/abs(j);
          uv.y := 1/abs(j);
         end;
         else
           begin
             uv.x := r*System.cos(a+r);
             uv.y := r*System.sin(a+r);
           end;
       end;
       P:= UV.Modf(cOne2f);
       //P := uv;
       //GlobalLogger.LogStatus('pt : X = '+Floattostr(p.x)+', Y= '+Floattostr(p.y));
       FUVMapLUT.Add(P);
       //FUVMapLut[i,j] := P;
    end;
  end;
end;

procedure TMainForm.ComputePixel(Sender : TObject; Index : Integer; Data : Pointer);
var
  {$CODEALIGN VARMIN=16}
  pt1: TBZVector2f;
  pt : TBZVector2i;
  {$CODEALIGN VARMIN=4}
  FragColor : TBZColor;

  i,j,k : Integer;

begin
  j := Index;// div FBitmapBuffer.Height;
//  i := Index mod FBitmapBuffer.Width;
  //GlobalLogger.LogNotice('I,J ='+j.ToString+', '+i.ToString);
  //AlphaScale := 1.0;
  //dc := 1.0 / 80;

   if FCurrentFX = 0 then
   begin
     if (j> FBitmapBuffer.CenterY-80) and (j< FBitmapBuffer.CenterY+80) then
     begin
       if (j =  FBitmapBuffer.CenterY) then AlphaScale := 0.0
       else if (j > FBitmapBuffer.CenterY) and (j < FBitmapBuffer.CenterY+80) then AlphaScale := AlphaScale+dc
       else AlphaScale := AlphaScale-dc;
     end
     else AlphaScale := 1.0;
   end;
  // pt1 := FUVMapLUT.Items[i,j];

   for i:=0 to FBitmapBuffer.MaxWidth do
   begin
     pt1 :=  FUVMapLUT.Items[i,j]; //FUVMapLut[i,j];

     //if AlphaScale>0 then
     //begin


       if (FCurrentFX = 6) then
       begin
         pt1 := pt1 * pt0 ;
         pt:= pt1.Round;
         FragColor := FTexture.getPixel(pt.X, pt.Y);
         FragColor := FragColor * alphaScale;
       end
       else
       begin



         pt1 := pt1 + pt0 ;
         pt1 := pt1 * pts;
         pt := pt1.fMod(ptb);
         FragColor := FTexture.getPixel(pt.X, pt.Y);

         //k := Round(255*alphaScale);
         FragColor := (FragColor * AlphaScale); // div 255 ;
       end;
        FBitmapBuffer.setPixel(i,j, FragColor);
     //end;
   end;
end;

procedure TMainForm.RenderScene(iTime : Double);
var
  {.$CODEALIGN VARMIN=16}
  //{pt0,} pt1{, ptc, pts, ptb} : TBZVector2f;
  //pt : TBZVector2i;
  {.$CODEALIGN VARMIN=4}

  //FragColor : TBZColor;
  {dc,}aTime {,alphascale}, st, ct : Single;
  //i,j: integer;
begin
   aTime := iTime;
  FBitmapBuffer.Clear(clrBlack);
  if (FCurrentFX =6) then
  begin
    st := cPI*sin(itime*0.8);
    ct := cPI*Cos(itime*0.3);

    ptc.Create(st,ct);
    ptb.Create(cPI*(-Sin(iTime)),cPI*Cos(iTime*0.3));
    ptc := ptc * 3;
    ptb := ptb * 7;
    pt0.Create(ptc.Length,ptb.Length);
    pt0 := pt0 * 20;
  end
  else if (FCurrentFX = 3) then
          pt0.Create(30*sin(aTime)*0.05, 50*Sin(aTime)*0.03)
       else
          pt0.Create(65, iTime * 0.3);//iTime + (50*Cos(iTime)*0.03)); //
          //pt0.Create(30*sin(iTime)*0.05,1.0);

  pts.Create(FTexture.Width,FTexture.Height);
  ptb.Create(FTexture.MaxWidth,FTexture.MaxHeight);

  ParallelFor(0,FBitmapBuffer.MaxHeight,@ComputePixel,nil);
  //AlphaScale := 1.0;
  dc := 1.0 / 100; // 80;

  //For j:=0 to FBitmapBuffer.MaxHeight do
  //begin
  //  if FCurrentFX = 0 then
  //  begin
  //    if (j> FBitmapBuffer.CenterY-80) and (j< FBitmapBuffer.CenterY+80) then
  //    begin
  //      if (j =  FBitmapBuffer.CenterY) then AlphaScale := 0.0
  //      else if (j > FBitmapBuffer.CenterY) and (j < FBitmapBuffer.CenterY+80) then AlphaScale := AlphaScale+dc
  //      else AlphaScale := AlphaScale-dc;
  //    end
  //    else AlphaScale := 1.0;
  //  end;
  //
  //  for i:=0 to FBitmapBuffer.MaxWidth do
  //  begin
  //    pt1 := FUVMapLUT.Items[i,j];
  //
  //    if (FCurrentFX = 6) then
  //    begin
  //      pt1 := pt1 * pt0 ;
  //      pt:=pt1.Round;
  //      FragColor := FTexture.getPixel(pt.X, pt.Y);
  //      FragColor := FragColor * alphaScale;
  //    end
  //    else
  //    begin
  //      pt1 := pt1 + pt0 ;
  //      pt1 := pt1 * pts;
  //      pt := pt1.fMod(ptb);
  //      FragColor := FTexture.getPixel(pt.X, pt.Y);
  //      FragColor := FragColor * alphaScale;
  //    end;
  //    FBitmapBuffer.setPixel(i,j, FragColor);
  //  end;
  //end;

end;

procedure TMainForm.InitScene;
Begin
  FCadencer := TBZCadencer.Create(self);
  FCadencer.Enabled := False;
  FCadencer.OnProgress := @CadencerProgress;

  FStopWatch := TBZStopWatch.Create(self);

  Randomize;
  FBitmapBuffer := TBZBitmap.Create;
  FBitmapBuffer.SetSize(Width,Height);
  FBitmapBuffer.Clear(clrBlack);
  FTexture := TBZBitmap.Create;

  FrameCounter:=0;
  FCurrentFX := 0;
  ComputeUVMapLUT(FCurrentFX);

end;

procedure TMainForm.InitColorMap;
begin
  // Init Palette color map if needed
end;

procedure TMainForm.SceneDone;
begin
  if FUVMapLUT<>nil then FreeAndNil(FUVMapLUT);
  FreeAndNil(FStopWatch);
  FreeAndNil(FCadencer);
  FreeAndNil(FTexture);
  FreeAndNil(FBitmapBuffer);
end;

end.

