unit uMainForm;

{$mode objfpc}{$H+}

// Code Alignment
{$IFDEF CPU64}
  {$ALIGN 16}
  {$CODEALIGN CONSTMIN=16}
  {$CODEALIGN LOCALMIN=16}
  {$CODEALIGN VARMIN=16}
{$ENDIF}

interface

uses
  Classes, SysUtils, Math, Forms, Controls, Graphics, Dialogs,
  BZMath, BZVectorMath, BZColors, BZGraphic, BZBitmap, BZArrayClasses,
  BZCadencer, BZStopWatch, BZBitmapIO;

Const
  cFormCaption : String = 'BZScene Demo - VectorBall - Hit [0..8]';
  cVectorBallsCount = 50;
  cperspective = 800; // constant for x,y,z --> x,y conversion

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

    {$CODEALIGN RECORDMIN=16}
    FMorphSteps : TBZVector4fList;   // hold increments when stepping from 'a' to 'tmp' (shape shifting)
    FMorphObjectFrom,      // hold points of original shape
    FMorphObjectTmp : TBZVector4fList; // hold points of next shape
    FMorphObjectTo  : TBZVector4fList;  // hold points of transformed shape
    {$CODEALIGN RECORDMIN=4}
    FShapeSprite : TBZBitmap; //Array[0..9] of TBZBitmap:
    FShapeSprites : array[0..4] of TBZBitmap;

    FPatternSize : Integer;
    FSteps : Word;
    procedure RotateVectorBalls;

    procedure GenerateSteps( n : word);
    procedure MorphToNextPoint;
    procedure BuildCube;
    procedure BuildPlane;
    procedure BuildSpiral;
    procedure BuildCircle;
    procedure BuildSpiral2;
    procedure BuildCircleWave;
    procedure BuildTwister;
    procedure BuildCube2;
    procedure BuildSphere;

  //FcurPattern = 1;        // 1 to 8, indicates which shape is currently being drawn
  //FcycleCount = 0 ;       // counts the number of frames a shape is animated before it is changed, used with autoNext
  //FautoNext = true;       // flag: turn on automatich shape changing
  //FSteps = 0;             // counter tweening steps between shapes


  // These can be read or written between call to animate()
  // to change the shape rotation, or speed at which it rotates
  //this.rx = 0.0; //public: x rotation used in next paint
  //this.ry = 0.0; //public: z rotation used in next paint
  //this.rz = 0.0; //public: y rotation used in next paint
  //this.dx = 0.0; //public: increments for x rotation used by animate()
  //this.dy = 0.0; //public: increments for y rotation used by animate()
  //this.dz = 0.0; //public: increments for z rotation used by animate()

    procedure CadencerProgress(Sender : TObject; const deltaTime, newTime : Double);
  public
    rx,ry,rz : Single;  //rotation used in next paint
    dx,dy,dz : Single;  //increments for x,y,z rotation used by animate()

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

uses BZLogger;
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
  if key = '0' then BuildCube;
  if key = '1' then BuildCube2;
  if key = '2' then BuildSpiral;
  if key = '3' then BuildSpiral2;
  if key = '4' then BuildCircle;
  if key = '5' then BuildCircleWave;
  if key = '6' then BuildTwister;
  if key = '7' then BuildPlane;
  if key = '8' then BuildSphere;
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
  FDisplayBuffer.Clear(clrblack);
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
var
  i:Integer;
  {$CODEALIGN VARMIN=16}
  p : TBZVector4f;
  {$CODEALIGN VARMIN=4}
// Initalisation des données de la scene
begin

  FMorphSteps := TBZVector4fList.Create;
  FMorphObjectFrom := TBZVector4fList.Create(200);
  FMorphObjectTmp := TBZVector4fList.Create;
  FMorphObjectTo  := TBZVector4fList.Create;
  //FShapeSprite := TBZBitmap.Create;
  for i := 0 to 4 do
  begin
    FShapeSprites[i] := TBZBitmap.Create;
  end;
  FShapeSprites[4].LoadFromFile('..\..\..\..\media\images\blob4.png');
  FShapeSprites[3].LoadFromFile('..\..\..\..\media\images\blob4.png');
  FShapeSprites[2].LoadFromFile('..\..\..\..\media\images\blob4.png');
  FShapeSprites[1].LoadFromFile('..\..\..\..\media\images\blob4.png');
  FShapeSprites[0].LoadFromFile('..\..\..\..\media\images\blob6d.png');

  //FShapeSprite.LoadFromFile('..\media\images\blob6a.png');

    p.Create(0,0,0,255);
    for i := 0 to 255 do
    begin
       FMorphObjectFrom.Add(p);
    end;


  BuildCube;
  //BuildPlane;
  //BuildSpiral;
  //BuildSpiral2;
  //BuildCircle;
  //BuildCircleWave;
  //BuildTwister;
  //BuildCube2;



  Self.dx := 0.01;
  Self.dy := 0.03;
  Self.dy := 0.02;
end;

procedure TMainForm.InitColorMap;
begin

end;


//******************************************
procedure TMainForm.RotateVectorBalls;
var
  sx,sy,sz,cx,cy,cz : Single;
  i, iMax : integer;
  {$CODEALIGN VARMIN=16}
  op,dp : TBZVector4f;
  {$CODEALIGN VARMIN=4}
begin
    //sx := sin(Self.rx);
    //sy := sin(Self.ry);
    //sz := sin(Self.rz);
    //cx := cos(Self.rx);
    //cy := cos(Self.ry);
    //cz := cos(Self.rz);




    FMorphObjectTo.Clear;
    iMax := FMorphSteps.Count - 1;
    for i := 0 to iMax do
    begin
        op :=  FMorphObjectFrom.Items[i];
        //dp.x := op.x * cy * cz - op.y * cy * sz + op.z * sy;
        //dp.y := op.x * sx * sy * cz + op.x * cx * sz - op.y * sx * sy * sz + op.y * cx * cz - op.z * sx * cy;
        //dp.z := -op.x * cx * sy * cz + op.x * sx * sz + op.y * cx * sy * sz + op.y * sx * cz + op.z * cx * cy;
        //dp.x := op.w;

        dp := op;
        dp := dp.RotateWithMatrixAroundX((Self.rx));
        dp := dp.RotateWithMatrixAroundY((Self.ry));
        dp := dp.RotateAroundZ((Self.rz));
        dp.w := op.w;
        FMorphObjectTo.Add(dp);
   end;
end;

procedure TMainForm.BuildCube;
var
  i,x,y,z: Integer;
  {$CODEALIGN VARMIN=16}
  p : TBZVector4f;
  {$CODEALIGN VARMIN=4}
begin
  //zeroAlphas();
  //if FMorphObjectFrom.Count>0 then
  //begin
  //  for i := 0 to FMorphObjectFrom.Count-1 do
  //  begin
  //     p:= FMorphObjectFrom.Items[i];
  //     p.w := 0;
  //     FMorphObjectFrom.Items[i] := p;
  //  end;
  //end;

  FMorphObjectTmp.Clear; //tmp
  //FPatternSize := (5 *80 - 80);// div 2;

  for z := -2 to 2 do
    for x:= -2 to 2 do
      for y := -2 to 2 do
      Begin
        //p.x := x;
        //p.y := y;
        //p.z := z;
        p.x := x * FShapeSprites[2].Width - FShapeSprites[0].CenterX + 10;
        p.y := y * FShapeSprites[2].Width - FShapeSprites[0].CenterX + 10;
        p.z := z * FShapeSprites[2].Width - FShapeSprites[0].CenterX + 10;
        p.w := 255;
        FMorphObjectTmp.Add(p);
      End;

  GenerateSteps(128);
end;

procedure TMainForm.BuildPlane;
var
  i, x,y: Integer;
  {$CODEALIGN VARMIN=16}
  p : TBZVector4f;
  {$CODEALIGN VARMIN=4}
begin
  //zeroAlphas();


  FMorphObjectTmp.Clear; //tmp
  FPatternSize := (9 *60 - 50) div 2;
    for x:= -4 to 4 do
      for y := -4 to 4 do
      Begin
        p.x := x * 60 - 50;
        p.y := y * 60 - 50;
        p.z := 0;
        p.w := 255;
        FMorphObjectTmp.Add(p);
      End;

    GenerateSteps(128);
end;

procedure TMainForm.BuildSpiral;
var
  i: Integer;
  n : single;
  {$CODEALIGN VARMIN=16}
  p : TBZVector4f;
  {$CODEALIGN VARMIN=4}
begin
  //zeroAlphas();

  n:= 5.0;

  FMorphObjectTmp.Clear; //tmp
  FPatternSize := (9 *60 - 50) div 2;
    for i := 1 to 42 do
    Begin
      p.x := (i * 5 + 15) * cos(i * c2Pi / n);
      p.y := (i * 5 + 15) * sin(i * c2Pi / n);
      p.z := i * (20 - n) - 180;
      p.w := 255;
      FMorphObjectTmp.Add(p);
      n :=n + 0.2;
    End;

    GenerateSteps(128);
end;

procedure TMainForm.BuildCircle;
var
  i: Integer;
  n : single;
  p : TBZVector4f;
begin
  //zeroAlphas();

  n:= 5.0;

  FMorphObjectTmp.Clear; //tmp
  FPatternSize := (9 *60 - 50) div 2;
    for i := 0 to 24 do
    Begin
      p.x := 180 * Cos(i * c2Pi / 25);
      p.y := 180 * Sin(i * c2Pi / 25);
      p.z := 0;//i * (20 - n) - 180;
      p.w := 255;
      FMorphObjectTmp.Add(p);
      //n :=n + 0.2;
    End;

    GenerateSteps(128);

end;

procedure TMainForm.BuildSpiral2;
var
  i: Integer;
  n : single;
  {$CODEALIGN VARMIN=16}
  p : TBZVector4f;
  {$CODEALIGN VARMIN=4}
begin
  //zeroAlphas();

  n:= 5.0;

  FMorphObjectTmp.Clear; //tmp
  FPatternSize := (9 *60 - 50) div 2;
    for i := 1 to 36 do
    Begin
      p.x := 100 * Cos(i * c2Pi / 18);
      p.y := 100 * Sin(i * c2Pi / 18);
      p.z := i * 10 - 180;
      p.w := 255;
      FMorphObjectTmp.Add(p);
      //n :=n + 0.2;
    End;

    GenerateSteps(128);

end;

procedure TMainForm.BuildCircleWave;
var
  i: Integer;
  n : single;
  {$CODEALIGN VARMIN=16}
  p : TBZVector4f;
  {$CODEALIGN VARMIN=4}
begin
  //zeroAlphas();

  n:= 5.0;

  FMorphObjectTmp.Clear; //tmp
  FPatternSize := (9 *60 - 50) div 2;
    for i := 1 to 36 do
    Begin
      p.x := 150 * Cos(i * c2Pi / 36);
      p.y := 150 * Sin(i * c2Pi / 36);
      p.z := 50 * Sin(i * c2Pi / 9);
      p.w := 255;
      FMorphObjectTmp.Add(p);
      //n :=n + 0.2;
    End;

    GenerateSteps(128);

end;

procedure TMainForm.BuildTwister;
var
  i,x,y: Integer;
  {$CODEALIGN VARMIN=16}
  p : TBZVector4f;
  {$CODEALIGN VARMIN=4}
begin


  FMorphObjectTmp.Clear; //tmp
  FPatternSize := (9 *60 - 50) div 2;
    for x:= 0 to 6 do
      for y := 0 to 5 do
      Begin
        p.x := 160 * Cos(y * c2Pi / 6 + x * 5);
        p.y := 160 * Sin(y * c2Pi / 6 + x * 5);
        p.z := 70 * x - 245;
        p.w := 255;
        FMorphObjectTmp.Add(p);
      End;
    GenerateSteps(128);

end;

procedure TMainForm.BuildCube2;
var
  i,x,y: Integer;
  {$CODEALIGN VARMIN=16}
  p : TBZVector4f;
  {$CODEALIGN VARMIN=4}
begin
  //zeroAlphas();

  FMorphObjectTmp.Clear; //tmp
  FPatternSize := (9 *60 - 50) div 2;
    for x:= 0 to 4 do
      Begin
        p.x := x * 50 - 100;
        p.y := 100;
        p.z := 100;
        p.w := 255;
        FMorphObjectTmp.Add(p);
      End;

    for x:= 0 to 4 do
      Begin
        p.x := x * 50 - 100;
        p.y := 100;
        p.z := -100;
        p.w := 255;
        FMorphObjectTmp.Add(p);
      End;

    for x:= 0 to 4 do
      Begin
        p.x := x * 50 - 100;
        p.y := -100;
        p.z := 100;
        p.w := 255;
        FMorphObjectTmp.Add(p);
      End;

    for x:= 0 to 4 do
      Begin
        p.x := x * 50 - 100;
        p.y := -100;
        p.z := -100;
        p.w := 255;
        FMorphObjectTmp.Add(p);
      End;

    for x:= 0 to 2 do
      Begin
        p.x := -100;
        p.y := 100;
        p.z := x * 50 - 50;
        p.w := 255;
        FMorphObjectTmp.Add(p);
      End;

    for x:= 0 to 2 do
      Begin
        p.x := 100;
        p.y := 100;
        p.z := x * 50 - 50;
        p.w := 255;
        FMorphObjectTmp.Add(p);
      End;

    for x:= 0 to 2 do
      Begin
        p.x := 100;
        p.y := -100;
        p.z := x * 50 - 50;
        p.w := 255;
        FMorphObjectTmp.Add(p);
      End;

    for x:= 0 to 2 do
      Begin
        p.x := -100;
        p.y := -100;
        p.z := x * 50 - 50;
        p.w := 255;
        FMorphObjectTmp.Add(p);
      End;

    for x:= 0 to 2 do
      Begin
        p.x := -100;
        p.y := x * 50 - 50;
        p.z := 100;
        p.w := 255;
        FMorphObjectTmp.Add(p);
      End;

    for x:= 0 to 2 do
      Begin
        p.x := 100;
        p.y := x * 50 - 50;
        p.z := 100;
        p.w := 255;
        FMorphObjectTmp.Add(p);
      End;

    for x:= 0 to 2 do
      Begin
        p.x := 100;
        p.y := x * 50 - 50;
        p.z := -100;
        p.w := 255;
        FMorphObjectTmp.Add(p);
      End;

    for x:= 0 to 2 do
      Begin
        p.x := -100;
        p.y := x * 50 - 50;
        p.z := -100;
        p.w := 255;
        FMorphObjectTmp.Add(p);
      End;

    GenerateSteps(128);

end;

procedure TMainForm.BuildSphere;
var
  v: TBZVector4f;
  Radius, a,b: Single;
  i,j, FVSeg, FHSeg: integer;
  ks,kt,da,db,rrx,rrz,rrxz,rry: Single;

begin
  FVSeg := 12;
  FHSeg := 12;
  Radius := 200;

  da:=cPi/FVSeg;
  db:=c2Pi/(FHSeg-1);

  ks:=1/(FHSeg-1);
  kt:=1/(FVSeg);
 FMorphObjectTmp.Clear; //tmp

    for i:=0 to FVSeg do
    begin
      //if NormalDirection = ndInside then
      a:=cPIDiv2-i*da;
      //else a:=i*da-pi/2;
      rry :=  Sin(a) * radius;
      rrxz := Cos(a) * radius;

      for j:=0 to FHSeg-1 do
      begin
        b:=j*db;
        rrx:=rrxz*cos(b);
        rrz:=rrxz*sin(b);
        //vi:=i*(FHSeg)+j;
        v.x:=rrx;
        v.y:=rry;
        v.z:=rrz;
        v.w := 255;
       FMorphObjectTmp.Add(v);
      end;
    end;
    GenerateSteps(128);
end;

procedure TMainForm.GenerateSteps(n : word);
var
  i,k : integer;
  {$CODEALIGN VARMIN=16}
  p,p1 : TBZVector4f;
  {$CODEALIGN VARMIN=4}
  dn : single;
begin
  dn := n;
  k := FMorphObjectTmp.Count - 1;
  FMorphSteps.Clear;
  FSteps := n;
  for i := 0 to k do
  begin
    //if i> FMorphObjectFrom.Count then
    //begin
    //  p := FMorphObjectTmp.Items[i];
    //  p.x := p.x / dn;
    //  p.y := p.y / dn;
    //  p.z := p.z / dn;
    //  p.w := p.w / dn;
    // // p1.Create(0,0,0,0);
    //  FMorphObjectFrom.Add(FMorphObjectTmp.Items[i]);
    //end
    //else
    //begin
      p := FMorphObjectTmp.Items[i];
      p1 := FMorphObjectFrom.Items[i];
      p1.W := 0;
      //p.x := p.x - p1.x;
      //p.y := p.y - p1.y;
      //p.z := p.z - p1.z;
      //p.w := p.w - p1.w;

      p := p - p1;
       p := p / dn;

      //p.x := p.x / dn;
      //p.y := p.y / dn;
      //p.z := p.z / dn;
      //p.w := p.w / dn;
   // end;
    FMorphSteps.Add(p);
  end;
  if (FMorphObjectTo.Count+FMorphObjectTmp.Count) < FMorphObjectFrom.Count then
  begin
    FMorphObjectFrom.Capacity := FMorphObjectFrom.Capacity + 500;
  end;
end;

procedure TMainForm.MorphToNextPoint;
var
  i,k : integer;
  {$CODEALIGN VARMIN=16}
  p,p1 : TBZVector4f;
  {$CODEALIGN VARMIN=4}
begin
 // FMorphObjectFrom.Clear;
  k := FMorphObjectFrom.Count - 1;
 // showmessage(k.ToString);
  Dec(FSteps);
  for i := 0 to k do
  begin
         //if i<  FMorphSteps.Count then
         //begin
           p := FMorphObjectFrom.Items[i];
           p1 := FMorphSteps.Items[i];
         //end;
         //else
         //begin
         //   p1.Create(0,0,0,0);
         //   p.Create(0,0,0,255);
         //end;
        //p := p + p1;
        p.x := p.x + p1.x;
        p.y := p.y + p1.y;
        p.z := p.z + p1.z;
        p.w := p.w + p1.w;
        FMorphObjectFrom.Items[i] := p;

  end;
end;

function ComparerZSortFunc(Const Item1, Item2 ) : Integer;
var
  {$CODEALIGN VARMIN=16}
  i1 : TBZVector4f ABSOLUTE Item1;
  i2 : TBZVector4f ABSOLUTE Item2;
  {$CODEALIGN VARMIN=4}
begin
  Result := 0;
  if ( i1.z =  i2.z ) then exit;
  if (i1.z-0.001) > (i2.z+0.001) then result:= -1 else result := 1;
end;

procedure TMainForm.RenderScene;
// Coeur du rendu de la scene
var
  i, iMax, idx : Integer;
  {$CODEALIGN VARMIN=16}
  screenPos, v1,v2,v3,Screentranslate : TBZVector2f;
  p : TBZVector2i;
  world,v4 : TBZVector4f;
  {$CODEALIGN VARMIN=4}
  AlphaSprite : Integer;
  s,t,r: Single;
begin
 // FMorphObjectTo. rotate();
  ScreenTranslate.Create(FDisplayBuffer.CenterX,FDisplayBuffer.CenterY);
  //FDisplayBuffer.ColorFilter.AdjustBrightness(0.90);
  FDisplayBuffer.Clear(clrBlue);
  // r := FDisplayBuffer.Height / FDisplayBuffer.Width;
   r := 1;
   RotateVectorBalls();
   s := cPerspective;
   v1.Create(s,s);
   //v1.Create(FDisplayBuffer.CenterX,FDisplayBuffer.CenterY);

   //v3.Create(FPatternSize,FPatternSize);
   FMorphObjectTo.Sort; //(soDescending);
   //if FSteps > 114 then
   //begin
   // GlobalLogger.LogNotice('---------------------------------');
   // for i := 0 to FMorphObjectTo.Count-1 do
   // begin
   //
   //     GlobalLogger.LogNotice('MorphTo.Z ='+FMorphObjectTo.Items[i].z.toString);
   // end;
   //end;
   //v4.Create(FDisplayBuffer.CenterX/2,FDisplayBuffer.CenterY/2,FDisplayBuffer.CenterX/2,0);
   iMax := FMorphObjectTo.Count - 1;
   AlphaSprite := 255;
   for  i := 0 to iMax do
   begin
     //interpolation value
     //AlphaSprite :=ClampByte(Round(Max(0, min(255,150 - FMorphObjectTo.Items[I].z * 0.25))));
     //AlphaSprite := ClampByte( max(0,Round(192 +  FMorphObjectTo.Items[I].z)));
     //AlphaSprite := Round((-(FMorphObjectTo.Items[I].z) /(4*FPatternSize)) * 255 + 255);
     AlphaSprite := Round(FMorphObjectTo.Items[I].w);
     if (AlphaSprite > 5) then
     begin
       //AlphaSprite := (AlphaSprite * 255) div 100;
       //AlphaSprite := Abs(511-AlphaSprite);
       AlphaSprite := 255;//92+AlphaSprite;
       t := 1/(cPerspective + (FMorphObjectTo.Items[i].z));
       v2.Create(t,t);
         //world := FMorphObjectTo.Items[I].XY;
       world := FMorphObjectTo.Items[I];

       //world := world + v4;
       ScreenPos := World.XY;
      // 	struct vector3d z_translate = {0, 0, 5};
      // 			struct vector3d world = {v_balls[i].x, v_balls[i].y, v_balls[i].z};
      // 			struct vector2d screen = {world.x, world.y};
      // 			struct vector2d screen_translate = {width / 2, height / 2};
      //
   			////move 3d vectors away from the camera
        //world := world + 5;
        //world := world + cTranslateZ;
        ////calculate perspective for 2d screen
        ScreenPos := ScreenPos * v1;
        //v1 := v1 * v2;
        //ScreenPos := ScreenPos + 1;
        ScreenPos := ScreenPos * v2; // (cPerspective+World.z);
        ////scale vectors for 2d screen
        //ScreenPos := ScreenPos *  ScreenScale;
        ////position vectors
        ScreenPos := ScreenPos + ScreenTranslate;
   			//add_vector3d(&world, &z_translate);
   			//divide_vector(&screen, world.z);
   			//multiply_vector(&screen, scale);
   			//add_vector(&screen, &screen_translate);

         //pf := pf * v1;
         //pf := pf * v2;
         //pf := pf + v4;// - v3;
         p := ScreenPos.Round;
////         GlobalLogger.LogNotice('World.Z = '+(World.Z/4).ToString);
         //idx  := (round(World.z*0.25));
         idx := 1;
         //idx := max(0,min(4, World.Z/4
//         GlobalLogger.LogNotice('Z/idx = '+(World.Z/4).ToString+' / '+Idx.ToString);
         //if (idx<-300) then idx := -1
         //else if (idx>=-300)  and (idx<-150) then idx := 0
         //else if (idx>=-150)  and (idx<0) then idx := 1
         //else if (idx>=0)  and (idx<150) then idx := 2
         //else if (idx>=150)  and (idx<300) then idx := 3
         //else if (idx>=300) then idx := -1
         //else if (idx=0)  and (idx<150) then idx := 2;



         //if idx>-1 then
         FDisplayBuffer.PutImage(FShapeSprites[Idx],p.x,p.Y,AlphaSprite);
         //FDisplayBuffer.PutImage(FShapeSprite,p.x,p.Y,Abs(AlphaSprite-128));


     end;

   end;
   {

       //reposition sprites
       sprite[i].className = "ball2";// + Math.max(0, Math.min(4, 4 - Math.round((150 + b[i].z) / 80)));

   }



      Self.rx := Self.rx + Self.dx;
      Self.ry := Self.ry + Self.dy;
      Self.rz := Self.rz + Self.dz;
   {
      if (autoNext)
          cycleCount++;

      if (cycleCount > 200)

          cycleCount = 0;
          _this.nextPattern();
      }


     if (Fsteps > 0)  then MorphToNextPoint;
   //FDisplayBuffer.BlurFilter.FastBlur;

end;

procedure TMainForm.DoneScene;
var
  i : Integer;
begin
  // Finalisation de la scene
   for i := 4 downto 0 do
   begin
     FreeAndNil(FShapeSprites[i]);
   end;
   FreeAndNil(FMorphSteps);
   FreeAndNil(FMorphObjectFrom);
   FreeAndNil(FMorphObjectTmp);
   FreeAndNil(FMorphObjectTo);

end;

end.

