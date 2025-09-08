unit pascubeunit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, ComCtrls,
  StdCtrls, OpenGLContext, GL, GLU, GLExt, Math, LCLType;

type
  { Tpascubeform }
  Tpascubeform = class(TForm)
    pascubeOpenGLControl: TOpenGLControl;
    Timer1: TTimer;
    PanelControls: TPanel;

    // TrackBars para posição da câmera
    TrackBarCamX: TTrackBar;
    TrackBarCamY: TTrackBar;
    TrackBarCamZ: TTrackBar;

    // Labels para mostrar valores
    LabelCamX: TLabel;
    LabelCamY: TLabel;
    LabelCamZ: TLabel;
    LabelValueX: TLabel;
    LabelValueY: TLabel;
    LabelValueZ: TLabel;

    // CheckBox para auto-rotação
    CheckBoxAutoRotate: TCheckBox;
    CheckBoxReflection: TCheckBox;

    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure pascubeOpenGLControlPaint(Sender: TObject);
    procedure pascubeOpenGLControlResize(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure TrackBarCamChange(Sender: TObject);
    procedure CheckBoxAutoRotateChange(Sender: TObject);
    procedure CheckBoxReflectionChange(Sender: TObject);
  private
    FRotationX: GLfloat;
    FRotationY: GLfloat;
    FRotationZ: GLfloat;
    FInitialized: Boolean;
    FLightAngle: GLfloat;
    FCameraX: GLfloat;
    FCameraY: GLfloat;
    FCameraZ: GLfloat;
    FAutoRotate: Boolean;
    FShowReflection: Boolean;
    FSkyboxTexture: GLuint;
    FEnvMapTexture: GLuint;
    procedure InitializeGL;
    procedure ApplyViewportAndProjection;
    procedure DrawMetallicCube;
    procedure DrawSkybox;
    procedure SetupLighting;
    procedure UpdateLighting;
    procedure SetMetallicMaterial(MetalType: Integer);
    procedure CreateControls;
    procedure UpdateCameraLabels;
    function LoadTexture(const FileName: string): GLuint;
    procedure CreateEnvironmentMap;
    procedure GenerateProceduralSkybox;
  public
  end;

var
  pascubeform: Tpascubeform;

implementation

{$R *.lfm}

{ Tpascubeform }

procedure Tpascubeform.FormCreate(Sender: TObject);
begin
  FRotationX := 30;
  FRotationY := 45;
  FRotationZ := 0;
  FInitialized := False;
  FLightAngle := 30; //0
  FAutoRotate := True;
  FShowReflection := True;

  // Posição inicial da câmera
  FCameraX := 5.0;
  FCameraY := 2.0;
  FCameraZ := 3.0;

  // Criar controles da interface
  CreateControls;

  // Configurar bits de profundidade
  pascubeOpenGLControl.DepthBits := 24;
  pascubeOpenGLControl.StencilBits := 8;

  // AutoResizeViewport ligado
  pascubeOpenGLControl.AutoResizeViewport := True;

  // 60 FPS
  Timer1.Interval := 4;
  Timer1.Enabled := True;
end;

procedure Tpascubeform.CreateControls;
begin
  // Criar painel para controles
  PanelControls := TPanel.Create(Self);
  PanelControls.Parent := Self;
  PanelControls.Align := alRight;
  PanelControls.Width := 200;
  PanelControls.Caption := '';
  PanelControls.BevelOuter := bvLowered;

  // --- Controle Camera X ---
  LabelCamX := TLabel.Create(Self);
  LabelCamX.Parent := PanelControls;
  LabelCamX.Left := 10;
  LabelCamX.Top := 10;
  LabelCamX.Caption := 'Câmera X:';

  LabelValueX := TLabel.Create(Self);
  LabelValueX.Parent := PanelControls;
  LabelValueX.Left := 150;
  LabelValueX.Top := 10;
  LabelValueX.Caption := '5.0';

  TrackBarCamX := TTrackBar.Create(Self);
  TrackBarCamX.Parent := PanelControls;
  TrackBarCamX.Left := 10;
  TrackBarCamX.Top := 30;
  TrackBarCamX.Width := 180;
  TrackBarCamX.Min := -100;
  TrackBarCamX.Max := 100;
  TrackBarCamX.Position := 50;
  TrackBarCamX.TickMarks := tmBoth;
  TrackBarCamX.TickStyle := tsNone;
  TrackBarCamX.OnChange := @TrackBarCamChange;

  // --- Controle Camera Y ---
  LabelCamY := TLabel.Create(Self);
  LabelCamY.Parent := PanelControls;
  LabelCamY.Left := 10;
  LabelCamY.Top := 70;
  LabelCamY.Caption := 'Câmera Y:';

  LabelValueY := TLabel.Create(Self);
  LabelValueY.Parent := PanelControls;
  LabelValueY.Left := 150;
  LabelValueY.Top := 70;
  LabelValueY.Caption := '3.0';

  TrackBarCamY := TTrackBar.Create(Self);
  TrackBarCamY.Parent := PanelControls;
  TrackBarCamY.Left := 10;
  TrackBarCamY.Top := 90;
  TrackBarCamY.Width := 180;
  TrackBarCamY.Min := -100;
  TrackBarCamY.Max := 100;
  TrackBarCamY.Position := 30;
  TrackBarCamY.TickMarks := tmBoth;
  TrackBarCamY.TickStyle := tsNone;
  TrackBarCamY.OnChange := @TrackBarCamChange;

  // --- Controle Camera Z ---
  LabelCamZ := TLabel.Create(Self);
  LabelCamZ.Parent := PanelControls;
  LabelCamZ.Left := 10;
  LabelCamZ.Top := 130;
  LabelCamZ.Caption := 'Câmera Z:';

  LabelValueZ := TLabel.Create(Self);
  LabelValueZ.Parent := PanelControls;
  LabelValueZ.Left := 150;
  LabelValueZ.Top := 130;
  LabelValueZ.Caption := '5.0';

  TrackBarCamZ := TTrackBar.Create(Self);
  TrackBarCamZ.Parent := PanelControls;
  TrackBarCamZ.Left := 10;
  TrackBarCamZ.Top := 150;
  TrackBarCamZ.Width := 180;
  TrackBarCamZ.Min := -100;
  TrackBarCamZ.Max := 100;
  TrackBarCamZ.Position := 50;
  TrackBarCamZ.TickMarks := tmBoth;
  TrackBarCamZ.TickStyle := tsNone;
  TrackBarCamZ.OnChange := @TrackBarCamChange;

  // --- CheckBox para auto-rotação ---
  CheckBoxAutoRotate := TCheckBox.Create(Self);
  CheckBoxAutoRotate.Parent := PanelControls;
  CheckBoxAutoRotate.Left := 10;
  CheckBoxAutoRotate.Top := 200;
  CheckBoxAutoRotate.Caption := 'Rotação Automática';
  CheckBoxAutoRotate.Checked := True;
  CheckBoxAutoRotate.OnChange := @CheckBoxAutoRotateChange;

  // --- CheckBox para reflexão ---
  CheckBoxReflection := TCheckBox.Create(Self);
  CheckBoxReflection.Parent := PanelControls;
  CheckBoxReflection.Left := 10;
  CheckBoxReflection.Top := 225;
  CheckBoxReflection.Caption := 'Reflexão de Ambiente';
  CheckBoxReflection.Checked := True;
  CheckBoxReflection.OnChange := @CheckBoxReflectionChange;

  // Ajustar o OpenGL Control
  pascubeOpenGLControl.Align := alClient;
end;

procedure Tpascubeform.GenerateProceduralSkybox;
var
  TextureData: array[0..255, 0..255, 0..2] of Byte;
  x, y: Integer;
  r, g, b: Byte;
  factor: Single;
begin
  // Gerar gradiente de céu procedural
  for y := 0 to 255 do
  begin
    for x := 0 to 255 do
    begin
      // Criar gradiente vertical de azul escuro para azul claro
      factor := y / 255.0;

      // Céu: de azul escuro (topo) para azul claro com toque de laranja (horizonte)
      r := Round(50 + factor * 150);  // De 50 a 200
      g := Round(80 + factor * 120);  // De 80 a 200
      b := Round(180 - factor * 50);  // De 180 a 130

      // Adicionar algumas "nuvens" usando ruído simples
      if (Sin(x * 0.05) * Cos(y * 0.03) > 0.5) then
      begin
        r := Min(255, r + 30);
        g := Min(255, g + 30);
        b := Min(255, b + 30);
      end;

      TextureData[y, x, 0] := r;
      TextureData[y, x, 1] := g;
      TextureData[y, x, 2] := b;
    end;
  end;

  // Criar textura OpenGL
  glGenTextures(1, @FSkyboxTexture);
  glBindTexture(GL_TEXTURE_2D, FSkyboxTexture);

  // Configurar parâmetros
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);

  // Enviar dados para GPU
  glTexImage2D(GL_TEXTURE_2D, 0, GL_RGB, 512, 512, 0, GL_RGB, GL_UNSIGNED_BYTE, @TextureData);
end;

procedure Tpascubeform.CreateEnvironmentMap;
begin
  // Por simplicidade, usar a mesma textura do skybox como environment map
  // Em uma implementação mais avançada, você criaria um cubemap real
  FEnvMapTexture := FSkyboxTexture;
end;

function Tpascubeform.LoadTexture(const FileName: string): GLuint;
var
  Picture: TPicture;
  Bitmap: TBitmap;
  Data: array of Byte;
  x, y, idx: Integer;
  PixelColor: TColor;
begin
  Result := 0;

  if not FileExists(FileName) then
    Exit;

  Picture := TPicture.Create;
  Bitmap := TBitmap.Create;
  try
    try
      Picture.LoadFromFile(FileName);

      Bitmap.Width := Picture.Width;
      Bitmap.Height := Picture.Height;
      Bitmap.PixelFormat := pf24bit;
      Bitmap.Canvas.Draw(0, 0, Picture.Graphic);

      glGenTextures(1, @Result);
      glBindTexture(GL_TEXTURE_2D, Result);

      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);

      SetLength(Data, Bitmap.Width * Bitmap.Height * 3);

      idx := 0;
      for y := Bitmap.Height - 1 downto 0 do
      begin
        for x := 0 to Bitmap.Width - 1 do
        begin
          PixelColor := Bitmap.Canvas.Pixels[x, y];
          Data[idx] := (PixelColor and $0000FF);
          Data[idx + 1] := (PixelColor and $00FF00) shr 8;
          Data[idx + 2] := (PixelColor and $FF0000) shr 16;
          Inc(idx, 3);
        end;
      end;

      glTexImage2D(GL_TEXTURE_2D, 0, GL_RGB, Bitmap.Width, Bitmap.Height,
                   0, GL_RGB, GL_UNSIGNED_BYTE, @Data[0]);

    except
      // Silencioso em caso de erro
    end;
  finally
    Bitmap.Free;
    Picture.Free;
  end;
end;

procedure Tpascubeform.TrackBarCamChange(Sender: TObject);
begin
  FCameraX := TrackBarCamX.Position / 10.0;
  FCameraY := TrackBarCamY.Position / 10.0;
  FCameraZ := TrackBarCamZ.Position / 10.0;

  UpdateCameraLabels;
  pascubeOpenGLControl.Invalidate;
end;

procedure Tpascubeform.CheckBoxAutoRotateChange(Sender: TObject);
begin
  FAutoRotate := CheckBoxAutoRotate.Checked;
end;

procedure Tpascubeform.CheckBoxReflectionChange(Sender: TObject);
begin
  FShowReflection := CheckBoxReflection.Checked;
  pascubeOpenGLControl.Invalidate;
end;

procedure Tpascubeform.UpdateCameraLabels;
begin
  LabelValueX.Caption := FormatFloat('0.0', FCameraX);
  LabelValueY.Caption := FormatFloat('0.0', FCameraY);
  LabelValueZ.Caption := FormatFloat('0.0', FCameraZ);
end;

procedure Tpascubeform.FormShow(Sender: TObject);
begin
  if not FInitialized then
  begin
    pascubeOpenGLControl.MakeCurrent;
    InitializeGL;
  end;
  pascubeOpenGLControlResize(nil);
  pascubeOpenGLControl.Invalidate;
end;

procedure Tpascubeform.FormDestroy(Sender: TObject);
begin
  Timer1.Enabled := False;

  if FSkyboxTexture > 0 then
    glDeleteTextures(1, @FSkyboxTexture);
end;

procedure Tpascubeform.InitializeGL;
var
  SkyboxFile: string;
begin
  if FInitialized then Exit;

  pascubeOpenGLControl.MakeCurrent;

  glClearColor(0.05, 0.05, 0.1, 1.0);

  glEnable(GL_DEPTH_TEST);
  glDepthFunc(GL_LEQUAL);

  glEnable(GL_LIGHTING);
  glEnable(GL_LIGHT0);
  glEnable(GL_LIGHT1);

  glDisable(GL_COLOR_MATERIAL);

  glShadeModel(GL_SMOOTH);
  glHint(GL_PERSPECTIVE_CORRECTION_HINT, GL_NICEST);

  glEnable(GL_NORMALIZE);

  glEnable(GL_LINE_SMOOTH);
  glEnable(GL_POLYGON_SMOOTH);
  glEnable(GL_BLEND);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

  SetupLighting;

  // Tentar carregar textura de skybox ou gerar procedural
  SkyboxFile := ExtractFilePath(Application.ExeName) + 'skybox.jpg';
  if not FileExists(SkyboxFile) then
    SkyboxFile := ExtractFilePath(Application.ExeName) + 'skybox.png';

  if FileExists(SkyboxFile) then
    FSkyboxTexture := LoadTexture(SkyboxFile)
  else
    GenerateProceduralSkybox;  // Gerar textura procedural se não encontrar arquivo

  CreateEnvironmentMap;

  FInitialized := True;
end;

procedure Tpascubeform.SetupLighting;
var
  Light0Ambient:   array[0..3] of GLfloat = (0.2, 0.2, 0.2, 1.0);
  Light0Diffuse:   array[0..3] of GLfloat = (0.8, 0.8, 0.8, 1.0);
  Light0Specular:  array[0..3] of GLfloat = (1.0, 1.0, 1.0, 1.0);
  Light0Position:  array[0..3] of GLfloat = (5.0, 5.0, 5.0, 1.0);

  Light1Ambient:   array[0..3] of GLfloat = (0.0, 0.0, 0.0, 1.0);
  Light1Diffuse:   array[0..3] of GLfloat = (0.3, 0.3, 0.5, 1.0);
  Light1Specular:  array[0..3] of GLfloat = (0.3, 0.3, 0.5, 1.0);
  Light1Position:  array[0..3] of GLfloat = (-5.0, -3.0, 3.0, 1.0);

  GlobalAmbient: array[0..3] of GLfloat = (0.1, 0.1, 0.1, 1.0);
begin
  glLightfv(GL_LIGHT0, GL_AMBIENT,  @Light0Ambient);
  glLightfv(GL_LIGHT0, GL_DIFFUSE,  @Light0Diffuse);
  glLightfv(GL_LIGHT0, GL_SPECULAR, @Light0Specular);
  glLightfv(GL_LIGHT0, GL_POSITION, @Light0Position);

  glLightfv(GL_LIGHT1, GL_AMBIENT,  @Light1Ambient);
  glLightfv(GL_LIGHT1, GL_DIFFUSE,  @Light1Diffuse);
  glLightfv(GL_LIGHT1, GL_SPECULAR, @Light1Specular);
  glLightfv(GL_LIGHT1, GL_POSITION, @Light1Position);

  glLightModelfv(GL_LIGHT_MODEL_AMBIENT, @GlobalAmbient);
  glLightModeli(GL_LIGHT_MODEL_TWO_SIDE, GL_TRUE);
end;

procedure Tpascubeform.UpdateLighting;
var
  Light0Position: array[0..3] of GLfloat;
begin
  Light0Position[0] := 5.0 * cos(FLightAngle * PI / 180);
  Light0Position[1] := 5.0;
  Light0Position[2] := 5.0 * sin(FLightAngle * PI / 180);
  Light0Position[3] := 1.0;

  glLightfv(GL_LIGHT0, GL_POSITION, @Light0Position);
end;

procedure Tpascubeform.SetMetallicMaterial(MetalType: Integer);
var
  MaterialAmbient: array[0..3] of GLfloat;
  MaterialDiffuse: array[0..3] of GLfloat;
  MaterialSpecular: array[0..3] of GLfloat;
  MaterialShininess: GLfloat;
begin
  case MetalType of
    0: // Aço polido
    begin
      MaterialAmbient[0] := 0.25; MaterialAmbient[1] := 0.25; MaterialAmbient[2] := 0.25; MaterialAmbient[3] := 1.0;
      MaterialDiffuse[0] := 0.4;  MaterialDiffuse[1] := 0.4;  MaterialDiffuse[2] := 0.4;  MaterialDiffuse[3] := 1.0;
      MaterialSpecular[0] := 0.9; MaterialSpecular[1] := 0.9; MaterialSpecular[2] := 0.9; MaterialSpecular[3] := 1.0;
      MaterialShininess := 100.0;
    end;

    1: // Ouro
    begin
      MaterialAmbient[0] := 0.24725; MaterialAmbient[1] := 0.1995; MaterialAmbient[2] := 0.0745; MaterialAmbient[3] := 1.0;
      MaterialDiffuse[0] := 0.75164; MaterialDiffuse[1] := 0.60648; MaterialDiffuse[2] := 0.22648; MaterialDiffuse[3] := 1.0;
      MaterialSpecular[0] := 0.9; MaterialSpecular[1] := 0.8; MaterialSpecular[2] := 0.6; MaterialSpecular[3] := 1.0;
      MaterialShininess := 80.0;
    end;

    2: // Cobre
    begin
      MaterialAmbient[0] := 0.19125; MaterialAmbient[1] := 0.0735; MaterialAmbient[2] := 0.0225; MaterialAmbient[3] := 1.0;
      MaterialDiffuse[0] := 0.7038;  MaterialDiffuse[1] := 0.27048; MaterialDiffuse[2] := 0.0828; MaterialDiffuse[3] := 1.0;
      MaterialSpecular[0] := 0.8; MaterialSpecular[1] := 0.6; MaterialSpecular[2] := 0.4; MaterialSpecular[3] := 1.0;
      MaterialShininess := 90.0;
    end;

    3: // Cromo
    begin
      MaterialAmbient[0] := 0.25; MaterialAmbient[1] := 0.25; MaterialAmbient[2] := 0.25; MaterialAmbient[3] := 1.0;
      MaterialDiffuse[0] := 0.4;  MaterialDiffuse[1] := 0.4;  MaterialDiffuse[2] := 0.4;  MaterialDiffuse[3] := 1.0;
      MaterialSpecular[0] := 0.95; MaterialSpecular[1] := 0.95; MaterialSpecular[2] := 0.95; MaterialSpecular[3] := 1.0;
      MaterialShininess := 128.0;
    end;

    4: // Prata
    begin
      MaterialAmbient[0] := 0.19225; MaterialAmbient[1] := 0.19225; MaterialAmbient[2] := 0.19225; MaterialAmbient[3] := 1.0;
      MaterialDiffuse[0] := 0.50754; MaterialDiffuse[1] := 0.50754; MaterialDiffuse[2] := 0.50754; MaterialDiffuse[3] := 1.0;
      MaterialSpecular[0] := 0.9; MaterialSpecular[1] := 0.9; MaterialSpecular[2] := 0.9; MaterialSpecular[3] := 1.0;
      MaterialShininess := 100.0;
    end;

    5: // Bronze
    begin
      MaterialAmbient[0] := 0.2125; MaterialAmbient[1] := 0.1275; MaterialAmbient[2] := 0.054; MaterialAmbient[3] := 1.0;
      MaterialDiffuse[0] := 0.714;  MaterialDiffuse[1] := 0.4284; MaterialDiffuse[2] := 0.18144; MaterialDiffuse[3] := 1.0;
      MaterialSpecular[0] := 0.7; MaterialSpecular[1] := 0.6; MaterialSpecular[2] := 0.4; MaterialSpecular[3] := 1.0;
      MaterialShininess := 75.0;
    end;

    6: // Latão (Brass)
    begin
      MaterialAmbient[0] := 0.329412; MaterialAmbient[1] := 0.223529; MaterialAmbient[2] := 0.027451; MaterialAmbient[3] := 1.0;
      MaterialDiffuse[0] := 0.780392; MaterialDiffuse[1] := 0.568627; MaterialDiffuse[2] := 0.113725; MaterialDiffuse[3] := 1.0;
      MaterialSpecular[0] := 0.9; MaterialSpecular[1] := 0.8; MaterialSpecular[2] := 0.5; MaterialSpecular[3] := 1.0;
      MaterialShininess := 85.0;
    end;

    7: // Platina
    begin
      MaterialAmbient[0] := 0.23; MaterialAmbient[1] := 0.23; MaterialAmbient[2] := 0.23; MaterialAmbient[3] := 1.0;
      MaterialDiffuse[0] := 0.45; MaterialDiffuse[1] := 0.45; MaterialDiffuse[2] := 0.45; MaterialDiffuse[3] := 1.0;
      MaterialSpecular[0] := 0.97; MaterialSpecular[1] := 0.97; MaterialSpecular[2] := 0.97; MaterialSpecular[3] := 1.0;
      MaterialShininess := 120.0;
    end;

    else // Default: Aço
    begin
      MaterialAmbient[0] := 0.25; MaterialAmbient[1] := 0.25; MaterialAmbient[2] := 0.25; MaterialAmbient[3] := 1.0;
      MaterialDiffuse[0] := 0.4;  MaterialDiffuse[1] := 0.4;  MaterialDiffuse[2] := 0.4;  MaterialDiffuse[3] := 1.0;
      MaterialSpecular[0] := 0.9; MaterialSpecular[1] := 0.9; MaterialSpecular[2] := 0.9; MaterialSpecular[3] := 1.0;
      MaterialShininess := 100.0;
    end;
  end;

  glMaterialfv(GL_FRONT_AND_BACK, GL_AMBIENT, @MaterialAmbient);
  glMaterialfv(GL_FRONT_AND_BACK, GL_DIFFUSE, @MaterialDiffuse);
  glMaterialfv(GL_FRONT_AND_BACK, GL_SPECULAR, @MaterialSpecular);
  glMaterialf(GL_FRONT_AND_BACK, GL_SHININESS, MaterialShininess);
end;

procedure Tpascubeform.ApplyViewportAndProjection;
var
  w, h: GLint;
  Aspect: GLdouble;
begin
  pascubeOpenGLControl.MakeCurrent;

  w := pascubeOpenGLControl.Width;
  h := pascubeOpenGLControl.Height;

  if h <= 0 then h := 1;

  glViewport(0, 0, w, h);

  glMatrixMode(GL_PROJECTION);
  glLoadIdentity;

  Aspect := GLdouble(w) / GLdouble(h);
  gluPerspective(45.0, Aspect, 0.1, 100.0);

  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity;
end;

procedure Tpascubeform.DrawSkybox;
const
  SIZE = 50.0;  // Tamanho grande para envolver toda a cena
begin
  // Desabilitar iluminação para o skybox
  glDisable(GL_LIGHTING);
  glDisable(GL_DEPTH_TEST);

  // Habilitar textura
  glEnable(GL_TEXTURE_2D);
  glBindTexture(GL_TEXTURE_2D, FSkyboxTexture);
  glColor3f(1.0, 1.0, 1.0);

  glPushMatrix;

  // O skybox deve se mover com a câmera (mas não rotacionar)
  glTranslatef(FCameraX, FCameraY, FCameraZ);

  // Desenhar as 6 faces do skybox
  // Face traseira
  glBegin(GL_QUADS);
    glTexCoord2f(0.0, 0.0); glVertex3f(-SIZE, -SIZE, -SIZE);
    glTexCoord2f(1.0, 0.0); glVertex3f( SIZE, -SIZE, -SIZE);
    glTexCoord2f(1.0, 1.0); glVertex3f( SIZE,  SIZE, -SIZE);
    glTexCoord2f(0.0, 1.0); glVertex3f(-SIZE,  SIZE, -SIZE);
  glEnd;

  // Face frontal
  glBegin(GL_QUADS);
    glTexCoord2f(1.0, 0.0); glVertex3f(-SIZE, -SIZE,  SIZE);
    glTexCoord2f(0.0, 0.0); glVertex3f( SIZE, -SIZE,  SIZE);
    glTexCoord2f(0.0, 1.0); glVertex3f( SIZE,  SIZE,  SIZE);
    glTexCoord2f(1.0, 1.0); glVertex3f(-SIZE,  SIZE,  SIZE);
  glEnd;

  // Face esquerda
  glBegin(GL_QUADS);
    glTexCoord2f(0.0, 0.0); glVertex3f(-SIZE, -SIZE,  SIZE);
    glTexCoord2f(1.0, 0.0); glVertex3f(-SIZE, -SIZE, -SIZE);
    glTexCoord2f(1.0, 1.0); glVertex3f(-SIZE,  SIZE, -SIZE);
    glTexCoord2f(0.0, 1.0); glVertex3f(-SIZE,  SIZE,  SIZE);
  glEnd;

  // Face direita
  glBegin(GL_QUADS);
    glTexCoord2f(1.0, 0.0); glVertex3f( SIZE, -SIZE,  SIZE);
    glTexCoord2f(0.0, 0.0); glVertex3f( SIZE, -SIZE, -SIZE);
    glTexCoord2f(0.0, 1.0); glVertex3f( SIZE,  SIZE, -SIZE);
    glTexCoord2f(1.0, 1.0); glVertex3f( SIZE,  SIZE,  SIZE);
  glEnd;

  // Face superior
  glBegin(GL_QUADS);
    glTexCoord2f(0.0, 1.0); glVertex3f(-SIZE,  SIZE,  SIZE);
    glTexCoord2f(0.0, 0.0); glVertex3f(-SIZE,  SIZE, -SIZE);
    glTexCoord2f(1.0, 0.0); glVertex3f( SIZE,  SIZE, -SIZE);
    glTexCoord2f(1.0, 1.0); glVertex3f( SIZE,  SIZE,  SIZE);
  glEnd;

  // Face inferior
  glBegin(GL_QUADS);
    glTexCoord2f(1.0, 1.0); glVertex3f(-SIZE, -SIZE,  SIZE);
    glTexCoord2f(0.0, 1.0); glVertex3f( SIZE, -SIZE,  SIZE);
    glTexCoord2f(0.0, 0.0); glVertex3f( SIZE, -SIZE, -SIZE);
    glTexCoord2f(1.0, 0.0); glVertex3f(-SIZE, -SIZE, -SIZE);
  glEnd;

  glPopMatrix;

  glDisable(GL_TEXTURE_2D);
  glEnable(GL_DEPTH_TEST);
  glEnable(GL_LIGHTING);
end;

procedure Tpascubeform.DrawMetallicCube;
const
  SIZE = 1.0;
var
  Normal: array[0..2] of GLfloat;
  i: Integer;
begin
  SetMetallicMaterial(4);

  // Se reflexão está habilitada, simular com mapeamento de ambiente
  if FShowReflection and (FEnvMapTexture > 0) then
  begin
    glEnable(GL_TEXTURE_2D);
    glBindTexture(GL_TEXTURE_2D, FEnvMapTexture);

    // Configurar geração automática de coordenadas de textura para reflexão
    glTexGeni(GL_S, GL_TEXTURE_GEN_MODE, GL_SPHERE_MAP);
    glTexGeni(GL_T, GL_TEXTURE_GEN_MODE, GL_SPHERE_MAP);
    glEnable(GL_TEXTURE_GEN_S);
    glEnable(GL_TEXTURE_GEN_T);

    // Misturar textura com material
    glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_MODULATE);
  end;

  // Desenhar faces do cubo
  glBegin(GL_QUADS);
    // Face frontal
    glNormal3f(0.0, 0.0, 1.0);
    glVertex3f(-SIZE, -SIZE,  SIZE);
    glVertex3f( SIZE, -SIZE,  SIZE);
    glVertex3f( SIZE,  SIZE,  SIZE);
    glVertex3f(-SIZE,  SIZE,  SIZE);

    // Face traseira
    glNormal3f(0.0, 0.0, -1.0);
    glVertex3f(-SIZE, -SIZE, -SIZE);
    glVertex3f(-SIZE,  SIZE, -SIZE);
    glVertex3f( SIZE,  SIZE, -SIZE);
    glVertex3f( SIZE, -SIZE, -SIZE);

    // Face superior
    glNormal3f(0.0, 1.0, 0.0);
    glVertex3f(-SIZE,  SIZE,  SIZE);
    glVertex3f( SIZE,  SIZE,  SIZE);
    glVertex3f( SIZE,  SIZE, -SIZE);
    glVertex3f(-SIZE,  SIZE, -SIZE);

    // Face inferior
    glNormal3f(0.0, -1.0, 0.0);
    glVertex3f(-SIZE, -SIZE,  SIZE);
    glVertex3f(-SIZE, -SIZE, -SIZE);
    glVertex3f( SIZE, -SIZE, -SIZE);
    glVertex3f( SIZE, -SIZE,  SIZE);

    // Face direita
    glNormal3f(1.0, 0.0, 0.0);
    glVertex3f( SIZE, -SIZE,  SIZE);
    glVertex3f( SIZE, -SIZE, -SIZE);
    glVertex3f( SIZE,  SIZE, -SIZE);
    glVertex3f( SIZE,  SIZE,  SIZE);

    // Face esquerda
    glNormal3f(-1.0, 0.0, 0.0);
    glVertex3f(-SIZE, -SIZE,  SIZE);
    glVertex3f(-SIZE,  SIZE,  SIZE);
    glVertex3f(-SIZE,  SIZE, -SIZE);
    glVertex3f(-SIZE, -SIZE, -SIZE);
  glEnd;

  // Desabilitar mapeamento de ambiente se estava ativo
  if FShowReflection and (FEnvMapTexture > 0) then
  begin
    glDisable(GL_TEXTURE_GEN_S);
    glDisable(GL_TEXTURE_GEN_T);
    glDisable(GL_TEXTURE_2D);
  end;

  // Adicionar bordas sutis
  glDisable(GL_LIGHTING);
  glColor3f(0.9, 0.9, 0.9);
  glLineWidth(0.5);

  glBegin(GL_LINE_LOOP);
    glVertex3f(-SIZE, -SIZE,  SIZE);
    glVertex3f( SIZE, -SIZE,  SIZE);
    glVertex3f( SIZE,  SIZE,  SIZE);
    glVertex3f(-SIZE,  SIZE,  SIZE);
  glEnd;

  glBegin(GL_LINE_LOOP);
    glVertex3f(-SIZE, -SIZE, -SIZE);
    glVertex3f( SIZE, -SIZE, -SIZE);
    glVertex3f( SIZE,  SIZE, -SIZE);
    glVertex3f(-SIZE,  SIZE, -SIZE);
  glEnd;

  glBegin(GL_LINES);
    glVertex3f(-SIZE, -SIZE,  SIZE); glVertex3f(-SIZE, -SIZE, -SIZE);
    glVertex3f( SIZE, -SIZE,  SIZE); glVertex3f( SIZE, -SIZE, -SIZE);
    glVertex3f( SIZE,  SIZE,  SIZE); glVertex3f( SIZE,  SIZE, -SIZE);
    glVertex3f(-SIZE,  SIZE,  SIZE); glVertex3f(-SIZE,  SIZE, -SIZE);
  glEnd;

  glEnable(GL_LIGHTING);
end;

procedure Tpascubeform.pascubeOpenGLControlPaint(Sender: TObject);
begin
  if not FInitialized then
  begin
    pascubeOpenGLControl.MakeCurrent;
    InitializeGL;
  end;

  pascubeOpenGLControl.MakeCurrent;
  ApplyViewportAndProjection;

  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);

  // Desenhar skybox primeiro (fundo)
  DrawSkybox;

  // Configurar câmera
  glLoadIdentity;
  gluLookAt(FCameraX, FCameraY, FCameraZ,
            0.0, 0.0, 0.0,
            0.0, 1.0, 0.0);

  UpdateLighting;

  // Aplicar rotações ao cubo
  glPushMatrix;
  glRotatef(FRotationX, 1.0, 0.0, 0.0);
  glRotatef(FRotationY, 0.0, 1.0, 0.0);
  glRotatef(FRotationZ, 0.0, 0.0, 1.0);

  // Desenhar o cubo metálico com reflexões
  DrawMetallicCube;

  glPopMatrix;

  pascubeOpenGLControl.SwapBuffers;
end;

procedure Tpascubeform.pascubeOpenGLControlResize(Sender: TObject);
begin
  if FInitialized then
  begin
    pascubeOpenGLControl.MakeCurrent;
    ApplyViewportAndProjection;
    pascubeOpenGLControl.Invalidate;
  end;
end;

procedure Tpascubeform.Timer1Timer(Sender: TObject);
begin
  if FAutoRotate then
    FRotationY := FRotationY + 0.5;

  FLightAngle := FLightAngle + 1.5;
  if FLightAngle > 360 then FLightAngle := FLightAngle - 360;

  if FRotationX > 360 then FRotationX := FRotationX - 360;
  if FRotationY > 360 then FRotationY := FRotationY - 360;
  if FRotationZ > 360 then FRotationZ := FRotationZ - 360;

  pascubeOpenGLControl.Invalidate;
end;

end.
