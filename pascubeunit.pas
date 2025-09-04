unit pascubeunit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  OpenGLContext, GL, GLU, LCLType;

type
  { Tpascubeform }
  Tpascubeform = class(TForm)
    pascubeOpenGLControl: TOpenGLControl;
    Timer1: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure pascubeOpenGLControlPaint(Sender: TObject);
    procedure pascubeOpenGLControlResize(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    FRotationX: GLfloat;
    FRotationY: GLfloat;
    FRotationZ: GLfloat;
    FInitialized: Boolean;
    FTextureIDs: array[0..2] of GLuint; // Array para 3 texturas
    FTextureCount: Integer;
    procedure InitializeGL;
    procedure ApplyViewportAndProjection;
    procedure DrawCube;
    procedure SetupLighting;
    function LoadTexture(const FileName: string; var TextureID: GLuint): Boolean;
    procedure LoadAllTextures;
  public
  end;

var
  pascubeform: Tpascubeform;

implementation

{$R *.lfm}

{ Tpascubeform }

procedure Tpascubeform.FormCreate(Sender: TObject);
var
  i: Integer;
begin
  FRotationX := 0;
  FRotationY := 0;
  FRotationZ := 0;
  FInitialized := False;
  FTextureCount := 0;

  // Inicializar array de texturas
  for i := 0 to 2 do
    FTextureIDs[i] := 0;

  // Configurar bits de profundidade
  pascubeOpenGLControl.DepthBits := 24;
  pascubeOpenGLControl.StencilBits := 8;

  // AutoResizeViewport ligado
  pascubeOpenGLControl.AutoResizeViewport := True;

  // 240 FPS aproximados
  Timer1.Interval := 4;
  Timer1.Enabled := True;
end;

procedure Tpascubeform.FormShow(Sender: TObject);
begin
  // Força inicialização e ajuste
  if not FInitialized then
  begin
    pascubeOpenGLControl.MakeCurrent;
    InitializeGL;
  end;
  pascubeOpenGLControlResize(nil);
  pascubeOpenGLControl.Invalidate;
end;

procedure Tpascubeform.FormDestroy(Sender: TObject);
var
  i: Integer;
begin
  Timer1.Enabled := False;

  // Liberar todas as texturas carregadas
  pascubeOpenGLControl.MakeCurrent;
  for i := 0 to 2 do
  begin
    if FTextureIDs[i] > 0 then
      glDeleteTextures(1, @FTextureIDs[i]);
  end;
end;

function Tpascubeform.LoadTexture(const FileName: string; var TextureID: GLuint): Boolean;
var
  Picture: TPicture;
  Bitmap: TBitmap;
  Data: array of Byte;
  x, y, idx: Integer;
  PixelColor: TColor;
begin
  Result := False;

  if not FileExists(FileName) then
  begin
    Exit; // Sair silenciosamente se não encontrar
  end;

  Picture := TPicture.Create;
  Bitmap := TBitmap.Create;
  try
    try
      // Carregar a imagem usando TPicture
      Picture.LoadFromFile(FileName);

      // Converter para bitmap
      Bitmap.Width := Picture.Width;
      Bitmap.Height := Picture.Height;
      Bitmap.PixelFormat := pf24bit;
      Bitmap.Canvas.Draw(0, 0, Picture.Graphic);

      // Gerar uma textura OpenGL
      glGenTextures(1, @TextureID);
      glBindTexture(GL_TEXTURE_2D, TextureID);

      // Configurar parâmetros da textura
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);

      // Criar array dinâmico para os dados
      SetLength(Data, Bitmap.Width * Bitmap.Height * 3);

      // Copiar pixels usando Canvas.Pixels
      idx := 0;
      for y := Bitmap.Height - 1 downto 0 do  // Inverter Y para OpenGL
      begin
        for x := 0 to Bitmap.Width - 1 do
        begin
          PixelColor := Bitmap.Canvas.Pixels[x, y];
          // Extrair componentes RGB manualmente
          Data[idx] := (PixelColor and $0000FF);         // R (Red)
          Data[idx + 1] := (PixelColor and $00FF00) shr 8;  // G (Green)
          Data[idx + 2] := (PixelColor and $FF0000) shr 16; // B (Blue)
          Inc(idx, 3);
        end;
      end;

      // Enviar textura para OpenGL
      glTexImage2D(GL_TEXTURE_2D, 0, GL_RGB, Bitmap.Width, Bitmap.Height,
                   0, GL_RGB, GL_UNSIGNED_BYTE, @Data[0]);

      Result := True;

    except
      on E: Exception do
      begin
        // Falha silenciosa
      end;
    end;
  finally
    Bitmap.Free;
    Picture.Free;
  end;
end;

procedure Tpascubeform.LoadAllTextures;
var
  BaseDir: string;
  Extensions: array[0..3] of string = ('.png', '.jpg', '.jpeg', '.bmp');
  i, j: Integer;
  FileName: string;
  TextureName: string;
  TexturesLoaded: Integer;
begin
  BaseDir := ExtractFilePath(Application.ExeName);
  TexturesLoaded := 0;

  // Tentar carregar texture1, texture2, texture3 com diferentes extensões
  for i := 1 to 3 do
  begin
    if TexturesLoaded >= 3 then Break; // Já carregamos 3 texturas

    for j := 0 to High(Extensions) do
    begin
      TextureName := Format('texture%d', [i]);
      FileName := BaseDir + TextureName + Extensions[j];

      if FileExists(FileName) then
      begin
        if LoadTexture(FileName, FTextureIDs[TexturesLoaded]) then
        begin
          Inc(TexturesLoaded);
      //    ShowMessage(Format('Textura %d carregada: %s', [TexturesLoaded, ExtractFileName(FileName)]));
          Break; // Próxima textura
        end;
      end;
    end;
  end;

  FTextureCount := TexturesLoaded;

  // Se não carregou nenhuma textura, permitir seleção manual
  if TexturesLoaded = 0 then
  begin
    if MessageDlg('Nenhuma textura encontrada',
                  'Nenhuma textura foi encontrada (texture1, texture2, texture3).' + sLineBreak +
                  'Deseja selecionar as texturas manualmente?',
                  mtConfirmation, [mbYes, mbNo], 0) = mrYes then
    begin
      with TOpenDialog.Create(nil) do
      try
        Title := 'Selecione até 3 texturas para o cubo';
        Filter := 'Imagens|*.bmp;*.jpg;*.jpeg;*.png|Todos|*.*';
        Options := Options + [ofAllowMultiSelect];

        if Execute then
        begin
          for i := 0 to Files.Count - 1 do
          begin
            if i >= 3 then Break; // Máximo 3 texturas

            if LoadTexture(Files[i], FTextureIDs[i]) then
            begin
              Inc(FTextureCount);
            //  ShowMessage(Format('Textura %d carregada: %s', [FTextureCount, ExtractFileName(Files[i])]));
            end;
          end;
        end;
      finally
        Free;
      end;
    end;
  end;

  if FTextureCount = 0 then
    ShowMessage('Nenhuma textura carregada. O cubo será exibido com cores sólidas.')
  else
   // ShowMessage(Format('%d textura(s) carregada(s) com sucesso!', [FTextureCount]));
end;

procedure Tpascubeform.InitializeGL;
begin
  if FInitialized then Exit;

  pascubeOpenGLControl.MakeCurrent;

  // Fundo azul escuro
  glClearColor(0.1, 0.1, 0.2, 1.0);

  // Habilitar teste de profundidade
  glEnable(GL_DEPTH_TEST);
  glDepthFunc(GL_LEQUAL);

  // Configurar iluminação
  glEnable(GL_LIGHTING);
  glEnable(GL_LIGHT0);

  // Configurar material
  glEnable(GL_COLOR_MATERIAL);
  glColorMaterial(GL_FRONT_AND_BACK, GL_AMBIENT_AND_DIFFUSE);

  glShadeModel(GL_SMOOTH);
  glHint(GL_PERSPECTIVE_CORRECTION_HINT, GL_NICEST);

  // Habilitar normalização automática
  glEnable(GL_NORMALIZE);

  // Habilitar texturas
  glEnable(GL_TEXTURE_2D);

  SetupLighting;

  // Carregar todas as texturas
  LoadAllTextures;

  FInitialized := True;
end;

procedure Tpascubeform.SetupLighting;
var
  LightAmbient:   array[0..3] of GLfloat = (0.5, 0.5, 0.5, 1.0);
  LightDiffuse:   array[0..3] of GLfloat = (0.8, 0.8, 0.8, 1.0);
  LightPosition:  array[0..3] of GLfloat = (5.0, 5.0, 5.0, 1.0);
  LightSpecular:  array[0..3] of GLfloat = (0.5, 0.5, 0.5, 1.0);
  MaterialSpec:   array[0..3] of GLfloat = (0.3, 0.3, 0.3, 1.0);
  MaterialShine:  GLfloat = 50.0;
begin
  glLightfv(GL_LIGHT0, GL_AMBIENT,  @LightAmbient);
  glLightfv(GL_LIGHT0, GL_DIFFUSE,  @LightDiffuse);
  glLightfv(GL_LIGHT0, GL_SPECULAR, @LightSpecular);
  glLightfv(GL_LIGHT0, GL_POSITION, @LightPosition);

  glMaterialfv(GL_FRONT, GL_SPECULAR, @MaterialSpec);
  glMaterialf(GL_FRONT, GL_SHININESS, MaterialShine);
end;

procedure Tpascubeform.ApplyViewportAndProjection;
var
  w, h: GLint;
  Aspect: GLdouble;
begin
  // Garante contexto
  pascubeOpenGLControl.MakeCurrent;

  // Pegar as dimensões reais do controle OpenGL
  w := pascubeOpenGLControl.Width;
  h := pascubeOpenGLControl.Height;

  // Proteção contra divisão por zero
  if h <= 0 then h := 1;

  // Configurar viewport para toda a área
  glViewport(0, 0, w, h);

  // Configurar matriz de projeção
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity;

  // Calcular aspect ratio
  Aspect := GLdouble(w) / GLdouble(h);

  // Campo de visão de 60 graus
  gluPerspective(60.0, Aspect, 0.1, 100.0);

  // Voltar para matriz modelview
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity;
end;

procedure Tpascubeform.DrawCube;
const
  SIZE = 1.0;
var
  TextureIndex: Integer;
begin
  // Face frontal - Textura 1
  TextureIndex := 0;
  if (FTextureCount > TextureIndex) and (FTextureIDs[TextureIndex] > 0) then
  begin
    glEnable(GL_TEXTURE_2D);
    glBindTexture(GL_TEXTURE_2D, FTextureIDs[TextureIndex]);
    glColor3f(1.0, 1.0, 1.0);
  end
  else
  begin
    glDisable(GL_TEXTURE_2D);
    glColor3f(1.0, 0.0, 0.0); // Vermelho
  end;

  glBegin(GL_QUADS);
    glNormal3f(0.0, 0.0, 1.0);
    glTexCoord2f(0.0, 0.0); glVertex3f(-SIZE, -SIZE,  SIZE);
    glTexCoord2f(1.0, 0.0); glVertex3f( SIZE, -SIZE,  SIZE);
    glTexCoord2f(1.0, 1.0); glVertex3f( SIZE,  SIZE,  SIZE);
    glTexCoord2f(0.0, 1.0); glVertex3f(-SIZE,  SIZE,  SIZE);
  glEnd;

  // Face traseira - Textura 1 (mesma da frontal)
  glBegin(GL_QUADS);
    glNormal3f(0.0, 0.0, -1.0);
    glTexCoord2f(1.0, 0.0); glVertex3f(-SIZE, -SIZE, -SIZE);
    glTexCoord2f(1.0, 1.0); glVertex3f(-SIZE,  SIZE, -SIZE);
    glTexCoord2f(0.0, 1.0); glVertex3f( SIZE,  SIZE, -SIZE);
    glTexCoord2f(0.0, 0.0); glVertex3f( SIZE, -SIZE, -SIZE);
  glEnd;

  // Face superior - Textura 2
  TextureIndex := 1;
  if (FTextureCount > TextureIndex) and (FTextureIDs[TextureIndex] > 0) then
  begin
    glEnable(GL_TEXTURE_2D);
    glBindTexture(GL_TEXTURE_2D, FTextureIDs[TextureIndex]);
    glColor3f(1.0, 1.0, 1.0);
  end
  else
  begin
    glDisable(GL_TEXTURE_2D);
    glColor3f(0.0, 0.0, 1.0); // Azul
  end;

  glBegin(GL_QUADS);
    glNormal3f(0.0, 1.0, 0.0);
    glTexCoord2f(0.0, 1.0); glVertex3f(-SIZE,  SIZE,  SIZE);
    glTexCoord2f(1.0, 1.0); glVertex3f( SIZE,  SIZE,  SIZE);
    glTexCoord2f(1.0, 0.0); glVertex3f( SIZE,  SIZE, -SIZE);
    glTexCoord2f(0.0, 0.0); glVertex3f(-SIZE,  SIZE, -SIZE);
  glEnd;

  // Face inferior - Textura 2 (mesma da superior)
  glBegin(GL_QUADS);
    glNormal3f(0.0, -1.0, 0.0);
    glTexCoord2f(1.0, 1.0); glVertex3f(-SIZE, -SIZE,  SIZE);
    glTexCoord2f(0.0, 1.0); glVertex3f(-SIZE, -SIZE, -SIZE);
    glTexCoord2f(0.0, 0.0); glVertex3f( SIZE, -SIZE, -SIZE);
    glTexCoord2f(1.0, 0.0); glVertex3f( SIZE, -SIZE,  SIZE);
  glEnd;

  // Face direita - Textura 3
  TextureIndex := 2;
  if (FTextureCount > TextureIndex) and (FTextureIDs[TextureIndex] > 0) then
  begin
    glEnable(GL_TEXTURE_2D);
    glBindTexture(GL_TEXTURE_2D, FTextureIDs[TextureIndex]);
    glColor3f(1.0, 1.0, 1.0);
  end
  else
  begin
    glDisable(GL_TEXTURE_2D);
    glColor3f(0.0, 1.0, 0.0); // Verde
  end;

  glBegin(GL_QUADS);
    glNormal3f(1.0, 0.0, 0.0);
    glTexCoord2f(1.0, 0.0); glVertex3f( SIZE, -SIZE,  SIZE);
    glTexCoord2f(1.0, 1.0); glVertex3f( SIZE, -SIZE, -SIZE);
    glTexCoord2f(0.0, 1.0); glVertex3f( SIZE,  SIZE, -SIZE);
    glTexCoord2f(0.0, 0.0); glVertex3f( SIZE,  SIZE,  SIZE);
  glEnd;

  // Face esquerda - Textura 3 (mesma da direita)
  glBegin(GL_QUADS);
    glNormal3f(-1.0, 0.0, 0.0);
    glTexCoord2f(0.0, 0.0); glVertex3f(-SIZE, -SIZE,  SIZE);
    glTexCoord2f(1.0, 0.0); glVertex3f(-SIZE,  SIZE,  SIZE);
    glTexCoord2f(1.0, 1.0); glVertex3f(-SIZE,  SIZE, -SIZE);
    glTexCoord2f(0.0, 1.0); glVertex3f(-SIZE, -SIZE, -SIZE);
  glEnd;
end;

procedure Tpascubeform.pascubeOpenGLControlPaint(Sender: TObject);
begin
  // Garantir que está inicializado
  if not FInitialized then
  begin
    pascubeOpenGLControl.MakeCurrent;
    InitializeGL;
  end;

  // Garantir contexto atual
  pascubeOpenGLControl.MakeCurrent;

  // Aplicar viewport e projeção
  ApplyViewportAndProjection;

  // Limpar buffers
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);

  // Configurar câmera
  glLoadIdentity;
  gluLookAt(0.0, 0.0, 4.0,   // Posição da câmera
            0.0, 0.0, 0.0,   // Ponto que a câmera olha
            0.0, 1.0, 0.0);  // Vetor "up"

  // Aplicar rotações
  glRotatef(FRotationX, 1.0, 0.0, 0.0);
  glRotatef(FRotationY, 0.0, 1.0, 0.0);
  glRotatef(FRotationZ, 0.0, 0.0, 1.0);

  // Desenhar o cubo
  DrawCube;

  // Trocar buffers
  pascubeOpenGLControl.SwapBuffers;
end;

procedure Tpascubeform.pascubeOpenGLControlResize(Sender: TObject);
begin
  // Quando a janela for redimensionada
  if FInitialized then
  begin
    pascubeOpenGLControl.MakeCurrent;
    ApplyViewportAndProjection;
    pascubeOpenGLControl.Invalidate;
  end;
end;

procedure Tpascubeform.Timer1Timer(Sender: TObject);
begin
  // Animação das rotações
  FRotationX := FRotationX + 0.5;
  FRotationY := FRotationY + 1.0;
  FRotationZ := FRotationZ + 0.3;

  // Manter ângulos entre 0 e 360
  if FRotationX > 360 then FRotationX := FRotationX - 360;
  if FRotationY > 360 then FRotationY := FRotationY - 360;
  if FRotationZ > 360 then FRotationZ := FRotationZ - 360;

  // Forçar redesenho
  pascubeOpenGLControl.Invalidate;
end;

end.
