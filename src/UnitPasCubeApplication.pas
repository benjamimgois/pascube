unit UnitPasCubeApplication;
{$ifdef fpc}
 {$mode delphi}
 {$ifdef cpu386}
  {$asmmode intel}
 {$endif}
 {$ifdef cpuamd64}
  {$asmmode intel}
 {$endif}
{$else}
 {$ifdef conditionalexpressions}
  {$if CompilerVersion>=24.0}
   {$legacyifend on}
  {$ifend}
 {$endif}
{$endif}
{$if defined(Win32) or defined(Win64)}
 {$define Windows}
{$ifend}

interface

uses SysUtils,
     Classes,
     Vulkan,
     PasVulkan.Types,
     PasVulkan.Math,
     PasVulkan.Framework,
     PasVulkan.Application,
     UnitPasCubeScreen;

type TPasCubeApplication=class(TpvApplication)
      public
       constructor Create; override;
       destructor Destroy; override;
       procedure Setup; override;
       procedure Start; override;
       procedure Stop; override;
      end;

var Application:TPasCubeApplication=nil;

implementation

constructor TPasCubeApplication.Create;
begin
 inherited Create;
 Application:=self;
end;

destructor TPasCubeApplication.Destroy;
begin
 Application:=nil;
 inherited Destroy;
end;

procedure TPasCubeApplication.Setup;
begin
 if Debugging then begin
  VulkanDebugging:=true;
  VulkanValidation:=true;
 end;
 Title:='PasCube 1.6.0 beta';
 PathName:='PasCube';
 StartScreen:=TPasCubeScreen;
 VisibleMouseCursor:=true;
 CatchMouse:=false;
 HideSystemBars:=true;
 AndroidSeparateMouseAndTouch:=true;
 UseAudio:=false;
 WaitOnPreviousFrames:=false;
 VulkanAPIVersion:=VK_API_VERSION_1_0;
 Blocking:=true;
 PresentMode:=TpvApplicationPresentMode.VSync;
end;

procedure TPasCubeApplication.Start;
begin
 inherited Start;
end;

procedure TPasCubeApplication.Stop;
begin
 inherited Stop;
end;

end.
