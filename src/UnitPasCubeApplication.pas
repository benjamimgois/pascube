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
     UnitPasCubeScreen,
     UnitTextOverlay;

type TPasCubeApplication=class(TpvApplication)
      private
       fTextOverlay:TTextOverlay;
      public
       constructor Create; override;
       destructor Destroy; override;
       procedure Setup; override;
       procedure Start; override;
       procedure Stop; override;
       procedure Load; override;
       procedure Unload; override;
       procedure AfterCreateSwapChain; override;
       procedure BeforeDestroySwapChain; override;
       procedure Update(const aDeltaTime:TpvDouble); override;
       procedure Draw(const aSwapChainImageIndex:TpvInt32;var aWaitSemaphore:TpvVulkanSemaphore;const aWaitFence:TpvVulkanFence=nil); override;
      published
       property TextOverlay:TTextOverlay read fTextOverlay;
      end;

var Application:TPasCubeApplication=nil;

implementation

constructor TPasCubeApplication.Create;
begin
 inherited Create;
 Application:=self;
 fTextOverlay:=nil;
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
 VulkanAPIVersion:=VK_API_VERSION_1_0;
 Blocking:=true;
 PresentMode:=TpvApplicationPresentMode.VSync;
 Width:=500;
 Height:=500;
 fTextOverlay:=TTextOverlay.Create;
end;

procedure TPasCubeApplication.Start;
begin
 inherited Start;
end;

procedure TPasCubeApplication.Stop;
begin
 inherited Stop;
end;

procedure TPasCubeApplication.Load;
begin
 inherited Load;
 if assigned(fTextOverlay) then begin
  fTextOverlay.Load;
 end;
end;

procedure TPasCubeApplication.Unload;
begin
 if assigned(fTextOverlay) then begin
  fTextOverlay.Unload;
 end;
 inherited Unload;
end;

procedure TPasCubeApplication.AfterCreateSwapChain;
begin
 inherited AfterCreateSwapChain;
 if assigned(fTextOverlay) then begin
  fTextOverlay.AfterCreateSwapChain;
 end;
end;

procedure TPasCubeApplication.BeforeDestroySwapChain;
begin
 if assigned(fTextOverlay) then begin
  fTextOverlay.BeforeDestroySwapChain;
 end;
 inherited BeforeDestroySwapChain;
end;

procedure TPasCubeApplication.Update(const aDeltaTime:TpvDouble);
begin
 if assigned(fTextOverlay) then begin
  fTextOverlay.PreUpdate(aDeltaTime);
 end;
 inherited Update(aDeltaTime);
 if assigned(fTextOverlay) then begin
  fTextOverlay.PostUpdate(aDeltaTime);
 end;
end;

procedure TPasCubeApplication.Draw(const aSwapChainImageIndex:TpvInt32;var aWaitSemaphore:TpvVulkanSemaphore;const aWaitFence:TpvVulkanFence=nil);
begin
 inherited Draw(aSwapChainImageIndex,aWaitSemaphore,aWaitFence);
 if assigned(fTextOverlay) then begin
  fTextOverlay.Draw(aSwapChainImageIndex,aWaitSemaphore,aWaitFence);
 end;
end;

end.
