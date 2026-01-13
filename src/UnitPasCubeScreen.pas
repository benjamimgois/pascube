unit UnitPasCubeScreen;
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
     PasVulkan.Application;

const CountTextures=4;


type PScreenExampleCubeUniformBuffer=^TScreenExampleCubeUniformBuffer;
     TScreenExampleCubeUniformBuffer=record
      ModelViewProjectionMatrix:TpvMatrix4x4;
      ModelViewMatrix:TpvMatrix4x4;
      ModelViewNormalMatrix:TpvMatrix4x4;
     end;

     PScreenExampleCubeState=^TScreenExampleCubeState;
     TScreenExampleCubeState=record
      Time:TpvDouble;
      AnglePhases:array[0..1] of TpvFloat;
     end;

     PScreenExampleCubeStates=^TScreenExampleCubeStates;
     TScreenExampleCubeStates=array[0..MaxInFlightFrames-1] of TScreenExampleCubeState;

     TPasCubeScreen=class(TpvApplicationScreen)
      private
       fVulkanGraphicsCommandPool:TpvVulkanCommandPool;
       fVulkanGraphicsCommandBuffer:TpvVulkanCommandBuffer;
       fVulkanGraphicsCommandBufferFence:TpvVulkanFence;
       fVulkanTransferCommandPool:TpvVulkanCommandPool;
       fVulkanTransferCommandBuffer:TpvVulkanCommandBuffer;
       fVulkanTransferCommandBufferFence:TpvVulkanFence;
       fCubeVertexShaderModule:TpvVulkanShaderModule;
       fCubeFragmentShaderModule:TpvVulkanShaderModule;
       fVulkanPipelineShaderStageCubeVertex:TpvVulkanPipelineShaderStage;
       fVulkanPipelineShaderStageCubeFragment:TpvVulkanPipelineShaderStage;
       fVulkanGraphicsPipeline:TpvVulkanGraphicsPipeline;
       fMouseLeftButtonDown:boolean;
       fLastMousePosition:TpvVector2;
       fAutoRotation:boolean;
       fVulkanRenderPass:TpvVulkanRenderPass;
       fVulkanVertexBuffer:TpvVulkanBuffer;
       fVulkanIndexBuffer:TpvVulkanBuffer;
       fVulkanUniformBuffers:array[0..MaxInFlightFrames-1] of TpvVulkanBuffer;
       fVulkanDescriptorPool:TpvVulkanDescriptorPool;
       fVulkanDescriptorSetLayout:TpvVulkanDescriptorSetLayout;
       fVulkanDescriptorSets:array[0..MaxInFlightFrames-1,0..CountTextures-1] of TpvVulkanDescriptorSet;
       fFaceDescriptorSets:array[0..MaxInFlightFrames-1,0..5] of TpvVulkanDescriptorSet; // Per-face overlay
       fVulkanPipelineLayout:TpvVulkanPipelineLayout;
       fVulkanCommandPool:TpvVulkanCommandPool;
       fVulkanRenderCommandBuffers:array[0..MaxInFlightFrames-1] of array of TpvVulkanCommandBuffer;
       fVulkanRenderSemaphores:array[0..MaxInFlightFrames-1] of TpvVulkanSemaphore;
       fUniformBuffer:TScreenExampleCubeUniformBuffer;
       fBoxAlbedoTextures:array[0..CountTextures-1] of TpvVulkanTexture;
       fOverlayTextures:array of TpvVulkanTexture; // Dynamic array
       fOverlayTextureNames:TStringList;
       fFaceOverlayIndices:array[0..5] of Integer; // Random texture index for each face
       fReady:boolean;
       fState:TScreenExampleCubeState;
       fStates:TScreenExampleCubeStates;
      public

       constructor Create; override;

       destructor Destroy; override;

       procedure Show; override;

       procedure Hide; override;

       procedure Resume; override;

       procedure Pause; override;

       procedure Resize(const aWidth,aHeight:TpvInt32); override;

       procedure AfterCreateSwapChain; override;

       procedure BeforeDestroySwapChain; override;

       function KeyEvent(const aKeyEvent:TpvApplicationInputKeyEvent):boolean; override;

       function PointerEvent(const aPointerEvent:TpvApplicationInputPointerEvent):boolean; override;

       function Scrolled(const aRelativeAmount:TpvVector2):boolean; override;

       function CanBeParallelProcessed:boolean; override;

       procedure Update(const aDeltaTime:TpvDouble); override;

       procedure Draw(const aSwapChainImageIndex:TpvInt32;var aWaitSemaphore:TpvVulkanSemaphore;const aWaitFence:TpvVulkanFence=nil); override;

     end;

implementation

uses UnitPasCubeApplication, UnitTextOverlay;

type PVertex=^TVertex;
     TVertex=record
      Position:TpvVector3;
      Tangent:TpvVector3;
      Bitangent:TpvVector3;
      Normal:TpvVector3;
      TexCoord:TpvVector2;
     end;

const CubeVertices:array[0..23] of TVertex=
       (// Left
        (Position:(x:-1;y:-1;z:-1;);Tangent:(x:0;y:0;z:1;);Bitangent:(x:0;y:1;z:0;);Normal:(x:-1;y:0;z:0;);TexCoord:(u:0;v:0)),
        (Position:(x:-1;y: 1;z:-1;);Tangent:(x:0;y:0;z:1;);Bitangent:(x:0;y:1;z:0;);Normal:(x:-1;y:0;z:0;);TexCoord:(u:0;v:1)),
        (Position:(x:-1;y: 1;z: 1;);Tangent:(x:0;y:0;z:1;);Bitangent:(x:0;y:1;z:0;);Normal:(x:-1;y:0;z:0;);TexCoord:(u:1;v:1)),
        (Position:(x:-1;y:-1;z: 1;);Tangent:(x:0;y:0;z:1;);Bitangent:(x:0;y:1;z:0;);Normal:(x:-1;y:0;z:0;);TexCoord:(u:1;v:0)),

        // Right
        (Position:(x: 1;y:-1;z: 1;);Tangent:(x:0;y:0;z:-1;);Bitangent:(x:0;y:1;z:0;);Normal:(x:1;y:0;z:0;);TexCoord:(u:0;v:0)),
        (Position:(x: 1;y: 1;z: 1;);Tangent:(x:0;y:0;z:-1;);Bitangent:(x:0;y:1;z:0;);Normal:(x:1;y:0;z:0;);TexCoord:(u:0;v:1)),
        (Position:(x: 1;y: 1;z:-1;);Tangent:(x:0;y:0;z:-1;);Bitangent:(x:0;y:1;z:0;);Normal:(x:1;y:0;z:0;);TexCoord:(u:1;v:1)),
        (Position:(x: 1;y:-1;z:-1;);Tangent:(x:0;y:0;z:-1;);Bitangent:(x:0;y:1;z:0;);Normal:(x:1;y:0;z:0;);TexCoord:(u:1;v:0)),

        // Bottom
        (Position:(x:-1;y:-1;z:-1;);Tangent:(x:1;y:0;z:0;);Bitangent:(x:0;y:0;z:1;);Normal:(x:0;y:-1;z:0;);TexCoord:(u:0;v:0)),
        (Position:(x:-1;y:-1;z: 1;);Tangent:(x:1;y:0;z:0;);Bitangent:(x:0;y:0;z:1;);Normal:(x:0;y:-1;z:0;);TexCoord:(u:0;v:1)),
        (Position:(x: 1;y:-1;z: 1;);Tangent:(x:1;y:0;z:0;);Bitangent:(x:0;y:0;z:1;);Normal:(x:0;y:-1;z:0;);TexCoord:(u:1;v:1)),
        (Position:(x: 1;y:-1;z:-1;);Tangent:(x:1;y:0;z:0;);Bitangent:(x:0;y:0;z:1;);Normal:(x:0;y:-1;z:0;);TexCoord:(u:1;v:0)),

        // Top
        (Position:(x:-1;y: 1;z:-1;);Tangent:(x:1;y:0;z:0;);Bitangent:(x:0;y:0;z:-1;);Normal:(x:0;y:1;z:0;);TexCoord:(u:0;v:0)),
        (Position:(x: 1;y: 1;z:-1;);Tangent:(x:1;y:0;z:0;);Bitangent:(x:0;y:0;z:-1;);Normal:(x:0;y:1;z:0;);TexCoord:(u:0;v:1)),
        (Position:(x: 1;y: 1;z: 1;);Tangent:(x:1;y:0;z:0;);Bitangent:(x:0;y:0;z:-1;);Normal:(x:0;y:1;z:0;);TexCoord:(u:1;v:1)),
        (Position:(x:-1;y: 1;z: 1;);Tangent:(x:1;y:0;z:0;);Bitangent:(x:0;y:0;z:-1;);Normal:(x:0;y:1;z:0;);TexCoord:(u:1;v:0)),

        // Back
        (Position:(x: 1;y:-1;z:-1;);Tangent:(x:-1;y:0;z:0;);Bitangent:(x:0;y:1;z:0;);Normal:(x:0;y:0;z:-1;);TexCoord:(u:0;v:0)),
        (Position:(x: 1;y: 1;z:-1;);Tangent:(x:-1;y:0;z:0;);Bitangent:(x:0;y:1;z:0;);Normal:(x:0;y:0;z:-1;);TexCoord:(u:0;v:1)),
        (Position:(x:-1;y: 1;z:-1;);Tangent:(x:-1;y:0;z:0;);Bitangent:(x:0;y:1;z:0;);Normal:(x:0;y:0;z:-1;);TexCoord:(u:1;v:1)),
        (Position:(x:-1;y:-1;z:-1;);Tangent:(x:-1;y:0;z:0;);Bitangent:(x:0;y:1;z:0;);Normal:(x:0;y:0;z:-1;);TexCoord:(u:1;v:0)),

        // Front
        (Position:(x:-1;y:-1;z:1;);Tangent:(x:1;y:0;z:0;);Bitangent:(x:0;y:1;z:0;);Normal:(x:0;y:0;z:1;);TexCoord:(u:0;v:0)),
        (Position:(x:-1;y: 1;z:1;);Tangent:(x:1;y:0;z:0;);Bitangent:(x:0;y:1;z:0;);Normal:(x:0;y:0;z:1;);TexCoord:(u:0;v:1)),
        (Position:(x: 1;y: 1;z:1;);Tangent:(x:1;y:0;z:0;);Bitangent:(x:0;y:1;z:0;);Normal:(x:0;y:0;z:1;);TexCoord:(u:1;v:1)),
        (Position:(x: 1;y:-1;z:1;);Tangent:(x:1;y:0;z:0;);Bitangent:(x:0;y:1;z:0;);Normal:(x:0;y:0;z:1;);TexCoord:(u:1;v:0))

       );

      CubeIndices:array[0..35] of TpvInt32=
       ( // Left
         0, 1, 2,
         0, 2, 3,

         // Right
         4, 5, 6,
         4, 6, 7,

         // Bottom
         8, 9, 10,
         8, 10, 11,

         // Top
         12, 13, 14,
         12, 14, 15,

         // Back
         16, 17, 18,
         16, 18, 19,

         // Front
         20, 21, 22,
         20, 22, 23);

      Offsets:array[0..0] of TVkDeviceSize=(0);

constructor TPasCubeScreen.Create;
var SearchRec:TSearchRec;
    BasePath,TexturesPath:String;
begin
 inherited Create;
 fOverlayTextureNames:=TStringList.Create;

 // Find assets directory
 BasePath:=IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0)))+'assets';
 if not DirectoryExists(BasePath) then begin
  BasePath:=ExpandFileName(IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0)))+'../share/pascube/assets');
 end;

 TexturesPath:=IncludeTrailingPathDelimiter(BasePath)+'textures';
 
 if FindFirst(IncludeTrailingPathDelimiter(TexturesPath)+'*.png',faAnyFile,SearchRec)=0 then begin
  repeat
   if (SearchRec.Name<>'.') and (SearchRec.Name<>'..') and
      (SearchRec.Name<>'box_albedo.png') and
      (SearchRec.Name<>'box_normalmap_heightmap.png') and
      (SearchRec.Name<>'metal.png') and
      (SearchRec.Name<>'reflection.png') and
      (SearchRec.Name<>'treeleafs.jpg') then begin
     fOverlayTextureNames.Add('textures/'+SearchRec.Name);
   end;
  until FindNext(SearchRec)<>0;
  FindClose(SearchRec);
 end;

 if fOverlayTextureNames.Count=0 then begin
  fOverlayTextureNames.Add('textures/icon_overlay.png'); // Fallback
 end;
 
 fMouseLeftButtonDown:=false;
 fLastMousePosition:=TpvVector2.Create(0.0,0.0);
 fAutoRotation:=true;
 FillChar(fState,SizeOf(TScreenExampleCubeState),#0);
 FillChar(fStates,SizeOf(TScreenExampleCubeStates),#0);
 fReady:=false;
end;

destructor TPasCubeScreen.Destroy;
begin
 FreeAndNil(fOverlayTextureNames);
 inherited Destroy;
end;

procedure TPasCubeScreen.Show;
var Stream:TStream;
    Index,SwapChainImageIndex:TpvInt32;
    ShuffledIndices:array[0..5] of Integer;
    AvailableIndices:array of Integer;
    i,RandomIndex,Temp:Integer;
begin
 inherited Show;

 fVulkanGraphicsCommandPool:=TpvVulkanCommandPool.Create(pvApplication.VulkanDevice,
                                                         pvApplication.VulkanDevice.GraphicsQueueFamilyIndex,
                                                         TVkCommandPoolCreateFlags(VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT));

 fVulkanGraphicsCommandBuffer:=TpvVulkanCommandBuffer.Create(fVulkanGraphicsCommandPool,VK_COMMAND_BUFFER_LEVEL_PRIMARY);

 fVulkanGraphicsCommandBufferFence:=TpvVulkanFence.Create(pvApplication.VulkanDevice);

 fVulkanTransferCommandPool:=TpvVulkanCommandPool.Create(pvApplication.VulkanDevice,
                                                         pvApplication.VulkanDevice.TransferQueueFamilyIndex,
                                                         TVkCommandPoolCreateFlags(VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT));

 fVulkanTransferCommandBuffer:=TpvVulkanCommandBuffer.Create(fVulkanTransferCommandPool,VK_COMMAND_BUFFER_LEVEL_PRIMARY);

 fVulkanTransferCommandBufferFence:=TpvVulkanFence.Create(pvApplication.VulkanDevice);

 fVulkanCommandPool:=TpvVulkanCommandPool.Create(pvApplication.VulkanDevice,
                                                 pvApplication.VulkanDevice.GraphicsQueueFamilyIndex,
                                                 TVkCommandPoolCreateFlags(VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT));
 for Index:=0 to MaxInFlightFrames-1 do begin
  SetLength(fVulkanRenderCommandBuffers[Index],pvApplication.CountSwapChainImages);
  for SwapChainImageIndex:=0 to pvApplication.CountSwapChainImages-1 do begin
   fVulkanRenderCommandBuffers[Index,SwapChainImageIndex]:=TpvVulkanCommandBuffer.Create(fVulkanCommandPool,VK_COMMAND_BUFFER_LEVEL_PRIMARY);
  end;
  fVulkanRenderSemaphores[Index]:=TpvVulkanSemaphore.Create(pvApplication.VulkanDevice);
 end;

 Stream:=pvApplication.Assets.GetAssetStream('shaders/cube/cube_vert.spv');
 try
  fCubeVertexShaderModule:=TpvVulkanShaderModule.Create(pvApplication.VulkanDevice,Stream);
 finally
  Stream.Free;
 end;

 Stream:=pvApplication.Assets.GetAssetStream('shaders/cube/cube_frag.spv');
 try
  fCubeFragmentShaderModule:=TpvVulkanShaderModule.Create(pvApplication.VulkanDevice,Stream);
 finally
  Stream.Free;
 end;

  for Index:=0 to CountTextures-1 do begin
   // Use metal.png for everything. Material properties are defined by PushConstants (Tint/Opacity) in Draw.
   Stream:=pvApplication.Assets.GetAssetStream('textures/metal.png');
   try
    fBoxAlbedoTextures[Index]:=TpvVulkanTexture.CreateFromImage(pvApplication.VulkanDevice,
                                                                pvApplication.VulkanDevice.GraphicsQueue,
                                                                fVulkanGraphicsCommandBuffer,
                                                                fVulkanGraphicsCommandBufferFence,
                                                                pvApplication.VulkanDevice.TransferQueue,
                                                                fVulkanTransferCommandBuffer,
                                                                fVulkanTransferCommandBufferFence,
                                                                Stream,
                                                                true,
                                                                true);
   finally
    Stream.Free;
   end;
   fBoxAlbedoTextures[Index].WrapModeU:=TpvVulkanTextureWrapMode.ClampToEdge;
   fBoxAlbedoTextures[Index].WrapModeV:=TpvVulkanTextureWrapMode.ClampToEdge;
   fBoxAlbedoTextures[Index].WrapModeW:=TpvVulkanTextureWrapMode.ClampToEdge;
   fBoxAlbedoTextures[Index].BorderColor:=VK_BORDER_COLOR_FLOAT_OPAQUE_BLACK;
   fBoxAlbedoTextures[Index].UpdateSampler;
  end;

 // Load all overlay textures
 SetLength(fOverlayTextures,fOverlayTextureNames.Count);
 for Index:=0 to fOverlayTextureNames.Count-1 do begin
  Stream:=pvApplication.Assets.GetAssetStream(fOverlayTextureNames[Index]);
  try
   fOverlayTextures[Index]:=TpvVulkanTexture.CreateFromImage(pvApplication.VulkanDevice,
                                                             pvApplication.VulkanDevice.GraphicsQueue,
                                                             fVulkanGraphicsCommandBuffer,
                                                             fVulkanGraphicsCommandBufferFence,
                                                             pvApplication.VulkanDevice.TransferQueue,
                                                             fVulkanTransferCommandBuffer,
                                                             fVulkanTransferCommandBufferFence,
                                                             Stream,
                                                             true,
                                                             true);
  finally
   Stream.Free;
  end;
  fOverlayTextures[Index].WrapModeU:=TpvVulkanTextureWrapMode.ClampToEdge;
  fOverlayTextures[Index].WrapModeV:=TpvVulkanTextureWrapMode.ClampToEdge;
  fOverlayTextures[Index].WrapModeW:=TpvVulkanTextureWrapMode.ClampToEdge;
  fOverlayTextures[Index].BorderColor:=VK_BORDER_COLOR_FLOAT_TRANSPARENT_BLACK;
  fOverlayTextures[Index].UpdateSampler;
 end;

 // Assign unique overlay textures to each of the 6 cube faces (avoiding repetitions)
 Randomize;
 if fOverlayTextureNames.Count >= 6 then begin
  // We have enough textures, shuffle and assign unique ones
  // Create a pool of available texture indices
  SetLength(AvailableIndices, fOverlayTextureNames.Count);
  for i := 0 to fOverlayTextureNames.Count - 1 do begin
   AvailableIndices[i] := i;
  end;
  
  // Fisher-Yates shuffle and pick first 6
  for i := 0 to 5 do begin
   RandomIndex := i + Random(Length(AvailableIndices) - i);
   ShuffledIndices[i] := AvailableIndices[RandomIndex];
   // Swap
   Temp := AvailableIndices[i];
   AvailableIndices[i] := AvailableIndices[RandomIndex];
   AvailableIndices[RandomIndex] := Temp;
  end;
  
  // Assign the shuffled indices to faces
  for i := 0 to 5 do begin
   fFaceOverlayIndices[i] := ShuffledIndices[i];
  end;
 end else begin
  // Not enough textures, just assign what we have sequentially with some randomization
  for Index := 0 to 5 do begin
   if Index < fOverlayTextureNames.Count then
    fFaceOverlayIndices[Index] := Index
   else
    fFaceOverlayIndices[Index] := Random(fOverlayTextureNames.Count);
  end;
 end;

 fVulkanPipelineShaderStageCubeVertex:=TpvVulkanPipelineShaderStage.Create(VK_SHADER_STAGE_VERTEX_BIT,fCubeVertexShaderModule,'main');

 fVulkanPipelineShaderStageCubeFragment:=TpvVulkanPipelineShaderStage.Create(VK_SHADER_STAGE_FRAGMENT_BIT,fCubeFragmentShaderModule,'main');

 fVulkanGraphicsPipeline:=nil;

 fVulkanRenderPass:=nil;

 fVulkanVertexBuffer:=TpvVulkanBuffer.Create(pvApplication.VulkanDevice,
                                             SizeOf(CubeVertices),
                                             TVkBufferUsageFlags(VK_BUFFER_USAGE_TRANSFER_DST_BIT) or TVkBufferUsageFlags(VK_BUFFER_USAGE_VERTEX_BUFFER_BIT),
                                             TVkSharingMode(VK_SHARING_MODE_EXCLUSIVE),
                                             [],
                                             TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT)
                                            );
 fVulkanVertexBuffer.UploadData(pvApplication.VulkanDevice.TransferQueue,
                                fVulkanTransferCommandBuffer,
                                fVulkanTransferCommandBufferFence,
                                CubeVertices,
                                0,
                                SizeOf(CubeVertices),
                                TpvVulkanBufferUseTemporaryStagingBufferMode.Yes);

 fVulkanIndexBuffer:=TpvVulkanBuffer.Create(pvApplication.VulkanDevice,
                                            SizeOf(CubeIndices),
                                            TVkBufferUsageFlags(VK_BUFFER_USAGE_TRANSFER_DST_BIT) or TVkBufferUsageFlags(VK_BUFFER_USAGE_INDEX_BUFFER_BIT),
                                            TVkSharingMode(VK_SHARING_MODE_EXCLUSIVE),
                                            [],
                                            TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT)
                                           );
 fVulkanIndexBuffer.UploadData(pvApplication.VulkanDevice.TransferQueue,
                               fVulkanTransferCommandBuffer,
                               fVulkanTransferCommandBufferFence,
                               CubeIndices,
                               0,
                               SizeOf(CubeIndices),
                               TpvVulkanBufferUseTemporaryStagingBufferMode.Yes);

 for Index:=0 to MaxInFlightFrames-1 do begin
  fVulkanUniformBuffers[Index]:=TpvVulkanBuffer.Create(pvApplication.VulkanDevice,
                                                       SizeOf(TScreenExampleCubeUniformBuffer),
                                                       TVkBufferUsageFlags(VK_BUFFER_USAGE_TRANSFER_DST_BIT) or TVkBufferUsageFlags(VK_BUFFER_USAGE_UNIFORM_BUFFER_BIT),
                                                       TVkSharingMode(VK_SHARING_MODE_EXCLUSIVE),
                                                       [],
                                                       TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT) or TVkMemoryPropertyFlags(VK_MEMORY_PROPERTY_HOST_COHERENT_BIT),
                                                       0,
                                                       0,
                                                       0,
                                                       0,
                                                       0,
                                                       0,
                                                       0,
                                                       [TpvVulkanBufferFlag.PersistentMapped]
                                                      );
  fVulkanUniformBuffers[Index].UploadData(pvApplication.VulkanDevice.TransferQueue,
                                          fVulkanTransferCommandBuffer,
                                          fVulkanTransferCommandBufferFence,
                                          fUniformBuffer,
                                          0,
                                          SizeOf(TScreenExampleCubeUniformBuffer),
                                          TpvVulkanBufferUseTemporaryStagingBufferMode.Yes);
 end;

 fVulkanDescriptorPool:=TpvVulkanDescriptorPool.Create(pvApplication.VulkanDevice,
                                                       TVkDescriptorPoolCreateFlags(VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT),
                                                       MaxInFlightFrames*(CountTextures+6)); // +6 for face descriptor sets
 fVulkanDescriptorPool.AddDescriptorPoolSize(VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER,MaxInFlightFrames*(CountTextures+6));
 fVulkanDescriptorPool.AddDescriptorPoolSize(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,MaxInFlightFrames*(CountTextures+6)*2);
 fVulkanDescriptorPool.Initialize;

 fVulkanDescriptorSetLayout:=TpvVulkanDescriptorSetLayout.Create(pvApplication.VulkanDevice);
 fVulkanDescriptorSetLayout.AddBinding(0,
                                       VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER,
                                       1,
                                       TVkShaderStageFlags(VK_SHADER_STAGE_VERTEX_BIT) or TVkShaderStageFlags(VK_SHADER_STAGE_FRAGMENT_BIT),
                                       []);
 fVulkanDescriptorSetLayout.AddBinding(1,
                                       VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,
                                       1,
                                       TVkShaderStageFlags(VK_SHADER_STAGE_FRAGMENT_BIT),
                                       []);
 fVulkanDescriptorSetLayout.AddBinding(2,
                                       VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,
                                       1,
                                       TVkShaderStageFlags(VK_SHADER_STAGE_FRAGMENT_BIT),
                                       []);
 fVulkanDescriptorSetLayout.Initialize;

 for Index:=0 to MaxInFlightFrames-1 do begin
  for SwapChainImageIndex:=0 to CountTextures-1 do begin
   fVulkanDescriptorSets[Index,SwapChainImageIndex]:=TpvVulkanDescriptorSet.Create(fVulkanDescriptorPool,
                                                                                   fVulkanDescriptorSetLayout);
   fVulkanDescriptorSets[Index,SwapChainImageIndex].WriteToDescriptorSet(0,
                                                                         0,
                                                                         1,
                                                                         TVkDescriptorType(VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER),
                                                                         [],
                                                                         [fVulkanUniformBuffers[Index].DescriptorBufferInfo],
                                                                         [],
                                                                         false
                                                                        );
   fVulkanDescriptorSets[Index,SwapChainImageIndex].WriteToDescriptorSet(1,
                                                                         0,
                                                                         1,
                                                                         TVkDescriptorType(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER),
                                                                         [fBoxAlbedoTextures[SwapChainImageIndex].DescriptorImageInfo],
                                                                         [],
                                                                         [],
                                                                         false
                                                                        );
   fVulkanDescriptorSets[Index,SwapChainImageIndex].WriteToDescriptorSet(2,
                                                                         0,
                                                                         1,
                                                                         TVkDescriptorType(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER),
                                                                         [fOverlayTextures[0].DescriptorImageInfo],
                                                                         [],
                                                                         [],
                                                                         false
                                                                        );
   fVulkanDescriptorSets[Index,SwapChainImageIndex].Flush;
  end;
 end;

 // Create per-face descriptor sets with random overlay textures
 for Index:=0 to MaxInFlightFrames-1 do begin
  for SwapChainImageIndex:=0 to 5 do begin // 6 faces
   fFaceDescriptorSets[Index,SwapChainImageIndex]:=TpvVulkanDescriptorSet.Create(fVulkanDescriptorPool,
                                                                                 fVulkanDescriptorSetLayout);
   fFaceDescriptorSets[Index,SwapChainImageIndex].WriteToDescriptorSet(0,
                                                                       0,
                                                                       1,
                                                                       TVkDescriptorType(VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER),
                                                                       [],
                                                                       [fVulkanUniformBuffers[Index].DescriptorBufferInfo],
                                                                       [],
                                                                       false
                                                                      );
   fFaceDescriptorSets[Index,SwapChainImageIndex].WriteToDescriptorSet(1,
                                                                       0,
                                                                       1,
                                                                       TVkDescriptorType(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER),
                                                                       [fBoxAlbedoTextures[0].DescriptorImageInfo],
                                                                       [],
                                                                       [],
                                                                       false
                                                                      );
   fFaceDescriptorSets[Index,SwapChainImageIndex].WriteToDescriptorSet(2,
                                                                       0,
                                                                       1,
                                                                       TVkDescriptorType(VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER),
                                                                       [fOverlayTextures[fFaceOverlayIndices[SwapChainImageIndex]].DescriptorImageInfo],
                                                                       [],
                                                                       [],
                                                                       false
                                                                      );
   fFaceDescriptorSets[Index,SwapChainImageIndex].Flush;
  end;
 end;

 fVulkanPipelineLayout:=TpvVulkanPipelineLayout.Create(pvApplication.VulkanDevice);
 fVulkanPipelineLayout.AddDescriptorSetLayout(fVulkanDescriptorSetLayout);
 // Push constant range should cover both vectors (2 * SizeOf(TpvVector4) = 32 bytes)
 fVulkanPipelineLayout.AddPushConstantRange(TVkShaderStageFlags(VK_SHADER_STAGE_FRAGMENT_BIT),0,SizeOf(TpvVector4)*2);
 fVulkanPipelineLayout.Initialize;

end;

procedure TPasCubeScreen.Hide;
var Index,SwapChainImageIndex:TpvInt32;
begin
 FreeAndNil(fVulkanPipelineLayout);
 for Index:=0 to MaxInFlightFrames-1 do begin
  for SwapChainImageIndex:=0 to CountTextures-1 do begin
   FreeAndNil(fVulkanDescriptorSets[Index,SwapChainImageIndex]);
  end;
  for SwapChainImageIndex:=0 to 5 do begin
   FreeAndNil(fFaceDescriptorSets[Index,SwapChainImageIndex]);
  end;
 end;
 FreeAndNil(fVulkanDescriptorSetLayout);
 FreeAndNil(fVulkanDescriptorPool);
 for Index:=0 to MaxInFlightFrames-1 do begin
  FreeAndNil(fVulkanUniformBuffers[Index]);
 end;
 FreeAndNil(fVulkanIndexBuffer);
 FreeAndNil(fVulkanVertexBuffer);
  FreeAndNil(fVulkanRenderPass);
  FreeAndNil(fVulkanGraphicsPipeline);
  FreeAndNil(fVulkanPipelineShaderStageCubeVertex);
 FreeAndNil(fVulkanPipelineShaderStageCubeFragment);
 FreeAndNil(fCubeFragmentShaderModule);
 FreeAndNil(fCubeVertexShaderModule);
  for Index:=0 to CountTextures-1 do begin
   FreeAndNil(fBoxAlbedoTextures[Index]);
  end;
 for Index:=0 to length(fOverlayTextures)-1 do begin
  FreeAndNil(fOverlayTextures[Index]);
 end;
 fOverlayTextures:=nil;
 for Index:=0 to MaxInFlightFrames-1 do begin
  for SwapChainImageIndex:=0 to length(fVulkanRenderCommandBuffers[Index])-1 do begin
   FreeAndNil(fVulkanRenderCommandBuffers[Index,SwapChainImageIndex]);
  end;
  fVulkanRenderCommandBuffers[Index]:=nil;
  FreeAndNil(fVulkanRenderSemaphores[Index]);
 end;
 FreeAndNil(fVulkanCommandPool);
 FreeAndNil(fVulkanTransferCommandBufferFence);
 FreeAndNil(fVulkanTransferCommandBuffer);
 FreeAndNil(fVulkanTransferCommandPool);
 FreeAndNil(fVulkanGraphicsCommandBufferFence);
 FreeAndNil(fVulkanGraphicsCommandBuffer);
 FreeAndNil(fVulkanGraphicsCommandPool);
 inherited Hide;
end;

procedure TPasCubeScreen.Resume;
begin
 inherited Resume;
end;

procedure TPasCubeScreen.Pause;
begin
 inherited Pause;
end;

procedure TPasCubeScreen.Resize(const aWidth,aHeight:TpvInt32);
begin
 inherited Resize(aWidth,aHeight);
end;

procedure TPasCubeScreen.AfterCreateSwapChain;
var Index,SwapChainImageIndex:TpvInt32;
    VulkanCommandBuffer:TpvVulkanCommandBuffer;
begin
 inherited AfterCreateSwapChain;

  FreeAndNil(fVulkanRenderPass);
  FreeAndNil(fVulkanGraphicsPipeline);

  fVulkanRenderPass:=TpvVulkanRenderPass.Create(pvApplication.VulkanDevice);

 fVulkanRenderPass.AddSubpassDescription(0,
                                         VK_PIPELINE_BIND_POINT_GRAPHICS,
                                         [],
                                         [fVulkanRenderPass.AddAttachmentReference(fVulkanRenderPass.AddAttachmentDescription(0,
                                                                                                                              pvApplication.VulkanSwapChain.ImageFormat,
                                                                                                                              VK_SAMPLE_COUNT_1_BIT,
                                                                                                                              VK_ATTACHMENT_LOAD_OP_CLEAR,
                                                                                                                              VK_ATTACHMENT_STORE_OP_STORE,
                                                                                                                              VK_ATTACHMENT_LOAD_OP_DONT_CARE,
                                                                                                                              VK_ATTACHMENT_STORE_OP_DONT_CARE,
                                                                                                                              VK_IMAGE_LAYOUT_UNDEFINED,
                                                                                                                              VK_IMAGE_LAYOUT_PRESENT_SRC_KHR
                                                                                                                             ),
                                                                             VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL
                                                                            )],
                                         [],
                                         fVulkanRenderPass.AddAttachmentReference(fVulkanRenderPass.AddAttachmentDescription(0,
                                                                                                                             pvApplication.VulkanDepthImageFormat,
                                                                                                                             VK_SAMPLE_COUNT_1_BIT,
                                                                                                                             VK_ATTACHMENT_LOAD_OP_CLEAR,
                                                                                                                             VK_ATTACHMENT_STORE_OP_DONT_CARE,
                                                                                                                             VK_ATTACHMENT_LOAD_OP_DONT_CARE,
                                                                                                                             VK_ATTACHMENT_STORE_OP_DONT_CARE,
                                                                                                                             VK_IMAGE_LAYOUT_UNDEFINED,
                                                                                                                             VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL
                                                                                                                            ),
                                                                                  VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL
                                                                                 ),
                                         []);
 fVulkanRenderPass.AddSubpassDependency(VK_SUBPASS_EXTERNAL,
                                        0,
                                        TVkPipelineStageFlags(VK_PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT),
                                        TVkPipelineStageFlags(VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT),
                                        TVkAccessFlags(VK_ACCESS_MEMORY_READ_BIT),
                                        TVkAccessFlags(VK_ACCESS_COLOR_ATTACHMENT_READ_BIT) or TVkAccessFlags(VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT),
                                        TVkDependencyFlags(VK_DEPENDENCY_BY_REGION_BIT));
 fVulkanRenderPass.AddSubpassDependency(0,
                                        VK_SUBPASS_EXTERNAL,
                                        TVkPipelineStageFlags(VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT),
                                        TVkPipelineStageFlags(VK_PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT),
                                        TVkAccessFlags(VK_ACCESS_COLOR_ATTACHMENT_READ_BIT) or TVkAccessFlags(VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT),
                                        TVkAccessFlags(VK_ACCESS_MEMORY_READ_BIT),
                                        TVkDependencyFlags(VK_DEPENDENCY_BY_REGION_BIT));
 fVulkanRenderPass.Initialize;

 fVulkanRenderPass.ClearValues[0].color.float32[0]:=0.15;
 fVulkanRenderPass.ClearValues[0].color.float32[1]:=0.15;
 fVulkanRenderPass.ClearValues[0].color.float32[2]:=0.15;
 fVulkanRenderPass.ClearValues[0].color.float32[3]:=1.0;

 fVulkanGraphicsPipeline:=TpvVulkanGraphicsPipeline.Create(pvApplication.VulkanDevice,
                                                           pvApplication.VulkanPipelineCache,
                                                           0,
                                                           [],
                                                           fVulkanPipelineLayout,
                                                           fVulkanRenderPass,
                                                           0,
                                                           nil,
                                                           0);

 fVulkanGraphicsPipeline.AddStage(fVulkanPipelineShaderStageCubeVertex);
 fVulkanGraphicsPipeline.AddStage(fVulkanPipelineShaderStageCubeFragment);

 fVulkanGraphicsPipeline.InputAssemblyState.Topology:=VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST;
 fVulkanGraphicsPipeline.InputAssemblyState.PrimitiveRestartEnable:=false;

 fVulkanGraphicsPipeline.VertexInputState.AddVertexInputBindingDescription(0,SizeOf(TVertex),VK_VERTEX_INPUT_RATE_VERTEX);
 fVulkanGraphicsPipeline.VertexInputState.AddVertexInputAttributeDescription(0,0,VK_FORMAT_R32G32B32_SFLOAT,TVkPtrUInt(pointer(@PVertex(nil)^.Position)));
 fVulkanGraphicsPipeline.VertexInputState.AddVertexInputAttributeDescription(1,0,VK_FORMAT_R32G32B32_SFLOAT,TVkPtrUInt(pointer(@PVertex(nil)^.Tangent)));
 fVulkanGraphicsPipeline.VertexInputState.AddVertexInputAttributeDescription(2,0,VK_FORMAT_R32G32B32_SFLOAT,TVkPtrUInt(pointer(@PVertex(nil)^.Bitangent)));
 fVulkanGraphicsPipeline.VertexInputState.AddVertexInputAttributeDescription(3,0,VK_FORMAT_R32G32B32_SFLOAT,TVkPtrUInt(pointer(@PVertex(nil)^.Normal)));
 fVulkanGraphicsPipeline.VertexInputState.AddVertexInputAttributeDescription(4,0,VK_FORMAT_R32G32_SFLOAT,TVkPtrUInt(pointer(@PVertex(nil)^.TexCoord)));

 fVulkanGraphicsPipeline.ViewPortState.AddViewPort(0.0,0.0,pvApplication.VulkanSwapChain.Width,pvApplication.VulkanSwapChain.Height,0.0,1.0);
 fVulkanGraphicsPipeline.ViewPortState.AddScissor(0,0,pvApplication.VulkanSwapChain.Width,pvApplication.VulkanSwapChain.Height);

 fVulkanGraphicsPipeline.RasterizationState.DepthClampEnable:=false;
 fVulkanGraphicsPipeline.RasterizationState.RasterizerDiscardEnable:=false;
 fVulkanGraphicsPipeline.RasterizationState.PolygonMode:=VK_POLYGON_MODE_FILL;
 fVulkanGraphicsPipeline.RasterizationState.CullMode:=TVkCullModeFlags(VK_CULL_MODE_BACK_BIT);
 fVulkanGraphicsPipeline.RasterizationState.FrontFace:=VK_FRONT_FACE_COUNTER_CLOCKWISE;
 fVulkanGraphicsPipeline.RasterizationState.DepthBiasEnable:=false;
 fVulkanGraphicsPipeline.RasterizationState.DepthBiasConstantFactor:=0.0;
 fVulkanGraphicsPipeline.RasterizationState.DepthBiasClamp:=0.0;
 fVulkanGraphicsPipeline.RasterizationState.DepthBiasSlopeFactor:=0.0;
 fVulkanGraphicsPipeline.RasterizationState.LineWidth:=1.0;

 fVulkanGraphicsPipeline.MultisampleState.RasterizationSamples:=VK_SAMPLE_COUNT_1_BIT;
 fVulkanGraphicsPipeline.MultisampleState.SampleShadingEnable:=false;
 fVulkanGraphicsPipeline.MultisampleState.MinSampleShading:=0.0;
 fVulkanGraphicsPipeline.MultisampleState.CountSampleMasks:=0;
 fVulkanGraphicsPipeline.MultisampleState.AlphaToCoverageEnable:=false;
 fVulkanGraphicsPipeline.MultisampleState.AlphaToOneEnable:=false;

 fVulkanGraphicsPipeline.ColorBlendState.LogicOpEnable:=false;
 fVulkanGraphicsPipeline.ColorBlendState.LogicOp:=VK_LOGIC_OP_COPY;
 fVulkanGraphicsPipeline.ColorBlendState.BlendConstants[0]:=0.0;
 fVulkanGraphicsPipeline.ColorBlendState.BlendConstants[1]:=0.0;
 fVulkanGraphicsPipeline.ColorBlendState.BlendConstants[2]:=0.0;
 fVulkanGraphicsPipeline.ColorBlendState.BlendConstants[3]:=0.0;
 fVulkanGraphicsPipeline.ColorBlendState.AddColorBlendAttachmentState(true,
                                                                      VK_BLEND_FACTOR_SRC_ALPHA,
                                                                      VK_BLEND_FACTOR_ONE_MINUS_SRC_ALPHA,
                                                                      VK_BLEND_OP_ADD,
                                                                      VK_BLEND_FACTOR_ONE,
                                                                      VK_BLEND_FACTOR_ZERO,
                                                                      VK_BLEND_OP_ADD,
                                                                      TVkColorComponentFlags(VK_COLOR_COMPONENT_R_BIT) or
                                                                      TVkColorComponentFlags(VK_COLOR_COMPONENT_G_BIT) or
                                                                      TVkColorComponentFlags(VK_COLOR_COMPONENT_B_BIT) or
                                                                      TVkColorComponentFlags(VK_COLOR_COMPONENT_A_BIT));

 fVulkanGraphicsPipeline.DepthStencilState.DepthTestEnable:=true;
 fVulkanGraphicsPipeline.DepthStencilState.DepthWriteEnable:=true; // Glass usually disables DepthWrite but here we want it for simplicity
 fVulkanGraphicsPipeline.DepthStencilState.DepthCompareOp:=VK_COMPARE_OP_LESS;
 fVulkanGraphicsPipeline.DepthStencilState.DepthBoundsTestEnable:=false;
 fVulkanGraphicsPipeline.DepthStencilState.StencilTestEnable:=false;

  fVulkanGraphicsPipeline.Initialize;
 
  fVulkanGraphicsPipeline.FreeMemory;
 
  for Index:=0 to pvApplication.CountInFlightFrames-1 do begin

  for SwapChainImageIndex:=0 to length(fVulkanRenderCommandBuffers[Index])-1 do begin
   FreeAndNil(fVulkanRenderCommandBuffers[Index,SwapChainImageIndex]);
  end;

  SetLength(fVulkanRenderCommandBuffers[Index],pvApplication.CountSwapChainImages);

  for SwapChainImageIndex:=0 to pvApplication.CountSwapChainImages-1 do begin

   fVulkanRenderCommandBuffers[Index,SwapChainImageIndex]:=TpvVulkanCommandBuffer.Create(fVulkanCommandPool,VK_COMMAND_BUFFER_LEVEL_PRIMARY);

  end;

 end;

end;



procedure TPasCubeScreen.BeforeDestroySwapChain;
begin
 FreeAndNil(fVulkanRenderPass);
 FreeAndNil(fVulkanGraphicsPipeline);
 inherited BeforeDestroySwapChain;
end;

function TPasCubeScreen.KeyEvent(const aKeyEvent:TpvApplicationInputKeyEvent):boolean;
begin
 result:=false;
 if fReady and (aKeyEvent.KeyEventType=TpvApplicationInputKeyEventType.Down) then begin
  case aKeyEvent.KeyCode of
   KEYCODE_ESCAPE:begin
    pvApplication.Terminate;
   end;
  end;
 end;
end;

function TPasCubeScreen.PointerEvent(const aPointerEvent:TpvApplicationInputPointerEvent):boolean;
var Delta:TpvVector2;
begin
 result:=inherited PointerEvent(aPointerEvent);
 case aPointerEvent.PointerEventType of
  TpvApplicationInputPointerEventType.Down:begin
   if aPointerEvent.Button=TpvApplicationInputPointerButton.Left then begin
    fMouseLeftButtonDown:=true;
    fLastMousePosition:=aPointerEvent.Position;
    fAutoRotation:=false;
   end;
  end;
  TpvApplicationInputPointerEventType.Up:begin
   if aPointerEvent.Button=TpvApplicationInputPointerButton.Left then begin
    fMouseLeftButtonDown:=false;
    fAutoRotation:=true;
   end;
  end;
  TpvApplicationInputPointerEventType.Motion:begin
   if fMouseLeftButtonDown then begin
    Delta:=aPointerEvent.Position-fLastMousePosition;
    fLastMousePosition:=aPointerEvent.Position;
    fState.AnglePhases[1]:=fState.AnglePhases[1]+(Delta.x*0.005);
    fState.AnglePhases[0]:=fState.AnglePhases[0]+(Delta.y*0.005);
   end;
  end;
 end;
end;

function TPasCubeScreen.Scrolled(const aRelativeAmount:TpvVector2):boolean;
begin
 result:=false;
end;

function TPasCubeScreen.CanBeParallelProcessed:boolean;
begin
 result:=true;
end;

procedure TPasCubeScreen.Update(const aDeltaTime:TpvDouble);
const f0=2.5/(2.0*pi);  // 2.5x rotation speed
      f1=1.25/(2.0*pi); // 2.5x rotation speed
      MaxFPS=120.0; // Maximum FPS for full rotation speed
var SpeedMultiplier:TpvDouble;
begin
 inherited Update(aDeltaTime);
 if fAutoRotation then begin
  // Calculate speed multiplier based on current FPS (0.0 to 1.0)
  // At 360 FPS = 1.0 (max speed), at 180 FPS = 0.5, at 60 FPS = 0.167
  SpeedMultiplier:=pvApplication.FramesPerSecond/MaxFPS;
  if SpeedMultiplier>1.0 then SpeedMultiplier:=1.0; // Clamp to max
  if SpeedMultiplier<0.0 then SpeedMultiplier:=0.0; // Prevent negative
  
  fState.Time:=fState.Time+aDeltaTime;
  fState.AnglePhases[0]:=frac(fState.AnglePhases[0]+(aDeltaTime*f0*SpeedMultiplier));
  fState.AnglePhases[1]:=frac(fState.AnglePhases[1]+(aDeltaTime*f1*SpeedMultiplier));
 end;
 fStates[pvApplication.UpdateInFlightFrameIndex]:=fState;
 fReady:=true;

 if assigned(UnitPasCubeApplication.Application) and assigned(UnitPasCubeApplication.Application.TextOverlay) then begin
  case Trunc(fState.Time*0.33) mod 4 of
   0: UnitPasCubeApplication.Application.TextOverlay.AddText(pvApplication.Width*0.5,pvApplication.Height*0.9,2.0,toaCenter,'Copper');
   1: UnitPasCubeApplication.Application.TextOverlay.AddText(pvApplication.Width*0.5,pvApplication.Height*0.9,2.0,toaCenter,'Gold');
   2: UnitPasCubeApplication.Application.TextOverlay.AddText(pvApplication.Width*0.5,pvApplication.Height*0.9,2.0,toaCenter,'Steel');
   3: UnitPasCubeApplication.Application.TextOverlay.AddText(pvApplication.Width*0.5,pvApplication.Height*0.9,2.0,toaCenter,'Titanium');
  end;
 end;
end;

procedure TPasCubeScreen.Draw(const aSwapChainImageIndex:TpvInt32;var aWaitSemaphore:TpvVulkanSemaphore;const aWaitFence:TpvVulkanFence=nil);
const TwoPI=2.0*pi;
var p:pointer;
    ModelMatrix:TpvMatrix4x4;
    ViewMatrix:TpvMatrix4x4;
    ProjectionMatrix:TpvMatrix4x4;
    State:PScreenExampleCubeState;
    TextureIndex,FaceIndex:TpvInt32;
    PushConstants:record 
                   Vector:TpvVector4; 
                   Params:TpvVector4;
                  end;
begin
 inherited Draw(aSwapChainImageIndex,aWaitSemaphore,nil);
 if assigned(fVulkanGraphicsPipeline) then begin

  State:=@fStates[pvApplication.DrawInFlightFrameIndex];

  ModelMatrix:=TpvMatrix4x4.CreateRotate(State^.AnglePhases[0]*TwoPI,TpvVector3.Create(0.0,0.0,1.0))*
               TpvMatrix4x4.CreateRotate(State^.AnglePhases[1]*TwoPI,TpvVector3.Create(0.0,1.0,0.0));
  ViewMatrix:=TpvMatrix4x4.CreateTranslation(0.0,0.0,-6.0);
  ProjectionMatrix:=TpvMatrix4x4.CreatePerspective(45.0,pvApplication.Width/pvApplication.Height,1.0,128.0);

  fUniformBuffer.ModelViewProjectionMatrix:=(ModelMatrix*ViewMatrix)*ProjectionMatrix;
  fUniformBuffer.ModelViewMatrix:=ModelMatrix*ViewMatrix;
  fUniformBuffer.ModelViewNormalMatrix:=TpvMatrix4x4.Create((ModelMatrix*ViewMatrix).ToMatrix3x3.Inverse.Transpose);

  p:=fVulkanUniformBuffers[pvApplication.DrawInFlightFrameIndex].Memory.MapMemory(0,SizeOf(TScreenExampleCubeUniformBuffer));
  if assigned(p) then begin
   Move(fUniformBuffer,p^,SizeOf(TScreenExampleCubeUniformBuffer));
   fVulkanUniformBuffers[pvApplication.DrawInFlightFrameIndex].Memory.UnmapMemory;
  end;

  fVulkanRenderCommandBuffers[pvApplication.DrawInFlightFrameIndex,aSwapChainImageIndex].Reset(TVkCommandBufferResetFlags(VK_COMMAND_BUFFER_RESET_RELEASE_RESOURCES_BIT));
  fVulkanRenderCommandBuffers[pvApplication.DrawInFlightFrameIndex,aSwapChainImageIndex].BeginRecording(TVkCommandBufferUsageFlags(VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT));

  fVulkanRenderPass.BeginRenderPass(fVulkanRenderCommandBuffers[pvApplication.DrawInFlightFrameIndex,aSwapChainImageIndex],
                                    pvApplication.VulkanFrameBuffers[aSwapChainImageIndex],
                                    VK_SUBPASS_CONTENTS_INLINE,
                                    0,
                                    0,
                                    pvApplication.VulkanSwapChain.Width,
                                    pvApplication.VulkanSwapChain.Height);

  fVulkanRenderCommandBuffers[pvApplication.DrawInFlightFrameIndex,aSwapChainImageIndex].CmdBindVertexBuffers(0,1,@fVulkanVertexBuffer.Handle,@Offsets);
  fVulkanRenderCommandBuffers[pvApplication.DrawInFlightFrameIndex,aSwapChainImageIndex].CmdBindIndexBuffer(fVulkanIndexBuffer.Handle,0,VK_INDEX_TYPE_UINT32);

  TextureIndex:=Trunc(State^.Time*0.33) mod CountTextures;

   case TextureIndex of
    0:begin
     // Copper (Swapped with Gold)
     PushConstants.Vector:=TpvVector4.Create(0.72, 0.45, 0.20, 1.0); 
     PushConstants.Params:=TpvVector4.Create(1.0, 0.6, 16.0, 0.0); 
    end;
    1:begin
     // Gold (Swapped with Copper)
     PushConstants.Vector:=TpvVector4.Create(1.0, 0.84, 0.0, 1.0); 
     PushConstants.Params:=TpvVector4.Create(1.0, 0.8, 32.0, 0.0); 
    end;
    2:begin
     // Steel
     PushConstants.Vector:=TpvVector4.Create(0.8, 0.8, 0.9, 1.0); 
     PushConstants.Params:=TpvVector4.Create(0.9, 0.9, 64.0, 0.0); // Shiny
    end;
    3:begin
     // Titanium
     PushConstants.Vector:=TpvVector4.Create(0.3, 0.3, 0.4, 1.0); 
     PushConstants.Params:=TpvVector4.Create(0.8, 0.5, 128.0, 0.0); // Matte-ish but smooth
    end;
    else begin
     PushConstants.Vector:=TpvVector4.Create(1.0, 1.0, 1.0, 1.0);
     PushConstants.Params:=TpvVector4.Create(1.0, 1.0, 32.0, 0.0);
    end;
   end;
 
   // Bind Standard Pipeline (for Front Faces or Opaque)
   fVulkanRenderCommandBuffers[pvApplication.DrawInFlightFrameIndex,aSwapChainImageIndex].CmdBindPipeline(VK_PIPELINE_BIND_POINT_GRAPHICS,fVulkanGraphicsPipeline.Handle);
  
  fVulkanRenderCommandBuffers[pvApplication.DrawInFlightFrameIndex,aSwapChainImageIndex].CmdPushConstants(fVulkanPipelineLayout.Handle,
                                                                                                        TVkShaderStageFlags(VK_SHADER_STAGE_FRAGMENT_BIT),
                                                                                                        0,
                                                                                                        SizeOf(TpvVector4)*2, // Send both vectors
                                                                                                        @PushConstants);

  // Draw each face with its assigned overlay texture
  for FaceIndex:=0 to 5 do begin
   fVulkanRenderCommandBuffers[pvApplication.DrawInFlightFrameIndex,aSwapChainImageIndex].CmdBindDescriptorSets(VK_PIPELINE_BIND_POINT_GRAPHICS,
                                                                                                                fVulkanPipelineLayout.Handle,
                                                                                                                0,
                                                                                                                1,
                                                                                                                @fFaceDescriptorSets[pvApplication.DrawInFlightFrameIndex,FaceIndex].Handle,
                                                                                                                0,
                                                                                                                nil);
   fVulkanRenderCommandBuffers[pvApplication.DrawInFlightFrameIndex,aSwapChainImageIndex].CmdDrawIndexed(6,1,FaceIndex*6,0,0);
  end;


  fVulkanRenderPass.EndRenderPass(fVulkanRenderCommandBuffers[pvApplication.DrawInFlightFrameIndex,aSwapChainImageIndex]);

  fVulkanRenderCommandBuffers[pvApplication.DrawInFlightFrameIndex,aSwapChainImageIndex].EndRecording;

  fVulkanRenderCommandBuffers[pvApplication.DrawInFlightFrameIndex,aSwapChainImageIndex].Execute(pvApplication.VulkanDevice.GraphicsQueue,
                                                                                                 TVkPipelineStageFlags(VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT),
                                                                                                 aWaitSemaphore,
                                                                                                 fVulkanRenderSemaphores[pvApplication.DrawInFlightFrameIndex],
                                                                                                 aWaitFence,
                                                                                                 false);

  aWaitSemaphore:=fVulkanRenderSemaphores[pvApplication.DrawInFlightFrameIndex];

 end;
end;

end.
