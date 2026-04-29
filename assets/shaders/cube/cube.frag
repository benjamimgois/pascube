#version 450

#extension GL_ARB_separate_shader_objects : enable
#extension GL_ARB_shading_language_420pack : enable

layout (location = 0) in vec3 inNormal;
layout (location = 1) in vec2 inTexCoord;

layout (binding = 1) uniform sampler2D samplerColor;

layout (push_constant) uniform PushConsts {
	vec4 vector;
	vec4 params;
} pushConsts;

layout (location = 0) out vec4 outColor;

void main() {
    vec3 N = normalize(inNormal);
    vec3 L = normalize(vec3(0.5, 0.7, 1.0)); // Light Dir
    vec3 V = vec3(0.0, 0.0, 1.0); // View Dir (approx)
    vec3 H = normalize(L + V);
    
    // Cartoon: quantize diffuse into 3 bands
    float diffRaw = max(dot(N, L), 0.0);
    float diff;
    if (diffRaw > 0.75) diff = 1.0;
    else if (diffRaw > 0.35) diff = 0.55;
    else diff = 0.2;
    
    // Cartoon: quantize specular into 2 bands
    float specRaw = pow(max(dot(N, H), 0.0), pushConsts.params.z);
    float spec;
    if (specRaw > 0.6) spec = 1.0;
    else spec = 0.0;
    
    // Ambient + Diffuse + Specular
    vec3 ambient = vec3(0.15);
    vec3 diffuse = diff * pushConsts.params.x * vec3(1.0);
    vec3 specular = spec * pushConsts.params.y * vec3(1.0);
    
    vec3 lighting = ambient + diffuse + specular;
    
    // Subtle brushed hint (very faint)
    float brush = sin(inTexCoord.y * 60.0) * 0.02 + 0.98;
    
    // Texture sample
    vec4 texColor = texture(samplerColor, inTexCoord);
    
    // Brighter steel tint
    vec3 tintedTex = texColor.rgb * pushConsts.vector.rgb * brush;
    
    // Cartoon material color
    vec3 materialColor = lighting * tintedTex;
    
    // Edge detection for cartoon outline effect
    float edgeThickness = 0.018;
    float edgeX = min(inTexCoord.x, 1.0 - inTexCoord.x);
    float edgeY = min(inTexCoord.y, 1.0 - inTexCoord.y);
    float edgeDist = min(edgeX, edgeY);
    float edgeFactor = smoothstep(0.0, edgeThickness, edgeDist);
    
    // Dark cartoon edge
    vec3 edgeColor = vec3(0.04, 0.04, 0.06);
    
    // Blend edge
    vec3 finalColor = mix(edgeColor, materialColor, edgeFactor);
    
    outColor = vec4(finalColor, pushConsts.vector.a);
}
