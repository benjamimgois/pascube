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
    
    // Ambient
    float ambientStrength = 0.05;
    vec3 ambient = ambientStrength * vec3(1.0);
    
    // Diffuse
    float diff = max(dot(N, L), 0.0);
    vec3 diffuse = diff * pushConsts.params.x * vec3(1.0); 
    
    // Specular
    float specularStrength = pushConsts.params.y; 
    float shininess = pushConsts.params.z;       
    float spec = pow(max(dot(N, H), 0.0), shininess);
    vec3 specular = specularStrength * spec * vec3(1.0);
    
    // Texture sample
    vec4 texColor = texture(samplerColor, inTexCoord);
    
    // Brushed steel effect: subtle horizontal streaks
    float brushFreq = 80.0;
    float brush = sin(inTexCoord.y * brushFreq) * 0.04 + 0.96;
    // Add finer noise-like variation
    float fineBrush = sin(inTexCoord.y * brushFreq * 3.7 + inTexCoord.x * 20.0) * 0.02 + 0.98;
    
    // Combine lighting
    vec3 lighting = (ambient + diffuse + specular);
    
    // Apply steel tint to texture with brushed effect
    vec3 tintedTex = texColor.rgb * pushConsts.vector.rgb * brush * fineBrush;
    
    // Material color with lighting
    vec3 materialColor = lighting * tintedTex;
    
    // Edge detection: darken near UV borders to show cube edges
    float edgeThickness = 0.015;
    float edgeX = min(inTexCoord.x, 1.0 - inTexCoord.x);
    float edgeY = min(inTexCoord.y, 1.0 - inTexCoord.y);
    float edgeDist = min(edgeX, edgeY);
    float edgeFactor = smoothstep(0.0, edgeThickness, edgeDist);
    
    // Dark edge color
    vec3 edgeColor = vec3(0.08, 0.08, 0.10);
    
    // Blend edge
    vec3 finalColor = mix(edgeColor, materialColor, edgeFactor);
    
    // Output
    outColor = vec4(finalColor, pushConsts.vector.a);
}
