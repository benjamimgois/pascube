#version 450

#extension GL_ARB_separate_shader_objects : enable
#extension GL_ARB_shading_language_420pack : enable

layout (location = 0) in vec3 inNormal;
layout (location = 1) in vec2 inTexCoord;

layout (binding = 1) uniform sampler2D samplerColor;
layout (binding = 2) uniform sampler2D samplerOverlay;

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
    
    // Texture Sample
    vec4 texColor = texture(samplerColor, inTexCoord);
    
    // Overlay Sample (icon texture) - scale to 50% size, centered
    vec2 overlayUV = (inTexCoord - 0.5) * 2.0 + 0.5; // Scale UVs to make icon 50% smaller
    vec4 overlayColor = vec4(0.0);
    if (overlayUV.x >= 0.0 && overlayUV.x <= 1.0 && overlayUV.y >= 0.0 && overlayUV.y <= 1.0) {
        overlayColor = texture(samplerOverlay, overlayUV);
    }
    float overlayAlpha = overlayColor.a * 0.25; // 25% opacity
    
    // Combine
    vec3 lighting = (ambient + diffuse + specular);
    
    // Apply Tint (vector.rgb) to Texture
    vec3 tintedTex = texColor.rgb * pushConsts.vector.rgb;
    
    // Material color with lighting
    vec3 materialColor = lighting * tintedTex;
    
    // Blend overlay on top (using overlay's alpha)
    vec3 finalColor = mix(materialColor, overlayColor.rgb, overlayAlpha);
    
    // Output
    outColor = vec4(finalColor, pushConsts.vector.a);
}
