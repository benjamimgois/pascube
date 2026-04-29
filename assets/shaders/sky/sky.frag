#version 450

#extension GL_ARB_separate_shader_objects : enable
#extension GL_ARB_shading_language_420pack : enable

layout (location = 0) in vec2 inUV;

layout (location = 0) out vec4 outColor;

float hash(vec2 p) {
    vec3 p3 = fract(vec3(p.xyx) * 0.1031);
    p3 += dot(p3, p3.yzx + 33.33);
    return fract((p3.x + p3.y) * p3.z);
}

float noise(vec2 p) {
    vec2 i = floor(p);
    vec2 f = fract(p);
    f = f * f * (3.0 - 2.0 * f);
    float a = hash(i);
    float b = hash(i + vec2(1.0, 0.0));
    float c = hash(i + vec2(0.0, 1.0));
    float d = hash(i + vec2(1.0, 1.0));
    return mix(mix(a, b, f.x), mix(c, d, f.x), f.y);
}

float fbm(vec2 p) {
    float value = 0.0;
    float amplitude = 0.5;
    for (int i = 0; i < 5; i++) {
        value += amplitude * noise(p);
        p *= 2.0;
        amplitude *= 0.5;
    }
    return value;
}

void main() {
    vec2 uv = inUV;
    
    // Very dark space base
    vec3 spaceDark = vec3(0.01, 0.01, 0.02);
    
    // Subtle nebula: blue-gray tones
    vec3 nebula1 = vec3(0.06, 0.07, 0.12); // Cool blue-gray
    vec3 nebula2 = vec3(0.08, 0.06, 0.10); // Purple-gray
    
    // Nebula noise clouds
    float n1 = fbm(uv * 3.0 + vec2(0.5, 0.2));
    float n2 = fbm(uv * 2.5 - vec2(0.3, 0.7));
    
    // Mix nebula colors softly
    vec3 nebulaColor = mix(nebula1, nebula2, n2);
    float nebulaMask = smoothstep(0.3, 0.7, n1) * 0.6;
    
    vec3 color = mix(spaceDark, nebulaColor, nebulaMask);
    
    // Very subtle stars (distant, small)
    float starField = hash(uv * 350.0);
    float star = smoothstep(0.996, 1.0, starField);
    color += vec3(0.85, 0.9, 1.0) * star * 0.5;
    
    // Larger, brighter stars (fewer)
    float brightStarField = hash(uv * 90.0);
    float brightStar = smoothstep(0.992, 1.0, brightStarField);
    color += vec3(1.0, 1.0, 1.0) * brightStar * 0.7;
    
    // Vignette
    vec2 vignetteUV = uv * (1.0 - uv);
    float vignette = vignetteUV.x * vignetteUV.y * 15.0;
    color *= clamp(vignette, 0.0, 1.0);
    
    outColor = vec4(color, 1.0);
}
