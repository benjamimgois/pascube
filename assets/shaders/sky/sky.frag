#version 450

#extension GL_ARB_separate_shader_objects : enable
#extension GL_ARB_shading_language_420pack : enable

layout (location = 0) in vec2 inUV;

layout (location = 0) out vec4 outColor;

// Hash function for pseudo-random numbers
float hash(vec2 p) {
    vec3 p3 = fract(vec3(p.xyx) * 0.1031);
    p3 += dot(p3, p3.yzx + 33.33);
    return fract((p3.x + p3.y) * p3.z);
}

// 2D noise
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

void main() {
    vec2 uv = inUV;
    
    // Night sky gradient: dark blue/purple at top, darker at bottom
    vec3 skyTop = vec3(0.02, 0.02, 0.08);      // Very dark blue
    vec3 skyMid = vec3(0.04, 0.03, 0.10);      // Dark purple-blue
    vec3 skyHorizon = vec3(0.06, 0.04, 0.12);  // Slightly lighter near horizon
    
    // City glow on horizon (warm orange/red)
    vec3 cityGlow = vec3(0.8, 0.3, 0.1);
    
    float t = uv.y;
    
    // Base sky gradient
    vec3 color;
    if (t > 0.3) {
        color = mix(skyMid, skyTop, (t - 0.3) / 0.7);
    } else {
        color = mix(skyHorizon, skyMid, t / 0.3);
    }
    
    // City glow band near horizon (bottom of screen)
    float glowIntensity = smoothstep(0.15, 0.0, t) * 0.15;
    // Add some variation to the glow
    float glowVar = noise(vec2(uv.x * 8.0, 0.0)) * 0.5 + 0.5;
    color += cityGlow * glowIntensity * glowVar;
    
    // Stars
    float starNoise = hash(uv * 400.0);
    float star = smoothstep(0.998, 1.0, starNoise);
    // Make stars twinkle slightly based on position
    float twinkle = sin(hash(uv * 100.0) * 100.0) * 0.3 + 0.7;
    color += vec3(0.9, 0.95, 1.0) * star * twinkle * 0.8;
    
    // Distant city lights (small scattered points near horizon)
    float cityLights = hash(uv * 120.0);
    float light = smoothstep(0.992, 0.998, cityLights) * smoothstep(0.12, 0.0, t);
    color += vec3(1.0, 0.8, 0.5) * light * 0.6;
    
    // Vignette (darker corners)
    vec2 vignetteUV = uv * (1.0 - uv);
    float vignette = vignetteUV.x * vignetteUV.y * 15.0;
    color *= clamp(vignette, 0.0, 1.0);
    
    outColor = vec4(color, 1.0);
}
