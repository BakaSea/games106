#version 450

layout (set = 2, binding = 0) uniform sampler2D samplerColorMap;
layout (set = 3, binding = 0) uniform sampler2D samplerMetallicRoughnessMap;
layout (set = 4, binding = 0) uniform sampler2D samplerNormalMap;
layout (set = 5, binding = 0) uniform sampler2D samplerOcclusionMap;
layout (set = 6, binding = 0) uniform sampler2D samplerEmissiveMap;
layout (set = 7, binding = 0) uniform samplerCube samplerIrradiance;
layout (set = 7, binding = 1) uniform sampler2D samplerBRDFLUT;
layout (set = 7, binding = 2) uniform samplerCube prefilteredMap;

layout (location = 0) in vec3 inNormal;
layout (location = 1) in vec3 inColor;
layout (location = 2) in vec2 inUV;
layout (location = 3) in vec3 inViewVec;
layout (location = 4) in vec3 inLightVec;

layout (push_constant) uniform PushConsts {
	vec4 baseColorFactor;
	vec3 emissiveFactor;
	int baseColorTextureIndex;
	int metallicRoughnessTextureIndex;
	float metallicFactor;
	float roughnessFactor;
	float alphaCutoff;
	int normalTextureIndex;
	int occlusionTextureIndex;
	int emissiveTextureIndex;
	int alphaMode;
} material;

layout (location = 0) out vec4 outFragColor;

#define PI 3.1415926535897932384626433832795

vec3 ALBEDO() {
	return texture(samplerColorMap, inUV).rgb;
}

// Normal Distribution function --------------------------------------
float D_GGX(float dotNH, float roughness)
{
	float alpha = roughness * roughness;
	float alpha2 = alpha * alpha;
	float denom = dotNH * dotNH * (alpha2 - 1.0) + 1.0;
	return (alpha2)/(PI * denom*denom); 
}

// Geometric Shadowing function --------------------------------------
float G_SchlicksmithGGX(float dotNL, float dotNV, float roughness)
{
	float r = (roughness + 1.0);
	float k = (r*r) / 8.0;
	float GL = dotNL / (dotNL * (1.0 - k) + k);
	float GV = dotNV / (dotNV * (1.0 - k) + k);
	return GL * GV;
}

// Fresnel function ----------------------------------------------------
vec3 F_Schlick(float cosTheta, vec3 F0)
{
	return F0 + (1.0 - F0) * pow(1.0 - cosTheta, 5.0);
}

vec3 F_SchlickR(float cosTheta, vec3 F0, float roughness)
{
	return F0 + (max(vec3(1.0 - roughness), F0) - F0) * pow(1.0 - cosTheta, 5.0);
}

vec3 prefilteredReflection(vec3 R, float roughness)
{
	const float MAX_REFLECTION_LOD = 9.0; // todo: param/const
	float lod = roughness * MAX_REFLECTION_LOD;
	float lodf = floor(lod);
	float lodc = ceil(lod);
	vec3 a = textureLod(prefilteredMap, R, lodf).rgb;
	vec3 b = textureLod(prefilteredMap, R, lodc).rgb;
	return mix(a, b, lod - lodf);
}

vec3 specularContribution(vec3 L, vec3 V, vec3 N, vec3 F0, float metallic, float roughness)
{
	// Precalculate vectors and dot products	
	vec3 H = normalize (V + L);
	float dotNH = clamp(dot(N, H), 0.0, 1.0);
	float dotNV = clamp(dot(N, V), 0.0, 1.0);
	float dotNL = clamp(dot(N, L), 0.0, 1.0);

	// Light color fixed
	vec3 lightColor = vec3(1.0);

	vec3 color = vec3(0.0);

	if (dotNL > 0.0) {
		// D = Normal distribution (Distribution of the microfacets)
		float D = D_GGX(dotNH, roughness); 
		// G = Geometric shadowing term (Microfacets shadowing)
		float G = G_SchlicksmithGGX(dotNL, dotNV, roughness);
		// F = Fresnel factor (Reflectance depending on angle of incidence)
		vec3 F = F_Schlick(dotNV, F0);		
		vec3 spec = D * F * G / (4.0 * dotNL * dotNV + 0.001);		
		vec3 kD = (vec3(1.0) - F) * (1.0 - metallic);			
		color += (kD * ALBEDO() / PI + spec) * dotNL;
	}

	return color;
}

void main() 
{
	vec3 N = normalize(inNormal);
	if (material.normalTextureIndex >= 0) {
		N = normalize(inNormal * texture(samplerNormalMap, inUV).xyz);
	}
	vec3 L = normalize(inLightVec);
	vec3 V = normalize(inViewVec);
	vec3 R = reflect(-V, N);

	float roughness = texture(samplerMetallicRoughnessMap, inUV).g * material.roughnessFactor;
	float metallic = texture(samplerMetallicRoughnessMap, inUV).b * material.metallicFactor;
	
	vec3 F0 = vec3(0.04);
	F0 = mix(F0, ALBEDO(), metallic);
	
	vec3 Lo = vec3(0.0);
	Lo += specularContribution(L, V, N, F0, metallic, roughness);

	vec2 brdf = texture(samplerBRDFLUT, vec2(max(dot(N, V), 0.0), roughness)).rg;
	vec3 reflection = prefilteredReflection(R, roughness).rgb;	
	vec3 irradiance = texture(samplerIrradiance, N).rgb;

	// Diffuse based on irradiance
	vec3 diffuse = irradiance * ALBEDO();	

	vec3 F = F_SchlickR(max(dot(N, V), 0.0), F0, roughness);

	// Specular reflectance
	vec3 specular = reflection * (F * brdf.x + brdf.y);

	// Ambient part
	vec3 kD = 1.0 - F;
	kD *= 1.0 - metallic;	  
	vec3 ambient = (kD * diffuse + specular);

	vec3 emit = vec3(0.0);
	if (material.emissiveTextureIndex >= 0) {
		emit = texture(samplerEmissiveMap, inUV).rgb * material.emissiveFactor;
	}

	vec3 color = emit + ambient + Lo;
	color *= inColor;

	float alpha = texture(samplerColorMap, inUV).a;
	if (material.alphaMode == 0) {
		alpha = 1.0;
	} else if (material.alphaMode == 1) {
		alpha = alpha;
	} else if (material.alphaMode == 2) {
		if (alpha < material.alphaCutoff) {
			discard;
		} else {
			alpha = 1.0;
		}
	}

	outFragColor = vec4(color, alpha);
}