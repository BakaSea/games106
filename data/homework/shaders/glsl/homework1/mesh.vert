#version 450

layout (location = 0) in vec3 inPos;
layout (location = 1) in vec3 inNormal;
layout (location = 2) in vec2 inUV;
layout (location = 3) in vec3 inColor;
layout (location = 4) in uint inIndex;

layout (set = 0, binding = 0) uniform UBOScene {
	mat4 projection;
	mat4 view;
	vec4 lightPos;
	vec4 viewPos;
} uboScene;

layout (std430, set = 1, binding = 0) readonly buffer Animation {
	mat4 transform[];
} model;

layout (location = 0) out vec3 outNormal;
layout (location = 1) out vec3 outColor;
layout (location = 2) out vec2 outUV;
layout (location = 3) out vec3 outViewVec;
layout (location = 4) out vec3 outLightVec;

void main() 
{
	outColor = inColor;
	outUV = inUV;
	mat4 model = model.transform[inIndex];
	outNormal = mat3(model) * inNormal;
	vec4 pos = model * vec4(inPos, 1.0);
	pos /= pos.w;
	vec3 lightPos = uboScene.lightPos.xyz / uboScene.lightPos.w;
	vec3 viewPos = inverse(mat3(uboScene.view)) * uboScene.viewPos.xyz;
	outLightVec = lightPos - pos.xyz;
	outViewVec = viewPos - pos.xyz;
	gl_Position = uboScene.projection * uboScene.view * pos;
}