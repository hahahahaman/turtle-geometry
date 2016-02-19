#version 330
layout (location = 0) in vec2 vertex;

out vec2 TexCoords;

uniform mat4 model;
uniform mat4 view;
uniform mat4 projection;

void main()
{
  gl_Position = projection * view * model * vec4(vertex, 0.0f, 1.0f);
}
