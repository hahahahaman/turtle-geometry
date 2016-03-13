#version 330 core
layout (location = 0) in vec3 vertex;
layout (location = 1) in vec4 color;

out vec4 Color;

uniform mat4 view;
uniform mat4 projection;

void main(){
  gl_Position = projection * view * vec4(vertex, 1.0);

  Color = color;
}
