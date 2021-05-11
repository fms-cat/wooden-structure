/* framework header */
#version 430

layout(location = 0) uniform vec2 iResolution;
layout(binding = 0) uniform sampler2D accumulatorTex;

void main() {
  vec4 tex = texelFetch( accumulatorTex, ivec2( gl_FragCoord.xy ), 0 );

  // vec3 col = pow( tex.rgb / tex.a, vec3( 0.4545 ) );
  vec3 col = sqrt( tex.rgb / tex.a );

  gl_FragColor = vec4(
    smoothstep( 0.1, 0.8, col.x ),
    smoothstep( 0.0, 1.0, col.y ),
    smoothstep( -0.2, 1.3, col.z ),
    1
  );
}
