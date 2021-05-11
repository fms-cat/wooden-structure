/* framework header */
#version 430

layout(location = 0) uniform vec2 iResolution;
layout(binding = 0) uniform sampler2D accumulatorTex;

void main() {
  vec4 tex = texelFetch( accumulatorTex, ivec2( gl_FragCoord.xy ), 0 );

  // vec3 col = pow( tex.rgb / tex.a, vec3( 0.4545 ) );
  vec3 col = sqrt( tex.rgb / tex.a );

  gl_FragColor = vec4(
    smoothstep( 0.08, 0.94, col.x ),
    smoothstep( 0.02, 0.98, col.y ),
    smoothstep( -0.06, 1.08, col.z ),
    1
  );
}
