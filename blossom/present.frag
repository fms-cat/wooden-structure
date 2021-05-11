/* framework header */
#version 430

layout(location = 0) uniform vec2 iResolution;
layout(binding = 0) uniform sampler2D accumulatorTex;

void main() {
  vec2 p = ( gl_FragCoord.xy - 0.5 * iResolution.xy ) / iResolution.y;

  vec4 tex = texelFetch(accumulatorTex,ivec2(gl_FragCoord.xy),0);
  vec3 col = tex.rgb / tex.a;

  col = pow( col, vec3( 0.4545 ) );
  col *= 1.0 - 0.2 * length( p );
  col = vec3(
    smoothstep( 0.08, 0.94, col.x ),
    smoothstep( 0.02, 0.98, col.y ),
    smoothstep( -0.06, 1.08, col.z )
  );

  gl_FragColor = vec4(col,1);
}
