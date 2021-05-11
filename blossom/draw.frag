/* framework header */
#version 430
layout(location = 0) uniform vec4 iResolution;
layout(location = 1) uniform int iFrame;

 


/* vvv your shader goes here vvv */

#define lofi(i,j) (floor((i)/(j))*(j))
#define saturate(i) clamp((i),0.,1.)
#define fs(i) (fract(sin((i)*114.514)*1919.810))

const float PI = acos( -1. );
const float TAU = 2.0 * PI;
const float EPSILON = 1E-3;
const float FAR = 1E2;
const float MARGIN = 1.0 / 64.0;
const vec3 DIELECTRIC_SPECULAR = vec3( 0.04 );

float seed;

float hash() {
  seed = fs( seed );
  return seed;
}

vec2 hash2() {
  return vec2( hash(), hash() );
}

vec3 importanceSampleGGX( vec2 Xi, float roughness, vec3 N ) {
  float a = roughness * roughness;

  float phi = TAU * Xi.x;
  float cosTheta = roughness > 1.0 // use lambert ???
    ? cos( asin( sqrt( Xi.y ) ) )
    : sqrt( ( 1.0 - Xi.y ) / ( 1.0 + ( a * a - 1.0 ) * Xi.y ) );
  float sinTheta = sqrt( 1.0 - cosTheta * cosTheta );

  // from spherical coordinates to cartesian coordinates
  vec3 H = vec3(
    cos( phi ) * sinTheta,
    sin( phi ) * sinTheta,
    cosTheta
  );

  // from tangent-space vector to world-space sample vector
  vec3 up = abs( N.y ) < 0.999 ? vec3( 0.0, 1.0, 0.0 ) : vec3( 1.0, 0.0, 0.0 );
  vec3 tangent = normalize( cross( up, N ) );
  vec3 bitangent = cross( N, tangent );

  vec3 sampleVec = tangent * H.x + bitangent * H.y + N * H.z;
  return normalize( sampleVec );
}

mat3 orthBas( vec3 d ) {
  vec3 z = normalize( d );
  vec3 x = normalize( cross( vec3( 0, 1, 0 ), z ) );
  vec3 y = cross( z, x );
  return mat3( x, y, z );
}

vec3 cyclicNoise( vec3 p ) {
  mat3 b = orthBas( vec3( 4, -5, 3 ) );
  vec3 sum = vec3( 0 );
  float warp = 1.1;
  float amp = 0.5;

  for ( int i = 0; i < 6; i ++ ) {
    p *= b;
    p *= 2.0;
    p += sin( p.zxy ) * warp;
    sum += cross( cos( p ), sin( p.yzx ) ) * amp;
    amp *= 0.5;
    warp *= 1.3;
  }

  return sum;
}

mat2 rot2d( float t ) {
  float c = cos( t );
  float s = sin( t );
  return mat2( c, -s, s, c );
}

// center, scale, hash
vec4 quadtree( vec2 p ) {
  float s = 1.0, h;
  vec2 pt;
  for ( int i = 0; i < 4; i ++ ) {
    s *= 0.5;
    pt = lofi( p, s ) + 0.5 * s;
    h = fs( pt.x + fs( pt.y ) );
    if ( h > s ) {
      break;
    }
  }
  return vec4( pt, s, h );
}

// near, normal
// Ref: https://iquilezles.org/www/articles/boxfunctions/boxfunctions.htm
vec4 isectbox( vec3 ro, vec3 rd, vec3 size ) {
  vec3 m = 1.0 / rd;
  vec3 n = m * ro;
  vec3 k = abs( m ) * size;
  vec3 t1 = -n - k;
  vec3 t2 = -n + k;

  float tNear = max( max( t1.x, t1.y ), t1.z );
  float tFar = min( min( t2.x, t2.y ), t2.z );

  if ( tNear > tFar || tFar < 0.0 ) {
    return vec4( FAR );
  }

  return vec4( tNear, -sign( rd ) * step( t1.yzx, t1.xyz ) * step( t1.zxy, t1.xyz ) );
}

// returns next cell, length
vec4 segment( float s, vec2 p, vec2 d ) {
  vec2 tow = sign( d ) * s * 0.5;
  vec2 v = ( tow - p ) / d;
  return vec4(
    tow * ( ( v.x < v.y ) ? vec2( 2, 0 ) : vec2( 0, 2 ) ),
    min( v.x, v.y ),
    0.0
  );
}

void main() {
  seed = fs( cyclicNoise( vec3( gl_FragCoord.xy, iFrame ) ) ).x;

  vec2 p = ( gl_FragCoord.xy + hash2() - 0.5 * iResolution.xy ) / iResolution.y;

  vec3 ro = vec3( -2.2, 0, 3.1 );
  vec3 rd = normalize( vec3( p, -1 ) );
  rd *= orthBas( vec3( 1, 2, 4 ) );
  rd.xy = rot2d( 0.8 ) * rd.xy;

  vec2 bokehOffset = ( hash2() - 0.5 ) * rot2d( PI * 0.25 );
  const float dofScale = 0.06;
  const float focusDistance = 2.5;
  ro.xy += bokehOffset * dofScale;
  rd.xy -= bokehOffset * dofScale / focusDistance;

  vec3 col = vec3( 0 );
  vec3 colRem = vec3( 1 );

  for ( int i = 0; i < 5; i ++ ) {
    vec2 rdxy = normalize( rd.xy );
    float dmul = length( rd ) / length( rd.xy );

    float rl = 4.0 * EPSILON;
    vec3 rp = ro + rd * rl;

    vec4 cell;
    vec4 isect;

    for( int j = 0; j < 50; j ++ ) {
      cell = quadtree( rp.xy );

      vec3 rpt = rp - vec3( cell.xy, 0 );
      float height = 0.2 * (
        sin( cell.x + cell.y )
        + cos( 2.0 * length( cell.xy ) )
        + fs( cell.x + fs( cell.y ) )
      );

      float dice = fs( cell.w );
      vec3 size = vec3( 0, 1, 1 ) * ( 0.5 * cell.z - MARGIN );
      size.y -= MARGIN;
      if ( dice < 0.5 ) {
        isect = isectbox( rpt, rd, vec3( size.zz, 1.0 + height ) );
      } else if ( dice < 0.9 ) {
        isect = isectbox( rpt - size.xyx, rd, vec3( size.z, MARGIN, 1.0 + height ) );
        vec4 isectb = isectbox( rpt + size.xyx, rd, vec3( size.z, MARGIN, 1.0 + height ) );
        isect = isectb.x < isect.x ? isectb : isect;
        isectb = isectbox( rpt - size.yxx, rd, vec3( MARGIN, size.z, 1.0 + height ) );
        isect = isectb.x < isect.x ? isectb : isect;
        isectb = isectbox( rpt + size.yxx, rd, vec3( MARGIN, size.z, 1.0 + height ) );
        isect = isectb.x < isect.x ? isectb : isect;
      } else {
        isect = vec4( FAR );
      }

      if ( isect.x < FAR ) {
        rl += isect.x;
        rp = ro + rd * rl;
        break;
      }

      vec4 seg = segment( cell.z, rpt.xy, rdxy );
      rl += seg.z * dmul + EPSILON;
      rp = ro + rd * rl;

      if ( rl > FAR ) break;
    }

    if ( isect.x < FAR ) {
      vec3 N = isect.yzw;

      float mokume = sin( 20.0 * cyclicNoise( rp / 10.0 ).x );
      vec3 color = pow( ( 0.5 + 0.3 * sin( cell.w + cell.x + cell.y + vec3( 0, 1.5, 2.5 ) ) ), vec3( 2.2 ) );
      float metallic = 0.04;
      vec3 albedo = mix( color * 0.96, vec3( 0 ), metallic );
      vec3 f0 = mix( vec3( 0.04 ), color, metallic );

      if ( hash() < 0.5 ) {
        colRem *= 1.0 / PI
          * albedo
          * mix( 0.9, 1.0, smoothstep( 0.0, 0.5, mokume ) )
          * exp( -0.1 * rl );
        ro = rp;
        rd = importanceSampleGGX( hash2(), 2.0, N );
      } else {
        colRem *= 1.0
          * f0
          * mix( 0.9, 1.0, smoothstep( 0.0, 0.5, mokume ) )
          * exp( -0.1 * rl );
        ro = rp;
        rd = reflect( rd, importanceSampleGGX( hash2(), 0.4 + 0.2 * mokume, N ) );
      }
    } else {
      col += colRem * vec3( step( 0.0, rd.z ) * 8.0 );
      break;
    }
  }

  gl_FragColor = vec4( col, 1 );
}
