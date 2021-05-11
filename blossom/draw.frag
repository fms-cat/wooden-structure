/* framework header */
#version 430
layout(location = 0) uniform vec2 iResolution;
layout(location = 1) uniform int iFrame;

 


/* vvv your shader goes here vvv */

#define L(i,j) (floor((i)/(j))*(j))
#define F(i) (fract(sin((i)*312.)*818.))

const float PI = acos( -1. );
const float TAU = 2.0 * PI;
const float EPSILON = 1E-3;
const float FAR = 1E2;
const float MARGIN = 1.0 / 64.0;

float seed;

float hash21( vec2 v ) {
  return F( v.x + F( v.y ) );
}

float random() {
  seed = F( seed );
  return seed;
}

mat3 orthBas( vec3 d ) {
  vec3 z = normalize( d );
  vec3 x = normalize( cross(
    abs( z.y ) < 0.999 ? vec3( 0.0, 1.0, 0.0 ) : vec3( 1.0, 0.0, 0.0 ),
    z
  ) );
  vec3 y = cross( z, x );
  return mat3( x, y, z );
}

vec3 importanceSampleGGX( float roughnessSqSq, vec3 N ) {
  float phi = TAU * random();
  float cosTheta = random();
  cosTheta = roughnessSqSq > 1.0 // use lambert ???
    ? cos( asin( sqrt( cosTheta ) ) )
    : sqrt( ( 1.0 - cosTheta ) / ( 1.0 + ( roughnessSqSq - 1.0 ) * cosTheta ) );
  float sinTheta = sqrt( 1.0 - cosTheta * cosTheta );

  return orthBas( N ) * vec3(
    cos( phi ) * sinTheta,
    sin( phi ) * sinTheta,
    cosTheta
  );
}

// center, scale, hash
vec4 quadtree( vec2 p ) {
  float s = 1.0, h;
  vec2 pt;
  for ( int i = 0; i < 4; i ++ ) {
    s *= 0.5;
    pt = L( p, s ) + 0.5 * s;
    h = 0.7 * hash21( pt );
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

  vec3 t = -n + k;
  float tFar = min( min( t.x, t.y ), t.z );
  t = -n - k;
  float tNear = max( max( t.x, t.y ), t.z );

  if ( tNear > tFar || tNear < 0.0 ) {
    return vec4( FAR );
  }

  return vec4( tNear, -sign( rd ) * step( t.yzx, t.xyz ) * step( t.zxy, t.xyz ) );
}

// length
float segment( float s, vec2 p, vec2 d ) {
  // vec2 tow = sign( d ) * s * 0.5;
  // vec2 v = ( tow - p ) / d;
  vec2 v = ( ( sign( d ) * s * 0.5 ) - p ) / d;

  // vec2 nextCell = tow * ( ( v.x < v.y ) ? vec2( 2, 0 ) : vec2( 0, 2 ) ),

  return min( v.x, v.y );
}

void main() {
  // seed = float( iFrame ) + hash21( gl_FragCoord.xy );
  seed = float( iFrame );

  vec2 p = ( gl_FragCoord.xy + vec2( random(), random() ) - 0.5 * iResolution.xy ) / iResolution.y;

  gl_FragColor = vec4( 0, 0, 0, 1 );

  float rl = 2.5;
  vec3 ro = vec3( -0.5, 0, 4 );
  vec3 rd = normalize( vec3( p, -1 ) );
  vec3 rp = ro + rd * rl;

  vec3 colRem = vec3( 1 );

  ro.xy += 0.1 * vec2( random(), random() );
  rd = normalize( rp - ro );

  rd *= orthBas( vec3( 1, 2, 5 ) );
  ro *= orthBas( vec3( 1, 2, 5 ) );
  rd.xy = mat2( 0.707, -0.707, 0.707, 0.707 ) * rd.xy;
  ro.xy = mat2( 0.707, -0.707, 0.707, 0.707 ) * ro.xy;

  for ( int i = 0; i < 3; i ++ ) {
    vec2 rdxy = normalize( rd.xy );
    float dmul = length( rd ) / length( rd.xy );

    rl = EPSILON;
    rp = ro + rd * rl;

    vec4 cell;
    vec4 isect;

    for( int j = 0; j < 50; j ++ ) {
      cell = quadtree( rp.xy );

      vec3 rpt = rp - vec3( cell.xy, 0 );
      float height = (
        1.0
        + 0.4 * hash21( cell.xy )
        + 0.2 * ( cell.x + cell.y )
      );

      float dice = F( cell.w );
      vec3 size = vec3( 0, 1, 1 ) * ( 0.5 * cell.z - MARGIN );
      size.y -= MARGIN;
      if ( dice < 0.5 ) {
        isect = isectbox( rpt, rd, vec3( size.zz, height ) );
      } else if ( dice < 0.9 ) {
        isect = isectbox( rpt - size.xyx, rd, vec3( size.z, MARGIN, height ) );
        vec4 isectb = isectbox( rpt + size.xyx, rd, vec3( size.z, MARGIN, height ) );
        isect = isectb.x < isect.x ? isectb : isect;
        isectb = isectbox( rpt - size.yxx, rd, vec3( MARGIN, size.z, height ) );
        isect = isectb.x < isect.x ? isectb : isect;
        isectb = isectbox( rpt + size.yxx, rd, vec3( MARGIN, size.z, height ) );
        isect = isectb.x < isect.x ? isectb : isect;
      } else {
        isect = vec4( FAR );
      }

      if ( isect.x < FAR ) {
        rl += isect.x;
        rp = ro + rd * rl;
        break;
      }

      float seg = segment( cell.z, rpt.xy, rdxy );
      rl += seg * dmul + EPSILON;
      rp = ro + rd * rl;

      if ( rl > FAR ) break;
    }

    if ( isect.x < FAR ) {
      vec3 N = isect.yzw;

      ro = rp;
      if ( random() < 0.12 ) {
        // weight should be 0.04 (DIELECTRIC_SPECULAR)
        rd = reflect(
          rd,
          importanceSampleGGX( 0.01, N )
        );
      } else {
        // weight should be 0.96 / PI ~= 0.305577 (albedo * (1.0 - DIELECTRIC_SPECULAR) / PI)
        colRem *= ( 0.5 + 0.3 * sin( dot( cell, vec4( 1 ) ) + vec3( 0, 1.5, 2.5 ) ) );
        colRem *= ( 0.5 + 0.3 * sin( dot( cell, vec4( 1 ) ) + vec3( 0, 1.5, 2.5 ) ) );
        rd = importanceSampleGGX( 2.0, N );
      }
    } else {
      gl_FragColor = vec4( colRem * step( 0.0, rd.z ), 1 );
      break;
    }
  }
}
