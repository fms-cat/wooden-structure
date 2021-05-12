/* framework header */
#version 430
layout(location = 0) uniform vec2 iResolution;
layout(location = 1) uniform int iFrame;

/* vvv your shader goes here vvv */

#define L(i,j) (floor((i)/(j))*(j))
#define F(i) (fract(sin((i)*2222.)*222.))

const float PI = acos( -1. );
const float TAU = 2.0 * PI;
const float FAR = 1E2;

float cellSize;
float seed;
vec2 cellPos;
vec4 cellHash;

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

float cyclicNoise( vec3 p, vec3 b, float pump ) {
  mat3 bas = orthBas( b );
  vec2 sum = vec2( 0.0 );

  for ( int i = 0; i < 5; i ++ ) {
    p *= bas * 2.0;
    p += sin( p.yzx );
    sum = pump * sum + vec2( dot( sin( p.zxy ), cos( p ) ), 1.0 );
  }

  return sum.x / sum.y;
}

vec3 importanceSampleGGX( float roughness, vec3 N ) {
  float phi = TAU * random();
  float cosTheta = random();
  cosTheta = roughness > 1.0 // use lambert ???
    ? cos( asin( sqrt( cosTheta ) ) )
    : sqrt( ( 1.0 - cosTheta ) / ( 1.0 + ( pow( roughness, 4.0 ) - 1.0 ) * cosTheta ) );
  float sinTheta = sqrt( 1.0 - cosTheta * cosTheta );

  return orthBas( N ) * vec3(
    cos( phi ) * sinTheta,
    sin( phi ) * sinTheta,
    cosTheta
  );
}

// distance
float sdbox( vec3 p, vec3 s ) {
  vec3 d = abs( p ) - s;
  return min( max( max( d.x, d.y ), d.z ), 0.0 ) + length( max( d, vec3( 0 ) ) );
}

float map( vec3 p ) {
  p -= vec3(
    cellPos,
    (
      0.3 * cellHash.w
      + 0.2 * ( cellPos.x + cellPos.y )
    )
  );

  return max(
    sdbox( p, step( cellHash.y, 0.9 ) * vec2(
      0.5 * cellSize - 1.0 / 64.0,
      8
    ).xxy ),
    -sdbox( p, step( cellHash.y, 0.4 ) * vec2(
      0.5 * cellSize - 3.0 / 64.0,
      9
    ).xxy )
  );
}

vec3 nMap( vec3 p, vec2 d ) {
  return normalize( vec3(
    map( p + d.yxx ) - map( p - d.yxx ),
    map( p + d.xyx ) - map( p - d.xyx ),
    map( p + d.xxy ) - map( p - d.xxy )
  ) );
}

void main() {
  // seed = float( iFrame ) + hash21( gl_FragCoord.xy );
  seed = float( iFrame );

  vec2 p = ( gl_FragCoord.xy + vec2( random(), random() ) - 0.5 * iResolution.xy ) / iResolution.y;

  gl_FragColor = vec4( 0, 0, 0, 1 );

  float rl = 2.0;
  vec3 ro = vec3( 3, 4, 9 );
  vec3 rd = normalize( vec3( p, -1 ) );
  vec3 rp = ro + rd * rl;

  vec3 colRem = vec3( 1 );

  ro.xy += 0.05 * vec2( random(), random() );

  rp *= orthBas( vec3( 2, 2, 5 ) );
  ro *= orthBas( vec3( 2, 2, 5 ) );
  rp.xy = mat2( 0.6, -0.7, 0.7, 0.6 ) * rp.xy;
  ro.xy = mat2( 0.6, -0.7, 0.7, 0.6 ) * ro.xy;

  rd = normalize( rp - ro );

  for ( int i = 0; i < 5; i ++ ) {
    vec2 rdxy = normalize( rd.xy );
    float dmul = length( rd ) / length( rd.xy );
    float dist;

    rl = 4E-3;
    rp = ro + rd * rl;

    vec4 cell;

    for( int i = 0; i < 50; i ++ ) {
      // quadtree begin
      // https://www.shadertoy.com/view/7d2Szc
      cellSize = 1.0;

      for ( int i = 0; i < 4; i ++ ) {
        cellSize *= 0.5;
        cellPos = L( rp.xy, cellSize ) + 0.5 * cellSize;
        cellHash = F( hash21( cellPos ) + vec4( 0, 1, 2, 3 ) );
        if ( 0.7 * cellHash.x > cellSize ) {
          break;
        }
      }
      // quadtree end

      // segment begin
      // vec2 tow = sign( rdxy ) * cellSize * 0.5;
      // vec2 v = ( tow - ( rp.xy - cellPos ) ) / rdxy;
      vec2 v = ( ( sign( rdxy ) * cellSize * 0.5 ) - rp.xy + cellPos ) / rdxy;

      // vec2 nextCell = tow * ( ( v.x < v.y ) ? vec2( 2, 0 ) : vec2( 0, 2 ) ),

      // float seg = min( v.x, v.y );
      // segment end

      float rlNext = rl + min( v.x, v.y ) * dmul + 1E-3;

      // march start
      for ( int i = 0; i < 100; i ++ ) {
        dist = map( rp );
        rl += dist;
        rp = ro + rd * rl;

        if ( dist < 1E-4 ) { break; }
        if ( rlNext < rl ) { break; }
      }
      // march end

      if ( dist < 1E-2 ) { break; }

      rl = rlNext;
      rp = ro + rd * rl;

      if ( rl > FAR ) { break; }
    }

    if ( dist > 1E-3 ) {
      gl_FragColor = vec4( colRem * step( 0.0, rd.z ), 1 );
      break;
    }

    vec3 N = nMap( rp, vec2( 0, 1E-4 ) );

    ro = rp;

    // Ref: https://www.shadertoy.com/view/ldscDM
    float ring = 200.0 * ( dot( rp, 0.5 - cellHash.xyz ) + cyclicNoise( 0.5 * rp, cellHash.xyz, 3.0 ) );
    ring = pow( sin( ring ) * 0.5 + 0.5, 9.0 ) + cos( ring ) * 0.7;

    float paint = smoothstep(
      -2.0,
      0.0,
      (
        cyclicNoise( vec3( 9, 1, 1 ) * rp, -cellHash.xyz, 1.0 )
        + 0.2 * ring
        - length( N - nMap( rp, vec2( 0, 5E-2 ) ) )
      )
    );

    // float F = mix( 0.04, 1.0, pow( 1.0 - dot( -rd, N ), 5.0 ) );
    // if ( random() < F / mix( 1.0 / PI, 1.0, F ) ) {
    if ( random() < 0.12 + 2.0 * smoothstep( 0.7, -0.8, dot( -rd, N ) ) ) { // what the fuck
      // weight should be F
      rd = reflect(
        rd,
        importanceSampleGGX( (
          0.9
          - paint * ( 0.4 + 0.3 * cyclicNoise( 8.0 * rp, cellHash.xyz, 1.0 ) )
          - 0.1 * ring
        ), N )
      );
    } else {
      // weight should be (1.0 - F) / PI (albedo * (1.0 - F) / PI)
      colRem *= mix(
        vec3( 0.3, 0.2, 0.1 ) + 0.01 * ring,
        // vec3( 0.0 ),
        pow(
          0.5 - 0.3 * cos( cellPos.x + cellPos.y + cellHash.w + vec3( 0, 1.5, 2.5 ) ),
          vec3( 2.0 - 0.2 * ring )
        ),
        paint
      );
      // colRem *= 0.0;
      rd = importanceSampleGGX( 2.0, N );
    }
  }
}
