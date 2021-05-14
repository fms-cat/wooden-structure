/* framework header */
#version 430
layout(location = 0) uniform vec2 iResolution;
layout(location = 1) uniform int iFrame;

/* vvv your shader goes here vvv */

#define L(i,j) (floor((i)/(j))*(j))
#define F(i) (fract(sin((i)*111.)*111.))

// const float PI = acos( -1. );

float cellSize;
float seed;
vec2 cellPos;
vec4 cellHash;

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
  return mat3( x, cross( z, x ), z );
}

float cyclicNoise( vec3 p, vec3 b, float pump ) {
  mat3 bas = orthBas( b );
  vec2 sum = vec2( 0.0 );

  for ( int i = 0; i < 6; i ++ ) {
    p *= bas * 2.0;
    p += sin( p.yzx );
    sum = pump * sum + vec2( dot( sin( p.zxy ), cos( p ) ), 1.0 );
  }

  return sum.x / sum.y;
}

float mapWater( vec3 p ) {
  return p.z + 0.04 * cyclicNoise( p, vec3( 1 ), 2.0 );
}

vec3 importanceSampleGGX( float roughness, vec3 N ) {
  float phi = 6.2832 * random();
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
      0.4 * cellHash.x
      + 0.2 * ( cellPos.x + cellPos.y ) - 8.2
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

void main() {
  // seed = float( iFrame ) + hash21( gl_FragCoord.xy );
  seed = float( iFrame );

  vec2 p = ( gl_FragCoord.xy + vec2( random(), random() ) - 0.5 * iResolution.xy ) / iResolution.y;

  gl_FragColor = vec4( 0, 0, 0, 1 );

  float medium = 1.0;
  float rl = 2.5;
  vec3 ro = vec3( 0, 0, rl );
  vec3 rd = normalize( vec3( p, -1 ) );
  vec3 rp = ro + rd * rl;

  vec3 colRem = vec3( 1.0 - 0.2 * length( p ) );

  ro.xy += 0.05 * vec2( random(), random() );

  rp *= orthBas( vec3( 2, 2, 5 ) );
  ro *= orthBas( vec3( 2, 2, 5 ) );
  rp.xy *= mat2( 0.6, 0.7, -0.7, 0.6 );
  ro.xy *= mat2( 0.6, 0.7, -0.7, 0.6 );

  rd = normalize( rp - ro );

  for ( int i = 0; i < 5; i ++ ) {
    vec2 rdxy = normalize( rd.xy );
    float dist;

    // water??
    rl = 0.01;
    rp = ro + rd * rl;

    for ( int i = 0; i < 20; i ++ ) {
      // dist = mapWater( rp ) * medium;
      rl += 0.9 * mapWater( rp ) * medium;
      rp = ro + rd * rl;
    }

    // float rlWater = dist < 1E-2 ? rl : 1E2;
    float rlWater = rl;
    // water end

    rl = 4E-3;
    rp = ro + rd * rl;

    for ( int i = 0; i < 50; i ++ ) {
      // quadtree begin
      // https://www.shadertoy.com/view/7d2Szc
      cellSize = 1.0;

      for ( int i = 0; i < 4; i ++ ) {
        cellSize *= 0.5;
        cellPos = L( rp.xy, cellSize ) + 0.5 * cellSize;
        cellHash = F( F( F( cellPos.y ) + cellPos.x ) + vec4( 0, 1, 2, 3 ) );
        if ( cellSize < 0.7 * cellHash.x ) break;
      }
      // quadtree end

      // segment begin
      // vec2 tow = sign( rdxy ) * cellSize * 0.5;
      // vec2 v = ( tow - ( rp.xy - cellPos ) ) / rdxy;
      vec2 v = ( ( sign( rdxy ) * cellSize * 0.5 ) - rp.xy + cellPos ) / rd.xy * length( rd );

      // vec2 nextCell = tow * ( ( v.x < v.y ) ? vec2( 2, 0 ) : vec2( 0, 2 ) ),

      // float seg = min( v.x, v.y );
      // segment end

      float rlNext = rl + min( v.x, v.y ) + 1E-3;

      // march start
      for ( int i = 0; i < 100; i ++ ) {
        dist = map( rp );
        rl += dist;
        rp = ro + rd * rl;

        if ( dist < 1E-3 ) break;
        if ( rlNext < rl ) break;
      }
      // march end

      if ( dist < 1E-2 ) break;

      rl = rlNext;
      rp = ro + rd * rl;
    }

    vec2 d = vec2( 0, 1E-3 );

    if ( dist > 1E-2 ) {
      gl_FragColor = vec4(
        colRem * pow( max( 0.0, rd.z + abs( rd.y ) ), 2.0 ),
        1
      );
      break;
    }

    rl = max( min( rl, rlWater ), 0.0 );
    colRem *= exp( 8.0 * ( medium - 1.0 ) * rl );

    if ( rl == rlWater ) {
      ro = ro + rd * rl;

      vec3 N = normalize( vec3(
        mapWater( ro + d.yxx ),
        mapWater( ro + d.xyx ),
        mapWater( ro + d.xxy )
      ) - mapWater( ro ) );

      // if ( random() < F ) {
      // if ( random() < mix( 0.04, 1.0, pow( 1.0 - dot( -rd, N ), 5.0 ) ) ) {
      if ( random() < 0.04 + pow( 1.0 - dot( -rd, N ), 5.0 ) ) {
        rd = reflect( rd, N );
      } else {
        rd = refract( rd, N, 1.04 - 0.3 * medium );
        medium = -medium;
      }
    } else {
      vec3 N = normalize( vec3(
        map( rp + d.yxx ),
        map( rp + d.xyx ),
        map( rp + d.xxy )
      ) - map( rp ) );

      ro = rp;

      // Ref: https://www.shadertoy.com/view/ldscDM
      float ring = 200.0 * ( dot( rp, 0.5 - cellHash.xyz ) + cyclicNoise( 0.5 * rp, cellHash.xyz, 3.0 ) );
      ring = pow( sin( ring ) * 0.5 + 0.5, 9.0 ) + cos( ring ) * 0.7;

      // float F = mix( 0.04, 1.0, pow( 1.0 - dot( -rd, N ), 5.0 ) );
      // if ( random() < F / mix( 1.0 / PI, 1.0, F ) ) {
      if ( random() < 0.12 + 2.0 * smoothstep( 0.7, -0.8, dot( -rd, N ) ) ) { // what the fuck
        // weight should be F
        rd = reflect(
          rd,
          importanceSampleGGX( (
            0.5
            - 0.3 * cyclicNoise( 8.0 * rp, cellHash.xyz, 1.0 )
            - 0.1 * ring
          ), N )
        );
      } else {
        // weight should be (1.0 - F) / PI (albedo * (1.0 - F) / PI)
        colRem *= pow(
          0.5 + 0.3 * sin( cellPos.x + cellPos.y + cellHash.w + vec3( 0, 1.5, 2.5 ) ),
          vec3( 2.0 - 0.2 * ring )
        );        // colRem *= 0.0;
        rd = importanceSampleGGX( 2.0, N );
      }
    }
  }
}
