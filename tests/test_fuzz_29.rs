use glsl_parser::*;

static SOURCE: &str = &r#"
float hash1( float n ) { return fract(43758.5453123*sin(n)); }
float hash1( vec2 n ) { return fract(43758.5453123*sin(dot(n,vec2(1.0,113.0)))); }
vec2 hash2( float n ) { return fract(43758.5453123*sin(vec2(n,n+1.0))); }
float gAnimTime;
float map( vec2 p )
{
 float f = textureLod( iChannel0, p/iChannelResolution[0].xy, 0.0 ).x;
 f *= sqrt( textureLod( iChannel2, (0.03*p + 2.0*gAnimTime)/256.0, 0.0 ).x );
 return 22.0*f;
}
vec3 calcNormal( in vec3 pos, in float ic )
{
 return mix( normalize(vec3(pos.x,0.0,pos.z)), vec3(0.0,1.0,0.0), ic );
}
vec4 castRay( in vec3 ro, in vec3 rd )
{
 vec2 pos = floor(ro.xz);
 vec2 ri = 1.0/rd.xz;
 vec2 rs = sign(rd.xz);
 vec2 ris = ri*rs;
 vec2 dis = (pos-ro.xz+ 0.5 + rs*0.5) * ri;
 vec4 res = vec4( -1.0, 0.0, 0.0, 0.0 );
 vec2 mm = vec2(0.0);
 for( int i=0; i<200; i++ )
 {
  float ma = map(pos);
  vec3 ce = vec3( pos.x+0.5, 0.0, pos.y+0.5 );
  vec3 rc = ro - ce;
  float a = dot( rd.xz, rd.xz );
  float b = dot( rc.xz, rd.xz );
  float c = dot( rc.xz, rc.xz ) - 0.249;
  float h = b*b - a*c;
  if( h>=0.0 )
  {
   float s = (-b - sqrt( h ))/a;
   if( s>0.0 && (ro.y+s*rd.y)<ma )
   {
    res = vec4( s, 0.0, pos );
       break;
   }
   s = (ma - ro.y)/rd.y;
   if( s>0.0 && (s*s*a+2.0*s*b+c)<0.0 )
   {
    res = vec4( s, 1.0, pos );
    break;
   }
  }
  mm = step( dis.xy, dis.yx );
  dis += mm*ris;
        pos += mm*rs;
 }
 return res;
}
float castShadowRay( in vec3 ro, in vec3 rd )
{
 vec2 pos = floor(ro.xz);
 vec2 ri = 1.0/rd.xz;
 vec2 rs = sign(rd.xz);
 vec2 ris = ri*rs;
 vec2 dis = (pos-ro.xz+ 0.5 + rs*0.5) * ri;
 float t = -1.0;
 float res = 1.0;
 vec2 mm = step( dis.xy, dis.yx );
 dis += mm * ris;
    pos += mm * rs;
 for( int i=0; i<16; i++ )
 {
  float ma = map(pos);
  vec3 ce = vec3( pos.x+0.5, 0.0, pos.y+0.5 );
  vec3 rc = ro - ce;
  float a = dot( rd.xz, rd.xz );
  float b = dot( rc.xz, rd.xz );
  float c = dot( rc.xz, rc.xz ) - 0.249;
  float h = b*b - a*c;
  if( h>=0.0 )
  {
   float t = (-b - sqrt( h ))/a;
   if( (ro.y+t*rd.y)<ma )
   {
    res = 0.0;
       break;
   }
  }
  mm = step( dis.xy, dis.yx );
  dis += mm * ris;
        pos += mm * rs;
 }
 return res;
}
vec3 cameraPath( float t )
{
    vec2 p = 200.0*sin( 0.01*t*vec2(1.2,1.0) + vec2(0.1,0.9) );
      p += 100.0*sin( 0.02*t*vec2(1.1,1.3) + vec2(1.0,4.5) );
 float y = 15.0 + 4.0*sin(0.05*t);
 float h;
 h = map( p+vec2(-1.0, 0.0) );
 h += map( p+vec2( 1.0, 0.0) );
 h += map( p+vec2( 0.0, 1.0) );
 h += map( p+vec2( 0.0,-1.0) );
 h /= 4.0;
 h += 5.0;
 y = max( y, h );
 return vec3( p.x, y, p.y );
}
vec4 texcyl( sampler2D sam, in vec3 p, in vec3 n )
{
 vec4 x = texture( sam, vec2(p.y,0.5+0.5*atan(n.x,n.z)/3.14) );
 vec4 y = texture( sam, p.xz );
 return mix( x, y, abs(n.y) );
}
vec3 desat( in vec3 col, float a )
{
    return mix( col, vec3(dot(col,vec3(0.333))), a );
}
vec3 lig = normalize(vec3(-0.7,0.25,0.6));
vec3 render( in vec3 ro, in vec3 rd )
{
  float sun = clamp( dot( rd, lig ), 0.0, 1.0 );
  vec3 bgcol = vec3(0.9,0.9,0.8) + 0.3*pow(sun,4.0)*vec3(1.0,1.0,0.0);
        vec3 col = bgcol;
  vec4 res = castRay( ro, rd );
        vec2 vos = res.zw;
        float t = res.x;
        if( t>0.0 )
        {
            vec3 pos = ro + rd*t;
   float id = hash1( vos );
   vec3 nor = calcNormal( fract(pos)-0.5, res.y );
   float h = map(vos);
   vec3 mate1 = 0.5 + 0.45*sin( 3.14*id + 0.8 + vec3(0.0,0.5,1.0) );
   vec3 mate2 = 0.5 + 0.45*sin( 6.28*id + vec3(0.0,0.5,1.0) );
   vec3 mate = mix( mate1, mate2, smoothstep( 9.0, 11.0, h) );
   vec3 uvw = pos - vec3(0.0,h,0.0);
   vec3 tex = texcyl( iChannel3, 0.2*uvw + 13.1*hash1(id), nor ).xyz;
   mate *= 0.2+4.0*pow( desat(tex,0.3), vec3(2.0) );
            mate *= 1.5*sqrt(texture( iChannel0, pos.xz/iChannelResolution[0].xy ).xyz);
            mate *= 0.02 + 0.98*smoothstep( 0.1,0.11, hash1(id) );
            vec3 bn = -1.0 + 2.0*texcyl( iChannel1, 0.2*uvw*vec3(1.0,0.2,1.0) + 13.1*hash1(id), nor ).xyz;
   float occ = nor.y*0.75;
   occ += 0.5*clamp( nor.x,0.0,1.0)*smoothstep( -0.5, 0.5, pos.y-map(vos+vec2( 1.0, 0.0)) );
   occ += 0.5*clamp(-nor.x,0.0,1.0)*smoothstep( -0.5, 0.5, pos.y-map(vos+vec2(-1.0, 0.0)) );
   occ += 0.5*clamp( nor.z,0.0,1.0)*smoothstep( -0.5, 0.5, pos.y-map(vos+vec2( 0.0, 1.0)) );
   occ += 0.5*clamp(-nor.z,0.0,1.0)*smoothstep( -0.5, 0.5, pos.y-map(vos+vec2( 0.0,-1.0)) );
   occ = 0.2 + 0.8*occ;
   occ *= pow( clamp((0.1+pos.y)/(0.1+map(floor(pos.xz))),0.0,1.0),2.0);
   occ = occ*0.5+0.5*occ*occ;
   float rim = pow( clamp( 1.0+dot(rd,nor), 0.0, 1.0 ),5.0 );
   float amb = 1.0;
   float bac = clamp( dot( nor, normalize(vec3(-lig.x,0.0,-lig.z)) ), 0.0, 1.0 ) * clamp(1.0-pos.y/20.0,0.0,1.0);;
   float sha = 0.0;
   float dif = dot( nor, lig );
   if( dif<0.0 ) dif=0.0; else sha = castShadowRay( pos, lig );
            float spe = pow( clamp( dot(lig,reflect(rd,nor)), 0.0, 1.0 ),3.0 );
   vec3 lin = 3.00*vec3(1.0,1.0,1.0)*0.7*sqrt(dif)*sha;
        lin += 0.40*vec3(0.4,1.0,1.7)*amb*occ;
        lin += 0.60*vec3(0.8,0.5,0.3)*bac*occ;
   col = mate * lin + tex.x*1.5*vec3(1.0,1.0,1.0)*(0.3+0.7*rim)*spe*dif*sha;
   col *= 1.1 + 0.5*dot( rd, lig );
            float ff = 1.0 - smoothstep( 0.0, 1.0, pow(t/160.0,1.8) );
   col = mix( col, bgcol, 1.0-ff );
  }
  col += 0.2*pow(sun,8.0)*vec3(1.0,0.7,0.2);
    return col;
}
void mainImage( out vec4 fragColor, in vec2 fragCoord )
{
 vec2 q = fragCoord.xy / iResolution.xy;
    vec2 mo = iMouse.xy / iResolution.xy;
    if( iMouse.w<=0.00001 ) mo=vec2(0.0);
    gAnimTime = iTime;
 vec3 tot = vec3(0.0);
 int a = 0;
 {
        vec2 p = -1.0 + 2.0*(fragCoord.xy) / iResolution.xy;
        p.x *= iResolution.x/ iResolution.y;
        float time = 4.0*iTime + 50.0*mo.x;
        vec3 ro = cameraPath( time );
        vec3 ta = cameraPath( time+5.0 ); ta.y = ro.y - 5.5;
        float cr = 0.2*cos(0.1*time*0.5);
        vec3 ww = normalize( ta - ro);
        vec3 uu = normalize(cross( vec3(sin(cr),cos(cr),0.0), ww ));
        vec3 vv = normalize(cross(ww,uu));
        float r2 = p.x*p.x*0.32 + p.y*p.y;
        p *= (7.0-sqrt(37.5-11.5*r2))/(r2+1.0);
        vec3 rd = normalize( p.x*uu + p.y*vv + 2.5*ww );
        vec3 col = render( ro, rd );
  tot += col;
 }
 tot /= float(1);
 tot = pow( clamp(tot,0.0,1.0), vec3(0.45) );
 tot *= 0.5 + 0.5*pow( 16.0*q.x*q.y*(1.0-q.x)*(1.0-q.y), 0.1 );
 fragColor = vec4( tot, 1.0 );
}
void mainVR( out vec4 fragColor, in vec2 fragCoord, in vec3 fragRayOri, in vec3 fragRayDir )
{
    vec3 cam = cameraPath( iTime );
    gAnimTime = iTime*0.2;
    vec3 ro = fragRayOri + cam + vec3(0.0,-2.0,0.0);
    vec3 rd = fragRayDir*vec3(-1.0,1.0,-1.0);
    vec3 col = render( ro, rd );
 col = pow( clamp(col,0.0,1.0), vec3(0.44) );
    fragColor = vec4( col, 1.0 );
}
"#;

#[test]
fn test_fuzz_29() {
    let chars = SOURCE.chars().collect::<Vec<_>>();
    let mut lexer = Lexer::new(&chars);
    let tokens = lexer.read_tokens().unwrap();
    let mut parser = Parser::new(&tokens);
    parser.parse().unwrap();
}
