use glsl_parser::*;

static SOURCE: &str = &r#"
float treeLine = 0.0;
float treeCol = 0.0;
vec3 sunLight = normalize( vec3( 0.4, 0.4, 0.48 ) );
vec3 sunColour = vec3(1.0, .9, .83);
float specular = 0.0;
vec3 cameraPos;
float ambient;
vec2 add = vec2(1.0, 0.0);
const mat2 rotate2D = mat2(1.3623, 1.7531, -1.7131, 1.4623);
float Hash12(vec2 p)
{
 vec3 p3 = fract(vec3(p.xyx) * .1031);
    p3 += dot(p3, p3.yzx + 19.19);
    return fract((p3.x + p3.y) * p3.z);
}
vec2 Hash22(vec2 p)
{
 vec3 p3 = fract(vec3(p.xyx) * vec3(.1031, .1030, .0973));
    p3 += dot(p3, p3.yzx+19.19);
    return fract((p3.xx+p3.yz)*p3.zy);
}
float Noise( in vec2 x )
{
    vec2 p = floor(x);
    vec2 f = fract(x);
    f = f*f*(3.0-2.0*f);
    float res = mix(mix( Hash12(p), Hash12(p + add.xy),f.x),
                    mix( Hash12(p + add.yx), Hash12(p + add.xx),f.x),f.y);
    return res;
}
vec2 Noise2( in vec2 x )
{
    vec2 p = floor(x);
    vec2 f = fract(x);
    f = f*f*(3.0-2.0*f);
    float n = p.x + p.y * 57.0;
   vec2 res = mix(mix( Hash22(p), Hash22(p + add.xy),f.x),
                  mix( Hash22(p + add.yx), Hash22(p + add.xx),f.x),f.y);
    return res;
}
float Trees(vec2 p)
{
    return Noise(p*13.0)*treeLine;
}
float Terrain( in vec2 p)
{
 vec2 pos = p*0.05;
 float w = (Noise(pos*.25)*0.75+.15);
 w = 66.0 * w * w;
 vec2 dxy = vec2(0.0, 0.0);
 float f = .0;
 for (int i = 0; i < 5; i++)
 {
  f += w * Noise(pos);
  w = -w * 0.4;
  pos = rotate2D * pos;
 }
 float ff = Noise(pos*.002);
 f += pow(abs(ff), 5.0)*275.-5.0;
 return f;
}
float Map(in vec3 p)
{
 float h = Terrain(p.xz);
 float ff = Noise(p.xz*.3) + Noise(p.xz*3.3)*.5;
 treeLine = smoothstep(ff, .0+ff*2.0, h) * smoothstep(1.0+ff*3.0, .4+ff, h) ;
 treeCol = Trees(p.xz);
 h += treeCol;
    return p.y - h;
}
float Terrain2( in vec2 p)
{
 vec2 pos = p*0.05;
 float w = (Noise(pos*.25)*0.75+.15);
 w = 66.0 * w * w;
 vec2 dxy = vec2(0.0, 0.0);
 float f = .0;
 for (int i = 0; i < 5; i++)
 {
  f += w * Noise(pos);
  w = - w * 0.4;
  pos = rotate2D * pos;
 }
 float ff = Noise(pos*.002);
 f += pow(abs(ff), 5.0)*275.-5.0;
 treeCol = Trees(p);
 f += treeCol;
 if (treeCol > 0.0) return f;
 for (int i = 0; i < 6; i++)
 {
  f += w * Noise(pos);
  w = - w * 0.4;
  pos = rotate2D * pos;
 }
 return f;
}
float FractalNoise(in vec2 xy)
{
 float w = .7;
 float f = 0.0;
 for (int i = 0; i < 4; i++)
 {
  f += Noise(xy) * w;
  w *= 0.5;
  xy *= 2.7;
 }
 return f;
}
vec3 GetClouds(in vec3 sky, in vec3 rd)
{
 if (rd.y < 0.01) return sky;
 float v = (200.0-cameraPos.y)/rd.y;
 rd.xz *= v;
 rd.xz += cameraPos.xz;
 rd.xz *= .010;
 float f = (FractalNoise(rd.xz) -.55) * 5.0;
 sky = mix(sky, vec3(.55, .55, .52), clamp(f*rd.y-.1, 0.0, 1.0));
 return sky;
}
vec3 GetSky(in vec3 rd)
{
 float sunAmount = max( dot( rd, sunLight), 0.0 );
 float v = pow(1.0-max(rd.y,0.0),5.)*.5;
 vec3 sky = vec3(v*sunColour.x*0.4+0.18, v*sunColour.y*0.4+0.22, v*sunColour.z*0.4+.4);
 sky = sky + sunColour * pow(sunAmount, 6.5)*.32;
 sky = sky+ sunColour * min(pow(sunAmount, 1150.0), .3)*.65;
 return sky;
}
vec3 ApplyFog( in vec3 rgb, in float dis, in vec3 dir)
{
 float fogAmount = exp(-dis* 0.00005);
 return mix(GetSky(dir), rgb, fogAmount );
}
void DoLighting(inout vec3 mat, in vec3 pos, in vec3 normal, in vec3 eyeDir, in float dis)
{
 float h = dot(sunLight,normal);
 float c = max(h, 0.0)+ambient;
 mat = mat * sunColour * c ;
 if (h > 0.0)
 {
  vec3 R = reflect(sunLight, normal);
  float specAmount = pow( max(dot(R, normalize(eyeDir)), 0.0), 3.0)*specular;
  mat = mix(mat, sunColour, specAmount);
 }
}
vec3 TerrainColour(vec3 pos, vec3 normal, float dis)
{
 vec3 mat;
 specular = .0;
 ambient = .1;
 vec3 dir = normalize(pos-cameraPos);
 vec3 matPos = pos * 2.0;
 float disSqrd = dis * dis;
 float f = clamp(Noise(matPos.xz*.05), 0.0,1.0);
 f += Noise(matPos.xz*.1+normal.yz*1.08)*.85;
 f *= .55;
 vec3 m = mix(vec3(.63*f+.2, .7*f+.1, .7*f+.1), vec3(f*.43+.1, f*.3+.2, f*.35+.1), f*.65);
 mat = m*vec3(f*m.x+.36, f*m.y+.30, f*m.z+.28);
 if (normal.y < .5)
 {
  float v = normal.y;
  float c = (.5-normal.y) * 4.0;
  c = clamp(c*c, 0.1, 1.0);
  f = Noise(vec2(matPos.x*.09, matPos.z*.095+matPos.yy*0.15));
  f += Noise(vec2(matPos.x*2.233, matPos.z*2.23))*0.5;
  mat = mix(mat, vec3(.4*f), c);
  specular+=.1;
 }
 if (matPos.y < 45.35 && normal.y > .65)
 {
  m = vec3(Noise(matPos.xz*.023)*.5+.15, Noise(matPos.xz*.03)*.6+.25, 0.0);
  m *= (normal.y- 0.65)*.6;
  mat = mix(mat, m, clamp((normal.y-.65)*1.3 * (45.35-matPos.y)*0.1, 0.0, 1.0));
 }
 if (treeCol > 0.0)
 {
  mat = vec3(.02+Noise(matPos.xz*5.0)*.03, .05, .0);
  normal = normalize(normal+vec3(Noise(matPos.xz*33.0)*1.0-.5, .0, Noise(matPos.xz*33.0)*1.0-.5));
  specular = .0;
 }
 if (matPos.y > 80.0 && normal.y > .42)
 {
  float snow = clamp((matPos.y - 80.0 - Noise(matPos.xz * .1)*28.0) * 0.035, 0.0, 1.0);
  mat = mix(mat, vec3(.7,.7,.8), snow);
  specular += snow;
  ambient+=snow *.3;
 }
 if (matPos.y < 1.45)
 {
  if (normal.y > .4)
  {
   f = Noise(matPos.xz * .084)*1.5;
   f = clamp((1.45-f-matPos.y) * 1.34, 0.0, .67);
   float t = (normal.y-.4);
   t = (t*t);
   mat = mix(mat, vec3(.09+t, .07+t, .03+t), f);
  }
  if (matPos.y < 0.0)
  {
   mat *= .2;
  }
 }
 DoLighting(mat, pos, normal,dir, disSqrd);
 if (matPos.y < 0.0)
 {
  float time = (iTime)*.03;
  vec3 watPos = matPos;
  watPos += -dir * (watPos.y/dir.y);
  float tx = cos(watPos.x*.052) *4.5;
  float tz = sin(watPos.z*.072) *4.5;
  vec2 co = Noise2(vec2(watPos.x*4.7+1.3+tz, watPos.z*4.69+time*35.0-tx));
  co += Noise2(vec2(watPos.z*8.6+time*13.0-tx, watPos.x*8.712+tz))*.4;
  vec3 nor = normalize(vec3(co.x, 20.0, co.y));
  nor = normalize(reflect(dir, nor));
        tx = watPos.y-matPos.y;
  mat = mix(mat, GetClouds(GetSky(nor)*vec3(.3,.3,.5), nor)*.1+vec3(.0,.02,.03), clamp((tx)*.4, .6, 1.));
        mat += vec3(.1)*clamp(1.-pow(tx+.5, 3.)*texture(iChannel1, watPos.xz*.1, -2.).x, 0.,1.0);
  float sunAmount = max( dot(nor, sunLight), 0.0 );
  mat = mat + sunColour * pow(sunAmount, 228.5)*.6;
        vec3 temp = (watPos-cameraPos*2.)*.5;
        disSqrd = dot(temp, temp);
 }
 mat = ApplyFog(mat, disSqrd, dir);
 return mat;
}
float BinarySubdivision(in vec3 rO, in vec3 rD, vec2 t)
{
    float halfwayT;
    for (int i = 0; i < 5; i++)
    {
        halfwayT = dot(t, vec2(.5));
        float d = Map(rO + halfwayT*rD);
         t = mix(vec2(t.x, halfwayT), vec2(halfwayT, t.y), step(0.5, d));
    }
 return halfwayT;
}
bool Scene(in vec3 rO, in vec3 rD, out float resT, in vec2 fragCoord )
{
    float t = 1. + Hash12(fragCoord.xy)*5.;
 float oldT = 0.0;
 float delta = 0.0;
 bool fin = false;
 bool res = false;
 vec2 distances;
 for( int j=0; j< 150; j++ )
 {
  if (fin || t > 240.0) break;
  vec3 p = rO + t*rD;
  float h = Map(p);
  if( h < 0.5)
  {
   fin = true;
   distances = vec2(oldT, t);
   break;
  }
  delta = max(0.01, 0.3*h) + (t*0.0065);
  oldT = t;
  t += delta;
 }
 if (fin) resT = BinarySubdivision(rO, rD, distances);
 return fin;
}
vec3 CameraPath( float t )
{
 float m = 1.0+(iMouse.x/iResolution.x)*300.0;
 t = (iTime*1.5+m+657.0)*.006 + t;
    vec2 p = 476.0*vec2( sin(3.5*t), cos(1.5*t) );
 return vec3(35.0-p.x, 0.6, 4108.0+p.y);
}
vec3 PostEffects(vec3 rgb, vec2 uv)
{
 rgb = (1.0 - exp(-rgb * 6.0)) * 1.0024;
 return rgb;
}
void mainImage( out vec4 fragColor, in vec2 fragCoord )
{
    vec2 xy = -1.0 + 2.0*fragCoord.xy / iResolution.xy;
 vec2 uv = xy * vec2(iResolution.x/iResolution.y,1.0);
 vec3 camTar;
 float h = 0.0;
 float f = 1.0;
 for (int i = 0; i < 7; i++)
 {
  h += Terrain(CameraPath((.6-f)*.008).xz) * f;
  f -= .1;
 }
 cameraPos.xz = CameraPath(0.0).xz;
 camTar.xyz = CameraPath(.005).xyz;
 camTar.y = cameraPos.y = max((h*.25)+3.5, 1.5+sin(iTime*5.)*.5);
 float roll = 0.15*sin(iTime*.2);
 vec3 cw = normalize(camTar-cameraPos);
 vec3 cp = vec3(sin(roll), cos(roll),0.0);
 vec3 cu = normalize(cross(cw,cp));
 vec3 cv = normalize(cross(cu,cw));
 vec3 rd = normalize( uv.x*cu + uv.y*cv + 1.5*cw );
 vec3 col;
 float distance;
 if( !Scene(cameraPos,rd, distance, fragCoord) )
 {
  col = GetSky(rd);
  col = GetClouds(col, rd);
 }
 else
 {
  vec3 pos = cameraPos + distance * rd;
  float p = min(.3, .0005+.00005 * distance*distance);
  vec3 nor = vec3(0.0, Terrain2(pos.xz), 0.0);
  vec3 v2 = nor-vec3(p, Terrain2(pos.xz+vec2(p,0.0)), 0.0);
  vec3 v3 = nor-vec3(0.0, Terrain2(pos.xz+vec2(0.0,-p)), -p);
  nor = cross(v2, v3);
  nor = normalize(nor);
  col = TerrainColour(pos, nor, distance);
 }
 col = PostEffects(col, uv);
 fragColor=vec4(col,1.0);
}
"#;

#[test]
fn test_fuzz_28() {
    let chars = SOURCE.chars().collect::<Vec<_>>();
    let mut lexer = Lexer::new(&chars);
    let tokens = lexer.read_tokens().unwrap();
    let mut parser = Parser::new(&tokens);
    parser.parse().unwrap();
}
