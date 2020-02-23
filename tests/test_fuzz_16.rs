use glsl_parser::*;

static SOURCE: &str = &r#"
vec3 hash3( vec2 p )
{
    vec3 q = vec3( dot(p,vec2(127.1,311.7)),
       dot(p,vec2(269.5,183.3)),
       dot(p,vec2(419.2,371.9)) );
 return fract(sin(q)*43758.5453);
}
float iqnoise( in vec2 x, float u, float v )
{
    vec2 p = floor(x);
    vec2 f = fract(x);
 float k = 1.0+63.0*pow(1.0-v,6.0);
 float va = 0.0;
 float wt = 0.0;
    for( int j=-2; j<=2; j++ )
    for( int i=-2; i<=2; i++ )
    {
        vec2 g = vec2( float(i),float(j) );
  vec3 o = hash3( p + g )*vec3(u,u,1.0);
  vec2 r = g - f + o.xy;
  float d = dot(r,r);
  float ww = pow( 1.0-smoothstep(0.0,1.414,sqrt(d)), k );
  va += o.z*ww;
  wt += ww;
    }
    return va/wt;
}
void mainImage( out vec4 fragColor, in vec2 fragCoord )
{
 vec2 uv = fragCoord.xy / iResolution.xx;
    vec2 p = 0.5 - 0.5*cos( iTime*vec2(1.0,0.5) );
 if( iMouse.w>0.001 ) p = vec2(0.0,1.0) + vec2(1.0,-1.0)*iMouse.xy/iResolution.xy;
 p = p*p*(3.0-2.0*p);
 p = p*p*(3.0-2.0*p);
 p = p*p*(3.0-2.0*p);
    if( cos(iTime*0.5)>0.5 )
    {
    p = uv * vec2( 1.0, iResolution.x/iResolution.y);
    }
 float f = iqnoise( 24.0*uv, p.x, p.y );
 fragColor = vec4( f, f, f, 1.0 );
}
"#;

#[test]
fn test_fuzz_16() {
    let chars = SOURCE.chars().collect::<Vec<_>>();
    let mut lexer = Lexer::new(&chars);
    let tokens = lexer.read_tokens().unwrap();
    let mut parser = Parser::new(&tokens);
    parser.parse().unwrap();
}
