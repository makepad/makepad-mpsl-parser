use glsl_parser::*;

static SOURCE: &str = &r#"
vec2 hash2( vec2 p )
{
 return textureLod( iChannel0, (p+0.5)/256.0, 0.0 ).xy;
}
vec3 voronoi( in vec2 x )
{
    vec2 n = floor(x);
    vec2 f = fract(x);
 vec2 mg, mr;
    float md = 8.0;
    for( int j=-1; j<=1; j++ )
    for( int i=-1; i<=1; i++ )
    {
        vec2 g = vec2(float(i),float(j));
  vec2 o = hash2( n + g );
        o = 0.5 + 0.5*sin( iTime + 6.2831*o );
        vec2 r = g + o - f;
        float d = dot(r,r);
        if( d<md )
        {
            md = d;
            mr = r;
            mg = g;
        }
    }
    md = 8.0;
    for( int j=-2; j<=2; j++ )
    for( int i=-2; i<=2; i++ )
    {
        vec2 g = mg + vec2(float(i),float(j));
  vec2 o = hash2( n + g );
        o = 0.5 + 0.5*sin( iTime + 6.2831*o );
        vec2 r = g + o - f;
        if( dot(mr-r,mr-r)>0.00001 )
        md = min( md, dot( 0.5*(mr+r), normalize(r-mr) ) );
    }
    return vec3( md, mr );
}
void mainImage( out vec4 fragColor, in vec2 fragCoord )
{
    vec2 p = fragCoord/iResolution.xx;
    vec3 c = voronoi( 8.0*p );
    vec3 col = c.x*(0.5 + 0.5*sin(64.0*c.x))*vec3(1.0);
    col = mix( vec3(1.0,0.6,0.0), col, smoothstep( 0.04, 0.07, c.x ) );
 float dd = length( c.yz );
 col = mix( vec3(1.0,0.6,0.1), col, smoothstep( 0.0, 0.12, dd) );
 col += vec3(1.0,0.6,0.1)*(1.0-smoothstep( 0.0, 0.04, dd));
 fragColor = vec4(col,1.0);
}
"#;

#[test]
fn test_fuzz_14() {
    let chars = SOURCE.chars().collect::<Vec<_>>();
    let mut lexer = Lexer::new(&chars);
    let tokens = lexer.read_tokens().unwrap();
    let mut parser = Parser::new(&tokens);
    parser.parse().unwrap();
}
