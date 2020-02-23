use glsl_parser::*;

static SOURCE: &str = &r#"
float distanceToMandelbrot( in vec2 c )
{
    {
        float c2 = dot(c, c);
        if( 256.0*c2*c2 - 96.0*c2 + 32.0*c.x - 3.0 < 0.0 ) return 0.0;
        if( 16.0*(c2+2.0*c.x+1.0) - 1.0 < 0.0 ) return 0.0;
    }
    float di = 1.0;
    vec2 z = vec2(0.0);
    float m2 = 0.0;
    vec2 dz = vec2(0.0);
    for( int i=0; i<300; i++ )
    {
        if( m2>1024.0 ) { di=0.0; break; }
        dz = 2.0*vec2(z.x*dz.x-z.y*dz.y, z.x*dz.y + z.y*dz.x) + vec2(1.0,0.0);
        z = vec2( z.x*z.x - z.y*z.y, 2.0*z.x*z.y ) + c;
        m2 = dot(z,z);
    }
 float d = 0.5*sqrt(dot(z,z)/dot(dz,dz))*log(dot(z,z));
    if( di>0.5 ) d=0.0;
    return d;
}
void mainImage( out vec4 fragColor, in vec2 fragCoord )
{
    vec2 p = (2.0*fragCoord-iResolution.xy)/iResolution.y;
 float tz = 0.5 - 0.5*cos(0.225*iTime);
    float zoo = pow( 0.5, 13.0*tz );
 vec2 c = vec2(-0.05,.6805) + p*zoo;
    float d = distanceToMandelbrot(c);
 d = clamp( pow(4.0*d/zoo,0.2), 0.0, 1.0 );
    vec3 col = vec3(d);
    fragColor = vec4( col, 1.0 );
}
"#;

#[test]
fn test_fuzz_32() {
    let chars = SOURCE.chars().collect::<Vec<_>>();
    let mut lexer = Lexer::new(&chars);
    let tokens = lexer.read_tokens().unwrap();
    let mut parser = Parser::new(&tokens);
    parser.parse().unwrap();
}
