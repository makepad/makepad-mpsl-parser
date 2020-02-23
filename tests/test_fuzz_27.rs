use glsl_parser::*;

static SOURCE: &str = &r#"
void mainImage( out vec4 fragColor, in vec2 fragCoord )
{
    vec2 q = fragCoord / iResolution.xy;
    const float focus = 2.35;
    vec4 acc = vec4(0.0);
    const int N = 12;
 for( int j=-N; j<=N; j++ )
    for( int i=-N; i<=N; i++ )
    {
        vec2 off = vec2(float(i),float(j));
        vec4 tmp = texture( iChannel0, q + off/vec2(800.0,450.0) );
        float depth = tmp.w;
        vec3 color = tmp.xyz;
        float coc = 0.05 + 12.0*abs(depth-focus)/depth;
        if( dot(off,off) < (coc*coc) )
        {
            float w = 1.0/(coc*coc);
            acc += vec4(color*w,w);
        }
    }
    vec3 col = acc.xyz / acc.w;
    col = pow( col, vec3(0.4545) );
    col = col*1.1 - 0.06;
    col *= 0.8 + 0.3*sqrt( 16.0*q.x*q.y*(1.0-q.x)*(1.0-q.y) );
    fragColor = vec4(col,1.0);
}
"#;

#[test]
fn test_fuzz_27() {
    let chars = SOURCE.chars().collect::<Vec<_>>();
    let mut lexer = Lexer::new(&chars);
    let tokens = lexer.read_tokens().unwrap();
    let mut parser = Parser::new(&tokens);
    parser.parse().unwrap();
}
