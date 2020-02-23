use glsl_parser::*;

static SOURCE: &str = &r#"
float hash12(vec2 p)
{
 vec3 p3 = fract(vec3(p.xyx) * .1031);
    p3 += dot(p3, p3.yzx + 19.19);
    return fract((p3.x + p3.y) * p3.z);
}
vec2 hash22(vec2 p)
{
 vec3 p3 = fract(vec3(p.xyx) * vec3(.1031, .1030, .0973));
    p3 += dot(p3, p3.yzx+19.19);
    return fract((p3.xx+p3.yz)*p3.zy);
}
void mainImage( out vec4 fragColor, in vec2 fragCoord )
{
    float resolution = 10. * exp2(-3.*iMouse.x/iResolution.x);
 vec2 uv = fragCoord.xy / iResolution.y * resolution;
    vec2 p0 = floor(uv);
    vec2 circles = vec2(0.);
    for (int j = -2; j <= 2; ++j)
    {
        for (int i = -2; i <= 2; ++i)
        {
   vec2 pi = p0 + vec2(i, j);
            vec2 hsh = pi;
            vec2 p = pi + hash22(hsh);
            float t = fract(0.3*iTime + hash12(hsh));
            vec2 v = p - uv;
            float d = length(v) - (float(2) + 1.)*t;
            float h = 1e-3;
            float d1 = d - h;
            float d2 = d + h;
            float p1 = sin(31.*d1) * smoothstep(-0.6, -0.3, d1) * smoothstep(0., -0.3, d1);
            float p2 = sin(31.*d2) * smoothstep(-0.6, -0.3, d2) * smoothstep(0., -0.3, d2);
            circles += 0.5 * normalize(v) * ((p2 - p1) / (2. * h) * (1. - t) * (1. - t));
        }
    }
    circles /= float((2*2+1)*(2*2+1));
    float intensity = mix(0.01, 0.15, smoothstep(0.1, 0.6, abs(fract(0.05*iTime + 0.5)*2.-1.)));
    vec3 n = vec3(circles, sqrt(1. - dot(circles, circles)));
    vec3 color = texture(iChannel0, uv/resolution - intensity*n.xy).rgb + 5.*pow(clamp(dot(n, normalize(vec3(1., 0.7, 0.5))), 0., 1.), 6.);
 fragColor = vec4(color, 1.0);
}
"#;

#[test]
fn test_fuzz_17() {
    let chars = SOURCE.chars().collect::<Vec<_>>();
    let mut lexer = Lexer::new(&chars);
    let tokens = lexer.read_tokens().unwrap();
    let mut parser = Parser::new(&tokens);
    parser.parse().unwrap();
}
