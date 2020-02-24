use glsl_parser::*;

static SOURCE: &str = &r#"
let aTessCoord: attribute vec2;

let alpha: Self::instance_x;

fn computeColor(position: vec2) -> vec3 {
    if position.x < 0.0 {
        if position.y < 0.0 {
            return vec3(0.0, 0.0, 0.0);
        } else {
            return vec3(0.0, 1.0, 0.0);
        }
    } else {
        if position.y < 0.0 {
            return vec3(1.0, 0.0, 0.0);
        } else {
            return vec3(1.0, 1.0, 0.0);
        }
    }
}

fn main() {
    let color = computeColor(aTessCoord);
    let i = 0;
    while i < 3 {
        color = color.yzx;
        ++i;
    }
    gl_FragColor = vec4(alpha * color, 1.0);
}
"#;

#[test]
fn test_extensions() {
    let chars = SOURCE.chars().collect::<Vec<_>>();
    let mut lexer = Lexer::new(&chars);
    let tokens = lexer.read_tokens().unwrap();
    let mut parser = Parser::new(&tokens);
    parser.parse().unwrap();
}
