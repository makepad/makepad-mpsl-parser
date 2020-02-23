use glsl_parser::*;

static SOURCE: &str = &r#"
float seed = 0.;
float rand() { return fract(sin(seed++)*43758.5453123); }
struct Ray { vec3 o, d; };
struct Sphere {
 float r;
 vec3 p, e, c;
 int refl;
};
Sphere lightSourceVolume = Sphere(20., vec3(50., 81.6, 81.6), vec3(12.), vec3(0.), 0);
Sphere spheres[9];
void initSpheres() {
 spheres[0] = Sphere(1e5, vec3(-1e5+1., 40.8, 81.6), vec3(0.), vec3(.75, .25, .25), 0);
 spheres[1] = Sphere(1e5, vec3( 1e5+99., 40.8, 81.6),vec3(0.), vec3(.25, .25, .75), 0);
 spheres[2] = Sphere(1e5, vec3(50., 40.8, -1e5), vec3(0.), vec3(.75), 0);
 spheres[3] = Sphere(1e5, vec3(50., 40.8, 1e5+170.),vec3(0.), vec3(0.), 0);
 spheres[4] = Sphere(1e5, vec3(50., -1e5, 81.6), vec3(0.), vec3(.75), 0);
 spheres[5] = Sphere(1e5, vec3(50., 1e5+81.6, 81.6),vec3(0.), vec3(.75), 0);
 spheres[6] = Sphere(16.5, vec3(27., 16.5, 47.), vec3(0.), vec3(1.), 1);
 spheres[7] = Sphere(16.5, vec3(73., 16.5, 78.), vec3(0.), vec3(.7, 1., .9), 2);
 spheres[8] = Sphere(600., vec3(50., 681.33, 81.6), vec3(12.), vec3(0.), 0);
}
float intersect(Sphere s, Ray r) {
 vec3 op = s.p - r.o;
 float t, epsilon = 1e-3, b = dot(op, r.d), det = b * b - dot(op, op) + s.r * s.r;
 if (det < 0.) return 0.; else det = sqrt(det);
 return (t = b - det) > epsilon ? t : ((t = b + det) > epsilon ? t : 0.);
}
int intersect(Ray r, out float t, out Sphere s, int avoid) {
 int id = -1;
 t = 1e5;
 s = spheres[0];
 for (int i = 0; i < 9; ++i) {
  Sphere S = spheres[i];
  float d = intersect(S, r);
  if (i!=avoid && d!=0. && d<t) { t = d; id = i; s=S; }
 }
 return id;
}
vec3 jitter(vec3 d, float phi, float sina, float cosa) {
 vec3 w = normalize(d), u = normalize(cross(w.yzx, w)), v = cross(w, u);
 return (u*cos(phi) + v*sin(phi)) * sina + w * cosa;
}
vec3 radiance(Ray r) {
 vec3 acc = vec3(0.);
 vec3 mask = vec3(1.);
 int id = -1;
 for (int depth = 0; depth < 4; ++depth) {
  float t;
  Sphere obj;
  if ((id = intersect(r, t, obj, id)) < 0) break;
  vec3 x = t * r.d + r.o;
  vec3 n = normalize(x - obj.p), nl = n * sign(-dot(n, r.d));
  if (obj.refl == 0) {
   float r2 = rand();
   vec3 d = jitter(nl, 2.*3.14159265359*rand(), sqrt(r2), sqrt(1. - r2));
   vec3 e = vec3(0.);
   {
    Sphere s = lightSourceVolume;
    int i = 8;
    vec3 l0 = s.p - x;
    float cos_a_max = sqrt(1. - clamp(s.r * s.r / dot(l0, l0), 0., 1.));
    float cosa = mix(cos_a_max, 1., rand());
    vec3 l = jitter(l0, 2.*3.14159265359*rand(), sqrt(1. - cosa*cosa), cosa);
    if (intersect(Ray(x, l), t, s, id) == i) {
     float omega = 2. * 3.14159265359 * (1. - cos_a_max);
     e += (s.e * clamp(dot(l, n),0.,1.) * omega) / 3.14159265359;
    }
   }
   float E = 1.;
   acc += mask * obj.e * E + mask * obj.c * e;
   mask *= obj.c;
   r = Ray(x, d);
  } else if (obj.refl == 1) {
   acc += mask * obj.e;
   mask *= obj.c;
   r = Ray(x, reflect(r.d, n));
  } else {
   float a=dot(n,r.d), ddn=abs(a);
   float nc=1., nt=1.5, nnt=mix(nc/nt, nt/nc, float(a>0.));
   float cos2t=1.-nnt*nnt*(1.-ddn*ddn);
   r = Ray(x, reflect(r.d, n));
   if (cos2t>0.) {
    vec3 tdir = normalize(r.d*nnt + sign(a)*n*(ddn*nnt+sqrt(cos2t)));
    float R0=(nt-nc)*(nt-nc)/((nt+nc)*(nt+nc)),
     c = 1.-mix(ddn,dot(tdir, n),float(a>0.));
    float Re=R0+(1.-R0)*c*c*c*c*c,P=.25+.5*Re,RP=Re/P,TP=(1.-Re)/(1.-P);
    if (rand()<P) { mask *= RP; }
    else { mask *= obj.c*TP; r = Ray(x, tdir); }
   }
  }
 }
 return acc;
}
void mainImage( out vec4 fragColor, in vec2 fragCoord ) {
 initSpheres();
 seed = iTime + iResolution.y * fragCoord.x / iResolution.x + fragCoord.y / iResolution.y;
 vec2 uv = 2. * fragCoord.xy / iResolution.xy - 1.;
 vec3 camPos = vec3((2. * (iMouse.xy==vec2(0.)?.5*iResolution.xy:iMouse.xy) / iResolution.xy - 1.) * vec2(48., 40.) + vec2(50., 40.8), 169.);
 vec3 cz = normalize(vec3(50., 40., 81.6) - camPos);
 vec3 cx = vec3(1., 0., 0.);
 vec3 cy = normalize(cross(cx, cz)); cx = cross(cz, cy);
 vec3 color = vec3(0.);
 for (int i = 0; i < 6; ++i)
    {
  color += radiance(Ray(camPos, normalize(.53135 * (iResolution.x/iResolution.y*uv.x * cx + uv.y * cy) + cz)));
    }
 fragColor = vec4(pow(clamp(color/float(6), 0., 1.), vec3(1./2.2)), 1.);
}
"#;

#[test]
fn test_fuzz_19() {
    let chars = SOURCE.chars().collect::<Vec<_>>();
    let mut lexer = Lexer::new(&chars);
    let tokens = lexer.read_tokens().unwrap();
    let mut parser = Parser::new(&tokens);
    parser.parse().unwrap();
}
