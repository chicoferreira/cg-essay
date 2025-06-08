#import "@preview/charged-ieee:0.1.3": ieee

#show: ieee.with(
  title: [State of the Art in Water Rendering],
  abstract: [
    #text(fill: red)[TODO]
  ],
  authors: (
    (
      name: "Francisco Macedo Ferreira",
      email: "pg55942@alunos.uminho.pt",
    ),
  ),
  index-terms: ("Water Rendering", "Waves", "Flow Maps", "Reflection", "Refraction", "Ray Tracing", "Buoyancy"),
  bibliography: bibliography("refs.yml"),
)

#show image: block.with(clip: true, radius: 5pt)

#text(fill: red)[TODO: remove "we" from the text]

= Introduction

= Water Reflection and Refraction

Rendering water involves simulating the interaction of light with the water surface. Water surfaces act like natural mirrors, reflecting the environment around them, and they also allow to see beneath the surface with distortion (refraction), especially at glancing angles (Fresnel effect). Achieving believable reflections in real time graphics is a challenging task, as it requires simulating or faking the interaction of light with the water surface, and the environment around it.

#figure(
  image("imgs/rdr2.jpg"),
  caption: [Screenshot of a river in Red Dead Redemption 2 @rdr2],
)

From now on we will assume that we have access to the normals of the water surface, so we can use them to simulate the reflection and refraction of light. How to compute them will be covered in @waves.

== Screen Space Reflections (SSR)

Screen Space Reflection is a widely used real-time technique to approximate reflective surfaces, thus it is a good candidate for water rendering. SSR works by reusing information already on screen: after the scene is rendered, a post-processing pass traces rays in the screen-space buffer depth buffer, trying to find a reflected hit for each pixel of a reflective surface. Basically, it marches a ray from the view, bouncing off the water surface, and intersects against the depth texture to find what on-screen geometry it would reflect.

Implementing SSR is a relatively simple task, as seen in Sugu Lee's blog post @ssr.
The SSR pass can be executed using a compute shader. Below is the pseudo code #footnote[You can find a Metal shader implementation in the Sugu Lee's blog post @ssr].

#import "@preview/lovelace:0.3.0": *

#let line_numbering_skipable(skip: 0) = i => if i < skip { return none } else { return i - skip + 1 }

#figure(
  kind: "algorithm",
  supplement: [Algorithm],
  pseudocode-list(
    numbered-title: smallcaps[SSR Compute Shader Pseudocode],
    booktabs: true,
    line-numbering: line_numbering_skipable(skip: 10),
  )[
    + *inputs*:
      + `color_buffer`: The color of each pixel.
      + `depth_buffer`: The depth (in clip space) of each pixel.
      + `reflection_mask`: A mask indicating which pixels are reflective (=1) or not (=0).
      + `normal_buffer`: The normal of each pixel.
      + `camera`: The camera view size, view transform matrix, projection matrix and inverse projection matrix.
    + *outputs*:
      + `out_color`: The final color of each pixel.
    + *algorithm*:
      + Fetch normal from the normal buffer and the reflective mask from the reflection mask buffer.
      + *if* the reflection mask is not 0 *then*
        + Compute the position, the reflection vector and the max distance for the current sample in texture space.
        + *if* the reflection vector is moving away from the camera *then*
          + Find the intersection between the reflection ray and the scene geometry by tracing the ray.
          + *if* the intersection is found *then*
            + Compute the reflection color by fetching the color from the color buffer at the intersection point.
          + *else*
            + Set the reflection to the background color.
          + *end*
        + *end*
      + *end*
      + Add the reflection color to the current sample to create the final color.
  ],
)

SSR became popular in games, because as we can see in the pseudo code, it integrates well with deffered rendering (using the existing depth and normal buffers) and it is easy to implement without requiring a lot of changes to the whole pipeline. The effect is also quite fast, as it is a post-processing effect, so its cost is independent of the number of objects in the scene, but rather depends on the number of pixels that are reflective.

#figure(image("imgs/ssr_buffers.png"), caption: [SSR Buffers used in the compute shader (Sugu Lee's blog post @ssr)])

However, SSR come with notable *limitations* due to its screen-space nature. It can only reflect the objects that are visible on the screen. This leads to the common artifact of "missing" reflections near the edges of the screen or holes where an occluding object in front causes the reflected obect to be absent in the depth buffer.

#figure(
  image("imgs/minecraftssr.png"),
  caption: [SSR implementation in Minecraft Shader Mod Rethinking Voxels @rethinkingvoxels],
)

#figure(
  image("imgs/minecraftssrbroken.png"),
  caption: [Missing terrain reflections from the example above],
)

Developers mitigate this by faking blur at the screen edges, fading out SSR, or blending with other reflection methods. Additionally, objects behind transparent objects (which won't appear in the depth buffer) may not be reflected properly. This can be worked around by rendering the transparent objects in a separate pass, after the SSR pass.

Despite these issues, SSR is still a good candidate for water rendering, it remains one of the best compromises between *visual fidelity* and *performance* for planar-ish reflectors like water. Almost every game uses it for water reflections in combination with cubemaps to cover its weaknesses.

== Planar Reflections

Although not used as much as SSR in water, being used more in mirrors, planar reflections are still a good choice for water rendering.
This technique involves rendering a second view of the scene from the perspective of a "mirror" positioned below the water plane.

#figure(
  image("imgs/gtaplanarreflections.JPG"),
  caption: [Planar reflection in a mirror in Grand Theft Auto V @gtav],
)

Essentially, one rendes the scene upside-down (flipped across the water surface) to a texture, and then during water rendering, samples that texture to get reflection color. Planar reflections produce *very high fidelity* results, since it's an actual render of the scene, all off-screen and occluded objects above the water can appear correctly in the reflection. This technique is especially straightforward for perfectly horizontal water planes (in oceans or lakes) because the mirror camera can be an expact reflection of the main camera's orientation, making the reflection very precise.

#figure(
  image("imgs/halflife2waterplanar.jpg"),
  caption: [Planar reflection #footnote[https://developer.valvesoftware.com/wiki/Water_%28shader%29] in the water level in Half-Life 2 @halflife2],
)


The *downside* is performance cost. Planar reflections render the world twice, one for the main view and another for the mirrored view. If a game has a huge open world, doubling the draw calls and triangle processing for reflections is expensive. Some optimizations are possible, for example, rendering the reflection at a lower resolution or using stricter culling rules. Scalability is also a problem, if there are multiple water patches at different orientations, each would need a different world render.

A hybrid approach seen in some titles is to use planar reflections only for important things like the sky and distant scenery, while still using SSR for fine details. For instance, Unreal Engine's water can use a "scene capture" @unrealenginescenecapture for the sky reflection and combine it with SSR for local reflections, achieving a good balance. Planar reflections yield the best image quality short of true ray tracing, but developers must budget for their significant cost.

== Environment Maps and Probes (Cubemaps)

Another light-weight approach is to use environment maps, also called cubemaps, to approximate the reflection of the environment. Environment maps resisted the test of time and are still a common technique in games.

#figure(
  image("imgs/cs2cubemaps.JPG"),
  caption: [Cubemap reflecting a window in Counter-Strike 2 @counterstrike2],
)

For water, environment maps are often used to reflect the sky and distant environment. Many engines include reflection probes that capture the scene from certain points; these can be used for water as well, although for large water surfaces a single static cubemap for the sky is common. The visual realism of environment maps is limited compared to SSR or planar methods as they only capture the world from one point (often not the player's exact position) and cannot reflect dynamic objects unless the cubemap is frequently updated (which would also impact performance).

#figure(
  image("imgs/cs2awpcubemaps.JPG"),
  caption: [Cubemap in the scope of the AWP weapon in Counter-Strike 2 @counterstrike2],
)

This means, for example, characters in Counter-Strike 2 cannot be reflected through the AWP scope (as seen in the image above), and the same will happen with water. Either way, a blurry reflection via a cubemap still adds a lot of realism to the game at virtually no cost. The *performance* of environment maps is also very good, as they are a single texture it's just a texture lookup. The main *draw back* is the lack of accuracy and dynamism.

== Ray Tracing

The advent of hardware-accelerated ray tracing (via APIs like DXR and hardware like NVIDIA's RTX or AMD's RDNA) has significantly advanced the realism of real-time water rendering. Unlike screen-space or planar techniques, ray tracing simulates the physical behavior of light, enabling reflections that are pixel-perfect, perspective-correct, and capable of including off-screen and dynamic elements.

For each pixel on a water surface, a ray-traced reflection computes the direction a reflected light ray would travel based on the surface normal—which itself may be animated or displaced by waves, ripples, or foam. The renderer traces this ray into the scene, testing for intersections with geometry or skydomes. If the ray hits an object, that object's shading result is used as the reflected color; if it misses, the renderer samples the environment map or skybox. This process is repeated per-pixel, producing highly accurate, temporally stable reflections that respect scene geometry and lighting.

Because it directly simulates light transport, ray tracing is conceptually clean and highly flexible. In addition to reflections, the same technique can be extended to handle refraction, caustics, and even light scattering effects, all within the same unified framework. The visual fidelity is generally unmatched: ray tracing eliminates the view-dependent artifacts of screen-space reflections and the geometric constraints of planar methods. Dynamic characters, off-screen geometry, and complex surfaces are all faithfully reflected, even across curved or undulating water.

#figure(
  image("imgs/cyberpunkraytracing.png"),
  caption: [Ray tracing in Cyberpunk 2077 @cyberpunk from Digital Foundry's video @cyberpunkdigitalfoundry],
)

The main limitation is performance. Tracing rays through a complex scene is computationally expensive, even with dedicated hardware acceleration. Real-time implementations typically reduce ray count, cap bounce depth, or trace at lower resolutions and then apply temporal and spatial denoising to produce a clean image. Many engines also incorporate upscaling technologies like DLSS or FSR to further amortize the cost.

Despite these optimizations, ray-traced reflections remain a premium feature, generally reserved for high-end hardware or enabled as an optional setting for quality presets. Developers must carefully budget for the added GPU cost and decide when to fall back to cheaper methods like SSR.

== Refraction and Lighting (Transparency, Fresnel, Caustics)

In addition to reflections, water rendering includes *refraction*, the part of seeing into the water.

Real water is typically transparent, but it is view-dependant: at shallow angles you mainly see reflection, while looking straight down you see through the underwater terrain. This behavior is governed by the *Fresnel effect*, which engines simulate by blending reflection vs. refraction based on the angle, often using Schlick's approximation. According to the Schlick's model, the specular reflection coefficient $R$ can be approximated by the following equation:

$
  R(theta) = R_0 + (1-R_0)(1-cos theta)^5
$

where

$
  R_0 = ((n_1 - n_2) / (n_1 + n_2))^2
$

where $theta$ is half the angle between the incoming and outgoing light direction, $n_1$ being the refractive index of the first medium (air, which can be approximated to 1) and $n_2$ the refractive index of the second medium (water), and $R_0$ is the reflection coefficient for light incoming parallel to the normal (when $theta = 0$) @schlick.

So, in practice, the water shader will take the reflection color (from SSR/planar/etc reflections) and blend it with the refraction color (usually by sampling the scene underwater).

The refracted scene color can be obtained by rendering the world from the camera with only the underwater parts (or by a cheap method: copy the color buffer and offset it by the water normal to mimic bending). For instance, one common approach is: render the scene without water to get the "background" image, then when drawing the water, sample that background texture with UVs perturbated by the water surface normal (scaled by the water depth) @oceansimulationfft. This produces a distorcion of the underwater view, approximating true refraction. If done well, you can see the lakebed or objects beneath the surface, distorced by ripples.

#figure(
  image("imgs/refraction.png"),
  caption: [Refraction in Photon Minecraft Shaders @photonshaders],
)

The water also attenuates light, so shaders often fade the refracted color to a deep color (blue/green) with depth, to simulate that light absorption.

Another important light effect is *specular highlights*, the small bright spots that appear on the surface of the water. This can be implemented with a simple approach using the Blinn-Phong model, where the specular highlight is computed as:

$
  S = max(0, arrow(N) dot arrow(H))^p
$

where $arrow(N)$ is the surface normal, $arrow(H)$ is the half vector between the view direction and the light direction, and $p$ is the shininess exponent. This value is then multiplied with the light color to get the final specular color. Artists can modify the shininess exponent to control the size and shape of the specular highlight @blinnphongspecular.

A more advanced method can use a BRDF (physically-based material) to calculate specular highlights, allowing for more physically accurate water surfaces @waterbrdf.

*Caustics* also deserve a special mention. These are the focused light pattern os surfaces caused by the water's surface curvature. Caustics can greatly enhance the realism of water rendering. Simulating real caustics is very costly, requiring tracing a giant amount of light rays through the water surface. Instead, games use clever approximations and tricks to simulate them.

A common approach is to simply project an animated texture onto the underwater surfaces that resembles caustic patterns @nvidiacaustics. Although not physically accurate, this can create surprisingly believable results.

#figure(
  image("imgs/caustics.png"),
  caption: [Underwater caustics in Minecraft Rethinking Voxels Shader @rethinkingvoxels using a projected texture],
)

However, using projected textures for caustics often results in visible repeating patterns, which can break immersion if not handled carefully.

With modern ray-tracing hardware, caustics can be generated in real time: each frame, the water's animated surface is rendered from the light's perspective into a "caustics map", rays are traced through it to collect hit data on underwater surfaces, and those contributions accumulate over frames (using temporal blending) into a dynamic buffer that is sampled during the final render, producing accurate, evolving caustic illumination @raytracingcaustics.

#figure(
  image("imgs/undersea-caustics.png"),
  caption: [Caustics from NVIDIA's raytracing technique @raytracingcaustics],
)

= Waves <waves>

#text(
  fill: red,
)[Now that we have covered the water shader techniques, we can move on to the water wave simulation techniques.]
A central task in water rendering is simulating the *waves* that define the surface shape. Water waves can be modeled in various ways depending on the desired realism and performance.

https://developer.nvidia.com/gpugems/gpugems/part-i-natural-effects/chapter-1-effective-water-simulation-physical-models

https://people.computing.clemson.edu/~jtessen/reports/papers_files/coursenotes2004.pdf


== Sum of Sines Waves

Realistic water waves are complex, but a simple model that approximates them is to use a sum of sines. This approach levarages the fact that many individual waves (a simple oscillatory function) can be added together to form the overall shape of the water surface.

The simplest approach to create waves is using sine or cosine functions. In a basic implementation, we can use them to approximate a water surface, because the water height at any point oscillates up and down following a sinusoidal pattern. For example, a single wave can be described through time $t$ by a height function:

$
  W_i (x, z, t) = A_i sin(D_i dot (x, z) times omega_i + t times phi_i)
$ <sumofsines>

having as parameters:
- Wavelength ($L$): the distance between two consecutive peaks or troughs, with $omega = 2/L$;
- Amplitude ($A$): the maximum height of the wave;
- Speed ($S$): the distance the wave travels in one second, with $phi = S times 2/L$;
- Direction ($D$): the direction the wave travels, with $D = (D_x, D_z)$ and $A dot B$ being the dot product between $A$ and $B$.

We can then sum all the waves to get the final height function:

$
  H(x, z, t) = sum_(i=1)^N W_i (x, z, t)
$

These parameters can be adjusted by the artist to create the desired wave shape. They can also be interpolated over time to create a more dynamic water surface.

This approach done in the GPU (in the vertex shader, for example) is extremely fast (just a sine calculations per vertex) and was common in older games or simple water shaders. Even today, some games still use this approach for small bodies of water or sometimes as a fallback to lower quality settings.

Another consideration is computing the surface _normals_ for the lightning. A naive approach would be to sample two point in the water surface and compute the normal by the cross product. But in this case, we have access to the height function, so we can compute the normal by using partial derivatives of each $x$ and $z$ axis in the height function and do a cross product with them.

As each point in the water surface can be calculated as:

$
  P(x, z, t) = (x, H(x, z, t), z)
$

For the $x$-axis (the binormal) we have:

$
  B(x, z, t) &= ((partial x) / (partial x), (partial) / (partial x) H(x, z, t), (partial z) / (partial x))\
  &= (1, (partial) / (partial x) H(x, z, t), 0)
$

Similarly, for the $z$-axis (the tangent) we have:

$
  T(x, z, t) &= ((partial x) / (partial z), (partial) / (partial z) H(x, z, t), (partial z) / (partial z))\
  &= (0, (partial) / (partial z) H(x, z, t), 1)
$

The normal is then computed by the cross product of the binormal and the tangent:

$
  N(x, z, t) = B(x, z, t) times T(x, z, t)
$

This equation expansion can be found in the NVIDIA GPU Gems -- Chapter 1 @nvidiawaves. The normal is then normalized to have a length of 1 and used in the lightning calculations.

One problem that can arrive from looking at the formula above, if we keep summing more sines to increase detail, the wave will start to look more like a "spiky" surface instead of a wave.

#figure(
  image("imgs/sumofwavesspiky.png"),
  caption: [The result of summing of a lot of sine waves from "How Games Fake Water" video @howgamesfakewater],
)

To address this issue, the author @howgamesfakewater suggests using Fractal Brownian Motion techniques @fractionalbrownianmotionanddomainwarping, as the formula effectively creates a type of white noise by summing sines with random parameters.

The process begins with amplitude and frequency values set to 1 and a random direction. With each iteration, the amplitude is multiplied by a value less than 1, and the frequency by a value greater than 1, using these updated values to compute the next wave. This approach causes the amplitude to decrease exponentially toward zero, allowing the summation to stop once the amplitude becomes negligible.

The method starts with a large wave and progressively adds smaller, higher-frequency waves with reduced amplitude. The higher frequencies contribute fine detail to the water surface, but their lower amplitude ensures they have less influence on the overall shape, thus avoiding the "spiky" appearance.

An additional enhancement involves applying domain warping @fractionalbrownianmotionanddomainwarping to the formula. During the summation of waves, the sampling position can be shifted by the derivative of the previous wave, creating the effect of waves interacting and pushing against each other.

$
  H(x, z, t) = sum_(i=1)^N W_i (x + (partial) / (partial x) W_(i-1), z + (partial) / (partial z) W_(i-1), t)
$

The result can be found in a Shadertoy #link("https://www.shadertoy.com/view/MdXyzX", underline[view]) @sumofsineswatershadertoy which produces a really convincing water surface.

#figure(
  image("imgs/shadertoysumofsines.png"),
  caption: [The result of the sum of sines water shader toy @sumofsineswatershadertoy],
)

== Gerstner Waves

While the sum of sines approach provides a simple and efficient way to approximate water surfaces, it only allows for vertical ($y$-axis) displacement of the vertices. This limitation means that the resulting waves move points up and down, but do not capture the characteristic forward and backward (horizontal) motion of real water waves. As a result, the surface can look artificial, especially when viewed at glancing angles.

Gerstner waves, first described by Franz Josef Gerstner in 1802, offer a more physically accurate model for water waves by introducing horizontal displacement in addition to the vertical movement. In the Gerstner wave model, each point on the water surface is displaced not only in the $y$-direction (height), but also in the $x$ and $z$ directions, following the same underlying sine function. This creates the effect of water particles moving in circular or trochoidal paths, which closely resembles the motion of real ocean waves. @gerstnerwaves

#import "@preview/lilaq:0.3.0" as lq

#lq.diagram(
  width: 8cm,
  height: 5cm,
  xlim: (-4, 16),
  ylim: (-5, 5),
  xaxis: none,
  yaxis: none,

  let x = lq.linspace(-7, 18, num: 70),
  let fx = x => x + calc.cos(x),
  let gx = x => calc.sin(x),

  lq.plot(x, x.map(fx), label: $f(x) = x + cos(x)$),
  lq.plot(x, x.map(gx), label: $g(x) = sin(x)$),
  lq.plot(x.map(fx), x.map(gx), label: $(f(x), g(x))$),
)

If we fix a point $x_0$ and add a time component, the motion of a water particle under a Gerstner wave can be visualized as:

$
  P(t) = (x_0 + cos(x_0 + t), sin(x_0 + t))
$

#align(
  center,
  lq.diagram(
    width: 8cm,
    height: 3cm,
    xlim: (-5, 5),
    ylim: (-2, 2),
    xaxis: none,
    yaxis: none,
    let x = lq.linspace(-6, 5),
    let fx = x => x + calc.cos(x),
    let gx = x => calc.sin(x),

    lq.plot(x.map(fx), x.map(gx), label: $(f(x), g(x))$),

    let t = lq.linspace(-(calc.pi), calc.pi, num: 20),
    let x0 = 0,
    lq.plot(
      t.map(t => x0 + calc.cos(x0 + t)),
      t.map(t => calc.sin(x0 + t)),
      label: $P(t)$,
    ),
  ),
)

This illustrates that as time progresses, the point $x_0$ moves along a circular path in the $x y$-plane, as described by $P(t)$. This circular motion is characteristic of Gerstner waves: it causes the wave crests to become sharper and the troughs to appear more rounded, closely resembling the shape of real ocean waves.

Just as we can sum multiple sine waves, we can also generalize Gerstner waves to three dimensions by combining all their components to compute the final vertex position, as described in @nvidiawaves:

$
  P = vec(
    x + sum (Q_i A_i  D_i_x cos(omega_i D_i dot (x, z) + phi_i t)),
    sum (A_i sin(omega_i D_i dot (x, z) + phi_i t)),
    z + sum (Q_i A_i  D_i_z cos(omega_i D_i dot (x, z) + phi_i t)),
  )
$

Here, $Q_i$ controls the steepness of each wave, while the other terms are as defined in @sumofsines.

Calculating the surface for Gerstner waves is more complex than for simple sine waves, but the resulting expression is still differentiable. Computing the surface normal is crucial for realistic lighting and shading, as it determines how light interacts with the water. The process for finding the normal is similar to that used for a sum of sine waves, but now using the Gerstner formulation. Due to the length of the full formula, it is not reproduced here; however, a derivation and final result can be found in @nvidiawaves.

Gerstner wave displacement creates ocean surfaces with waves that move in a clear direction, conveying the sense of wind or water current. Unlike the sum of sine waves, which can appear as random noise, Gerstner waves produce more natural patterns @gerstnerwavessimulation.

Despite their ability to produce more realistic wave shapes, Gerstner waves are less commonly used than the sum of sine waves. This is largely due to the simplicity of the sum of sines (that with Fractal Brownian Motion can produce an approximate wave peak shape), as well as the superior visual quality offered by FFT-based wave approaches.

Another challenge with Gerstner waves is that their parameters must be carefully tuned to avoid unnatural artifacts. Specifically, if the sum $Q_i times omega_i times A_i$ exceeds 1, the $y$ component of the surface normal can become negative at the wave crests. This causes the wave to fold over itself, resulting in visually broken or looping artifacts.

#lq.diagram(
  width: 8cm,
  height: 3cm,
  xlim: (-5, 5),
  ylim: (-2, 2),
  xaxis: none,
  yaxis: none,
  let x = lq.linspace(-7, 5),
  let qi = 1.4,
  let px = x => x + qi * calc.cos(x),
  let py = x => calc.sin(x),

  lq.plot(x.map(px), x.map(py), label: [$P $ when $Q_i = 1.4$]),
),

So artists need to be specially careful to not generate parameters that will cause the wave to break.

== Spectral FFT-Based Waves

We can only reduce the repeating pattern of the sum waves by summing more sines. Of course, doing this would be very expensive.

For expansive ocean surfaces, a common approach is to use spectral wave syntesis via the *Fast Fourier Transform (FFT)*, concretely the inverse of it. Using an inverse FFT we can effectively simulate a large number of waves with statistically modeled amplitudes and frequencies, producing a realistic ocean heightfield. Modern GPUs can compute an inverse FFT of a reasonably high-resolution heightfield each frame, enabling detailed ocean simulations.

The technique involves generating a wave spectrum using a statistical model and then computing the inverse FFT of the wave spectrum to get the height of the waves for each point in the water surface.

The wave spectrum


= Rendering Rivers (Flow Maps)

= Buoyancy

- https://www.gamedeveloper.com/programming/water-interaction-model-for-boats-in-video-games
- https://nejclesek.blogspot.com/2016/05/thousand-ships-with-real-time-buoyancy.html

= Conclusion

#[
  #set text(fill: red)
  - god rays
  - underwater fog
  - volumetric lighting underwater
  - water level of detail
  - water interaction
  - water displacement to not render inside objects
  - water foam
]
