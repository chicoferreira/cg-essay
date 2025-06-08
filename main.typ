#import "@preview/charged-ieee:0.1.3": ieee
#import "@preview/lilaq:0.3.0" as lq


#show: ieee.with(
  title: [Real-time Water Rendering Techniques],
  abstract: [
    Rendering realistic water surfaces in real-time graphics is a challenging task due to the complex nature of water's interaction with light and its inherently dynamic properties. Pure particle-based simulations are computationally prohibitive in real-time scenarios, compelling developers to employ clever approximations. This paper explores various rendering techniques, including Screen Space Reflections (SSR), planar reflections, cubemaps, and ray tracing, each providing a trade-off between realism and performance. Additionally, it examines wave simulation approaches, such as sum-of-sines, Gerstner waves, and FFT-based spectral methods, as well as specialized techniques like flow maps for rivers and a brief mention of buoyancy calculations for interactive realism.
  ],
  authors: (
    (
      name: "Francisco Macedo Ferreira",
      email: "pg55942@alunos.uminho.pt",
    ),
  ),
  index-terms: (
    "Water Rendering",
    "Screen Space Reflections",
    "Planar Reflections",
    "Environment Mapping",
    "Ray Tracing",
    "Wave Simulation",
    "Flow Maps",
    "Buoyancy",
  ),
  bibliography: bibliography("refs.yml"),
)

#show image: block.with(clip: true, radius: 5pt)

= Introduction

Real-time simulation and rendering of water remain some of the most challenging tasks in computer graphics due to water's complex and dynamic physical properties. Achieving a realistic portrayal traditionally involves particle-based simulations, yet this method is impractical for real-time applications due to computational costs. Consequently, developers must rely on creative approximations and strategic optimizations to simulate water convincingly within strict performance constraints. This paper reviews various sophisticated approaches to water rendering, balancing visual fidelity and computational efficiency, crucial for interactive applications such as video games.

#figure(
  image("imgs/thelastofusp2mainmenu.png"),
  caption: [The Last of Us Part II main menu @thelastofusp2],
)

#colbreak()

= Water Reflection and Refraction

Rendering water involves simulating the interaction of light with the water surface. Water surfaces act like natural mirrors, reflecting the environment around them, and they also allow to see beneath the surface with distortion (refraction), especially at glancing angles (Fresnel effect). Achieving believable reflections in real-time graphics is a challenging task, as it requires simulating or faking the interaction of light with the water surface, and the environment around it.

#figure(
  image("imgs/rdr2.jpg"),
  caption: [Screenshot of a river in Red Dead Redemption 2 @rdr2],
)

From this point forward, it is assumed that access to the normals of the water surface is available, allowing their use to simulate the reflection and refraction of light. The method for computing these normals will be covered in @waves.

== Screen Space Reflections (SSR)

Screen Space Reflection is a widely used real-time technique to approximate reflective surfaces, thus it is a good candidate for water rendering. SSR works by reusing information already on screen: after the scene is rendered, a post-processing pass traces rays in the screen-space depth buffer, trying to find a reflected hit for each pixel of a reflective surface. Basically, it marches a ray from the view, bouncing off the water surface, and intersects against the depth texture to find what on-screen geometry it would reflect.

Implementing SSR is a relatively simple task, as seen in Sugu Lee's blog post @ssr.
The SSR pass can be executed using a compute shader as a post-processing pass. Below is the pseudo code #footnote[You can find a Metal shader implementation in the Sugu Lee's blog post @ssr].

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

SSR became popular in games because, as shown in the pseudo code, it integrates well with deferred rendering (using the existing depth and normal buffers) and is easy to implement without requiring significant changes to the overall pipeline. The effect is also quite fast, as it is a post-processing effect, so its cost is independent of the number of objects in the scene and instead depends on the number of pixels that are reflective.

#figure(image("imgs/ssr_buffers.png"), caption: [SSR Buffers used in the compute shader (Sugu Lee's blog post @ssr)])

However, SSR comes with notable *limitations* due to its screen-space nature. It can only reflect the objects that are visible on the screen. This leads to the common artifact of "missing" reflections near the edges of the screen or holes where an occluding object in front causes the reflected object to be absent in the depth buffer.

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

Essentially, one renders the scene upside-down (flipped across the water surface) to a texture, and then during water rendering, samples that texture to get reflection color. Planar reflections produce *very high fidelity* results, since it's an actual render of the scene, all off-screen and occluded objects above the water can appear correctly in the reflection. This technique is especially straightforward for perfectly horizontal water planes (in oceans or lakes) because the mirror camera can be an exact reflection of the main camera's orientation, making the reflection very precise.

#figure(
  image("imgs/halflife2waterplanar.jpg"),
  caption: [Planar reflection #footnote[https://developer.valvesoftware.com/wiki/Water_%28shader%29] in the water level in Half-Life 2 @halflife2],
)


The *downside* is performance cost. Planar reflections render the world twice, one for the main view and another for the mirrored view. If a game has a huge open world, doubling the draw calls and triangle processing for reflections is expensive. Some optimizations are possible, for example, rendering the reflection at a lower resolution or using stricter culling rules. Scalability is also a problem, if there are multiple water patches at different orientations, each would need a different world render.

A hybrid approach seen in some titles is to use planar reflections only for important things like the sky and distant scenery, while still using SSR for fine details. For instance, Unreal Engine's water can use a "scene capture" @unrealenginescenecapture for the sky reflection and combine it with SSR for local reflections, achieving a good balance. Planar reflections yield the best image quality short of true ray tracing, but developers must budget for their significant cost.

== Environment Maps and Probes (Cubemaps)

Another lightweight approach is to use environment maps, also called cubemaps, to approximate the reflection of the environment. Environment maps resisted the test of time and are still a common technique in games.

#figure(
  image("imgs/cs2cubemaps.JPG"),
  caption: [Cubemap reflecting a window in Counter-Strike 2 @counterstrike2],
)

For water, environment maps are often used to reflect the sky and distant environment. Many engines include pre-calculated reflection probes that capture the scene from certain points; these can be used for water as well, although for large water surfaces a single static cubemap for the sky is common. The visual realism of environment maps is limited compared to SSR or planar methods as they only capture the world from one point (often not the player's exact position) and cannot reflect dynamic objects unless the cubemap is frequently updated (which would also impact performance).

#figure(
  image("imgs/cs2awpcubemaps.JPG", width: 100%, height: 10%),
  caption: [Cubemap in the scope of the AWP weapon in Counter-Strike 2 @counterstrike2],
) <cs2awpcubemaps>

This means, for example, characters in Counter-Strike 2 cannot be reflected through the AWP scope (as seen in @cs2awpcubemaps), and the same will happen with water. Either way, a blurry reflection via a cubemap still adds a lot of realism to the game at virtually no cost. The *performance* of environment maps is also very good, as they are a single texture it is just a texture lookup. The main *drawback* is the lack of accuracy and dynamism.

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

In addition to reflections, water rendering must account for *refraction*, the bending of light that allows to see into the water.

Real water is typically transparent, but it is view-dependent: at shallow angles you mainly see reflection, while looking straight down you see through the underwater terrain. This behavior is governed by the *Fresnel effect*, which engines simulate by blending reflection vs. refraction based on the angle, often using Schlick's approximation. According to the Schlick's model, the specular reflection coefficient $R$ can be approximated by the following equation:

$
  R(theta) = R_0 + (1-R_0)(1-cos theta)^5
$

where

$
  R_0 = ((n_1 - n_2) / (n_1 + n_2))^2
$

where $theta$ is half the angle between the incoming and outgoing light direction, $n_1$ being the refractive index of the first medium (air, which can be approximated to 1) and $n_2$ the refractive index of the second medium (water), and $R_0$ is the reflection coefficient for light incoming parallel to the normal (when $theta = 0$) @schlick.

So, in practice, the water shader will take the reflection color (from SSR/planar/etc reflections) and blend it with the refraction color (usually by sampling the scene underwater).

The refracted scene color can be obtained by rendering the world from the camera with only the underwater parts (or by a cheap method: copy the color buffer and offset it by the water normal to mimic bending). For instance, one common approach is: render the scene without water to get the "background" image, then when drawing the water, sample that background texture with UVs perturbed by the water surface normal (scaled by the water depth) @oceansimulationfft. This produces a distortion of the underwater view, approximating true refraction. If done well, you can see the lakebed or objects beneath the surface, distorted by ripples.

#figure(
  image("imgs/refraction.png", width: 100%, height: 16%),
  caption: [Water refraction in Photon Minecraft Shaders @photonshaders],
)

The water also attenuates light, so shaders often fade the refracted color to a deep color (blue/green) with depth, to simulate that light absorption.

Another important light effect is *specular highlights*, the small bright spots that appear on the surface of the water. This can be implemented with a simple approach using the Blinn-Phong model, where the specular highlight is computed as:

$
  S = max(0, arrow(N) dot arrow(H))^p
$

where $arrow(N)$ is the surface normal, $arrow(H)$ is the half vector between the view direction and the light direction, and $p$ is the shininess exponent. This value is then multiplied with the light color to get the final specular color. Artists can modify the shininess exponent to control the size and shape of the specular highlight @blinnphongspecular.

A more advanced method can use a BRDF (physically-based material) to calculate specular highlights, allowing for more physically accurate water surfaces @waterbrdf.

*Caustics* also deserve a special mention. These are the focused light patterns on surfaces caused by the water's surface curvature. Caustics can greatly enhance the realism of water rendering. Simulating real caustics is very costly, requiring tracing a vast number of light rays through the water surface. Instead, games use clever approximations and tricks to simulate them.

A common approach is to simply project an animated texture onto the underwater surfaces that resembles caustic patterns @nvidiacaustics. Although not physically accurate, this can create surprisingly believable results as seen in @caustics.

#figure(
  image("imgs/rdr2caustics.png"),
  caption: [Underwater caustics in Red Dead Redemption 2 @rdr2 using a projected texture],
) <caustics>

However, using projected textures for caustics often results in visible repeating patterns, which can break immersion if not handled carefully.

With modern ray-tracing hardware, caustics can be generated in real time: each frame, the water's animated surface is rendered from the light's perspective into a "caustics map", rays are traced through it to collect hit data on underwater surfaces, and those contributions accumulate over frames (using temporal blending) into a dynamic buffer that is sampled during the final render, producing accurate, evolving caustic illumination @raytracingcaustics.

#figure(
  image("imgs/undersea-caustics.png"),
  caption: [Caustics from NVIDIA's raytracing technique @raytracingcaustics],
)

= Waves <waves>

With the water shader techniques covered, the discussion can now move on to water wave simulation techniques.

A central task in water rendering is simulating the *waves* that define the surface shape. Water waves can be modeled in various ways depending on the desired realism and performance.

== Sum of Sines Waves

Realistic water waves are complex, but a simple model that approximates them is to use a sum of sines. This approach leverages the fact that many individual waves (a simple oscillatory function) can be added together to form the overall shape of the water surface.

The simplest approach to create waves is using sine or cosine functions. In a basic implementation, these functions can approximate a water surface, because the water height at any point oscillates up and down following a sinusoidal pattern. For example, a single wave can be described through time $t$ by a height function:

$
  W_i (x, z, t) = A_i sin(D_i dot (x, z) times omega_i + t times phi_i)
$ <sumofsines>

having as parameters:
- Wavelength ($L$): the distance between two consecutive peaks or troughs, with $omega = 2/L$;
- Amplitude ($A$): the maximum height of the wave;
- Speed ($S$): the distance the wave travels in one second, with $phi = S times 2/L$;
- Direction ($D$): the direction the wave travels, with $D = (D_x, D_z)$ and $A dot B$ being the dot product between $A$ and $B$.

All the waves can then be summed to get the final height function:

$
  H(x, z, t) = sum_(i=1)^N W_i (x, z, t)
$

These parameters can be adjusted by the artist to create the desired wave shape. They can also be interpolated over time to create a more dynamic water surface.

This approach done in the GPU (in the vertex shader, for example) is extremely fast (just sine calculations per vertex) and was common in older games or simple water shaders. Even today, some games still use this approach for small bodies of water or sometimes as a fallback to lower quality settings.

Another consideration is computing the surface _normals_ for the lighting. A naive approach is to sample two points on the water surface and compute the normal by the cross product. However, with access to the height function, the normal can be computed by using the partial derivatives with respect to the $x$ and $z$ axes in the height function and performing a cross product with them.

As each point in the water surface can be calculated as:

$
  P(x, z, t) = (x, H(x, z, t), z)
$

For the $x$-axis (the binormal), consider:

$
  B(x, z, t) &= ((partial x) / (partial x), (partial) / (partial x) H(x, z, t), (partial z) / (partial x))\
  &= (1, (partial) / (partial x) H(x, z, t), 0)
$

Similarly, for the $z$-axis (the tangent):

$
  T(x, z, t) &= ((partial x) / (partial z), (partial) / (partial z) H(x, z, t), (partial z) / (partial z))\
  &= (0, (partial) / (partial z) H(x, z, t), 1)
$

The normal is then computed by the cross product of the binormal and the tangent:

$
  N(x, z, t) = B(x, z, t) times T(x, z, t)
$

This equation expansion can be found in the NVIDIA GPU Gems -- Chapter 1 @nvidiawaves. The normal is then normalized to have a length of 1 and used in the lighting calculations.

One problem that can arise from the formula above is that, as more sines are summed to increase detail, the wave starts to look more like a "spiky" surface instead of a wave.

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

Another additional refinement involves replacing $sin(x)$ with $e^(sin(x) - 1)$ in the wave function. This transformation produces waves with sharper, more pronounced crests and broader, flatter troughs, resulting in a more visually striking and natural appearance. Adjusting the sharpness and width of these features is straightforward: simply scale a constant in the exponent to control the effect.

#lq.diagram(
  width: 8cm,
  height: 5cm,
  xlim: (0, 20),
  ylim: (-2, 2),
  xaxis: none,
  yaxis: none,

  let x = lq.linspace(0, 20, num: 70),

  lq.plot(x, x.map(x => calc.sin(x)), label: $sin(x)$),
  lq.plot(x, x.map(x => 1.6 * calc.exp(1.2 * calc.sin(x) - 1) - 1), label: $e^(sin(x) - 1)$),
)

The calculation of the surface normal is slightly modified when using this function, but it remains feasible. For a detailed solution of the updated normal computation, refer to the video @howgamesfakewater or the implementation @sumofsineswatershadertoy.

The result can be found in a Shadertoy #link("https://www.shadertoy.com/view/MdXyzX", underline[view]) @sumofsineswatershadertoy which produces a really convincing water surface.

#figure(
  image("imgs/shadertoysumofsines.png"),
  caption: [The result of the sum of sines water shader toy @sumofsineswatershadertoy],
)

== Gerstner Waves

While the sum of sines approach provides a simple and efficient way to approximate water surfaces, it only allows for vertical ($y$-axis) displacement of the vertices. This limitation means that the resulting waves move points up and down, but do not capture the characteristic forward and backward (horizontal) motion of real water waves. As a result, the surface can look artificial, especially when viewed at glancing angles.

Gerstner waves, first described by Franz Josef Gerstner in 1802, offer a more physically accurate model for water waves by introducing horizontal displacement in addition to the vertical movement. In the Gerstner wave model, each point on the water surface is displaced not only in the $y$-direction (height), but also in the $x$ and $z$ directions, following the same underlying sine function. This creates the effect of water particles moving in circular or trochoidal paths, which closely resembles the motion of real ocean waves. @gerstnerwaves

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

If a point $x_0$ is fixed and a time component is added, the motion of a water particle under a Gerstner wave can be visualized as:

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

Just as multiple sine waves can be summed, Gerstner waves can also be generalized to three dimensions by combining all their components to compute the final vertex position, as described in @nvidiawaves:

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

Although Gerstner waves can produce more realistic wave shapes, they are less commonly used than the sum of sine waves. This is mainly because the sum of sines is simpler to implement and control (and using other functions can make the waves crests more pronounced). Additionally, FFT-based wave techniques offer even higher visual quality, making them a popular choice in modern applications.

Another challenge with Gerstner waves is that their parameters must be carefully tuned to avoid unnatural artifacts. Specifically, if the sum $Q_i times omega_i times A_i$ exceeds 1, the $y$ component of the surface normal can become negative at the wave crests @nvidiawaves. This causes the wave to fold over itself, resulting in visually broken or looping artifacts.

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
)

Therefore, artists must be especially careful to not generate parameters that will cause the wave to break.

== Spectral FFT-Based Waves

One limitation of the previous wave techniques is that they inherently produce repeating patterns. When the simulated water surface is large enough, these periodic patterns become noticeable and can break the illusion of realism.

#figure(
  image("imgs/sinrepeatingpattern.png", width: 100%, height: 17%),
  caption: [The repeating pattern of a sum of sines wave in @sumofsineswatershadertoy when viewed from afar],
)

There are a few ways to mitigate this: you can reduce the visible area of water (for example, by making the water body smaller or using fog to obscure distant regions), or you can try to hide the repetition by adding more sine waves, increasing the complexity and detail of the surface so that the periodicity is less apparent. Of course, doing the latter would be very expensive.

For expansive ocean surfaces, a common approach is to use spectral wave synthesis via the *Fast Fourier Transform (FFT)*, specifically, its inverse transform. This approach was popularized in Jerry Tessendorf's paper "Simulating Ocean Water" @tessendorfffts, and it has since been adopted in numerous films and games. The core idea is to treat the ocean surface as a superposition of thousands of waves with different frequencies, amplitudes, and directions, according to an oceanographic wave spectrum. Instead of manually summing many sine waves, an inverse FFT allows to get the result of summing them without having to compute each wave, which makes the process much faster.

The technique involves generating a wave spectrum using a statistical model and then computing the inverse FFT of the wave spectrum to get the height of the waves for each point in the water surface.

The wave spectrum describes how the amplitude and phase of waves are distributed across all possible directions and frequencies. In other words, it tells how much energy is present in waves of different sizes and directions. There are several physical models for generating a wave spectrum.

// , but Tessendorf's original paper @tessendorfffts uses the Phillips spectrum, which is defined as follows:

// Given a wave with a specific direction and frequency, we can represent it using a wave vector:

// $
//   k = omega_"direction" times omega_"frequency"
// $

// For each wave vector $k$, the Phillips spectrum gives us a way to calculate the amplitude of that wave:

// $
//   P(k) = A times (exp(-1/(k L)^2)) / (k^4) times |hat(k) dot hat(w)|^2
// $

// where $hat(k) = k / (|k|)$ is the normalized wave vector, and $hat(w)=omega / (|omega|)$ is the normalized wind direction. $A$ is a constant that controls the overall amplitude, $L$ is a length scale related to the largest waves generated by the wind, and $omega$ is the wind direction, with its magnitude representing the wind speed.

This paper will not delve into the specifics of generating the wave spectrum, the various models available for its creation or computing the wave normals. However, there are many excellent resources that cover these topics in depth, such as @oceansimulationfft, @tessendorfffts, @itriedsimulatingtheentireocean and @jumptrajectoryfftvideo.

The wave spectrum can be stored in a texture, allowing multiple clients to generate an identical wave surface—this approach is used effectively in games like Sea of Thieves @seaofthieves. Once the wave spectrum is available, the inverse FFT can be used to obtain the height of the waves for each point on the water surface. The result is a heightmap of the water surface, capturing a wide range of wave sizes: from small ripples to large swells, all in one unified approach.

Modern GPUs are capable of computing the inverse FFT for grids of size $256 times 256$ or $512 times 512$ well within the time constraints of a single frame. This is typically done in a compute shader, with the resulting data stored in a texture.

For large ocean surfaces, this grid can be tiled to cover a greater area. However, doing this tiling can reintroduce the repeating patterns that the technique is meant to avoid. To overcome this limitation, multiple FFT cascades at different length scales can be combined—much like summing many sine waves together @itriedsimulatingtheentireocean. Since modern GPUs can efficiently compute several inverse FFTs per frame, layering these cascades creates an ocean surface with the complexity and detail of millions of summed sine waves, mitigating the repeating patterns.

By blending the cascades in different ways, artists can simulate a wide range of ocean conditions, from calm waters to stormy seas.

#figure(
  image("imgs/acerolafft.png"),
  caption: [The result of the inverse FFT of the wave spectrum in @itriedsimulatingtheentireocean],
)

= Rendering Rivers (Flow Maps)

Flowing rivers present a different challenge from open oceans -- the water is not just waving in place but moving persistently along a path. A straightforward way to animate flow is to scroll the water's texture or normal map uniformly along a path. However, real rivers twist and turn, so a single uniform scroll looks obviously fake when the river bends or splits.

*Flow maps* were introduced to address this, allowing spatially-varying flow direction and speed across a water surface @waterusingflowmaps. A flow map is a 2D vector field texture mapped over the river; each pixel stores a flow direction (as a 2D vector) and often a magnitude (vector length) that represents the local water velocity.

#figure(
  image("imgs/flowmap.png", height: 20%),
  caption: [A flow map texture from @waterusingflowmaps],
)

The water shader uses this vector field to *warp* its *UV Coordinates* over time, essentially creating a *flow* effect. This produces a convincingly realistic movement, water can accelerate down rapids, slow in pools and even curl around obstacles, all driven by a texture authored by an artist rather than a complex fluid simulation.

To implement flow mapping, the engine samples the flow map to find the direction for each water pixel, then offsets the lookup into the water's normal map (and other textures) accordingly. As time advances, the UVs slide along the flow vector, making the water appear to flow in that direction.

One practical challenge is that continuously offsetting the UVs will eventually stretch the texture too far (which will cause visible repeats or blurring). To overcome this, developers run the animation in repeating cycles: after the offset reaches a certain limit, it resets back, and by using two layers of the animated texture out-of-phase, the shader can smoothly blend between a resetting layer and the next one without a visible jump.

#figure(
  image("imgs/flowmapsblendgraph.png", width: 80%),
  caption: [The blending of two flow maps (from @waterusingflowmaps) to avoid visible jumps],
)

Valve employs this technique in their games, for instance, to animate the toxic pools in Portal 2 @portal2flowmaps.

#figure(
  image("imgs/portal2vectorfield.png"),
  caption: [The vector field used in Portal 2 from @portal2flowmaps],
)

This technique is also used in Red Dead Redemption 2 @rdr2, where rivers show calm, glassy water in pools and fast, detailed currents around rocks and rapids. The flow maps add subtle eddies and variations, making the water look highly realistic and dynamic.

#figure(
  image("imgs/rdr2flowmaps.png"),
  caption: [Flow maps being used in Red Dead Redemption 2 @rdr2 rivers],
)

= Buoyancy

Beyond visual fidelity, water in games must interact believably with objects through buoyancy. This refers to the upward force exerted by the water that makes objects float, as described by Archimedes' principle: the buoyant force equals the weight of the displaced fluid, which is formulated as:

$
  F_"buoyant" = rho_"water" times g times V_"object underwater"
$

where $rho_"water"$ is the density of the water (in real life it varies depending on temperature and salinity, but for simplicity it can be assumed as a constant of $1000 (k g)/m^3$), $g$ is the acceleration due to gravity, and $V_"object underwater"$ is the volume of the displaced fluid @buoyancyfordummies.

Implementing buoyancy involves estimating the $V$ part of the equation, which involves determining the submerged volume of the object. A common technique is to subdivide the object into sample points or volumes (e.g. voxels) and test each one against the water level. Each submerged portion applies an upward force equal to the weight of the water it displaces, and the forces are summed to get the net buoyant force on the object. This method is both efficient and stable, and has been used for gameplay elements like floating crates and props in Half-Life 2 @halflife2 that when placed underwater, it raises a heavy ramp which solves a gameplay puzzle.

More advanced buoyancy models are used for larger or irregular shapes like ships @waterinteractionmodelforboats.

Ensuring consistency between the rendered waves (often generated on the GPU) and the physics simulation (on the CPU) is crucial; otherwise an object might visibly float above or below the water @buoyancyfordummies. Techniques to handle this include duplicating the wave calculation on the CPU, or using asynchronous readback of the GPU height field.

Developers also dampen buoyant motion with drag forces to prevent objects from jittering or bouncing too violently on rough water. The end result, when tuned well, is a natural bobbing and rocking motion.

= Conclusion

This paper has discussed various techniques employed for real-time water rendering, including reflections, refractions, wave simulations, river flow using flow maps, and a brief mention of physical interactions via buoyancy calculations.

While these methods significantly enhance visual realism, the scope of this paper does not exhaustively cover all aspects of realistic water simulation. Missing components that further improve realism but remain topics for future exploration include:

- Underwater god rays, fog and volumetric lighting;
- Dynamic water level-of-detail adjustments;
- Water interactions with dynamic objects;
- Techniques for preventing water rendering inside buoyant objects;
- Simulation and rendering of water foam, especially in breaking waves and shorelines;
- Generation and rendering of water spray particles.

Each of these areas offers further avenues for enhancing visual realism and computational efficiency in real-time water rendering.

#pagebreak()

// #[
//   #set text(fill: red)
//   - underwater god rays
//   - underwater fog
//   - underwater volumetric lighting
//   - water level of detail
//   - water interaction
//   - water displacement to not render inside objects
//   - water foam (in breaking waves and in shoreline)
//   - water spray
// ]
