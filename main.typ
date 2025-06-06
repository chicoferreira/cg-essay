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

This means, for example, characters in Counter-Strike 2 cannot be reflected through the AWP scope (as seen in the image above). Either way, a blurry reflection via a cubemap still adds a lot of realism to the game at virtually no cost. The *performance* of environment maps is also very good, as they are a single texture it's just a texture lookup. The main *draw back* is the lack of accuracy and dynamism.

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

In addition to reflections, water rendering includes *refraction*, the part of seeing into the water. Real water is typically transparent, but it is view-dependant: at shallow angles you mainly see reflection, while looking straight down you see through the underwater terrain. This behavior is governed by the *Fresnel effect*, which engines simulate by blending reflection vs. refraction based on the angle, often using Schlick's approximation. According to the Schlick's model, the specular reflection coefficient $R$ can be approximated by the following equation:

$
  R(theta) = R_0 + (1-R_0)(1-cos theta)^5
$

where

$
R_0 = ((n_1 - n_2) / (n_1 + n_2))^2
$

where $theta$ is half the angle between the incoming and outgoing light direction, $n_1$ being the refractive index of the first medium (air, which can be approximated to 1) and $n_2$ the refractive index of the second medium (water), and $R_0$ is the reflection coefficient for light incoming parallel to the normal (when $theta = 0$) @schlick.



= Waves <waves>

== Sum of Sines Waves

== Gerstner Waves

== Spectral FFT-Based Waves

= Rendering Rivers (Flow Maps)

= Buoyancy

- https://www.gamedeveloper.com/programming/water-interaction-model-for-boats-in-video-games
- https://nejclesek.blogspot.com/2016/05/thousand-ships-with-real-time-buoyancy.html

= Conclusion
