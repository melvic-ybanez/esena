package com.melvic.esena.lights

import com.melvic.esena.canvas.Color
import com.melvic.esena.patterns.Pattern
import com.melvic.esena.reflections.Refraction

final case class Material(
    color: Color = Color.White,
    ambient: Double = 0.1,
    diffuse: Double = 0.9,
    specular: Double = 0.9,
    shininess: Double = 200.0,
    pattern: Option[Pattern] = None,
    reflective: Double = 0.0,
    transparency: Double = 0.0, // opaque by default
    refractiveIndex: Double = Refraction.index.Default
) {
  def isReflective: Boolean = reflective > 0

  def isTransparent: Boolean = transparency > 0
}
