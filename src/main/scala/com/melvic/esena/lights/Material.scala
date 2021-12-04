package com.melvic.esena.lights

import com.melvic.esena.Real
import com.melvic.esena.canvas.Color
import com.melvic.esena.patterns.Pattern
import com.melvic.esena.dielectrics.Refraction

final case class Material(
    color: Color = Color.White,
    ambient: Real = 0.1,
    diffuse: Real = 0.9,
    specular: Real = 0.9,
    shininess: Real = 200.0,
    pattern: Option[Pattern] = None,
    reflective: Real = 0.0,
    transparency: Real = 0.0, // opaque by default
    refractiveIndex: Real = Refraction.index.Default
) {
  def isReflective: Boolean = reflective > 0

  def isTransparent: Boolean = transparency > 0
}
