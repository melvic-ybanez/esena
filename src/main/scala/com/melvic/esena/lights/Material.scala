package com.melvic.esena.lights

import com.melvic.esena.canvas.Color
import com.melvic.esena.lights.Material.{
  DefaultAmbient,
  DefaultColor,
  DefaultDiffuse,
  DefaultShininess,
  DefaultSpecular
}
import com.melvic.esena.patterns.Pattern
import com.melvic.esena.patterns.Pattern.NoPattern

final case class Material(
    color: Color = DefaultColor,
    ambient: Double = DefaultAmbient,
    diffuse: Double = DefaultDiffuse,
    specular: Double = DefaultSpecular,
    shininess: Double = DefaultShininess,
    pattern: Pattern = NoPattern
)

object Material {
  val DefaultColor     = Color.White
  val DefaultAmbient   = 0.1
  val DefaultDiffuse   = 0.9
  val DefaultSpecular  = 0.9
  val DefaultShininess = 200.0
}
