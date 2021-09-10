package com.melvic.esena.light

import com.melvic.esena.canvas.Color

final case class Material(
    color: Color,
    ambient: Double,
    diffuse: Double,
    specular: Double,
    shininess: Double
)

object Material {
  def apply(): Material =
    Material(Color.White, 0.1, 0.9, 0.9, 200.0)
}
