package com.melvic.esena.lights

import com.melvic.esena.canvas.Color
import com.melvic.esena.tuples.{Point, Vec}

trait Lighting {
  def lighting(
      material: Material,
      light: PointLight,
      point: Point, // point being illuminated
      eyeVec: Vec,
      normalVec: Vec
  ): Color = {
    val effectiveColor = material.color * light.intensity
    val lightVec = (light.position - point).toVec.normalize
    val ambient = effectiveColor * material.ambient

    // cosine of the angle between the light and the normal vectors
    val lightDotNormal = lightVec.dot(normalVec)

    val (diffuse, specular) = {
      if (lightDotNormal < 0) {
        // light is on the other side of the surface
        (Color.Black, Color.Black)
      } else {
        val diffuse = effectiveColor * material.diffuse * lightDotNormal
        val reflectVec = -lightVec.reflect(normalVec)

        // cosine of the angle between reflection vector and the eye vector
        val reflectDotEye = reflectVec.dot(eyeVec)

        if (reflectDotEye <= 0) {
          // light reflects away from the eye
          (diffuse, Color.Black)
        } else {
          val factor = math.pow(reflectDotEye, material.shininess)
          (diffuse, light.intensity * material.specular * factor)
        }
      }
    }

    (ambient + diffuse + specular).toColor
  }
}
