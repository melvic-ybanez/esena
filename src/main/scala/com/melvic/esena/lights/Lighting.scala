package com.melvic.esena.lights

import com.melvic.esena.canvas.Color
import com.melvic.esena.rays.{Computations, Intersections}
import com.melvic.esena.dielectrics
import com.melvic.esena.scene.World
import com.melvic.esena.shapes.Shape
import com.melvic.esena.tuples.{Point, Vec}

trait Lighting {
  def lighting(
      material: Material,
      obj: Shape,
      light: PointLight,
      point: Point, // point being illuminated
      eyeVec: Vec,
      normalVec: Vec,
      inShadow: Boolean = false,
  ): Color = {
    val color = material.pattern.fold(material.color)(_.at(obj, point))

    val effectiveColor = color * light.intensity
    val lightVec       = (light.position - point).normalize
    val ambient        = effectiveColor * material.ambient

    // cosine of the angle between the light and the normal vectors
    val lightDotNormal = lightVec.dot(normalVec)

    lazy val (diffuse, specular) = {
      if (lightDotNormal < 0) {
        // light is on the other side of the surface
        (Color.Black, Color.Black)
      } else {
        val diffuse    = effectiveColor * material.diffuse * lightDotNormal
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

    if (inShadow) ambient.toColor
    else (ambient + diffuse + specular).toColor
  }

  def shadeHit(world: World, comps: Computations, depth: Int = dielectrics.DefaultDepth) =
    world.light.fold(Color.Black) { light =>
      val shadowed = world.isShadowedAt(comps.overPoint)
      val surface = lighting(
        comps.obj.material,
        comps.obj,
        light,
        comps.overPoint,
        comps.eyeVec,
        comps.normalVec,
        shadowed
      )
      val reflected = dielectrics.reflectedColor(world, comps, depth)
      val refracted = dielectrics.refractedColor(world, comps, depth)

      val material = comps.obj.material
      if (material.isReflective && material.isTransparent) {
        val reflectance = Intersections.schlick(comps)
        surface + reflected * reflectance + refracted * (1 - reflectance)
      } else surface + reflected + refracted
    }
}
