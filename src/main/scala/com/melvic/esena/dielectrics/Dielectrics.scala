package com.melvic.esena.dielectrics

import com.melvic.esena.canvas.Color
import com.melvic.esena.rays.{Computations, Ray}
import com.melvic.esena.scene.World

trait Dielectrics {
  val DefaultDepth = 5

  def reflectedColor(world: World, comps: Computations, depth: Int = DefaultDepth): Color =
    if (depth == 0 || comps.obj.material.reflective == 0) Color.Black
    else {
      val reflectRay = Ray(comps.overPoint, comps.reflectVec)
      val color = world.colorAt(reflectRay, depth - 1)
      color * comps.obj.material.reflective
    }

  def refractedColor(world: World, comps: Computations, depth: Int): Color =
    if (depth == 0 || comps.obj.material.transparency == 0)
      Color.Black
    else {
      // According to Snell's Law, sin(theta_i) / sin(theta_t) = n2 / n1.
      // In our case, theta_i is the angle of the incoming ray and theta_t
      // is the angle of the refracted ray.
      val nRatio = comps.n1 / comps.n2
      val cosI   = comps.eyeVec.dot(comps.normalVec)

      // Pythagorean identity: sin^2 (t) + cos^2 (t) = 1
      val sin2T = nRatio * nRatio * (1.0 - cosI * cosI)

      val isTotalInternalReflection = sin2T > 1
      if (isTotalInternalReflection) Color.Black
      else {
        // again, from Pythagorean identities
        val cosT = math.sqrt(1.0 - sin2T)
        // direction of the refracted ray
        val direction = comps.normalVec * (nRatio * cosI - cosT) - comps.eyeVec * nRatio

        val refractedRay = Ray(comps.underPoint, direction)
        world.colorAt(refractedRay, depth - 1) * comps.obj.material.transparency
      }
    }
}
