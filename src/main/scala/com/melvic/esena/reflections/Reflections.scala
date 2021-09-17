package com.melvic.esena.reflections

import com.melvic.esena.canvas.Color
import com.melvic.esena.rays.{Computations, Ray}
import com.melvic.esena.scene.World

trait Reflections {
  val DefaultDepth = 4

  def reflectedColor(world: World, comps: Computations, depth: Int = DefaultDepth): Color =
    if (depth == 0 || comps.obj.material.reflective == 0) Color.Black
    else {
      val reflectRay = Ray(comps.overPoint, comps.reflectVec)
      val color = world.colorAt(reflectRay, depth - 1)
      color * comps.obj.material.reflective
    }
}
