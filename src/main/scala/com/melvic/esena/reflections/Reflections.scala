package com.melvic.esena.reflections

import com.melvic.esena.canvas.Color
import com.melvic.esena.rays.{Computations, Ray}
import com.melvic.esena.scene.World

trait Reflections {
  def reflectedColor(world: World, comps: Computations): Color =
    if (comps.obj.material.reflective == 0) Color.Black
    else {
      val reflectRay = Ray(comps.overPoint, comps.reflectVec)
      val color = world.colorAt(reflectRay)
      color * comps.obj.material.reflective
    }
}
