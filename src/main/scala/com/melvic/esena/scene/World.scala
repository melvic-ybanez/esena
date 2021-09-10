package com.melvic.esena.scene

import com.melvic.esena.lights.PointLight
import com.melvic.esena.shapes.Shape

trait World {
  def lightSource: Option[PointLight]

  def objects: Vector[Shape]
}

object World {
  final case class WorldImpl(lightSource: Option[PointLight], objects: Vector[Shape]) extends World

  def apply(): World =
    WorldImpl(None, Vector())
}
