package com.melvic.esena

import com.melvic.esena.matrix.view
import com.melvic.esena.scene.Camera
import com.melvic.esena.tuples.{Point, Vec}

package object demos {
  val DefaultCamera = Camera(1000, 600, math.Pi / 3).transform(view(Point(0, 1.5, -5), Point(0, 1, 0), Vec(0, 1, 0)))
}
