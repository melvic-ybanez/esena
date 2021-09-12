package com.melvic.esena.scene

import com.melvic.esena.matrix.Matrix
import com.melvic.esena.matrix.Matrix.Identity4x4

final case class Camera(
    hSize: Double,
    vSize: Double,
    fieldOfView: Double,
    transformation: Matrix = Identity4x4
)
