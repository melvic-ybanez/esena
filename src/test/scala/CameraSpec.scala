import com.melvic.esena.Math.{roundTo, roundTo5}
import com.melvic.esena.matrix.Matrix.Identity4x4
import com.melvic.esena.matrix.{rotationY, translation}
import com.melvic.esena.scene.Camera
import com.melvic.esena.tuples.{Point, Vec}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class CameraSpec extends AnyFlatSpec with should.Matchers {
  "Constructing a camera" should "have a default transformation as the identity matrix" in {
    val camera = Camera(160, 120, math.Pi / 2)
    camera.hSize should be (160)
    camera.vSize should be (120)
    camera.fieldOfView should be (math.Pi / 2)
    camera.transformation should be (Identity4x4)
  }

  "The pixel size" should "be calculated correctly for the horizontal canvas" in {
    val cam = Camera(200, 125, math.Pi / 2)
    roundTo(2)(cam.pixelSize) should be (0.01)
  }

  "The pixel size" should "be calculated correctly for the vertical canvas" in {
    val cam = Camera(125, 200, math.Pi / 2)
    roundTo(2)(cam.pixelSize) should be (0.01)
  }

  "A ray through the center of the canvas" should "provide the correct origin and direction" in {
    val cam = Camera(201, 101, math.Pi / 2)
    val ray = cam.rayForPixel(100, 50)
    ray.origin should be (Point(0, 0, 0))
    ray.direction should be (Vec(0, 0, -1))
  }

  "A ray through a corner of the canvas" should "provide the correct origin and direction" in {
    val cam = Camera(201, 101, math.Pi / 2)
    val ray = cam.rayForPixel(0, 0)
    ray.origin should be (Point.Origin)
    ray.direction.map(roundTo5) should be (Vec(0.66519, 0.33259, -0.66851))
  }

  "A ray when the camera is transformed" should "provide the correct origin and direction" in {
    val cam = Camera(201, 101, math.Pi / 2)
      .copy(transformation = rotationY(math.Pi / 4) * translation(0, -2, 5))
    val ray = cam.rayForPixel(100, 50)
    ray.origin should be (Point(0, 2, -5))    // inverse of the camera translation
    ray.direction should be (Vec(math.sqrt(2) / 2, 0, -math.sqrt(2) / 2))
  }
}
