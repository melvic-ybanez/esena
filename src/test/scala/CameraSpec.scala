import com.melvic.esena.Math.roundTo
import com.melvic.esena.matrix.Matrix.Identity4x4
import com.melvic.esena.scene.Camera
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
}
