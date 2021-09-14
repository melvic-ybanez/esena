import com.melvic.esena.lights.Material
import com.melvic.esena.matrix.Matrix.Identity4x4
import com.melvic.esena.shapes.TestShape
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class ShapeSpec extends AnyFlatSpec with should.Matchers {
  val s = TestShape()

  "The default transformation" should "be the identity matrix" in {
    s.transformation should be (Identity4x4)
  }

  "The default material" should "be the same as material with empty args" in {
    s.material should be (Material())
  }
}
