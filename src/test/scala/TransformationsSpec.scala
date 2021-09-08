import com.melvic.esena.{Point, Vec}
import com.melvic.esena.matrix.Matrix
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class TransformationsSpec extends AnyFlatSpec with should.Matchers {
  "Multiplying by a translation matrix" should "update the elements of a point" in {
    val transform = Matrix.for3D.translate(5, -3, 2)
    val point = Point(-3, 4, 5)
    (transform * point) should be (Point(2, 1, 7))
  }

  "Multiplying by the inverse of a translation" should "move points in reverse" in {
    val transform = Matrix.for3D.translate(5, -3, 2)
    val inverse = transform.inverse.get
    val p = Point(-3, 4, 5)
    (inverse * p) should be (Point(-8, 7, 3))
  }

  "Translating a vector" should "not alter the vector" in {
    val transform = Matrix.for3D.translate(5, -3, 2)
    val v = Vec(-3, 4, 5)
    (transform * v) should be (v)
  }
}
