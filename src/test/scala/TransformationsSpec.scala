import com.melvic.esena.matrix.Matrix3D
import com.melvic.esena.tuples.{Point, Vec}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class TransformationsSpec extends AnyFlatSpec with should.Matchers {
  "Multiplying by a translation matrix" should "update the elements of a point" in {
    val transform = Matrix3D.translation(5, -3, 2)
    val point = Point(-3, 4, 5)
    (transform * point) should be (Point(2, 1, 7))
  }

  "Multiplying by the inverse of a translation" should "move points in reverse" in {
    val transform = Matrix3D.translation(5, -3, 2)
    val p = Point(-3, 4, 5)
    (transform.inverse * p) should be (Point(-8, 7, 3))
  }

  "Translating a vector" should "not alter the vector" in {
    val transform = Matrix3D.translation(5, -3, 2)
    val v = Vec(-3, 4, 5)
    (transform * v) should be (v)
  }

  "Scaling a point" should "multiply each element by its corresponding factor" in {
    val transform = Matrix3D.scaling(2, 3, 4)
    val p = Point(-4, 6, 8)
    (transform * p) should be (Point(-8, 18, 32))
  }

  "Scaling a vector" should "work similarly as scaling a point" in {
    val transform = Matrix3D.scaling(2, 3, 4)
    val v = Vec(-4, 6, 8)
    (transform * v) should be (Vec(-8, 18, 32))
  }

  "Scaling by the inverse" should "shrink the vector" in {
    val transform = Matrix3D.scaling(2, 3, 4)
    val v = Vec(-4, 6, 8)
    (transform.inverse * v) should be (Vec(-2, 2, 2))
  }
}
