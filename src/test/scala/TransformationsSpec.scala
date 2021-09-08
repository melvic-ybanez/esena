import com.melvic.esena.matrix.Matrix4D
import com.melvic.esena.tuples.{Point, Vec}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class TransformationsSpec extends AnyFlatSpec with should.Matchers {
  "Multiplying by a translation matrix" should "update the elements of a point" in {
    val transform = Matrix4D.translation(5, -3, 2)
    val point = Point(-3, 4, 5)
    (transform * point) should be (Point(2, 1, 7))
  }

  "Multiplying by the inverse of a translation" should "move points in reverse" in {
    val transform = Matrix4D.translation(5, -3, 2)
    val p = Point(-3, 4, 5)
    (transform.inverse * p) should be (Point(-8, 7, 3))
  }

  "Translating a vector" should "not alter the vector" in {
    val transform = Matrix4D.translation(5, -3, 2)
    val v = Vec(-3, 4, 5)
    (transform * v) should be (v)
  }

  "Scaling a point" should "multiply each element by its corresponding factor" in {
    val transform = Matrix4D.scaling(2, 3, 4)
    val p = Point(-4, 6, 8)
    (transform * p) should be (Point(-8, 18, 32))
  }

  "Scaling a vector" should "work similarly as scaling a point" in {
    val transform = Matrix4D.scaling(2, 3, 4)
    val v = Vec(-4, 6, 8)
    (transform * v) should be (Vec(-8, 18, 32))
  }

  "Scaling by the inverse" should "shrink the vector" in {
    val transform = Matrix4D.scaling(2, 3, 4)
    val v = Vec(-4, 6, 8)
    (transform.inverse * v) should be (Vec(-2, 2, 2))
  }

  "Reflection" should "be the same as scaling by a negative value" in {
    val transform = Matrix4D.scaling(-1, 1, 1)
    val p = Point(2, 3, 4)
    (transform * p) should be (Point(-2, 3, 4))
  }

  "Rotating a point along the x-axis" should "update the y and z axes accordingly" in {
    val p = Point(0, 1, 0)
    val halfQuarter = Matrix4D.rotationX(math.Pi / 4)
    val fullQuarter = Matrix4D.rotationX(math.Pi / 2)
    (halfQuarter * p) should be (Point(0, math.sqrt(2) / 2, math.sqrt(2) / 2))
    (fullQuarter * p) should be (Point(0, 0, 1))
  }

  "The inverse of an x-rotation" should "rotate in the opposite direction" in {
    val p = Point(0, 1, 0)
    val halfQuarter = Matrix4D.rotationX(math.Pi / 4)
    (halfQuarter.inverse * p) should be (Point(0, math.sqrt(2) / 2, -math.sqrt(2) / 2))
  }

  "Rotating the point along the y axis" should "update the x and z axes accordingly" in {
    val p = Point(0, 0, 1)
    val halfQuarter = Matrix4D.rotationY(math.Pi / 4)
    val fullQuarter = Matrix4D.rotationY(math.Pi / 2)
    (halfQuarter * p) should be (Point(math.sqrt(2) / 2, 0, math.sqrt(2) / 2))
    (fullQuarter * p) should be (Point(1, 0, 0))
  }
}
