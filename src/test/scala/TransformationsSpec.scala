import com.melvic.esena.Math.roundTo5
import com.melvic.esena.matrix._
import com.melvic.esena.tuples.{Point, Vec}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class TransformationsSpec extends AnyFlatSpec with should.Matchers {
  "Multiplying by a translation matrix" should "update the elements of a point" in {
    val transform = translation(5, -3, 2)
    val point     = Point(-3, 4, 5)
    (transform * point) should be(Point(2, 1, 7))
  }

  "Multiplying by the inverse of a translation" should "move points in reverse" in {
    val transform = translation(5, -3, 2)
    val p         = Point(-3, 4, 5)
    (transform.inverse * p) should be(Point(-8, 7, 3))
  }

  "Translating a vector" should "not alter the vector" in {
    val transform = translation(5, -3, 2)
    val v         = Vec(-3, 4, 5)
    (transform * v) should be(v)
  }

  "Scaling a point" should "multiply each element by its corresponding factor" in {
    val transform = scaling(2, 3, 4)
    val p         = Point(-4, 6, 8)
    (transform * p) should be(Point(-8, 18, 32))
  }

  "Scaling a vector" should "work similarly as scaling a point" in {
    val transform = scaling(2, 3, 4)
    val v         = Vec(-4, 6, 8)
    (transform * v) should be(Vec(-8, 18, 32))
  }

  "Scaling by the inverse" should "shrink the vector" in {
    val transform = scaling(2, 3, 4)
    val v         = Vec(-4, 6, 8)
    (transform.inverse * v) should be(Vec(-2, 2, 2))
  }

  "Reflection" should "be the same as scaling by a negative value" in {
    val transform = scaling(-1, 1, 1)
    val p         = Point(2, 3, 4)
    (transform * p) should be(Point(-2, 3, 4))
  }

  "Rotating a point along the x-axis" should "update the y and z axes accordingly" in {
    val p           = Point(0, 1, 0)
    val halfQuarter = rotationX(math.Pi / 4)
    val fullQuarter = rotationX(math.Pi / 2)
    (halfQuarter * p) should be(Point(0, math.sqrt(2) / 2, math.sqrt(2) / 2))
    (fullQuarter * p) should be(Point(0, 0, 1))
  }

  "The inverse of an x-rotation" should "rotate in the opposite direction" in {
    val p           = Point(0, 1, 0)
    val halfQuarter = rotationX(math.Pi / 4)
    (halfQuarter.inverse * p) should be(Point(0, math.sqrt(2) / 2, -math.sqrt(2) / 2))
  }

  "Rotating the point along the y axis" should "update the x and z axes accordingly" in {
    val p           = Point(0, 0, 1)
    val halfQuarter = rotationY(math.Pi / 4)
    val fullQuarter = rotationY(math.Pi / 2)
    (halfQuarter * p) should be(Point(math.sqrt(2) / 2, 0, math.sqrt(2) / 2))
    (fullQuarter * p) should be(Point(1, 0, 0))
  }

  "Rotating the point along the z axis" should "update the x and y axes accordingly" in {
    val p           = Point(0, 1, 0)
    val halfQuarter = rotationZ(math.Pi / 4)
    val fullQuarter = rotationZ(math.Pi / 2)
    (halfQuarter * p) should be(Point(-math.sqrt(2) / 2, math.sqrt(2) / 2, 0))
    (fullQuarter * p) should be(Point(-1, 0, 0))
  }

  "A shearing transformation" should "move x in proportion to y" in {
    val transform = shearing(1, 0, 0, 0, 0, 0)
    (transform * Point(2, 3, 4)) should be(Point(5, 3, 4))
  }

  "A shearing transformation" should "move x in proportion to z" in {
    val transform = shearing(0, 1, 0, 0, 0, 0)
    (transform * Point(2, 3, 4)) should be(Point(6, 3, 4))
  }

  "A shearing transformation" should "move y in proportion to x" in {
    val transform = shearing(0, 0, 1, 0, 0, 0)
    (transform * Point(2, 3, 4)) should be(Point(2, 5, 4))
  }

  "A shearing transformation" should "move y in proportion to z" in {
    val transform = shearing(0, 0, 0, 1, 0, 0)
    (transform * Point(2, 3, 4)) should be(Point(2, 7, 4))
  }

  "A shearing transformation" should "move z in proportion to x" in {
    val transform = shearing(0, 0, 0, 0, 1, 0)
    (transform * Point(2, 3, 4)) should be(Point(2, 3, 6))
  }

  "A shearing transformation" should "move z in proportion to y" in {
    val transform = shearing(0, 0, 0, 0, 0, 1)
    (transform * Point(2, 3, 4)) should be(Point(2, 3, 7))
  }

  "Individual transformations" should "be applied in sequence" in {
    val p = Point(1, 0, 1)
    val a = rotationX(math.Pi / 2)
    val b = scaling(5, 5, 5)
    val c = translation(10, 5, 7)

    val p2 = a * p
    p2 should be(Point(1, -1, 0))

    val p3 = b * p2
    p3 should be(Point(5, -5, 0))

    val p4 = c * p3
    p4 should be(Point(15, 0, 7))
  }

  "Chained transformations" should "be applied in reverse order" in {
    val p = Point(1, 0, 1)
    val a = rotationX(math.Pi / 2)
    val b = scaling(5, 5, 5)
    val c = translation(10, 5, 7)
    val t = c * b * a
    (t * p) should be(Point(15, 0, 7))
  }

  "The view transformation matrix for the default orientation" should "be the identity matrix" in {
    val from = Point.Origin
    val to   = Point(0, 0, -1)
    val up   = Vec(0, 1, 0)
    view(from, to, up) should be(Matrix.Identity4x4)
  }

  "The view transformation matrix looking in positive z direction" should "be reflecting across the z axis" in {
    val from = Point.Origin
    val to = Point(0, 0, 1)
    val up = Vec(0, 1, 0)

    // front to back, left to right
    view(from, to, up) should be (scaling(-1, 1, -1))
  }

  "The view transformation" should "move the world, not the eye" in {
    val from = Point(0, 0, 8)
    val to = Point.Origin
    val up = Vec(0, 1, 0)
    view(from, to, up) should be (translation(0, 0, -8))
  }

  "An arbitrary view transformation" should "combine shearing, scaling, and translation" in {
    val from = Point(1, 3, 2)
    val to = Point(4, -2, 8)
    val up = Vec(1, 1, 0)
    val t = view(from, to, up)
    t.map(roundTo5) should be (Matrix.of4x4(
      (-0.50709, 0.50709, 0.67612, -2.36643),
      (0.76772, 0.60609, 0.12122, -2.82843),
      (-0.35857, 0.59761, -0.71714, 0.00000),
      (0.00000, 0.00000, 0.00000, 1.00000),
    ))
  }
}
