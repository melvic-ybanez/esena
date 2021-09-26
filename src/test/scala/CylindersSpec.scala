import com.melvic.esena.MathUtils.roundTo5
import com.melvic.esena.rays.Ray
import com.melvic.esena.shapes.Cylinder
import com.melvic.esena.tuples.{Point, Vec}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class CylindersSpec extends AnyFlatSpec with should.Matchers {
  val cyl = Cylinder

  "A ray missing a cylinder" should "not produce any intersections" in {
    val data = Vector(
      Point(1, 0, 0) -> Vec(0, 1, 0),
      Point.Origin -> Vec(0, 1, 0),
      Point(0, 0, 5) -> Vec(1, 1, 1)
    )
    data.foreach { case (origin, direction) =>
      val nDirection = direction.normalize
      val ray = Ray(origin, nDirection)
      cyl.localIntersect(ray).size should be (0)
    }
  }

  "A ray striking a cylinder" should "work on different cases" in {
    val data = Vector(
      (Point(1, 0, -5), Vec(0, 0, 1), 5, 5),
      (Point(0, 0, -5), Vec(0, 0, 1), 4, 6),
      (Point(0.5, 0, -5), Vec(0.1, 1, 1), 6.80798, 7.08872)
    )

    data.foreach { case (origin, direction, t0, t1) =>
      val nDirection = direction.normalize
      val ray = Ray(origin, nDirection)
      val xs = cyl.localIntersect(ray)
      xs.size should be (2)
      roundTo5(xs(0).t) should be (t0)
      roundTo5(xs(1).t) should be (t1)
    }
  }

  "The normal of a cylinder" should "work on different points on the surface" in {
    val data = Vector(
      Point(1, 0, 0) -> Vec(1, 0, 0),
      Point(0, 5, -1) -> Vec(0, 0, -1),
      Point(0, -2, 1) -> Vec(0, 0, 1),
      Point(-1, 1, 0) -> Vec(-1, 0, 0)
    )
    data.foreach { case (point, normal) =>
      cyl.localNormalAt(point) should be (normal)
    }
  }

  "The default minimum and maximum of a cylinder" should "be -infinity and +infinity" in {
    cyl.min should be (Double.NegativeInfinity)
    cyl.max should be (Double.PositiveInfinity)
  }

  "A cylinder" should "support truncation at either end" in {
    val cyl = Cylinder.withMin(1).withMax(2)

    val data = Vector(
      // a diagonal ray from the inside
      (Point(0, 1.5, 0), Vec(0.1, 1, 0), 0),
      // perpendicular to the y-axis, from above
      (Point(0, 3, -5), Vec(0, 0, 1), 0),
      // perpendicular to the y-axis, from below
      (Point(0, 0, -5), Vec(0, 0, 1), 0),
      // maximum, should be out of bounds
      (Point(0, 2, -5), Vec(0, 0, 1), 0),
      // minimum, should be out of bounds
      (Point(0, 1, -5), Vec(0, 0, 1), 0),
      // perpendicular through the middle,
      // should produce 2 intersections
      (Point(0, 1.5, -2), Vec(0, 0, 1), 2)
    )

    data.foreach { case (point, direction, count) =>
      val nDirection = direction.normalize
      val ray = Ray(point, nDirection)
      cyl.localIntersect(ray).size should be (count)
    }
  }

  "A cylinder" should "not be closed by default" in {
    cyl.closed should be (false)
  }

  "A closed cylinder" should "cause intersection at the end caps" in {
    val cyl = Cylinder.withMin(1).withMax(2).withClosed(true)
    val data = Vector(
      (Point(0, 3, 0), Vec(0, -1, 0), 2),
      (Point(0, 3, -2), Vec(0, -1, 2), 2),
      (Point(0, 4, -2), Vec(0, -1, 1), 2),
      (Point(0, 0, -2), Vec(0, 1, 2), 2),
      (Point(0, -1, -2), Vec(0, 1, 1), 2)
    )
    data.foreach { case (point, direction, count) =>
      val ray = Ray(point, direction.normalize)
      val xs = cyl.localIntersect(ray)
      xs.size should be (count)
    }
  }

  "The normal vector" should "account for closed cylinders" in {
    val cyl = Cylinder.withMin(1).withMax(2).withClosed(true)
    val data = Vector(
      Point(0, 1, 0) -> Vec(0, -1, 0),
      Point(0.5, 1, 0) -> Vec(0, -1, 0),
      Point(0, 1, 0.5) -> Vec(0, -1, 0),
      Point(0, 2, 0) -> Vec(0, 1, 0),
      Point(0.5, 2, 0) -> Vec(0, 1, 0),
      Point(0, 2, 0.5) -> Vec(0, 1, 0)
    )
    data.foreach { case (point, normal) =>
      cyl.localNormalAt(point) should be (normal)
    }
  }
}
