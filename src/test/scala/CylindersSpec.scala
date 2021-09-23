import com.melvic.esena.MathUtils.roundTo5
import com.melvic.esena.rays.Ray
import com.melvic.esena.shapes.Cylinder
import com.melvic.esena.tuples.{Point, Vec}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class CylindersSpec extends AnyFlatSpec with should.Matchers {
  "A ray missing a cylinder" should "not produce any intersections" in {
    val cyl = Cylinder()
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
    final case class Row(origin: Point, direction: Vec, t0: Double, t1: Double)

    val data = Vector(
      Row(Point(1, 0, -5), Vec(0, 0, 1), 5, 5),
      Row(Point(0, 0, -5), Vec(0, 0, 1), 4, 6),
      Row(Point(0.5, 0, -5), Vec(0.1, 1, 1), 6.80798, 7.08872)
    )

    data.foreach { case Row(origin, direction, t0, t1) =>
      val cyl = Cylinder()
      val nDirection = direction.normalize
      val ray = Ray(origin, nDirection)
      val xs = cyl.localIntersect(ray)
      xs.size should be (2)
      roundTo5(xs(0).t) should be (t0)
      roundTo5(xs(1).t) should be (t1)
    }
  }
}
