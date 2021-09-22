import com.melvic.esena.rays.Ray
import com.melvic.esena.shapes.Cube
import com.melvic.esena.tuples.{Point, Vec}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class CubesSpec extends AnyFlatSpec with should.Matchers {
  val cube = Cube()

  "A ray intersecting a cube" should "work correctly with any of the six directions" in {
    case class Data(label: String, origin: Point, direction: Vec, t1: Double, t2: Double)

    val data = Vector(
      Data("+x", Point(5, 0.5, 0), Vec(-1, 0, 0), 4, 6),
      Data("-x", Point(-5, 0.5, 0), Vec(1, 0, 0), 4, 6),
      Data("+y", Point(0.5, 5, 0), Vec(0, -1, 0), 4, 6),
      Data("-y", Point(0.5, -5, 0), Vec(0, 1, 0), 4, 6),
      Data("+z", Point(0.5, 0, 5), Vec(0, 0, -1), 4, 6),
      Data("-z", Point(0.5, 0, -5), Vec(0, 0, 1), 4, 6),
      Data("inside", Point(0, 0.5, 0), Vec(0, 0, 1), -1, 1)
    )

    data.foreach { datum =>
      val ray = Ray(datum.origin, datum.direction)
      val xs = cube.localIntersect(ray)
      xs.size should be (2)
      xs(0).t should be (datum.t1)
      xs(1).t should be (datum.t2)
    }
  }

  "A ray missing a cube" should "not have any intersections" in {
    val data = Vector(
      (Point(-2, 0, 0), Vec(0.2673, 0.5345, 0.8018)),
      (Point(0, -2, 0), Vec(0.8018, 0.2673, 0.5345)),
      (Point(0, 0, -2), Vec(0.5345, 0.8018, 0.2673)),
      (Point(2, 0, 2), Vec(0, 0, -1)),
      (Point(0, 2, 2), Vec(0, -1, 0)),
      (Point(2, 2, 0), Vec(-1, 0, 0))
    )
    data.foreach { case (origin, direction) =>
      val ray = Ray(origin, direction)
      val xs = cube.localIntersect(ray)
      xs.size should be (0)
    }
  }

  "The normal of a cube" should "work with all six faces" in {
    val data = Vector(
      (Point(1, 0.5, -0.8), Vec(1, 0, 0)),
      (Point(-1, -0.2, 0.9), Vec(-1, 0, 0)),
      (Point(-0.4, 1, -0.1), Vec(0, 1, 0)),
      (Point(0.3, -1, -0.7), Vec(0, -1, 0)),
      (Point(-0.6, 0.3, 1), Vec(0, 0, 1)),
      (Point(0.4, 0.4, -1), Vec(0, 0, -1)),
      (Point(1, 1, 1), Vec(1, 0, 0)),
      (Point(-1, -1, -1), Vec(-1, 0, 0))
    )
    data.foreach { case (point, normal) =>
      cube.localNormalAt(point) should be (normal)
    }
  }
}
