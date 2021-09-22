import com.melvic.esena.rays.Ray
import com.melvic.esena.shapes.Cube
import com.melvic.esena.tuples.{Point, Vec}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class CubesSpec extends AnyFlatSpec with should.Matchers {
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

    val cube = Cube()
    data.foreach { data =>
      val ray = Ray(data.origin, data.direction)
      val xs = cube.localIntersect(ray)
      xs.size should be (2)
      xs(0).t should be (data.t1)
      xs(1).t should be (data.t2)
    }
  }
}
