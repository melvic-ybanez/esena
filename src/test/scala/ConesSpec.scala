import com.melvic.esena.MathUtils.roundTo5
import com.melvic.esena.rays.Ray
import com.melvic.esena.shapes.Cone
import com.melvic.esena.tuples.{Point, Vec}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class ConesSpec extends AnyFlatSpec with should.Matchers {
  "Ray-cone intersection" should "be almost the same as ray-cylinder's" in {
    val shape = Cone
    val data = Vector(
      (Point(0, 0, -5), Vec(0, 0, 1), 5, 5),
      (Point(0, 0, -5), Vec(1, 1, 1), 8.66025, 8.66025),
      (Point(1, 1, -5), Vec(-0.5, -1, 1), 4.55006, 49.44994)
    )
    data.foreach { case (origin, direction, t0, t1) =>
      val ray = Ray(origin, direction.normalize)
      val xs = shape.localIntersect(ray)
      roundTo5(xs(0).t) should be (t0)
      roundTo5(xs(1).t) should be (t1)
    }
  }
}
