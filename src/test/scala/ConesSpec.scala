import com.melvic.esena.MathUtils.roundTo5
import com.melvic.esena.rays.Ray
import com.melvic.esena.shapes.Cone
import com.melvic.esena.tuples.{Point, Vec}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class ConesSpec extends AnyFlatSpec with should.Matchers {
  "Ray-cone intersection" should "be almost the same as ray-cylinder's" in {
    val data = Vector(
      (Point(0, 0, -5), Vec(0, 0, 1), 5, 5),
      (Point(0, 0, -5), Vec(1, 1, 1), 8.66025, 8.66025),
      (Point(1, 1, -5), Vec(-0.5, -1, 1), 4.55006, 49.44994)
    )
    data.foreach { case (origin, direction, t0, t1) =>
      val ray = Ray(origin, direction.normalize)
      val xs = Cone.localIntersect(ray)
      roundTo5(xs(0).t) should be (t0)
      roundTo5(xs(1).t) should be (t1)
    }
  }

  "Intersecting a cone with a ray parallel to one of its halves" should "hit the other half" in {
    val direction = Vec(0, 1, 1).normalize
    val ray = Ray(Point(0, 0, -1), direction)
    val xs = Cone.localIntersect(ray)
    xs.size should be (1)
    roundTo5(xs(0).t) should be (0.35355)
  }

  "A cone" should "produce the correct normal" in {
    val data = Vector(
      Point.Origin -> Vec.Zero,
      Point(1, 1, 1) -> Vec(1, -math.sqrt(2), 1),
      Point(-1, -1, 0) -> Vec(-1, 1, 0)
    )
    data.foreach { case (point, normal) =>
      Cone.localNormalAt(point) should be (normal)
    }
  }
}
