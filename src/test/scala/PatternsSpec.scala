import com.melvic.esena.canvas.Color
import com.melvic.esena.matrix.{scaling, translation}
import com.melvic.esena.patterns.Pattern.StripePattern
import com.melvic.esena.shapes.Sphere
import com.melvic.esena.tuples.Point
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class PatternsSpec extends AnyFlatSpec with should.Matchers {
  val stripePattern = StripePattern(Color.White, Color.Black)

  val stripeAtPoint: Point => Color = stripePattern.applyAt

  "A stripe pattern" should "contain two colors" in {
    stripePattern.first should be (Color.White)
    stripePattern.second should be (Color.Black)
  }

  "A stripe pattern" should "be constant in y" in {
    stripeAtPoint(Point.Origin) should be (Color.White)
    stripeAtPoint(Point(0, 1, 0)) should be (Color.White)
    stripeAtPoint(Point(0, 2, 0)) should be (Color.White)
  }

  "A stripe pattern" should "be constant in z" in {
    stripeAtPoint(Point.Origin) should be (Color.White)
    stripeAtPoint(Point(0, 0, 1)) should be (Color.White)
    stripeAtPoint(Point(0, 0, 2)) should be (Color.White)
  }

  "A stripe pattern" should "alternative colors in x" in {
    stripeAtPoint(Point.Origin) should be (Color.White)
    stripeAtPoint(Point(0.9, 0, 0)) should be (Color.White)
    stripeAtPoint(Point(1, 0, 0)) should be (Color.Black)
    stripeAtPoint(Point(-0.1, 0, 0)) should be (Color.Black)
    stripeAtPoint(Point(-1, 0, 0)) should be (Color.Black)
    stripeAtPoint(Point(-1.1, 0, 0)) should be (Color.White)
  }

  "Stripes with an object transformation" should "transform the stripes accordingly" in {
    val shape = Sphere.transform(scaling(2, 2, 2))
    val color = stripePattern.applyAt(shape, Point(1.5, 0, 0))
    color should be (Color.White)
  }

  "Stripes with a pattern transformation" should "transform the pattern" in {
    val shape = Sphere()
    val newPattern = stripePattern.transform(scaling(2, 2, 2))
    val color = newPattern.applyAt(shape, Point(1.5, 0, 0))
    color should be (Color.White)
  }

  "Stripes with both an object and pattern transformation" should "transform both accordingly" in {
    val shape = Sphere.transform(scaling(2, 2, 2))
    val newPattern = stripePattern.transform(translation(0.5, 0, 0))
    val color = newPattern.applyAt(shape, Point(2.5, 0, 0))
    color should be (Color.White)
  }
}
