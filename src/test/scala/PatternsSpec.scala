import com.melvic.esena.canvas.Color
import com.melvic.esena.matrix.{Matrix, scaling, translation}
import com.melvic.esena.patterns.Pattern.{StripePattern, TestPattern}
import com.melvic.esena.shapes.Sphere
import com.melvic.esena.tuples.Point
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class PatternsSpec extends AnyFlatSpec with should.Matchers {
  val stripePattern = StripePattern(Color.White, Color.Black)

  val stripeAtPoint: Point => Color = stripePattern.at

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
    val color = stripePattern.at(shape, Point(1.5, 0, 0))
    color should be (Color.White)
  }

  "Stripes with a pattern transformation" should "transform the pattern" in {
    val shape = Sphere()
    val newPattern = stripePattern.transform(scaling(2, 2, 2))
    val color = newPattern.at(shape, Point(1.5, 0, 0))
    color should be (Color.White)
  }

  "Stripes with both an object and pattern transformation" should "transform both accordingly" in {
    val shape = Sphere.transform(scaling(2, 2, 2))
    val newPattern = stripePattern.transform(translation(0.5, 0, 0))
    val color = newPattern.at(shape, Point(2.5, 0, 0))
    color should be (Color.White)
  }

  "The default pattern transformation" should "be the identity matrix" in {
    val pattern = TestPattern()
    pattern.transformation should be (Matrix.Identity4x4)
  }

  "Assigning a transformation" should "override the previous transformation" in {
    val pattern = TestPattern.transform(translation(1, 2, 3))
    pattern.transformation should be (translation(1, 2, 3))
  }

  "A pattern with an object transformation" should "transform the pattern" in {
    val shape = Sphere.transform(scaling(2, 2, 2))
    val pattern = TestPattern()
    val color = pattern.at(shape, Point(2, 3, 4))
    color should be (Color(1, 1.5, 2))
  }

  "A pattern with a pattern transformation" should "transform the pattern" in {
    val shape = Sphere()
    val pattern = TestPattern.transform(scaling(2, 2, 2))
    val color = pattern.at(shape, Point(2, 3, 4))
    color should be (Color(1, 1.5, 2))
  }

  "A pattern with both an object and transformation" should "transform the pattern using both" in {
    val shape = Sphere.transform(scaling(2, 2, 2))
    val pattern = TestPattern.translate(0.5, 1, 1.5)
    val color = pattern.at(shape, Point(2.5, 3, 3.5))
    color should be (Color(0.75, 0.5, 0.25))
  }
}
