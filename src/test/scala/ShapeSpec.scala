import com.melvic.esena.MathUtils.roundTo5
import com.melvic.esena.lights.Material
import com.melvic.esena.matrix.Matrix.Identity4x4
import com.melvic.esena.matrix.{rotationZ, scaling, translation}
import com.melvic.esena.rays.Ray
import com.melvic.esena.shapes.TestShape
import com.melvic.esena.tuples.{Point, Vec}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class ShapeSpec extends AnyFlatSpec with should.Matchers {
  val shape = TestShape

  "The default transformation" should "be the identity matrix" in {
    shape.transformation should be (Identity4x4)
  }

  "A shape transformation" should "be updatable" in {
    val newShape = shape.withTransformation(translation(2, 3, 4))
    newShape.transformation should be (translation(2, 3, 4))
  }

  "The default material" should "be the same as material with empty args" in {
    shape.material should be (Material())
  }

  "A shape material" should "be updatable" in {
    val material = Material(ambient = 1)
    val newShape = shape.withMaterial(material)
    newShape.material should be (material)
  }

  "Intersecting a scaled shape with a ray" should "scale the ray" in {
    val ray = Ray(Point(0, 0, -5),  Vec(0, 0, 1))
    val newShape = shape.withTransformation(scaling(2, 2, 2))
    newShape.intersect(ray)
    newShape.transformedRay.origin should be (Point(0, 0, -2.5))
    newShape.transformedRay.direction should be (Vec(0, 0, 0.5))
  }

  "Intersecting a translated shape with a ray" should "translate the ray" in {
    val ray = Ray(Point(0, 0, -5), Vec(0, 0, 1))
    val newShape = shape.withTransformation(translation(5, 0, 0))
    newShape.intersect(ray)
    newShape.transformedRay.origin should be (Point(-5, 0, -5))
    newShape.transformedRay.direction should be (Vec(0, 0, 1))
  }

  "Translation" should "not affect the normal" in {
    val newShape = shape.withTransformation(translation(0, 1, 0))
    val normal = newShape.normalAt(Point(0, 1.70711, -0.70711))
    normal.map(roundTo5) should be (Vec(0, 0.70711, -0.70711))
  }

  "Scaling and rotation" should "affect the normal" in {
    val newShape = shape.withTransformation(scaling(1, 0.5, 1) * rotationZ(math.Pi / 5))
    val normal = newShape.normalAt(Point(0, math.sqrt(2) / 2, -math.sqrt(2) / 2))
    normal.map(roundTo5) should be (Vec(0, 0.97014, -0.24254))
  }
}
