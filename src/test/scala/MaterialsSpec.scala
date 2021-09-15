import com.melvic.esena.MathUtils.roundTo
import com.melvic.esena.canvas.Color
import com.melvic.esena.lights.{Material, PointLight, lighting}
import com.melvic.esena.patterns.StripePattern
import com.melvic.esena.shapes.Sphere
import com.melvic.esena.tuples.{Point, Vec}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class MaterialsSpec extends AnyFlatSpec with should.Matchers {
  val mat = Material()
  val position = Point.Origin
  val eyeVec = Vec(0, 0, -1)
  val normalVec = Vec(0, 0, -1)

  "A material" should "have the correct default values" in {
    mat.color should be (Color.White)
    mat.ambient should be (0.1)
    mat.diffuse should be (0.9)
    mat.specular should be (0.9)
    mat.shininess should be (200.0)
  }

  "Lighting with the eye between the light and the surface" should "maximize ambient, diffuse and specular" in {
    val light = PointLight(Point(0, 0, -10), Color.White)
    val result = lighting(mat, Sphere(), light, position, eyeVec, normalVec)
    result should be (Color(1.9, 1.9, 1.9))
  }

  "Lighting with the eye, between the light and surface, offset 45 deg" should "keep the ambient and diffuse unchanged" in {
    val eyeVec = Vec(0, math.sqrt(2) / 2, -math.sqrt(2) / 2)
    val light = PointLight(Point(0, 0, -10), Color(1, 1, 1))
    val result = lighting(mat, Sphere(), light, position, eyeVec, normalVec)
    result should be (Color(1.0, 1.0, 1.0))
  }

  "Lighting with the eye on the opposite surface, and light offset 45 deg" should "set specular to 0" in {
    val light = PointLight(Point(0, 10, -10), Color(1, 1, 1))
    val result = lighting(mat, Sphere(), light, position, eyeVec, normalVec)
    result.map(roundTo(4)) should be (Color(0.7364, 0.7364, 0.7364))
  }

  "Lighting with the eye in the path of the reflection" should "maximize specular component" in {
    val eyeVec = Vec(0, -math.sqrt(2) / 2, -math.sqrt(2) / 2)
    val light = PointLight(Point(0, 10, -10), Color.White)
    val result = lighting(mat, Sphere(), light, position, eyeVec, normalVec)
    result.map(roundTo(4)) should be (Color(1.6364, 1.6364, 1.6364))
  }

  "Lighting with the light behind the surface" should "set diffuse and specular to 0" in {
    val light = PointLight(Point(0, 0, 10), Color.White)
    val result = lighting(mat, Sphere(), light, position, eyeVec, normalVec)
    result should be (Color(0.1, 0.1, 0.1))
  }

  "Lighting with the surface in shadow" should "ignore the diffuse the diffuse and specular components" in {
    val light = PointLight(Point(0, 0, -10), Color.White)
    val inShadow = true
    val result = lighting(mat, Sphere(), light, position, eyeVec, normalVec, inShadow)
    result should be (Color(0.1, 0.1, 0.1))
  }

  "Lighting with a pattern applied" should "return the color of the pattern" in {
    val newMat = mat.copy(
      pattern = Some(StripePattern(Color.White, Color.Black)),
      ambient = 1,
      diffuse = 0,
      specular = 0
    )
    val eyeVec = Vec(0, 0, -1)
    val normalVec = Vec(0, 0, -1)
    val light = PointLight(Point(0, 0, -10), Color.White)
    val c1 = lighting(newMat, Sphere(), light, Point(0.9, 0, 0), eyeVec, normalVec)
    val c2 = lighting(newMat, Sphere(), light, Point(1.1, 0, 0), eyeVec, normalVec)
    c1 should be (Color.White)
    c2 should be (Color.Black)
  }
}
