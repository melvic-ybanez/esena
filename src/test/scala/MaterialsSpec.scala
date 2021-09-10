import com.melvic.esena.canvas.Color
import com.melvic.esena.light.Material
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class MaterialsSpec extends AnyFlatSpec with should.Matchers {
  "A material" should "have the correct default values" in {
    val m = Material()
    m.color should be (Color.White)
    m.ambient should be (0.1)
    m.diffuse should be (0.9)
    m.specular should be (0.9)
    m.shininess should be (200.0)
  }
}
