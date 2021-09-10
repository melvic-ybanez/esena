import com.melvic.esena.canvas.Color
import com.melvic.esena.light.PointLight
import com.melvic.esena.tuples.Point
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class LightsSpec extends AnyFlatSpec with should.Matchers  {
  "A point light" should "have a position and an intensity" in {
    val intensity = Color(1, 1, 1)
    val position = Point(0, 0, 0)
    val light = PointLight(position, intensity)
    light.position should be (position)
    light.intensity should be (intensity)
  }
}
