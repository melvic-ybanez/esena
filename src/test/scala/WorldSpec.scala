import com.melvic.esena.canvas.Color
import com.melvic.esena.lights.{Material, PointLight}
import com.melvic.esena.matrix.scaling
import com.melvic.esena.scene.World
import com.melvic.esena.shapes.Sphere
import com.melvic.esena.tuples.Point
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class WorldSpec extends AnyFlatSpec with should.Matchers {
  "An empty world" should "contain no objects and no light source" in {
    val w = World()
    w.light should be (None)
    w.objects should be (Vector())
  }

  "The default world" should "have the default settings for light and objects" in {
    val light = PointLight(Point(-10, 10, -10), Color.White)
    val s1 = Sphere(
      material = Material(
        color = Color(0.8, 1.0, 0.6),
        diffuse = 0.7,
        specular = 0.2
      )
    )
    val s2 = Sphere(transformation = scaling(0.5, 0.5, 0.5))
    val w = World.default
    w.light should be (Some(light))
    assert(w.contains(s1))
    assert(w.contains(s2))
  }
}
