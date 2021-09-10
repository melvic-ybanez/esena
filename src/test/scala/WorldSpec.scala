import com.melvic.esena.scene.World
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class WorldSpec extends AnyFlatSpec with should.Matchers {
  "An empty world" should "contain no objects and no light source" in {
    val w = World()
    w.lightSource should be (None)
    w.objects should be (Vector())
  }
}
