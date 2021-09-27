import com.melvic.esena.matrix.Matrix.Identity4x4
import com.melvic.esena.shapes.Group
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class GroupsSpec extends AnyFlatSpec with should.Matchers {
  "A default group" should "have an empty set of shapes" in {
    val group = Group.default
    group.transformation should be (Identity4x4)
    assert(group.isEmpty)
  }
}
