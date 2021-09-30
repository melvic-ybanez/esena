import com.melvic.esena.matrix.Matrix.Identity4x4
import com.melvic.esena.matrix.scaling
import com.melvic.esena.rays.{Intersections, Ray}
import com.melvic.esena.shapes.{Group, Sphere, TestShape}
import com.melvic.esena.tuples.{Point, Vec}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class GroupsSpec extends AnyFlatSpec with should.Matchers {
  val group = Group.default

  "A default group" should "have an empty set of shapes" in {
    group.transformation should be (Identity4x4)
    assert(group.isEmpty)
  }

  "Adding a child to a group" should "make the group the child's parent" in {
    val shape = TestShape
    val newGroup = group + shape
    assert(!newGroup.isEmpty)
    assert(newGroup.contains(shape))
    shape.withParent(newGroup).parent should be (Some(newGroup))
  }

  "Intersecting a ray with an empty group" should "produce no intersections" in {
    val ray = Ray(Point(0, 0, 0), Vec(0, 0, 1))
    val xs = group.localIntersect(ray)
    xs should be (Intersections.None)
  }

  "Intersecting a ray with a non-empty group" should "return the intersections, if any, of the children" in {
    val s1 = Sphere
    val s2 = Sphere.translate(0, 0, -3)
    val s3 = Sphere.translate(5, 0, 0)
    val group = Group.default.addMany(s1, s2, s3)
    val ray = Ray(Point(0, 0, -5), Vec(0, 0, 1))
    val xs = group.localIntersect(ray)
    xs.size should be (4)
    xs(0).obj should be (s2)
    xs(1).obj should be (s2)
    xs(2).obj should be (s1)
    xs(3).obj should be (s1)
  }

  "Group and child transformations" should "both apply" in {
    val shape = Sphere.translate(5, 0, 0)
    val group = Group.scale(2, 2, 2) + shape
    val ray = Ray(Point(10, 0, -10), Vec(0, 0, 1))
    val xs = group.intersect(ray)
    xs.size should be (2)
  }
}
