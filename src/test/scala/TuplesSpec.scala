import com.melvic.esena.ETuple
import org.scalatest._
import flatspec._
import matchers._

class TuplesSpec extends AnyFlatSpec with should.Matchers {
  "A point" should "have w=1.0" in {
    val a = ETuple(4.3, -4.2, 3.1, 1.0)
    a.x should be (4.3)
    a.y should be (-4.2)
    a.z should be (3.1)
    a.w should be (1.0)
    assert(ETuple.isPoint(a))
    assert(!ETuple.isVector(a))
  }

  "A vector" should "have w=0.0" in {
    val a = ETuple(4.3, -4.2, 3.1, 0.0)
    a.x should be (4.3)
    a.y should be (-4.2)
    a.z should be (3.1)
    a.w should be (0.0)
    assert(!ETuple.isPoint(a))
    assert(ETuple.isVector(a))
  }

  "point()" should "create tuple with w=1.0" in {
    val p = ETuple.point(4, -4, 3)
    p should be (ETuple(4, -4, 3, 1))
  }

  "vector()" should "create tuple with w=0.0"  in {
    val v = ETuple.vector(4, -4, 3)
    v should be (ETuple(4, -4, 3, 0))
  }

  "Adding two tuples" should "add all their components" in {
    val a1 = ETuple(3, -2, 5, 1)
    val a2 = ETuple(-2, 3, 1, 0)
    (a1 + a2) should be (ETuple(1, 1, 6, 1))
  }

  "Subtracting two points" should "subtract all their components" in {
    val p1 = ETuple.point(3, 2, 1)
    val p2 = ETuple.point(5, 6, 7)
    (p1 - p2) should be (ETuple.vector(-2, -4, -6))
  }
}
