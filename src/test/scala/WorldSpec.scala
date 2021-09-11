import com.melvic.esena.Math.roundTo5
import com.melvic.esena.canvas.Color
import com.melvic.esena.lights
import com.melvic.esena.lights.{Material, PointLight}
import com.melvic.esena.matrix.scaling
import com.melvic.esena.rays.{Computations, Intersection, Ray}
import com.melvic.esena.scene.World
import com.melvic.esena.shapes.Sphere
import com.melvic.esena.tuples.{Point, Vec}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class WorldSpec extends AnyFlatSpec with should.Matchers {
  val world = World.default

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
    world.light should be (Some(light))
    assert(world.contains(s1))
    assert(world.contains(s2))
  }

  "Intersecting a world with a ray" should "return all intersections with the world's objects" in {
    val xs = world.intersect(Ray(Point(0, 0, -5), Vec(0, 0, 1)))
    xs.size should be (4)
    xs(0).t should be (4)
    xs(1).t should be (4.5)
    xs(2).t should be (5.5)
    xs(3).t should be (6)
  }

  "Shading an intersection" should "return the color from the given world" in {
    val r = Ray(Point(0, 0, -5), Vec(0, 0, 1))
    val shape = world.objects.head
    val i = Intersection(4, shape)
    val comps = Computations.prepare(i, r)
    val c = lights.shadeHit(world, comps)
    c.map(roundTo5) should be (Color(0.38066, 0.47583, 0.28550))
  }

  "Shading an intersection" should "be possible from the inside" in {
    val w = World.default.withLight(PointLight(Point(0, 0.25, 0), Color.White))
    val r = Ray(Point.Origin,  Vec(0, 0, 1))
    val shape = w.objects(1)
    val i = Intersection(0.5, shape)
    val comps = Computations.prepare(i, r)
    lights.shadeHit(w, comps).map(roundTo5) should be (Color(0.90498, 0.90498, 0.90498))
  }

  "The color when a ray misses" should "be black" in {
    val r = Ray(Point(0, 0, -5), Vec(0, 1, 0))
    world.colorAt(r) should be (Color.Black)
  }

  "The color when a ray hits" should "be from the object the ray intersects with" in {
    val r = Ray(Point(0, 0, -5), Vec(0, 0, 1))
    val c = world.colorAt(r)
    c.map(roundTo5) should be (Color(0.38066, 0.47583, 0.28550))
  }

  "The color with an intersection behind the ray" should "return the color of the pointed-to object" in {
    val outer = world.objects.head match {
      case s: Sphere => s.withMaterial(s.material.copy(ambient = 1))
    }
    val inner = world.objects(1) match {
      case s: Sphere => s.withMaterial(s.material.copy(ambient = 1))
    }
    val updatedWorld = world.copy(objects = Vector(outer, inner))
    val r = Ray(Point(0, 0, 0.75), Vec(0, 0, -1))
    updatedWorld.colorAt(r) should be (inner.material.color)
  }
}
