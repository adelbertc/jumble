# jumble
Currently only provides a `Hashable` type class.

This work is based on the [hashable](http://hackage.haskell.org/package/hashable)
Haskell library. The future goal is to also include a `HashMap` and a
`HashSet`, possible based on the
[unordered-collections](http://hackage.haskell.org/package/unordered-containers)
Haskell library.

## Usage

```scala
import jumble.syntax.hashable._

5.hash
List("hello", "world").hash

Option(3).hashWithSalt(10)


import jumble.Hashable

case class Box[A](run: A)

object Box {
  implicit def boxHashable[A : Hashable]: Hashable[Box[A]] =
    new Hashable[Box[A]] {
      override def hash(x: Box[A]): Int = Hashable[A].hash(x.run)
    }
}
```

## License
Please see LICENSE for licensing details.
