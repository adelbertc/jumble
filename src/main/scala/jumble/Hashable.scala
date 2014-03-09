package jumble

import scalaz.{ \/, -\/, \/-, IList }

/** Hashable type class.
  *
  * Minimal implementation: `hash` or `hashWithSalt`.
  */
trait Hashable[A] {
  def hash(x: A): Int =
    hashWithSalt(Hashable.defaultSalt, x)

  def hashWithSalt(salt: Int, x: A): Int =
    Hashable.combine(salt, hash(x))

  val hashableSyntax: jumble.syntax.HashableSyntax[A] =
    new jumble.syntax.HashableSyntax[A] {
      def F = Hashable.this
    }
}

object Hashable extends HashableInstances with HashableFunctions {
  def apply[A](implicit A: Hashable[A]): Hashable[A] = A
}

sealed abstract class HashableInstances {
  implicit val unitHashable: Hashable[Unit] = 
    new Hashable[Unit] {
      override def hash(x: Unit): Int = 0
    }

  implicit val booleanHashable: Hashable[Boolean] =
    new Hashable[Boolean] {
      override def hash(x: Boolean): Int = if (x) 1 else 0
    }

  implicit val intHashable: Hashable[Int] =
    new Hashable[Int] {
      override def hash(x: Int): Int = x
    }

  implicit val longHashable: Hashable[Long] =
    new Hashable[Long] {
      override def hash(x: Long): Int = if (x.isValidInt) x.intValue else (x ^ (x >> 32)).intValue
    }

  implicit val charHashable: Hashable[Char] =
    new Hashable[Char] {
      override def hash(x: Char): Int = x.intValue()
    }

  /** TODO: Suspicious.. */
  implicit val floatHashable: Hashable[Float] =
    new Hashable[Float] {
      override def hash(x: Float): Int = Hashable[Int].hash(x.intValue)
    }

  implicit val doubleHashable: Hashable[Double] =
    new Hashable[Double] {
      override def hash(x: Double): Int = Hashable[Long].hash(x.longValue)
    }

  implicit def optionHashable[A](implicit A: Hashable[A]): Hashable[Option[A]] =
    new Hashable[Option[A]] {
      override def hash(x: Option[A]): Int =
        x match {
          case None => 0
          case Some(a) => A.hashWithSalt(Hashable.distinguisher, a)
        }

      override def hashWithSalt(salt: Int, x: Option[A]): Int =
        x match {
          case None => Hashable.combine(salt, 0)
          case Some(a) => A.hashWithSalt(Hashable.combine(salt, Hashable.distinguisher), a)
        }
    }

  implicit def disjunctionHashable[A, B](implicit A: Hashable[A], B: Hashable[B]): Hashable[A \/ B] =
    new Hashable[A \/ B] {
      override def hash(x: A \/ B): Int =
        x match {
          case -\/(a) => A.hashWithSalt(0, a)
          case \/-(b) => B.hashWithSalt(Hashable.distinguisher, b)
        }

      override def hashWithSalt(salt: Int, x: A \/ B): Int =
        x match {
          case -\/(a) => A.hashWithSalt(Hashable.combine(salt, 0), a)
          case \/-(b) => B.hashWithSalt(Hashable.combine(salt, Hashable.distinguisher), b)
        }
    }

  implicit def eitherHashable[A, B](implicit A: Hashable[A], B: Hashable[B]): Hashable[Either[A, B]] =
    new Hashable[Either[A, B]] {
      override def hash(x: Either[A, B]): Int = Hashable[A \/ B].hash(\/.fromEither(x))

      override def hashWithSalt(salt: Int, x: Either[A, B]): Int = Hashable[A \/ B].hash(\/.fromEither(x))
    }

  implicit val stringHashable: Hashable[String] =
    new Hashable[String] {
      override def hashWithSalt(salt: Int, x: String): Int = x.foldLeft(salt)(Hashable[Char].hashWithSalt)
    }

  implicit def listHashable[A](implicit A: Hashable[A]): Hashable[List[A]] =
    new Hashable[List[A]] {
      override def hashWithSalt(salt: Int, x: List[A]): Int = x.foldLeft(salt)(A.hashWithSalt)
    }

  implicit def ilistHashable[A](implicit A: Hashable[A]): Hashable[IList[A]] =
    new Hashable[IList[A]] {
      override def hashWithSalt(salt: Int, x: IList[A]): Int = x.foldLeft(salt)(A.hashWithSalt)
    }

  /** TODO: Add Hashable instances for Tuple{2 - 12} */
}

sealed trait HashableFunctions {
  private[jumble] val defaultSalt = 17

  /** Combines two hash values */
  private[jumble] def combine(hash1: Int, hash2: Int): Int =
    ((hash1 + hash2) << 5) ^ hash2

  private[jumble] val distinguisher: Int = Byte.MaxValue.intValue / 3
}
