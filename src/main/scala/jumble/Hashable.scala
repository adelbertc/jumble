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

  implicit def tuple2Hashable[A, B](implicit A: Hashable[A], B: Hashable[B]): Hashable[(A, B)] =
    new Hashable[(A, B)] {
      override def hash(x: (A, B)): Int =
        B.hashWithSalt(A.hash(x._1), x._2)
      override def hashWithSalt(salt: Int, x: (A, B)): Int =
        B.hashWithSalt(A.hashWithSalt(salt, x._1), x._2)
    }

  implicit def tuple3Hashable[A, B, C](implicit A: Hashable[A], B: Hashable[B], C: Hashable[C]): Hashable[(A, B, C)] =
    new Hashable[(A, B, C)] {
      override def hash(x: (A, B, C)): Int =
        C.hashWithSalt(B.hashWithSalt(A.hash(x._1), x._2), x._3)
      override def hashWithSalt(salt: Int, x: (A, B, C)): Int =
        C.hashWithSalt(B.hashWithSalt(A.hashWithSalt(salt, x._1), x._2), x._3)
    }

  implicit def tuple4Hashable[A, B, C, D](implicit A: Hashable[A], B: Hashable[B], C: Hashable[C], D: Hashable[D]): Hashable[(A, B, C, D)] =
    new Hashable[(A, B, C, D)] {
      override def hash(x: (A, B, C, D)): Int =
        D.hashWithSalt(C.hashWithSalt(B.hashWithSalt(A.hash(x._1), x._2), x._3), x._4)
      override def hashWithSalt(salt: Int, x: (A, B, C, D)): Int =
        D.hashWithSalt(C.hashWithSalt(B.hashWithSalt(A.hashWithSalt(salt, x._1), x._2), x._3), x._4)
    }

  implicit def tuple5Hashable[A, B, C, D, E](implicit A: Hashable[A], B: Hashable[B], C: Hashable[C], D: Hashable[D], E: Hashable[E]): Hashable[(A, B, C, D, E)] =
    new Hashable[(A, B, C, D, E)] {
      override def hash(x: (A, B, C, D, E)): Int =
        E.hashWithSalt(D.hashWithSalt(C.hashWithSalt(B.hashWithSalt(A.hash(x._1), x._2), x._3), x._4), x._5)
      override def hashWithSalt(salt: Int, x: (A, B, C, D, E)): Int =
        E.hashWithSalt(D.hashWithSalt(C.hashWithSalt(B.hashWithSalt(A.hashWithSalt(salt, x._1), x._2), x._3), x._4), x._5)
    }

  implicit def tuple6Hashable[A, B, C, D, E, F](implicit A: Hashable[A], B: Hashable[B], C: Hashable[C], D: Hashable[D], E: Hashable[E], F: Hashable[F]): Hashable[(A, B, C, D, E, F)] =
    new Hashable[(A, B, C, D, E, F)] {
      override def hash(x: (A, B, C, D, E, F)): Int =
        F.hashWithSalt(E.hashWithSalt(D.hashWithSalt(C.hashWithSalt(B.hashWithSalt(A.hash(x._1), x._2), x._3), x._4), x._5), x._6)
      override def hashWithSalt(salt: Int, x: (A, B, C, D, E, F)): Int =
        F.hashWithSalt(E.hashWithSalt(D.hashWithSalt(C.hashWithSalt(B.hashWithSalt(A.hashWithSalt(salt, x._1), x._2), x._3), x._4), x._5), x._6)
    }

  implicit def tuple7Hashable[A, B, C, D, E, F, G](implicit A: Hashable[A], B: Hashable[B], C: Hashable[C], D: Hashable[D], E: Hashable[E], F: Hashable[F], G: Hashable[G]): Hashable[(A, B, C, D, E, F, G)] =
    new Hashable[(A, B, C, D, E, F, G)] {
      override def hash(x: (A, B, C, D, E, F, G)): Int =
        G.hashWithSalt(F.hashWithSalt(E.hashWithSalt(D.hashWithSalt(C.hashWithSalt(B.hashWithSalt(A.hash(x._1), x._2), x._3), x._4), x._5), x._6), x._7)
      override def hashWithSalt(salt: Int, x: (A, B, C, D, E, F, G)): Int =
        G.hashWithSalt(F.hashWithSalt(E.hashWithSalt(D.hashWithSalt(C.hashWithSalt(B.hashWithSalt(A.hashWithSalt(salt, x._1), x._2), x._3), x._4), x._5), x._6), x._7)
    }

  implicit def tuple8Hashable[A, B, C, D, E, F, G, H](implicit A: Hashable[A], B: Hashable[B], C: Hashable[C], D: Hashable[D], E: Hashable[E], F: Hashable[F], G: Hashable[G], H: Hashable[H]): Hashable[(A, B, C, D, E, F, G, H)] =
    new Hashable[(A, B, C, D, E, F, G, H)] {
      override def hash(x: (A, B, C, D, E, F, G, H)): Int =
        H.hashWithSalt(G.hashWithSalt(F.hashWithSalt(E.hashWithSalt(D.hashWithSalt(C.hashWithSalt(B.hashWithSalt(A.hash(x._1), x._2), x._3), x._4), x._5), x._6), x._7), x._8)
      override def hashWithSalt(salt: Int, x: (A, B, C, D, E, F, G, H)): Int =
        H.hashWithSalt(G.hashWithSalt(F.hashWithSalt(E.hashWithSalt(D.hashWithSalt(C.hashWithSalt(B.hashWithSalt(A.hashWithSalt(salt, x._1), x._2), x._3), x._4), x._5), x._6), x._7), x._8)
    }

  implicit def tuple9Hashable[A, B, C, D, E, F, G, H, I](implicit A: Hashable[A], B: Hashable[B], C: Hashable[C], D: Hashable[D], E: Hashable[E], F: Hashable[F], G: Hashable[G], H: Hashable[H], I: Hashable[I]): Hashable[(A, B, C, D, E, F, G, H, I)] =
    new Hashable[(A, B, C, D, E, F, G, H, I)] {
      override def hash(x: (A, B, C, D, E, F, G, H, I)): Int =
        I.hashWithSalt(H.hashWithSalt(G.hashWithSalt(F.hashWithSalt(E.hashWithSalt(D.hashWithSalt(C.hashWithSalt(B.hashWithSalt(A.hash(x._1), x._2), x._3), x._4), x._5), x._6), x._7), x._8), x._9)
      override def hashWithSalt(salt: Int, x: (A, B, C, D, E, F, G, H, I)): Int =
        I.hashWithSalt(H.hashWithSalt(G.hashWithSalt(F.hashWithSalt(E.hashWithSalt(D.hashWithSalt(C.hashWithSalt(B.hashWithSalt(A.hashWithSalt(salt, x._1), x._2), x._3), x._4), x._5), x._6), x._7), x._8), x._9)
    }

  implicit def tuple10Hashable[A, B, C, D, E, F, G, H, I, J](implicit A: Hashable[A], B: Hashable[B], C: Hashable[C], D: Hashable[D], E: Hashable[E], F: Hashable[F], G: Hashable[G], H: Hashable[H], I: Hashable[I], J: Hashable[J]): Hashable[(A, B, C, D, E, F, G, H, I, J)] =
    new Hashable[(A, B, C, D, E, F, G, H, I, J)] {
      override def hash(x: (A, B, C, D, E, F, G, H, I, J)): Int =
        J.hashWithSalt(I.hashWithSalt(H.hashWithSalt(G.hashWithSalt(F.hashWithSalt(E.hashWithSalt(D.hashWithSalt(C.hashWithSalt(B.hashWithSalt(A.hash(x._1), x._2), x._3), x._4), x._5), x._6), x._7), x._8), x._9), x._10)
      override def hashWithSalt(salt: Int, x: (A, B, C, D, E, F, G, H, I, J)): Int =
        J.hashWithSalt(I.hashWithSalt(H.hashWithSalt(G.hashWithSalt(F.hashWithSalt(E.hashWithSalt(D.hashWithSalt(C.hashWithSalt(B.hashWithSalt(A.hashWithSalt(salt, x._1), x._2), x._3), x._4), x._5), x._6), x._7), x._8), x._9), x._10)
    }

  implicit def tuple11Hashable[A, B, C, D, E, F, G, H, I, J, K](implicit A: Hashable[A], B: Hashable[B], C: Hashable[C], D: Hashable[D], E: Hashable[E], F: Hashable[F], G: Hashable[G], H: Hashable[H], I: Hashable[I], J: Hashable[J], K: Hashable[K]): Hashable[(A, B, C, D, E, F, G, H, I, J, K)] =
    new Hashable[(A, B, C, D, E, F, G, H, I, J, K)] {
      override def hash(x: (A, B, C, D, E, F, G, H, I, J, K)): Int =
        K.hashWithSalt(J.hashWithSalt(I.hashWithSalt(H.hashWithSalt(G.hashWithSalt(F.hashWithSalt(E.hashWithSalt(D.hashWithSalt(C.hashWithSalt(B.hashWithSalt(A.hash(x._1), x._2), x._3), x._4), x._5), x._6), x._7), x._8), x._9), x._10), x._11)
      override def hashWithSalt(salt: Int, x: (A, B, C, D, E, F, G, H, I, J, K)): Int =
        K.hashWithSalt(J.hashWithSalt(I.hashWithSalt(H.hashWithSalt(G.hashWithSalt(F.hashWithSalt(E.hashWithSalt(D.hashWithSalt(C.hashWithSalt(B.hashWithSalt(A.hashWithSalt(salt, x._1), x._2), x._3), x._4), x._5), x._6), x._7), x._8), x._9), x._10), x._11)
    }

  implicit def tuple12Hashable[A, B, C, D, E, F, G, H, I, J, K, L](implicit A: Hashable[A], B: Hashable[B], C: Hashable[C], D: Hashable[D], E: Hashable[E], F: Hashable[F], G: Hashable[G], H: Hashable[H], I: Hashable[I], J: Hashable[J], K: Hashable[K], L: Hashable[L]): Hashable[(A, B, C, D, E, F, G, H, I, J, K, L)] =
    new Hashable[(A, B, C, D, E, F, G, H, I, J, K, L)] {
      override def hash(x: (A, B, C, D, E, F, G, H, I, J, K, L)): Int =
        L.hashWithSalt(K.hashWithSalt(J.hashWithSalt(I.hashWithSalt(H.hashWithSalt(G.hashWithSalt(F.hashWithSalt(E.hashWithSalt(D.hashWithSalt(C.hashWithSalt(B.hashWithSalt(A.hash(x._1), x._2), x._3), x._4), x._5), x._6), x._7), x._8), x._9), x._10), x._11), x._12)
      override def hashWithSalt(salt: Int, x: (A, B, C, D, E, F, G, H, I, J, K, L)): Int =
        L.hashWithSalt(K.hashWithSalt(J.hashWithSalt(I.hashWithSalt(H.hashWithSalt(G.hashWithSalt(F.hashWithSalt(E.hashWithSalt(D.hashWithSalt(C.hashWithSalt(B.hashWithSalt(A.hashWithSalt(salt, x._1), x._2), x._3), x._4), x._5), x._6), x._7), x._8), x._9), x._10), x._11), x._12)
    }
}

sealed trait HashableFunctions {
  private[jumble] val defaultSalt = 17

  /** Combines two hash values */
  private[jumble] def combine(hash1: Int, hash2: Int): Int =
    ((hash1 + hash2) << 5) ^ hash2

  private[jumble] val distinguisher: Int = Byte.MaxValue.intValue / 3
}
