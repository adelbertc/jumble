package jumble
package syntax

final class HashableOps[F] private[syntax](val self: F)(implicit F: Hashable[F]) {
  final def hash: Int = F.hash(self)

  final def hashWithSalt(salt: Int): Int = F.hashWithSalt(salt, self)
}

trait ToHashableOps {
  implicit def ToHashableOps[F](x: F)(implicit F: Hashable[F]) =
    new HashableOps(x)
}

trait HashableSyntax[F] {
  implicit def ToHashableOps(x: F): HashableOps[F] =
    new HashableOps(x)(HashableSyntax.this.F)

  def F: Hashable[F]

  def hash(x: F)(implicit F: Hashable[F]): Int = F.hash(x)
  
  def hashWithSalt(salt: Int, x: F)(implicit F: Hashable[F]): Int = F.hashWithSalt(salt, x)
}
