trait StateT[F[_], S, +A] { self =>
	def apply(initial: S): F[(S, A)]
	def run(initial: S): F[(S, A)] = apply(initial)
	def runZero(implicit S: Monoid[S]): F[(S, A)] = run(S.zero)
}
