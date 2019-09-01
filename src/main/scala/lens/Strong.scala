package lens

type Optical[P[_, _], TA, TB, A, B] = P[A, B] => P[TA, TB]
type Lens[TA, TB, A, B]             = [P[_,_]] =>> P[A, B] => P[TA, TB]
                                   //        ^ : Strong[P]

// trait Strong[P[_, _]: Profunctor] 

// def lens[S, T, A, B](getter: S => A, setter: S => (B => T)): Lens[S, T, A, B] = 
//   ???
