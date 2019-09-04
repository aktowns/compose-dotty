package tinyfp.exts

def (a: A) x [A, B] (b: B): (A, B) =
  (a, b)
def (a: (A, B)) x [A, B, C] (b: C): (A, B, C) =
  (a._1, a._2, b)
def (a: (A, B, C)) x [A, B, C, D] (b: D): (A, B, C, D) =
  (a._1, a._2, a._3, b)
def (a: (A, B, C, D, E)) x [A, B, C, D, E] (b: E): (A, B, C, D, E) = 
  (a._1, a._2, a._3, a._4, b)
def (a: (A, B, C, D, E, F)) x [A, B, C, D, E, F] (b: F): (A, B, C, D, E, F) = 
  (a._1, a._2, a._3, a._4, a._5, b)
def (a: (A, B, C, D, E, F, G)) x [A, B, C, D, E, F, G] (b: G): (A, B, C, D, E, F, G) = 
  (a._1, a._2, a._3, a._4, a._5, a._6, b)
def (a: (A, B, C, D, E, F, G, H)) x [A, B, C, D, E, F, G, H] (b: H): (A, B, C, D, E, F, G, H) = 
  (a._1, a._2, a._3, a._4, a._5, a._6, a._7, b)