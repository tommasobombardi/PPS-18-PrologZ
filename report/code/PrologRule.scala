val s = Struct("s")
val sum = Predicate("sum")
val mul = Predicate("mul")

sum("X",0,"X")
sum("X",s("Y"),s("Z")) :- sum("X","Y","Z")
mul("X",0,0)
mul("X",s("Y"),"Z") :- (mul("X","Y","W"), sum("X","W","Z"))
