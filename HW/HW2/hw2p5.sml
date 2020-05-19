(* HW2 P5*)

fun Square(a:real) = a * a;

fun Cube(a:real) = a * a *a;

fun  tabulate(a:real, d:real, n:int,f)=
	if n = ~1 then []
	else (a,f(a))::tabulate(a+d,d,n-1,f);

(* 2 Inputs: *)

(* Real function: Square*)
tabulate(1.0, 2.0, 2, Square);
(* Real function: Cube*)
tabulate(1.0, 2.0, 2, Cube);
