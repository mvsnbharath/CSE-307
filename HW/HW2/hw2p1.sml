(* HW2 P1*)

(* 1a) genPoly(x) *)
fun genPoly(x) = 
	if x = 0 then []
	else 1.0::genPoly(x-1);

(* 2 Inputs: *)
genPoly(3);
genPoly(5);
	

(* 1b) evalPoly(P,a) *)
fun evalPoly([],a)= 0.0
	|evalPoly(head::rest,a) =  head + a* evalPoly(rest,a);

(* 2 Inputs: *)
evalPoly([10.0, 3.0, 1.0], 2.0);
evalPoly([1.0, 1.0, 1.0], 1.0);