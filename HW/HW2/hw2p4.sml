(* HW2 P4*)

fun first(L)=
	if L = nil then nil
	else hd(L)::fourth(tl(L))
and
	fourth(L) = 
		if L = nil then nil
		else third(tl(L))
and
	third(L) =
		if L = nil then nil
		else second(tl(L))
and
	second(L) =
		if L = nil then nil
		else first(tl(L));

(* 2 Inputs: *)

(* 1) [1,2,3,4,5,6,7,8,9,10] *)
val x = [1,2,3,4,5,6,7,8,9,10];
first(x);
second(x); 
third(x);
fourth(x);

(* 2) [1,2,3,4,5] *)
val y = [1,2,3,4,5];
first(y);
second(y);
third(y);
fourth(y);
