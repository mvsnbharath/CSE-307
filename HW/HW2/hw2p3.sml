(* HW2 P3*)

(* 3a)  removeFst(x, L) *)
fun removeFst(x,L) = 
	if (L=[]) then []
	else if x=hd(L)then tl(L)
	else hd(L)::removeFst(x,tl(L));

(* 2 Inputs: *)
removeFst(1,[1,2,1,3,4]); (* x is present in the list*)
removeFst(5,[1,2,1,3,4]); (* x is not present in the list*)

fun reverse_helper(L,L2) =
	if L = nil then L2
	else reverse_helper(tl(L),hd(L)::L2);

fun reverse(L) = reverse_helper(L,[]);

(* 3b)  removeFst(x, L) *)
fun removeLst(x,L) = reverse(removeFst(x,reverse(L)));


removeLst(2, [1,2,3,4,2,5,2]); (* x is present in the list*)
removeLst(6, [1,2,3,4,2,5,2]); (* x is present in the list*)