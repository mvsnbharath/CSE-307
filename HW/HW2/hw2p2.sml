(* HW2 P2*)

(* 2a) revCycle(L) *)
fun concat([],y) =  y
|concat(x,y)= hd(x)::concat(tl(x),y);

fun revCycle([]) = []
	|revCycle(L) = concat(tl(L),[hd(L)]);

(* 2 Inputs: *)
revCycle([1,2,3,4,5,6]);
revCycle([6, 5, 4, 3, 2, 1]);


(* 2a) revCycles(L, i)  *)
fun revCycles(L, 0)=L
	|revCycles(L, i) = revCycles(revCycle(L),i-1);

(* 2 Inputs: *)
val y = [1,2,3,4,5,6];
revCycles(y,4);
revCycles(y,6);