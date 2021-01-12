(* Daniel Bondar 206560856 danielbondar@campus.technion.ac.il  Gur Telem 206631848 gurt@campus.technion.ac.il *)

datatype 'a seq = Nil | Cons of 'a * (unit-> 'a seq);

local
fun real_arithmeticSeq (a1,d) = Cons(a1,fn()=> real_arithmeticSeq (a1+d,d));
in
fun arithmeticSeq a1 = fn d => real_arithmeticSeq (a1,d);
end;

local
fun real_getSubSeq (Nil : int seq ,s, e)= nil 
  |real_getSubSeq ((Cons(x,xf)) ,s, e) = if s <= 1 then 
                                              if e >0 then
                                              x::real_getSubSeq (xf() ,s, e-1)
                                              else nil
                                          else
                                              real_getSubSeq (xf() ,s-1, e-1);
in
fun getSubSeq (Nil : int seq) s e= nil 
  |getSubSeq  (Cons(x,xf)) s e = real_getSubSeq ((Cons(x,xf)) ,s, e);
end;

local
fun real_getKDivElems ((Nil : int seq),n,k) = nil
  |real_getKDivElems ((Cons(x,xf)),n,k) = if n > 0 then
                                              if (x mod k) = 0 then 
                                                  x::real_getKDivElems (xf(),n-1,k)
                                               else real_getKDivElems (xf(),n,k)
                                          else nil
in
fun getKDivElems (Nil : int seq) n k = nil
  |getKDivElems (Cons(x,xf)) n k = real_getKDivElems ((Cons(x,xf)),n,k);
end;



datatype 'a lazyTree = tNil | tCons of 'a * (unit -> 'a lazyTree) * (unit -> 'a lazyTree);

fun lazyTreeFrom a1= tCons(a1,(fn()=>lazyTreeFrom (a1*2)),(fn()=>lazyTreeFrom ((a1+1)*2)));
lazyTreeFrom(1);

fun lazyTreeMap (f,tNil) = tNil
  |lazyTreeMap (f,(tCons(a1,fa,fb))) = tCons(f(a1),fn()=>lazyTreeMap (f,fa()),fn()=>lazyTreeMap (f,fb()));

fun lazyTreeFilter (f,tNil) = tNil
  |lazyTreeFilter (f,(tCons(a1,fa,fb))) = if f(a1) = true then 
                                              tCons(a1,fn()=>lazyTreeFilter (f,fa()),fn()=>lazyTreeFilter (f,fb()))
                                          else tNil;

