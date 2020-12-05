datatype ('a,'b) heterolist = Nil
	| ::: of 'a*('b, 'a) heterolist;



fun build4 (x, one, y, two) = x:::one:::y:::two:::Nil;


local 
  fun make_list Nil = nil
    |make_list (x:::Nil) = x::nil
    |make_list (x:::xs:::xss) = x::make_list (xss);
in
fun unzip (x:::xs) = (make_list(x:::xs),make_list(xs)) ;
end;

exception Empty;

local
  fun 
    make_hetrolist (nil,nil) = Nil
    |make_hetrolist (x,nil) = raise Empty
    |make_hetrolist (nil,y) = raise Empty
    
    |make_hetrolist (x::xs,y::ys) = x:::y:::make_hetrolist (xs,ys);
in
fun zip (list_a,list_b) = make_hetrolist (list_a,list_b);
end;