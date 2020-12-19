Daniel Bondar 206560856 danielbondar@campus.technion.ac.il  Gur Telem 206631848 gurt@campus.technion.ac.il

local
fun get_tail(x::xs, depth) =  if x=")" then 
                                 if depth=0 then 
                                    xs 
                                 else 
                                    get_tail (xs, depth-1)
                              else if x="(" then
                                 get_tail(xs, depth+1)
                              else
                                 get_tail (xs, depth)
   | get_tail(nil, depth) = nil;

fun real_parse (x::xs) = if isNumber(x)=true then CONS (ATOM (NUMBER (atoi (x))), real_parse xs)
                        else if x = "(" then CONS (real_parse xs,real_parse (get_tail (xs,0)))
                        else if x = ")" then ATOM (NIL)
                        else CONS (ATOM (SYMBOL x), real_parse xs)
   |real_parse (nil) = ATOM (NIL);

fun drop_final (x::nil) = nil
  | drop_final (x::xs) = x::drop_final xs
  | drop_final (nil) = nil;
in
fun parse ((x:string)::xs) =  if x = "(" then real_parse (drop_final(xs))
                              else ATOM (SYMBOL x)
  |parse nil = ATOM (NIL);
end;
