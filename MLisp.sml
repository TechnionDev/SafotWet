datatype Atom =
   SYMBOL of string | NUMBER of int | NIL;
datatype SExp =
   ATOM of Atom | CONS of (SExp * SExp);

fun isNumber (str:string) = 
	if size(str)=0
		then true
	else if Char.isDigit(String.sub(str,0)) 
		then isNumber (String.extract(str,1,NONE)) 
	else false;
	
fun asDigit ch = ord(ch)-ord(#"0");

fun atoiHelper (str, ind) = 
    if ind=(~1) then 
        0
    else
        asDigit(String.sub(str, ind)) + (10 * atoiHelper(str, ind-1));

fun atoi (str:string) = 
  if  isNumber(str) then
      atoiHelper(str, size(str)-1)
  else 0;

fun tokenize (str:string) = String.tokens (fn c => c = #" ") 
				(String.translate (fn c => if c = #"("then " ( "
                                            else if c = #")" then " ) "
											else String.str(c)) str);

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
