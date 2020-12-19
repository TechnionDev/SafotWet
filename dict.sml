Daniel Bondar 206560856 danielbondar@campus.technion.ac.il  Gur Telem 206631848 gurt@campus.technion.ac.il

datatype ('a, 'b) dictionary = 
    Nil
  | Dict of {key: 'a, value: 'b} list;
exception ItemNotPresent;

local
fun insert_pair (nil,k,v) = {key=k,value=v}::nil
  |insert_pair (x::xs,k,v) = if k = #key(x) then {key=k,value=v}::xs
                                else x::insert_pair ((xs),k,v);
fun mid (Dict(x::xs),k,v) = Dict(insert_pair (x::xs,k,v))
  | mid (Nil,k,v) = Dict [{key=k,value=v}];
in
fun insert dic = fn k => fn v => mid(dic,k,v);
end;

local
fun find_key (Dict(x::xs),k) = if k = #key(x)then #value(x) else find_key (Dict(xs),k)
  |find_key (Dict(nil),k) = raise ItemNotPresent
  |find_key (Nil,k) = raise ItemNotPresent;
in
fun find dic = fn k => find_key(dic,k);
end;


local
fun remove_pair (Dict(nil),k) = raise ItemNotPresent
  |remove_pair (Nil,k) = raise ItemNotPresent
  |remove_pair (Dict(x::xs),k) = if k = #key(x) then xs else (x::remove_pair(Dict xs,k));
fun conv (Dict(x::nil),k) = if k = #key(x) then Nil else raise ItemNotPresent
  |conv (Dict(nil),k) = raise ItemNotPresent
  |conv (Nil,k) = raise ItemNotPresent
  |conv (Dict(x::xs),k) = Dict(remove_pair (Dict(x::xs),k))
in
  fun remove dic = fn k => conv(dic,k);
end;

fun keys (Dict(x::xs)) = #key(x)::keys (Dict(xs))
  |keys (Dict(nil)) = nil
  |keys Nil = nil;

fun values (Dict(x::xs)) = #value(x)::values (Dict(xs))
  |values (Dict(nil)) = nil
  |values Nil = nil;