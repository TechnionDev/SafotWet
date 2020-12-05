datatype 'a HashTableEntry = Empty
    | Deleted
    | Value of int * 'a;

type 'a HashTable = 'a HashTableEntry list;

fun create 0 = nil
  |create len = Empty::create(len-1);

fun insert_t (f, ht, i, c, tail, Value(k,v)::xs,key,value) = 
if c = 0 then 
  if key = k then 
    tail@(Value(key,value)::xs)
  else
    insert_t(f,ht, f(i), f(i), [], ht, key, value)
else 
  insert_t(f,ht,i,c-1,tail@[Value(k,v)], xs,key,value)
  |insert_t(f, ht,i,c,tail,Deleted::xs,key,value) = 
    insert_t(f,ht, f(i), f(i), [], ht, key, value)
  |insert_t(f, ht,i,c,tail,x::xs,key,value) = 
    if c = 0 then
      tail@Value(key,value)::xs
    else
      insert_t (f,ht,i,c-1,tail@[x],xs,key,value)
  |insert_t(f,ht,i,c,tail,nil,key,value) = nil
  |insert_t(f,nil,i,c,tail,ht,key,value) = nil;



fun insert f = fn hash_table => fn (key,value) => insert_t(f,hash_table,f(key), f(key),[],hash_table,key,value);

fun get_t (f, ht, oi, Value(pk, pv)::ct, i, k) = 
  if i = 0 then
    if k = pk then
      pv
    else
      get_t(f, ht, f(oi), ht, f(oi), k)
  else
    get_t(f, ht, oi, ct, i-1, k)
  | get_t (f, ht, oi, Deleted::ct, i, k) = get_t(f, ht, f(oi), ht, f(oi), k)
  | get_t (f, ht, oi, Empty::ct, i, k) = get_t(f, ht, oi, ct, i-1, k)
  ;


fun get f = fn ht => fn key => get_t(f, ht, f(key), ht, f(key), key);


fun remove_t (f, ht, oi, rt, Value(pk, pv)::ct, i, k) = 
  if i = 0 then
    if k = pk then
      rt@Deleted::ct
    else
      remove_t(f, ht, f(oi), [], ht, f(oi), k)
  else
    remove_t(f, ht, oi, rt@[Value(pk, pv)], ct, i-1, k)
  | remove_t (f, ht, oi, rt, Deleted::ct, i, k) = remove_t(f, ht, f(oi), [], ht, f(oi), k)
  | remove_t (f, ht, oi, rt, Empty::ct, i, k) = remove_t(f, ht, oi, rt@[Empty], ct, i-1, k)
  ;


fun remove f = fn ht => fn key => remove_t(f, ht, f(key), [], ht, f(key), key);
