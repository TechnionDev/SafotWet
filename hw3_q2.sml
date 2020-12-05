fun to_binary 0 = nil
  |to_binary num = (num mod 2)::to_binary(num div 2);

local 
	fun num_of_ones (1::xs) = 1+num_of_ones(xs)
	    |num_of_ones (0::xs) = num_of_ones(xs)
	    |num_of_ones (nil) = 0;

	fun num_of_zeros (1::xs) = num_of_zeros(xs)
	    |num_of_zeros (0::xs) = 1+num_of_zeros(xs)
	    |num_of_zeros (nil) = 0;

	fun encode_list (num_ones,num_zeros,x::xs,index) = 
	  if num_ones = num_zeros then (x::xs)@to_binary(index)
	   else if x=1 then 0::encode_list(num_ones-1,num_zeros+1,xs,index+1)
	  else 1::encode_list(num_ones+1,num_zeros-1,xs,index+1)
	  |encode_list (num_ones,num_zeros,nil,index) = nil;
in
	fun encode (x::xs) =  encode_list(num_of_ones(x::xs),num_of_zeros (x::xs),x::xs,0)
	  |encode nil = [];
end;

local
  fun to_decimal ((x::xs),exp) = x*floor(Math.pow(2.0,real(exp)))+to_decimal(xs,exp+1)
    |to_decimal (nil,exp) = 0;

  fun concat_h((x::xs),num) = if num <> 0 then x::concat_h((xs),num-1)
  else nil
    |concat_h(nil,num) = nil;

  fun concat_t((x::xs),num) = if num <> 0 then concat_t((xs),num-1)
  else x::xs
    |concat_t(nil,num) = nil;

  fun switch((1::xs),num) = if num <> 0 then 0::switch((xs),num-1)
  else 1::xs
    |switch((0::xs),num) = if num <> 0 then 1::switch((xs),num-1)
  else 0::xs;
in
  fun decode ((x::xs),num) = switch(concat_h((x::xs),num),to_decimal(concat_t((x::xs),num),0));
end;