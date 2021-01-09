use 'mlisp.sml';
use 'hw3_q3.sml';

exception MlispError;

(* // TODO: put in local *)
fun isUser (x) = true;


(* // TODO: Remove this shit *)
val n = (NUMBER 5);
val s = (SYMBOL "n");
val nn = NIL;
val c = CONS (ATOM n, ATOM s);
val a = ATOM s;


(* // TODO: put in local and finish *)
fun bind (p:SExp, env:(string -> SExp) list) = env;


fun eval_t(s:SExp, env) = 
    case s of
    ATOM a => 
        (case a of
            SYMBOL sym => ((find sym env), env)
            |NUMBER num => (s, env)
            |NIL => (s, env))
    | CONS (f,p) => 
          (case f of
            ATOM a => 
            (case a of 
              SYMBOL str => eval_t (find str env, bind (p, env))
              |_ => raise MlispError
            )
          )
    | _ => raise MlispError;

val e = emptyNestedEnv();
val e = defineNested "s" e (ATOM s);
val e = defineNested "n" e (ATOM n);

eval_t (a, e);


fun eval(s:NIL, env) = (s, env)
    | eval(s:NUMBER, env) = (s, env)
    | eval(s:SYMBOL, env) = eval (find s env, env); (* // TODO: probably fix this *)
    (*
    | eval(f::xs, env) =if isUser then
                            (((find (eval (f, env)) env) xs), env) (* // TODO: Bind (define) actual to formal in a new env scope *)
                        else
                            (NIL, env);

                            *)