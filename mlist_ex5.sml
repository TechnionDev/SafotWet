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

fun bind_t(f:SExp, p:SExp, env:(string -> SExp) list) =
    (case f of
        ATOM NIL => (case p of ATOM NIL => env |_ => raise MlispError)
        |CONS (fa,fb) => 
            (case fa of
                ATOM fat => (
                    case fat of
                        SYMBOL s =>
                        (case p of
                            CONS (pa, pb) => 
                            (case pa of ATOM pat => (case pat of NUMBER _ =>  
                                (bind_t (fb, pb, defineNested s env pa))
                                | _ => raise MlispError) | _ => raise MlispError
                            )|_ => raise MlispError
                        )
                        |_ => raise MlispError
                )
                |_ => raise MlispError
            )
        |_ => raise MlispError
    );


fun bind (f, p:SExp, env:(string -> SExp) list) = 
    case f of
        CONS (a,_) => 
            (case a of
                CONS b => (bind_t (a, p, pushEnv (initEnv()) env))
                |_ => raise MlispError)
        |_ => raise MlispError
    ;




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
                SYMBOL fname => 
                (* Handle builtins first *)
                if isUser fname then
                
                else
                eval_t (find fname env, bind (find fname env, p, env))
                |_ => raise MlispError
            )
            |_ => raise MlispError
          )
    | _ => raise MlispError;


fun eval s env = 
    case s of
        SExp => eval_t(s, env)
        |_ => raise MlispError;


val env = emptyNestedEnv();

val (res,env) = (eval (parse (tokenize "(define pi 3)")) env);
