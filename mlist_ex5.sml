exception MlispError;
exception PresumablyImpossibleError;

(* // TODO: put in local *)
fun isBuiltin (x) =    if x = "+" then true
                    else if x = "-" then true
                    else if x = "*" then true
                    else if x = "div" then true
                    else if x = "cons" then true
                    else if x = "car" then true
                    else if x = "cdr" then true
                    else if x = "define" then true
                    else false;


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
                                |_ => raise MlispError) |_ => raise MlispError
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


fun first (a, b) = a;


fun eval_t(s:SExp, env) = 
    case s of
    ATOM a => 
        (case a of
            SYMBOL sym => ( (find sym env), env) (* Eval and not just return it *)
            |NUMBER num => (s, env)
            |NIL => (s, env))
    | CONS (f,p) => 
          (case f of
            ATOM a => 
            (case a of 
                SYMBOL fname => 
                (* Handle builtins first *)
                if isBuiltin fname then
                    if fname = "+" then (
                        case p of
                            CONS (p1, p2c) => (
                                case p2c of
                                    CONS (p2, ATOM NIL) => (
                                        case first (eval_t (p1, env)) of
                                            ATOM p1a => (
                                                case p1a of 
                                                    NUMBER n1 => (
                                                        case first (eval_t (p2, env)) of
                                                            ATOM p2a => (
                                                                case p2a of 
                                                                    NUMBER n2 => (
                                                                        (ATOM (NUMBER (n1+n2)), env)
                                                                    )|_ => raise MlispError
                                                            )|_ => raise MlispError
                                                    )|_ => raise MlispError
                                            )|_ => raise MlispError
                                    )|_ => raise MlispError
                            )|_ => raise MlispError
                    )
                    else if fname = "-" then (
                        case p of
                            CONS (p1, p2c) => (
                                case p2c of
                                    CONS (p2, ATOM NIL) => (
                                        case first (eval_t (p1, env)) of
                                            ATOM p1a => (
                                                case p1a of 
                                                    NUMBER n1 => (
                                                        case first (eval_t (p2, env)) of
                                                            ATOM p2a => (
                                                                case p2a of 
                                                                    NUMBER n2 => (
                                                                        (ATOM (NUMBER (n1-n2)), env)
                                                                    )|_ => raise MlispError
                                                            )|_ => raise MlispError
                                                    )|_ => raise MlispError
                                            )|_ => raise MlispError
                                    )|_ => raise MlispError
                            )|_ => raise MlispError
                    )
                    else if fname = "*" then (
                        case p of
                            CONS (p1, p2c) => (
                                case p2c of
                                    CONS (p2, ATOM NIL) => (
                                        case first (eval_t (p1, env)) of
                                            ATOM p1a => (
                                                case p1a of 
                                                    NUMBER n1 => (
                                                        case first (eval_t (p2, env)) of
                                                            ATOM p2a => (
                                                                case p2a of 
                                                                    NUMBER n2 => (
                                                                        (ATOM (NUMBER (n1*n2)), env)
                                                                    )|_ => raise MlispError
                                                            )|_ => raise MlispError
                                                    )|_ => raise MlispError
                                            )|_ => raise MlispError
                                    )|_ => raise MlispError
                            )|_ => raise MlispError
                    )
                    else if fname = "div" then (
                        case p of
                            CONS (p1, p2c) => (
                                case p2c of
                                    CONS (p2, ATOM NIL) => (
                                        case first (eval_t (p1, env)) of
                                            ATOM p1a => (
                                                case p1a of 
                                                    NUMBER n1 => (
                                                        case first (eval_t (p2, env)) of
                                                            ATOM p2a => (
                                                                case p2a of 
                                                                    NUMBER n2 => (
                                                                        (ATOM (NUMBER (n1 div n2)), env)
                                                                    )|_ => raise MlispError
                                                            )|_ => raise MlispError
                                                    )|_ => raise MlispError
                                            )|_ => raise MlispError
                                    )|_ => raise MlispError
                            )|_ => raise MlispError
                    )
                    else if fname = "cons" then (
                        case p of
                            CONS (p1, p2c) => (
                                case p2c of
                                    CONS (p2, ATOM NIL) => (CONS (p1, p2), env)
                                    |_                  => raise MlispError
                            )|_ => raise MlispError
                    )
                    else if fname = "car" then (
                        case p of
                            CONS (p1, _)    => (p1, env)
                            |_              => raise MlispError
                    )
                    else if fname = "cdr" then (
                        case p of
                            CONS (_, p2c) => (
                                case p2c of
                                    CONS (p2, ATOM NIL) => (p2, env)
                                    |_                  => raise MlispError
                            )|_ => raise MlispError
                    )
                    else if fname = "define" then (
                        case p of
                            CONS (p1, p2c) => (
                                case p2c of
                                    CONS (p2, ATOM NIL) => (
                                        s,env (* TODO: Implement *)
                                    )|_ => raise MlispError
                            )|_ => raise MlispError
                    )
                    else raise PresumablyImpossibleError
                else
                    (case find fname env of
                        CONS (_, body) => (eval_t (body, bind (find fname env, p, env)))
                        |_ => raise MlispError
                    )
                |_ => raise MlispError
            )
            |_ => raise MlispError
          )
    | _ => raise MlispError;


fun eval s env = 
    case s of
        SExp => eval_t(s, env)
        |_ => raise MlispError;


(* // TODO: Remove this *)
val env: (string -> SExp) list = emptyNestedEnv();
val env = defineNested "s" env (ATOM s);
val env = defineNested "n" env (ATOM n);
val env = pushEnv (initEnv()) env;
val env = defineNested "pi" env (ATOM (NUMBER 3));

val env = defineNested "area" env (CONS (CONS(ATOM (SYMBOL "r"), ATOM NIL),ATOM(NUMBER 5)));


val (res,env) = (eval (parse (tokenize "(define pi 3)")) env);

val e: (string -> SExp) list = emptyNestedEnv();
val e = defineNested "s" e (ATOM s);
val e = defineNested "n" e (ATOM n);
val env = pushEnv (initEnv()) e;
val env = defineNested "pi" env (ATOM (NUMBER 3));

val env = defineNested "area" env (CONS (CONS(ATOM (SYMBOL "r"), ATOM NIL),ATOM (SYMBOL "pi")));

(* val (res,env) = (eval (parse (tokenize "(define area (r) ( * pi ( * r r)))")) env);  *)

parse (tokenize "(+ 2 3)");

val (res,env) = (eval (parse (tokenize "(+ 7 3)")) env);
val (res,env) = (eval (parse (tokenize "(cons 7 3)")) env);