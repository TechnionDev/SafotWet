(* Daniel Bondar 206560856 danielbondar@campus.technion.ac.il  Gur Telem 206631848 gurt@campus.technion.ac.il *)

exception MlispError;

local
exception PresumablyImpossibleError;
fun isBuiltin (x) =    if x = "+" then true
                    else if x = "-" then true
                    else if x = "*" then true
                    else if x = "div" then true
                    else if x = "cons" then true
                    else if x = "car" then true
                    else if x = "cdr" then true
                    else if x = "define" then true
                    else false;

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
                                (bind_t (fb, pb, defineNested s env pa))
                            |_ => raise MlispError
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
                CONS _ => (bind_t (a, p, pushEnv (initEnv()) env))
                |_ => raise MlispError)
        |_ => raise MlispError
    ;


fun first (a, b) = a;


fun eval_t(s:SExp, env) = 
    case s of
    ATOM a => 
        (case a of
            SYMBOL sym => if sym = "nil" then
                    (ATOM NIL, env)
                else
                    (case find sym env of
                        ATOM (NUMBER n) => (ATOM (NUMBER n), env)
                        |ATOM NIL => (ATOM NIL, env)
                        |ATOM (SYMBOL sym) => (first (eval_t (ATOM (SYMBOL sym), env)), env)
                        |CONS cons => (first (eval_t (CONS cons, env)), env)
                    )
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
                                    CONS (p2, ATOM NIL) => (CONS (first (eval_t (p1, env)), first (eval_t(p2,env))), env)
                                    |_                  => raise MlispError
                            )|_ => raise MlispError
                    )
                    else if fname = "car" then (
                        case p of
                            CONS (p1, ATOM NIL)    => (
                                case first (eval_t (p1, env)) of
                                    CONS (car, _) => (car, env)
                                    |_ => raise MlispError
                                )
                            |_ => raise MlispError
                    )
                    else if fname = "cdr" then (
                        case p of
                            CONS (p1, ATOM NIL)    => (
                                case first (eval_t (p1, env)) of
                                    CONS (_, cdr) => (cdr, env)
                                    |_ => raise MlispError
                                )
                            |_ => raise MlispError
                    )
                    else if fname = "define" then (
                        case p of
                            (* Handle define of SExp *)
                            CONS (ATOM (SYMBOL p1), CONS (p2, ATOM NIL)) => (
                                ATOM NIL, defineNested p1 env p2
                            (* Handle function define *)
                            )|CONS (ATOM (SYMBOL p1), CONS (CONS p2, CONS(p3, ATOM NIL))) => (
                                ATOM NIL, defineNested p1 env (CONS (CONS p2, p3))
                            )|_ => raise MlispError
                    )
                    else raise PresumablyImpossibleError
                else
                    (case find fname env of
                        CONS (params, body) => (case (eval_t (body, bind (find fname env, p, env))) of
                            (res, _) => (res, env))
                        |_ => raise MlispError
                    )
                |_ => raise MlispError
            )
            |_ => raise MlispError
          );
in
fun eval s env = eval_t(s, env) handle _ => raise MlispError
end;


"here";
val (res,env) = (eval (parse (tokenize "(define pi 3)")) (emptyNestedEnv ()));
val (res,env) = (eval (parse (tokenize "(define identity (r) r)")) env);
val (res,env) = (eval (parse (tokenize "(identity (cons 2 (cons pi nil))")) env);
res = ATOM (NUMBER 1);

val (res,env) = (eval (parse (tokenize "(identity nil)")) env);
res = ATOM (NUMBER 2);

val (res,env) = (eval (parse (tokenize "(define pi 3)")) env);
val (res,env) = (eval (parse (tokenize "(define area (r) (* pi (* r r)))")) env);
val (res,env) = (eval (parse (tokenize "(define pi 7)")) env);
val (res,env) = (eval (parse (tokenize "(area 1)")) env);
res = ATOM (NUMBER 7);

val (res,env) = (eval (parse (tokenize "(area (+ 1 1))")) env);
res = ATOM (NUMBER 28);

val (res,env) = (eval (parse (tokenize "(define pi 3)")) env);
val (res,env) = (eval (parse (tokenize "(define area (r p) (* pi (* p r)))")) env);
val (res,env) = (eval (parse (tokenize "(area (+ 1 1) 5)")) env);
res = ATOM (NUMBER 30);

val (res,env) = (eval (parse (tokenize "(define pi car (cons 1 (cons 2 nil)))")) env);
val (res,env) = (eval (parse (tokenize "(equal pi)")) env);
res = ATOM (NUMBER 1);

raise Undefined;