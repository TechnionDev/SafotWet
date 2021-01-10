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
            SYMBOL sym => if sym = "nil" then
                    (ATOM NIL, env)
                else
                    ((find sym env), env) (* TODO: Eval and not just return it *)
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
                            (* Handle const define *)
                            CONS (ATOM (SYMBOL p1), CONS (p2, ATOM NIL)) => (
                                ATOM NIL, defineNested p1 env p2
                                (* ATOM NIL, defineNested fname env (CONS (p1, p2)) *)
                            )|CONS (ATOM (SYMBOL p1), CONS (CONS p2, CONS(CONS p3, ATOM NIL))) => (
                                ATOM NIL, defineNested fname env (CONS (CONS p2, CONS p3))
                            )|_ => raise MlispError (* TODO: Define for funsions *)
                    )
                    else raise PresumablyImpossibleError
                else
                    (case find fname env of
                        CONS (_, body) => (eval_t (body, bind (find fname env, p, env))) (* // TODO: popEnv *)
                        |_ => raise MlispError
                    )
                |_ => raise MlispError
            )
            |_ => raise MlispError
          );


fun eval s env = eval_t(s, env);


(* // TODO: Remove this *)

val (res,env) = (eval (parse (tokenize "(define calc (p) (+ p 3))")) (emptyNestedEnv ()));
val (res,env) = (eval (parse (tokenize "(define pi 3)")) env);

(* val (res,env) = (eval (parse (tokenize "(define area (r) ( * pi ( * r r)))")) env);  *)
val aaa="here";
val (res, env) = eval (parse (tokenize "(calc 1)")) env;
val (res, env) = eval (parse (tokenize "(+ 5 pi)")) env;

val (res, env) = eval (parse (tokenize "(define hello nil)")) env;

val (res,env) = eval (parse (tokenize "(+ 2 3)")) (emptyNestedEnv ()); 
res = ATOM (NUMBER 5);
val (res,env) = eval (parse (tokenize "(- 2 3)")) (emptyNestedEnv ());
res = ATOM (NUMBER ~1);
val (res,env) = eval (parse (tokenize "(* 2 3)")) (emptyNestedEnv ());
res = ATOM (NUMBER 6);
val (res,env) = eval (parse (tokenize "(div 10 2)")) (emptyNestedEnv ());
res = ATOM (NUMBER 5);
val (res,env) = eval (parse (tokenize "(cons 1 2)")) (emptyNestedEnv ());
res = CONS (ATOM (NUMBER 1),ATOM (NUMBER 2));
val (res,env) = eval (parse (tokenize "(cons 1 (cons 2 3))")) (emptyNestedEnv ());
res = CONS (ATOM (NUMBER 1),CONS (ATOM (NUMBER 2),ATOM (NUMBER 3)));
val (res,env) = eval (parse (tokenize "(cons 1 (cons 2 (cons 3 nil)))")) (emptyNestedEnv ());
res =
  CONS
    (ATOM (NUMBER 1),CONS (ATOM (NUMBER 2),CONS (ATOM (NUMBER 3),ATOM NIL)));
val (res,env) = eval (parse (tokenize "(car (cons 1 (cons 2 nil)))")) (emptyNestedEnv ());
res = ATOM (NUMBER 1);
val (res,env) = eval (parse (tokenize "(cdr (cons 1 (cons 2 nil)))")) (emptyNestedEnv ());
res = CONS (ATOM (NUMBER 2),ATOM NIL);


exception END;
raise END;