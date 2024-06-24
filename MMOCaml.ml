open MSyntax
let emptyenv () = [] (*handle environment empty environment *)
let ext(*extend*) env x v(*3 argus env variable and value*) = (x,v) :: env (*add variable x and its value v pair to env*)               
let rec lookup (x) env =
  match env with
    | [] -> failwith ("unbound variable: " ^ x)
    | (y,v)::tl -> if x = y  then v 
  else lookup x tl 
let rec eval0 term env =
  let rec find_cases v0 caseList env =  
    match caseList with
    | [] -> failwith ("fail with pattern matching")
    | (pattern,term)::caseList -> (*c is pair of pattern and term so we replace *)
     (*the pair of exp var and pattern var*)
     try let env1 = compare_pattern0 v0 pattern env in
      eval0 term env1 
      with _ -> find_cases v0 caseList env 
    in     
   match term with
    | IntCons(n) -> IntVal(n)
    | Plus(term1, term2) -> 
      (match (eval0 term1 env, eval0 term2 env) with
      | (IntVal(n1), IntVal(n2)) -> IntVal(n1 + n2)
      | _ -> failwith "Integer values expected")
    | Times(term1, term2) -> 
      (match (eval0 term1 env, eval0 term2 env) with
      | (IntVal(n1), IntVal(n2)) -> IntVal(n1 * n2)
      | _ -> failwith "Integer values expected")
    | Div(term1, term2) -> 
      (match (eval0 term1 env, eval0 term2 env) with
        | (IntVal(n1), IntVal(n2)) -> if n2 != 0 then IntVal(n1 / n2) else failwith "Division by zero"
        | _ -> failwith "Integer values expected")
    | Minus(term1, term2) -> 
        (match (eval0 term1 env, eval0 term2 env) with
        | (IntVal(n1), IntVal(n2)) -> IntVal(n1 - n2)
        | _ -> failwith "Integer values expected")
    | Greater(term1, term2) -> 
      (match (eval0 term1 env, eval0 term2 env) with
      | (IntVal(n1), IntVal(n2)) -> BoolVal(n1 > n2)
      | _ -> failwith "Integer values expected")
    | Less(term1, term2) -> 
      (match (eval0 term1 env, eval0 term2 env) with
      | (IntVal(n1), IntVal(n2)) -> BoolVal(n1 < n2)
      | _ -> failwith "Integer values expected")
    | Eq(term1,term2) ->
        begin
        match (eval0 term1 env, eval0 term2 env) with
          |   (IntVal(n1),IntVal(n2)) -> BoolVal(n1=n2)
          |   (BoolVal(b1),BoolVal(b2)) -> BoolVal(b1=b2)
          |   _ -> failwith "wrong value"
        end
    | BoolCons(b) -> BoolVal(b)
    | If(term1(*condition*),term2,term3) ->
        begin
        match (eval0 term1(*if e1 is true then evaluate e2*)env) with
          | BoolVal(true) -> eval0 term2 env
          | BoolVal(false) -> eval0 term3 env
          | _ -> failwith "wrong value"
        end

    | Var(x)    -> lookup x env
    | Fun(x, term1) -> FunVal(x, term1, env)
    | App(term1,term2) ->
      let funpart = (eval0 term1 env) in
      let arg = (eval0 term2 env) in
        begin
         match funpart with
         | FunVal(x,body,env1) ->
            let env2 = (ext env1 x arg) in
            eval0 body env2
         |LetRecVal0 (f,x,body,env1) ->
              let env2 = (ext (ext env1 x arg) f funpart) in
              eval0 body env2
         | _ -> failwith "wrong value in App"
        end
        
    | LetRec(f, x, term1, term2) ->
        let env1 = ext env f (LetRecVal0 (f, x, term1, env)) in 
        eval0 term2 env1
  
    | Match(term1, caseList) ->   (*match e with
                                    | x + y ->   Not OK
                                    | (x,y) ->   OK
                                    | <$x $y> ->   OK
                                    | < $x + $y > -> OK
                                    | < fun x -> ...  OK
                                    | < ($x,$y) > ->   OK*)
      let vt = eval0 term1 env in 
      find_cases vt caseList env 

    | Code (term) -> (*the stage of Code(term) is 0 so the stage of term inside the code is only 1*)
        let v = evali term env 1 in 
        CodeVal(v)
        (*we need to carry the env of stage zero to eval stage i because if we find out splice in code we need to use this env*)
        (*v1 is the value of the term inside code type of v1 is the term we need to lift v1 into value at stage 0 *)
      (*eg. let x = <1+2> in <$x * 3>*)

    | Splice (term) -> 
      failwith ("Splice at level 0 is not allowed")(*at stage zero splice must not be appered*)
    
    | Run (term) -> (*!<e> -> e *)
      let v0 = eval0 term env in (*we need to evaluate the term first v0 must be a code bcuz the argu of run must be a code*)
      begin (* we need to check v0 whether it is code value or not*)
        match v0 with
        | CodeVal(term) -> 
          let env1 = emptyenv() in
           eval0 term env1 
        | _ -> failwith ("failwith Run")     
      end  
      
    | Pair (term1, term2) ->
       let v1 = eval0 term1 env in 
       let v2 = eval0 term2 env in
       PairVal(v1,v2)
  and 
  compare_pattern0 v0 pattern env = (*penv is a list of pairs of exp var and pattern var (x,y)*)
  match (v0,pattern) with (*if the corresponding var of x is in lst y must be the same with this  *)
  
    | (v0,(PVar(x))) -> 
      let env1 = ext env x v0 in
      env1

    | (IntVal(v0), (PIntCons(int))) -> 
      if v0 = int 
        then env 
      else failwith("failwith integer value")

    | (BoolVal(v0), (PBoolCons(bool))) -> 
      if v0 = bool
        then env 
      else failwith("failwith integer value")

    | (CodeVal(vi), (PCode(pattern))) -> (*CodeVal(vi) has the type value 0*)(*match <t12> with
                                              | <pattern> -> env*)
        let penvi = emptyenv() in
        compare_patterni vi pattern env penvi 1 (*vi is term penvi is needed to check the occourance of variable in pattern *)

    | (PairVal(v0,v01), PPair(pattern1,pattern2)) -> 
        let env1 = compare_pattern0 v0 pattern1 env in 
        let env2 = compare_pattern0 v01 pattern2 env1 in 
        env2
    
    | _ -> failwith ("failwith pattern")  

    and 

    evali term env i=
     
    let rec remove_splices caseList env i =
      match caseList with
        | [] -> [] 
        | (pattern,term)::caseList -> (*c is pair of pattern and term so we replace *)
                                       (*we should check the splice for term*)
          let v = evali term env i in
          let caseList1 = remove_splices caseList env i in
          (pattern, v)::caseList1
     in    
   
      match term with
        | IntCons(n) -> IntCons(n) (*we would like to replace the value at stage i with term because the definition are exactly same*)
        | BoolCons(b) -> BoolCons(b)
        | Var(x)  ->  Var(x)

        | Plus(term1, term2) -> (*<1 + $(<2+3>)> to eval this we need to focus on splice we need to decrease the stage*)
          let v1 = evali term1 env i in
          let v2 = evali term2 env i in(*we need to check the occourance of splice in term1 and 2 of plus*)
          Plus(v1,v2) (* plus is the constructor of term*)
   
       | Times(term1, term2) -> 
          let v1 = evali term1 env i in
          let v2 = evali term2 env i in(*we need to check the occourance of splice in term1 and 2 of plus*)
          Times(v1,v2) (* Times is the constructor of term*)
   
       | Div(term1, term2) -> 
          let v1 = evali term1 env i in
          let v2 = evali term2 env i in(*we need to check the occourance of splice in term1 and 2 of plus*)
          Div(v1,v2) (* Div is the constructor of term*)
   
       | Minus(term1, term2) -> 
          let v1 = evali term1 env i in
          let v2 = evali term2 env i in(*we need to check the occourance of splice in term1 and 2 of plus*)
          Minus(v1,v2) (* minus is the constructor of term*)
   
       | Greater(term1, term2) -> 
          let v1 = evali term1 env i in
          let v2 = evali term2 env i in(*we need to check the occourance of splice in term1 and 2 of plus*)
          Greater(v1,v2) (* Greater is the constructor of term*)
   
       | Less(term1, term2) -> 
          let v1 = evali term1 env i in
          let v2 = evali term2 env i in(*we need to check the occourance of splice in term1 and 2 of plus*)
          Less(v1,v2) (* Less is the constructor of term*)
   
       | Eq(term1,term2) ->
          let v1 = evali term1 env i in
          let v2 = evali term2 env i in(*we need to check the occourance of splice in term1 and 2 of plus*)
          Eq(v1,v2) (* Eq is the constructor of term*)
   
       | If(term1(*condition*),term2,term3) ->
          let v1 = evali term1 env i in 
          let v2 = evali term2 env i in 
          let v3 = evali term3 env i in 
         If(v1,v2,v3)
   
       | Fun(x, term1) -> 
         let v1 = evali term1 env i in
         Fun(x, v1)
   
       | App(term1,term2) ->
         let v1 = evali term1 env i in
         let v2 = evali term2 env i in
         App(v1, v2)
   
       | LetRec(f, x, term1, term2) ->
         let v1 = evali term1 env i in
         let v2 = evali term2 env i in
         LetRec(f, x, v1, v2)
     
       | Match(term1, caseList) ->
         let v1 = evali term1 env i in 
         let updated_caseList = remove_splices caseList env i in 
         Match(v1, updated_caseList)
       
       | Code (term) -> 
         let v = evali term env (i+1) in (*we need to increase stage number for code*)
         Code (v)
       
       | Splice (term) -> 
         if i > 1 then 
           evali term env (i-1) (*we need to decrease stage number for splice*)
         else 
         let v0 = eval0 term env in (*return value of evali must be a term not value0*)
         begin 
           match v0 with (* $(<term>)->term*)
           | CodeVal(term) -> term 
           | _ -> failwith ("fail with splice")
         end 
   
       | Run (term) -> 
         let v = evali term env i in 
         Run(v)
   
       | Pair (term1, term2) -> 
         let v1 = evali term1 env i in
         let v2 = evali term2 env i in 
         Pair (v1, v2)
   
    and 

    compare_patterni vi pattern env penvi i= (*lst is a list of pairs of two var (x,y)*)
       
      match (vi,pattern) with (*if the corresponding var of x is in lst y must be the same with this  *)
     
        | (Splice(t1),(PSplice(x))) -> (*cuz vi is term so give the new name as t1*)
           if i > 1 then  (*match <~(<2+3>) + 1> with
                            | <.~x + 1> -> ...*)
            let ts = evali t1 env (i-1) in 
            let env1 = ext env x (CodeVal(ts)) in 
           env1
           else 
             let ts = eval0 t1 env in 
             let env1 = ext env x ts in 
             env1
   
        | (IntCons(t1), (PIntCons(int))) -> 
           if t1 = int then 
             env 
         else failwith ("failwith integer value")
           
        | (BoolCons(t1), (PBoolCons(bool))) -> 
           if t1 = bool then 
              env
         else failwith("failwith boolean value")
         
        | (Code(t1), (PCode(pattern))) -> (*in this case we need to compare t1 and pattern*)
           compare_patterni t1 pattern env penvi (i+1) 
         
        | (Plus(t1,t2), (PPlus(pattern1,pattern2))) -> (*match t1 with
                                                   | p1+p2 -> env*)
            let env1 = compare_patterni t1 pattern1 env penvi i in
            let env2 = compare_patterni t2 pattern2 env1 penvi i in 
           env2 
     
        | (Minus(t1,t2), (PMinus(pattern1,pattern2))) -> (*match t1 with
                                                   | p1-p2 -> env*)
            let env1 = compare_patterni t1 pattern1 env penvi i in
            let env2 = compare_patterni t2 pattern2 env1 penvi i in 
           env2 
   
        | (Times(t1,t2), (PTimes(pattern1,pattern2))) -> (*match t1 with
                                                   | p1xp2 -> env*)
           let env1 = compare_patterni t1 pattern1 env penvi i in
           let env2 = compare_patterni t2 pattern2 env1 penvi i in 
           env2 
   
        | (Div(t1,t2), (PDiv(pattern1,pattern2))) -> (*match t1 with
                                                   | p1/p2 -> env*)
           let env1 = compare_patterni t1 pattern1 env penvi i in
           let env2 = compare_patterni t2 pattern2 env1 penvi i in 
           env2 
   
        | (Eq(t1,t2), (PEq(pattern1,pattern2))) -> (*match t1 with
                                                         | p1=p2 -> env*)
            let env1 = compare_patterni t1 pattern1 env penvi i in
            let env2 = compare_patterni t2 pattern2 env1 penvi i in 
           env2 
     
        | (Greater(t1,t2), (PGreater(pattern1,pattern2))) ->(*match t1 with
                                                                 | p1>p2 -> env*)
            let env1 = compare_patterni t1 pattern1 env penvi i in
            let env2 = compare_patterni t2 pattern2 env1 penvi i in 
           env2   
           
        | (Less(t1,t2), (PLess(pattern1,pattern2))) ->(*match t1 with
                                                                 | p1>p2 -> env*)
            let env1 = compare_patterni t1 pattern1 env penvi i in
            let env2 = compare_patterni t2 pattern2 env1 penvi i in 
           env2 
     
        | (Fun(y,t1), (PFun(x,pattern))) -> 
           begin
               try let _ = lookup x penvi in failwith "failwith checking occourance"
               with _ -> try let _ = lookup y penvi in failwith "failwith checking occourance"
               with _ ->
               let env1 =  compare_patterni t1 pattern env (ext penvi x y) i in
               env1
            end

        | (App(t1,t2), (PApp(pattern1,pattern2))) ->
           let env1 = compare_patterni t1 pattern1 env penvi i in
           let env2 = compare_patterni t2 pattern2 env1 penvi i in 
           env2
        
        | (Pair(t1,t2), (PPair(pattern1, pattern2))) -> 
           let env1 = compare_patterni t1 pattern1 env penvi i in
           let env2 = compare_patterni t2 pattern2 env1 penvi i in 
           env2
   
        | _ -> failwith ("failwith pattern")  
       
   (*let factorial = LetRec("fact", "n", 
                       If(Eq(Var("n"), IntCons(1)),
                          IntCons(1),
                          Times(Var("n"), App(Var("fact"), Minus(Var("n"), IntCons(1))))),
                       App(Var("fact"), IntCons(5)))        
                       eval0 factorial env
                       
                      
      eval0 (Plus(IntCons(5), IntCons(3))) (emptyenv ()) 
      eval0 (Div(IntCons(10), IntCons(0))) (emptyenv ())
      eval0 (Greater(IntCons(5), IntCons(3))) (emptyenv ())
      let fun_expr = Fun("x", Plus(Var("x"), IntCons(2)))
      let app_expr = App(fun_expr, IntCons(3))
      eval0 app_expr (emptyenv ())
      eval0 (Var("x")) (emptyenv ()) ( x should be unbound variable)
      let match_expr = Match(IntCons(2), [(PIntCons(1), BoolCons(false)); (PIntCons(2), BoolCons(true))])
      eval0 match_expr (emptyenv ())
      Match(Code(Fun("x", Plus(Var("x"),IntCons(1)))), 
      [PCode(PFun("y", PApp(PSplice("z"), PVar("y")))), Var("z")])
      evali (IntCons(5)) (emptyenv ())
      let env = ext (emptyenv ()) "x" (IntVal(10))
      evali (Var("x")) env 1
      evali (Plus(IntCons(3), IntCons(2))) (emptyenv ()) 1
      let expr = Code(Plus(IntCons(3), IntCons(2)))
      evali expr (emptyenv ()) 1
      let expr = Code(Splice(Code(Plus(IntCons(3), IntCons(2)))))
      evali expr (emptyenv ()) 2
      let expr = Fun("x", Plus(Var("x"), IntCons(1)))
      let app_expr = App(Code(expr), IntCons(2))
      evali app_expr (emptyenv ()) 1

       *)