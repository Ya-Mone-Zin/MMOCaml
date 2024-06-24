type pattern = 
   | PVar of string 
   | PSplice of string 
   | PIntCons of int 
   | PBoolCons of bool 
   | PCode of pattern 
   | PPlus of pattern * pattern     (* e + e *)
   | PMinus of pattern * pattern    (* e - e *)
   | PTimes of pattern * pattern    (* e * e *)
   | PDiv of pattern * pattern      (* e / e *)
   | PEq of pattern * pattern       (* e = e *)
   | PGreater of pattern * pattern  (* e > e *)
   | PLess of pattern * pattern     (* e < e *)
   | PFun of string * pattern       (* fun x -> e *)
   | PApp of pattern * pattern      (* function application i.e. e e *) 
   (*| PHop of string * pattern list  (*Highter order pattern and argu is not fix*) *)
   | PPair of pattern * pattern
   and 
   
  case = pattern * term and 

  caseList = case list (*include empty, list of one elements and so on*)
 (* | Empty                 (*  *)
  | Com of case * caseList   (*type term = Plus of term * term
                          type cases = XXX of caseList * case*)*) and 

  term = 
  | Var of string         (* variable e.g. x *)
  | IntCons of int         (* integer constant e.g. 17 *)
  | BoolCons of bool       (* boolean constant e.g. true *)
  | Fun of string * term   (* fun x -> e *)
  | App of term * term      (* function application i.e. e e *)
  | Plus of term * term     (* e + e *)
  | Minus of term * term    (* e - e *)
  | Times of term * term    (* e * e *)
  | Div of term * term      (* e / e *)
  | Eq of term * term       (* e = e *)
  | Greater of term * term  (* e > e *)
  | Less of term * term     (* e < e *)
  | LetRec of string * string * term * term   (* letrec f x=e in e *)
  | If of term * term * term (* if e then e else e *)
  | Match of term * caseList 
  | Code of term 
  | Splice of term 
  | Run of term 
  | Pair of term * term 


type ty = 
  | TInt 
  | TBool 
  | TArrow of ty * ty 
  | TCode of ty 
  | TPair of ty * ty 

  
(*type valuei = (*value at level 1 or higher*)
  | VarVali of string         (* variable e.g. x *)
  | IntVali of int         (* integer constant e.g. 17 *)
  | BoolVali of bool       (* boolean constant e.g. true *)
  | FunVali of string * valuei   (* fun x -> e *)
  | AppVali of valuei * valuei      (* function application i.e. e e *)
  | PlusVali of valuei * valuei     (* e + e *)
  | MinusVali of valuei * valuei    (* e - e *)
  | TimesVali of valuei * valuei    (* e * e *)
  | DivVali of valuei * valuei      (* e / e *)
  | EqVali of valuei * valuei       (* e = e *)
  | GreaterVali of valuei * valuei  (* e > e *)
  | LessVali of valuei * valuei     (* e < e *)
  | LetRecVali of string * string * valuei * valuei   (* letrec f x=e in e *)
  | IfVali of valuei * valuei * valuei (* if e then e else e *)
  | MatchVali of valuei * cases 
  | CodeVali of valuei 
  | SpliceVali of valuei 
  | RunVali of valuei 
  | PairVali of valuei * valuei  *)

(*its the same as term*)

type value0 = 
  | IntVal of int 
  | BoolVal of bool 
  | FunVal of string * term * env 
  | LetRecVal0 of string * string * term * env (* term is the body of the function and we need env to find the value of the variable.*)
  | CodeVal of term (*the value within code is valuei *)
  | PairVal of value0 * value0 
  and 
  env = (string * value0) list (*(string * int) a pair of var name and stage number*)
  