
type MyBool = NoValue | BoolVal of bool 
type Literal = MyValues of MyBool | LiteralIde of string | Not of Literal 
type B = Atom of Literal | Vel of B * B | And of B * B

exception NotBindedException of string
let rec concreteVal ide e =
            match e with
            NoValue -> raise (NotBindedException(ide))  
            |BoolVal(x) -> x 
//let rec mylittlestak s n v = match 
//let bind(r,l,e) = (l,e)::r
let rec bind (l: (string * MyBool) list) (ide:string) (valore:MyBool) = 
            match l with
            [] -> [(ide,valore)]
            |(x,e)::xs -> if (x = ide) then (x,valore)::xs
                            else (x,e)::(bind xs ide (valore))
let rec bindlist (r:(string*MyBool) list) (b:(string*MyBool) list) = 
        match r,b with
        (_,[]) -> r
        | _, (i,e)::eil -> bindlist (bind r i e) eil
let rec lookup l ide =
            match l with
            [] -> failwith (ide + " not present in stack")
            |(x,e)::xs -> if (x = ide) then concreteVal x e else lookup xs ide



let rec valutaliteral x literals= 
            match x with
            MyValues(esp) -> concreteVal "semplice valore" esp
            //|LiteralIde(name) ->
            |Not(v) -> not( valutaliteral v literals)
            |LiteralIde(iden) -> (lookup literals iden)  

let rec valuta b literals= 
            match b with
            Atom(x) -> valutaliteral x literals
            |Vel(x1,x2) -> if (valuta x1 literals) then true
                            else (valuta x2 literals)
            |And(x1,x2) -> if (valuta x1 literals) then
                                valuta x2 literals
                            else false


let rec alloc (disgliterals:string list) r = 
    match disgliterals with 
    |[] -> r
    |x::xs -> if (x.[0]='~') then
                    alloc xs ((x.[1 .. x.Length-1],NoValue)::r)
                else
                    alloc xs ((x,NoValue)::r)


let getSubVel (str:string) =
    let mutable i = 0 
    let mutable j = 0 
    //devo tenere una lista mutable di stringhe che sono i letterali della disguiunzione
    let mutable (ides:string list) = []
    while str.[i] <> '\n' do
        if (str.[i] = '(' || str.[i] = ' ' || str.[i] = 'V') then //è il primo
            i <- i + 1
            if (str.[i] = ' ') then
                i <- i + 1
            let mutable (tmpide : string) = ""
            while (str.[i] <> ' ' && str.[i] <> ')') do
                tmpide <- tmpide + (str.[i]).ToString()
                i <- i + 1
            ides <- tmpide::ides
        i <- i + 1

    (str.[i+1 .. str.Length-1],ides,alloc ides [])
    ;;
//per ogni disgiunto sulla linea devo chiamare questo metodo
//e metterlo in end con cio che già ho ottenuto
let rec getVelB (ll:string list)=
    match ll with
     [] -> Atom(MyValues(BoolVal(true)))
    |[x] -> if (x.[0]='~') then
                Atom(Not(LiteralIde(x.[1 .. x.Length-1])))
                else
                Atom(LiteralIde(x))
    |x::xs ->  if (x.[0]='~') then
                Vel(Atom(Not(LiteralIde(x.[1 .. x.Length-1]))),getVelB xs)
                else
                Vel(Atom(LiteralIde(x)),getVelB xs)

let getEmptyStack (strs:string list) =
        alloc strs []
//string -> B * ((string * MyBool)list)
let rec getAndB (str:string)=            
        if (str.Length>1) then
            let remaning,disg,stackofthem = getSubVel str in
            //let dowstack = alloc disg stk in 
            let res1,res2 = getAndB remaning in
            (And(res1,getVelB(disg))), (bindlist res2 stackofthem)//getAndB remaning (And(getVelB(disg)),res1) (bindlist disg stackofthem)
        else
        Atom(MyValues(BoolVal(true))),[]


            


(*let finalv =
    try 
        valuta mybexpr mystack
    with
        NotBindedException(ide) -> System.Console.WriteLine("Dare un valore a " + ide); false

esempi di aggiunta nello stack e di B espressioni con relative chiamate di valutazione
//let mystack =[("x",NoValue);"y",NoValue]

let c = (And(Atom(MyValues(BoolVal(false))),Vel(Atom(LiteralIde("x")),Atom(Not(LiteralIde("y"))))))
let b = (And(Vel(Atom(LiteralIde("x")),Atom(Not(LiteralIde("y")))),Atom(MyValues(BoolVal(false)))))
let a = (Vel(Atom(MyValues(BoolVal(false))),Vel(Atom(LiteralIde("x")),Atom(Not(LiteralIde("y"))))))
//c genera un eccezione perché cercando il valore associato al letterale "x" non trova un booleano ma indefined
//b non genera un eccezione perché va a calcoare in modo lazy l'espressione
let res =
    try 
        valuta b mystack
    with
        NotBindedException(ide) -> System.Console.WriteLine("Dare un valore a " + ide); true
;;

*)
let rec tabverita stack= 
        match stack with
        [(i,v)] -> [[(i,BoolVal(true))];[(i,BoolVal(false))];]
        |(i,v)::xs -> let listofstacks = tabverita xs in
                        let mutable (finalstacks: ((string * MyBool) list)list) = [] in 
                        for st in listofstacks do 
                            let mutable l1 = st in
                            let mutable l2 = st in
                            l2 <- (i,BoolVal(false))::st
                            l1 <- (i,BoolVal(true))::st
                            System.Console.WriteLine(l1)
                            finalstacks <- l1::l2::finalstacks
                        ;
                        finalstacks
        | _ -> failwith "error in tabverita"                        
;;
//findsol ha bisgono di tutte le configurazioni
//
let rec findsol be l =  
        match l with
        [] -> failwith "insoddisfacibile"
        |x::xs ->
            let res =
                try 
                    valuta be x
                with
                    NotBindedException(ide) -> System.Console.WriteLine("Dare un valore a " + ide); true
            in 
            if (res) then x else findsol be xs
;;

//combinations costruisce le combinazioni una a una e valuta l'espressione solo una volta che tutte sono state assegnate
//se trova una combinazione che rende vera b, la formula booleana allora restituisce true e l'ambiente in cui la forula vale true
//altrimenti restituisce false,e l'ambiente generato (in cui tutti i letterali sono uguali a false) tele che la formula
//in cui la formula valutata con quell'ambiente restituisce false
//Quando combinations restituisce false la formula è insoddisfacibile
let rec combinations (assigned:(string *MyBool) list) toassign b=
        match toassign with
        [] ->   //System.Console.WriteLine(assigned);
                let res =
                    try 
                        valuta b assigned
                    with
                        NotBindedException(ide) -> System.Console.WriteLine("Dare un valore a " + ide); false
                res,assigned

        |(i,v)::xs ->   let partialres,ambient = (combinations (assigned@[(i,BoolVal(true))]) xs b) in
                        if (not partialres) then //è risultato falso
                            let partialres,ambient = (combinations (assigned@[(i,BoolVal(false))]) xs b)        
                            if (not partialres) then false,ambient
                            else 
                            true,ambient
                        else
                        true,ambient

let findsol2 b stack = let r,a = combinations [] stack b
                        in 
                        if (r) then a
                        else
                        failwith "insoddisfacibile"




//let str = "(ascas V ~dasdas)\n(canepasta V francesco)\n(~ascas V ~dasdas)\n(canepasta V francesco)\n"
//let str ="(~Mitico V ~Mortale)\n(Mitico V Mammifero)\n(Mitico V Mortale)\n(Mortale V Corna)\n(~Mammifero V Corna)\n(Corna V Magico)\n(~Corna)\n"
let str ="(~Mitico V ~Mortale)\n(Mitico V Mammifero)\n(Mitico V Mortale)\n(Mortale V Corna)\n(~Mammifero V Corna)\n(Corna V Magico)\n(~Mitico)\n"
let mybexpr,mystack = getAndB str

//Generando tutte le combinazioni
let allstaks =(tabverita mystack)
findsol mybexpr allstaks

//al caso pessimo genera tutte le combinazioni
findsol2 mybexpr mystack


(*
#load "Flogspace.fsx"
open Flogspace
let mynodes = [1;2;3;4;]
let archi = [(1,2);(1,3);(1,4);(2,3);(3,2);(3,4);]
let mynodes1 = [1;2;3;4]
let archi1 = [(1,2);(2,3);(3,1);(1,4)]
let str = reg1 mynodes + (reg2 mynodes) + (reg3 mynodes) + (reg4 mynodes) + (reg5 mynodes archi)
let mybexpr,mystack = getAndB str
let allstaks =(tabverita mystack)
findsol mybexpr allstaks
findsol2 mybexpr mystack

let mynodes1 = [1;2;3;4]
let archi1 = [(1,2);(2,3);(3,1);(1,4)]

let mynodes = [1;2;3;4;5;6]
let archi = [(1,2);(1,4);(1,6);(2,3);(2,5);(2,6);(3,1);(4,5);(5,2);(6,1);(6,2)]
let str = reg1 mynodes + (reg2 mynodes) + (reg3 mynodes) + (reg4 mynodes) + (reg5 mynodes archi)
let mybexpr,mystack = getAndB str
findsol2 mybexpr mystack
*)