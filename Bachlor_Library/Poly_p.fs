module Poly_p
open Field_Int


// type N5 () =
//        interface INum with
//            member i.GetValue () = 5

 type Polynomial<'a> = 
         abstract modulus : int
         abstract my_poly : 'a
         abstract zero : 'a
         abstract plus : 'a -> 'a 
         abstract mins : 'a -> 'a
         abstract multy : 'a -> 'a

//type Polynomial< 'p when 'p :> Field_Int.Mod<N5> 
//                     and 'p : (new : unit -> 'p)> (x: int List) =
//      let  p = (new 'p ()).Mod
//      let make a b = match a % b with 
//                | x  when x < 0 -> a + p
//                | x -> x 

//type Polynomial<'p,'n when 'n :> INum and 'p :> Field_Int.Mod<'n> 
//                     and 'n : (new : unit -> 'n)
//                     and 'p : (new : unit -> 'p)> (x: int List) =
//      let  p = (new 'n ()).GetValue
//      let make a b = match a % b with 
//                | x  when x < 0 -> a + p
//                | x -> x 

type Poly<'n when 'n :> INum  
                       and 'n :(new : unit -> 'n)> (x: int List) =
      let p = (new 'n ()).GetValue()
      let make a b = match a % b with 
                | x  when x < 0 -> a + p
                | x -> x 

      
      let value =List.map (function  x -> make(x) p) x    
      let rec add2 = function
                    |([],[]) -> []
                    |([],y) -> y
                    |(x,[]) -> x
                    |(x::xs,y::ys) -> x+y::add2(xs,ys)
      
      let rec sub2 = function
                    |([],[]) -> []
                    |([],y) -> y
                    |(x,[]) -> x
                    |(x::xs,y::ys) -> x-y::sub2(xs,ys)

      let rec sum = function
                      |(x1::x2::xs) -> sum(add2(x1,x2)::xs)
                      |(x1::[])     -> x1 
                      |([])         -> []

      let add_multi = function 
                        |xs,y -> (List.map (fun i -> i*y) xs)
      
      let rec add_multi_switch = function  
                |x,0 -> x
                |x,i ->   0::add_multi_switch(x,i-1) 
     
      let rec add_position = function
                              |(x,y::ys,i) -> add_multi_switch(add_multi(x,y),i)::add_position(x,ys,i+1)
                              |(_,[],_)    -> []

      let rec add_position_start (x,y) = add_position(x,y,0)

      let rev  = function 
                    |x -> List.rev(x)

      let rec zeroreducer_run = function 
                    | x::xs when x = 0 -> xs
                    | x  -> x
      
      let zeroreducer x = rev(zeroreducer_run(rev(x)))

      let test n:INum =  n

      member x.multi y =     
                             let a =  zeroreducer(sum(add_position_start(x.Value,y)))      
                             Poly<'n>(a)

      member x.add y = 
                             let a = zeroreducer(add2(x.Value,y))
                             Poly<'n>(a)

      member x.sub y = 
                             let a = zeroreducer(sub2(x.Value,y))
                             Poly<'n>(a)

      member  __.Value      = value
      member  __.deg        = value.Length-1
      static member (+) (a : Poly<'n>, b : Poly<'n>) = a.add(b.Value)
      static member (-) (a : Poly<'n>, b : Poly<'n>) = a.sub(b.Value)
      static member (*) (a : Poly<'n>, b : Poly<'n>) = a.multi(b.Value)
      static member create x = Poly<'n> x
      interface Polynomial<Poly<'n>> with
             member x.modulus = p
             member x.my_poly = Poly<'n>(x.Value) 
             member x.zero = Poly<'n>([]) 
             member x.plus y = x + y
             member x.mins y = x - y
             member x.multy y = x * y
     
     
     
     
  type Ent_Field<'a> =         
         abstract add : 'a -> 'a 
         abstract devide : 'a -> 'a
        // inherit I_Polynomial<'a>


  type Ent_Poly<'a when 'a :> Polynomial<'a> 
      and 'a : (new : unit -> 'a) > (x: int List) = 
      let q = (new 'a ()).my_poly
      let p = (new 'a ()).modulus 
      let make a b = match a % b with 
                | x  when x < 0 -> a + p
                | x -> x 

      
      let value =List.map (function  x -> make(x) p) x    
      let rec add2 = function
                    |([],[]) -> []
                    |([],y) -> y
                    |(x,[]) -> x
                    |(x::xs,y::ys) -> x+y::add2(xs,ys)
      
      let rec sub2 = function
                    |([],[]) -> []
                    |([],y) -> y
                    |(x,[]) -> x
                    |(x::xs,y::ys) -> x-y::sub2(xs,ys)

      let rec sum = function
                      |(x1::x2::xs) -> sum(add2(x1,x2)::xs)
                      |(x1::[])     -> x1 
                      |([])         -> []

      let add_multi = function 
                        |xs,y -> (List.map (fun i -> i*y) xs)
      
      let rec add_multi_switch = function  
                |x,0 -> x
                |x,i ->   0::add_multi_switch(x,i-1) 
     
      let rec add_position = function
                              |(x,y::ys,i) -> add_multi_switch(add_multi(x,y),i)::add_position(x,ys,i+1)
                              |(_,[],_)    -> []

      let rec add_position_start (x,y) = add_position(x,y,0)

      let rev  = function 
                    |x -> List.rev(x)

      let rec zeroreducer_run = function 
                    | x::xs when x = 0 -> xs
                    | x  -> x
      
      let zeroreducer x = rev(zeroreducer_run(rev(x)))

      let test n:INum =  n

      member x.multi y =     
                             let a =  zeroreducer(sum(add_position_start(x.Value,y)))      
                             Ent_Poly<'a>(a)

      member x.add y = 
                             let a = zeroreducer(add2(x.Value,y))
                             Ent_Poly<'a>(a)

      member x.sub y = 
                             let a = zeroreducer(sub2(x.Value,y))
                             Ent_Poly<'a>(a)

      member  __.Value      = value
      member  __.deg        = value.Length-1
      static member (+) (a : Ent_Poly<'a>, b : Ent_Poly<'a>) = a.add(b.Value)
      static member (-) (a : Ent_Poly<'a>, b : Ent_Poly<'a>) = a.sub(b.Value)
      static member (*) (a : Ent_Poly<'a>, b : Ent_Poly<'a>) = a.multi(b.Value)
      static member create x = Ent_Poly<'a> x
      interface Ent_Field<Ent_Poly<'a>> with 
                member x.add y = x + y

    



