module Poly


open Field_Int

type Polynomial<'a> = 
         //abstract modulus : int
         abstract my_poly : 'a
         abstract zero : 'a
         abstract plus : 'a -> 'a 
         abstract mins : 'a -> 'a
         abstract multy : 'a -> 'a

type Poly<'n when 'n :> Field_Int.IField<'n>> (x: 'n list) =
      
//      let rec value (xs : 'f list when 'f :> IField<'a>) = 
//                match xs with 
//                 |xs -> List.map (fun x -> x.value) xs
       
      
      let value = x

      let rec value2 (xs : 'f list when 'f :> IField<'n>) = 
                match xs  with 
                | [] -> []
                | x::xs -> x.value::value2(xs)

      let rec sum (xs : 'f list when 'f :> IField<'n>) = 
                match xs with 
                | [] -> failwith "No input"
                | x::[] -> x
                | x::xs -> x.plus (sum(xs))

     


      let rec add2 (xs : 'f list , ys :'f list  when 'f :> IField<'n>)  = 
                   match (xs,ys) with 
                    |([],[]) -> []
                    |(x,[]) -> x
                    |([],y) -> y
                    |(x::xs,y::ys) -> (x.plus y)::add2(xs,ys)
      
      let rec sub2 (xs : 'f list , ys :'f list  when 'f :> IField<'n>  )  = 
                   match (xs,ys) with 
                    |([],[]) -> []
                    |([],y) -> y
                    |(x,[]) -> x
                    |(x::xs,y::ys) -> (x.mins y)::sub2(xs,ys)

      let rec sum = function
                      |(x1::x2::xs) -> sum(add2(x1,x2)::xs)
                      |(x1::[])     -> x1 
                      |([])         -> []

      let rec add_multi (xs :'f list , y :'f when 'f :> IField<'n> ) =
                    match  (xs,y) with
                     |([],_)    -> []
                     |(x::xs,y) -> (x.multy y)::add_multi(xs,y)
      
      let rec add_multi_switch (xs : 'f list, i : int when 'f :> IField<'n>) = 
               match (xs,i) with  
                    |x,0 -> x
                    |(x::xs as x_list),i ->   x.zero::add_multi_switch(x_list,i-1) 
     

      let rec add_position = function
                              |(x,y::ys,i) -> add_multi_switch(add_multi(x,y),i)::add_position(x,ys,i+1)
                              |(_,[],_)    -> []

      let rec add_position_start (x,y) = add_position(x,y,0)

      let rev  = function 
                    |x -> List.rev(x)
      
      let rec zeroreducer_run (xs : 'f list when 'f :> IField<'n>) = 
                match xs with 
                  | x::xs when x.value = 0 -> zeroreducer_run xs
                  | _  -> xs 
     
                   
      let zeroreducer x = 
                         x |> rev
                           |> zeroreducer_run
                           |> rev
      
      let zero2 (xs :'f list when 'f :> IField<'n>)  =  
            match xs with 
                | []    -> failwith "no input .Zero2"
                | x::xs -> x.zero
      
      let poly_divide_const (xs :'f list , ys :'f list when 'f :> IField<'n>) = 
                match (xs,ys) with
                | [],_ -> failwith "emptylist poly_divide_const x"
                | _,[] -> failwith "emptylist poly_divide_const y"
                | x::xs,y::ys -> x.devide y 
      

      let rec make_zeros(i :int , xs :'f list  when 'f :> IField<'n>) = 
                match (i,xs) with
                    | _,[]    -> failwith "list empty"  
                    | 0,xs    -> []
                    | i,(x::xs as s_list) -> x.zero::make_zeros(i-1,s_list) 

      let poly_divide_const_2 (xs :'f list , ys :'f list when 'f :> IField<'n>) = 
                match (xs,ys) with
                 | ((x::xs as x_list),ys) when 0 > (x_list.Length - ys.Length) -> Poly([x.zero])
                 | (xs,ys) when 0 < (xs.Length - ys.Length) -> Poly(List.rev(poly_divide_const(List.rev(xs),List.rev(ys))::make_zeros(xs.Length-ys.Length,xs)))
      
      let rec p_mod_runner (xs :'f list , ys :'f list , q :'f list when 'f :> IField<'n>) =
                match (xs,ys,q) with 
                   |(x::xs as x_list),ys,q  when x_list.Length < ys.Length -> Poly(xs),Poly([x.zero])
                   |xs,ys,q                             -> (axs,ys,q) |> p_mod_runner          

      let p_mod_start (xs :'f list , ys :'f list when 'f :> IField<'n>) =
                match (xs,ys) with
                 |xs,ys when xs.Length < ys.Length -> Poly(xs),poly_divide_const_2(xs,ys)
                 |x::xs as x_list,ys when xs.Length > ys.Length -> p_mod_runner((x_list),(ys),([x.zero]))
                    

      member x.add y = 
                       let a = zeroreducer_run(add2(x.Value,y))
                       Poly<'n>(a)

      member x.sub y = 
                       let a = zeroreducer(sub2(x.Value,y))
                       Poly<'n>(a) 

      member x.multi y =     
                             let a =  zeroreducer(sum(add_position_start(x.Value,y)))      
                             Poly<'n>(a)
      member  __.Value      = value 
      member  __.deg        = value.Length-1
      member  x.Int_list    = value2 x.Value
      static member (+) (a : Poly<'n>, b : Poly<'n>) = a.add(b.Value)
      static member (-) (a : Poly<'n>, b : Poly<'n>) = a.sub(b.Value)
      static member (*) (a : Poly<'n>, b : Poly<'n>) = a.multi(b.Value)
      static member create x = Poly x
      interface Polynomial<Poly<'n>> with           
             member x.my_poly = Poly<'n>(x.Value) 
             member x.zero = Poly<'n>([]) 
             member x.plus y = x + y
             member x.mins y = x - y
             member x.multy y = x * y