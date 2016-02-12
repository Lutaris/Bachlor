module Field_Int 

   

//    type INum_poly =
//        abstract GetValue : unit -> int List

    type INum =
        abstract GetValue : unit -> int
     

    type N5 () =
        interface INum with
            member i.GetValue () = 5

//    [<AbstractClass>]
    type IField<'a> = 
         abstract value : int   
         abstract zero : 'a
         abstract plus : 'a -> 'a 
         abstract mins : 'a -> 'a
         abstract multy : 'a -> 'a
         abstract devide : 'a -> 'a

        
    type PrimeField<'n when 'n :> INum 
                and 'n : (new : unit -> 'n)> (x : int) = 
        let n = (new 'n ()).GetValue()     
        let make a b = match a % b with 
                | x  when x < 0 -> a + n
                | x -> x 


        let value = make x n
        
      


        let rec Eulicid a b = match (a,b) with 
                                |(a,0) -> (a,1,0)
                                |(a,b) -> 
                                    let (d,s,t) = Eulicid b (a % b)
                                    (d,t,s - t * (a / b))
        member x.inverse  =
                                          let (_,a',_) =  Eulicid x.Value n 
                                          PrimeField<'n> (a')
        member x.plus (y :PrimeField<'n> ) =  PrimeField<'n> (x.Value+ y.Value)
        member x.mins (y :PrimeField<'n> ) =  PrimeField<'n> (x.Value- y.Value)
        member x.multy (y :PrimeField<'n>) =  PrimeField<'n> (x.Value * y.Value)
        member __.Value      = value
        static member create x = PrimeField<'n> x
        interface IField<PrimeField<'n>> with
                member x.value = x.Value
                member x.zero = PrimeField<'n>(0) 
                member x.plus y = x.plus y
                member x.mins y = x.mins y
                member x.multy y = x.multy y
                member x.devide y =x.multy y

   
    

    let rec sum (xs : 'f list when 'f :> IField<'a>) = 
                match xs with 
                | [] -> failwith "No input"
                | x::[] -> x
                | x::xs -> x.plus (sum(xs))




    let mod' a b =
        match a % b with
        | pos when pos >= 0 -> pos
        | neg               -> -neg

    type Mod<'n when 'n :> INum 
                and 'n : (new : unit -> 'n)> (x : int) =
         let n = (new 'n ()).GetValue()     
         let make a b = match a % b with 
                | x  when x < 0 -> a + n
                | x -> x 
     
         
        
               
         let rec extEulkid a b =
           match (a,b) with
                | (a,0) -> (a,1,0)
                | (a,b) -> 
                    let (d,s,t) = extEulkid b (mod' a b)
                    (d,t,s - t * (a / b))

         let rec Eulicid a b = match (a,b) with 
                                |(a,0) -> (a,1,0)
                                |(a,b) -> 
                                    let (d,s,t) = Eulicid b (mod' a b)
                                    (d,t,s - t * (a / b))
 
//         let invMod<'n when 'n :> INum and 'n : (new : unit -> 'n)> = 
//             let n = (new 'n()).GetValue()
//             fun a ->8
//                 let (_,a',_) = extEulkid a n
//                 mod' a' n

         let value = make x n
         let this_mod = n
         let RemoveNegative (a : Mod<'n>, b : Mod<'n>)  = match  a.Value - b.Value  with
                                                            | a when a < 0  -> a + n
                                                            | a -> a

                                                  
         member x.inverse  =
                                          let (_,a',_) =  Eulicid x.Value n 
                                          Mod<'n> (a')

         member private __.Value      = value
         
         member         __.Mod      = this_mod
         override       __.ToString() = 
             sprintf "%d (MOD %d)" value n
         static member (+) (a : Mod<'n>, b : Mod<'n>) = Mod<'n> (a.Value+ b.Value)
         static member (*) (a,x:Mod<'n>)              = Mod<'n>((a*x.Value) )
         static member (*) (x:Mod<'n>,y:Mod<'n>)      = Mod<'n>((x.Value*y.Value)) 
         static member (-) (x:Mod<'n>,y:Mod<'n>)      = Mod<'n>((x.Value-y.Value)) 
         static member (%) (x:Mod<'n>,y:Mod<'n>)      = Mod<'n>((x.Value%y.Value))
         static member (/) (x:Mod<'n>,y:Mod<'n>)      =  x * y.inverse
         static member create x = Mod<'n> x
        

//        let modN<'n when 'n :> INum and 'n : (new : unit -> 'n)> x =
//            Mod<'n>.create x



