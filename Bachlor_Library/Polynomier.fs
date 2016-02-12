
module Polynomier

open Z_numbers
open Field_Int


type Mod_poly<'p,'n when 'p  :> INum_poly and 'n :> INum  
                                             and 'n : (new : unit -> 'n ) 
                                             and 'p : (new : unit -> 'p ) > (x : INum ) =
                     let n = (new 'n ()).GetValue() 
                     let p = (new 'p ()).GetValue() 
                     

      
  
   

type Polynomier = |Poly of N_number list  


let rec make_list = function  
         | [] -> []
         | x::xs  -> Z_numbers.make(x)::make_list(xs)

let make x = Poly(make_list(x))

let rec value = function Poly(x) -> x

let rec add_poly = function
         |[],[]  -> []
         |[],y::ys -> y::add_poly([],ys)
         |x::xs,[] -> x::add_poly(xs,[])
         |x::xs,y::ys -> (x+y)::add_poly(xs,ys)

let rec sub_poly = function
         |[],[]  -> []
         |[],y::ys -> y::sub_poly([],ys)
         |x::xs,[] -> x::sub_poly(xs,[])
         |x::xs,y::ys -> (x-y)::sub_poly(xs,ys)


let rec zeroreducer_run = function 
            | x::xs when Z_numbers.value(x) = 0 -> zeroreducer_run(xs)
            | x  -> x

let zeroreducer x = match value(x) with
            | x-> Poly(List.rev(zeroreducer_run(List.rev(x))))
            | [] -> failwith "fuck" 
            

type Polynomier with 
     static member (+) (x:Polynomier,y:Polynomier) = zeroreducer(Poly(add_poly(value(x),(value(y)))))
     static member (-) (x:Polynomier,y:Polynomier) = zeroreducer(Poly(sub_poly(value(x),(value(y)))))
     member x.Length = value(x).Length 
     member x.Deg  = (value(x).Length-1) 

let add_multi = function 
            |xs,y -> (List.map (fun i -> i*y) (value(xs)))
       

let rec add_multi_switch = function  
                |x,0 -> x
                |x,i ->   Z_numbers.make(0)::add_multi_switch(x,i-1) 
                                        

let add_position(x,y) = Poly(add_multi_switch(x,y))

let rec multy_poly_count = function
            |x,[],counter -> make([0])
            |x,y::ys,counter -> add_position(add_multi(x,y),counter) + multy_poly_count(x,ys,counter+1)
             
let  multy_poly (x,y) = multy_poly_count(x,value(y),0)

type Polynomier with 
     static member (*) (x:Polynomier,y:Polynomier) = multy_poly(x,y)
        
let prime_poly = Poly(make_list([1;1;0;0;0;1]))



let poly_divide_const = function 
                | x::xs,y::ys -> x/y 
                



let rec poly_gen_shift = function 
        |(multi,0) -> multi::poly_gen_shift(multi,-1)
        |(multi,-1) -> []
        |(multi,i) -> Z_numbers.make(0)::poly_gen_shift(multi,i-1)



let reduse_poly = function 
            |(x,y) -> Poly(poly_gen_shift(poly_divide_const( List.rev(value(x)),List.rev(value(y))) , x.Length-y.Length))



let rec Poly_Euclid = function 
            |(x,y,q_list,counter,s,t) when value(y) = [] ->x,y,q_list,counter,s,t
            |(x,y,q_list,counter,(s0::s1::_ as s_list),(t0::t1::_ as t_list)) when value(x).Length >= value(y).Length -> Poly_Euclid(y,x - y*reduse_poly(x,y),reduse_poly(x,y)::q_list,counter+1,(s1-s0*reduse_poly(x,y))::s_list,(t1-t0*reduse_poly(x,y))::t_list)
            |(x,y,q_list,counter,(s0::s1::_ as s_list),(t0::t1::_ as t_list))  -> Poly_Euclid(y, x,make([0])::q_list,counter+1,(s1-s0*reduse_poly(x,y))::s_list,(t1-t0*reduse_poly(x,y))::t_list)
            
          
let Poly_Euclid_start = function 
            |x,y -> Poly_Euclid(x,y,[],0,[make([0]);make([1])],[make([1]);make([0])])

let modInverseSub_poly(f,g) = match Poly_Euclid_start(f,g)  with
                    | (_,_,_,_,_,t0::t1::tx) -> t1

type Polynomier with                                                         
        static member (/)  (x:Polynomier,y:Polynomier) = x * modInverseSub_poly(prime_poly,y)







