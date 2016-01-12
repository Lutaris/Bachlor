
module Polynomier

open Z_numbers

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
         |[],y::ys -> y::add_poly([],ys)
         |x::xs,[] -> x::add_poly(xs,[])
         |x::xs,y::ys -> (x-y)::add_poly(xs,ys)

type Polynomier with 
     static member (+) (x:Polynomier,y:Polynomier) = Poly(add_poly(value(x),(value(y))))
     static member (-) (x:Polynomier,y:Polynomier) = Poly(sub_poly(value(x),(value(y))))


let add_multi = function 
            |xs,y -> (List.map (fun i -> i*y) (value(xs)))
            
                             
let rec add_nulti_position_count = function 
            |_,y,_ when Z_numbers.value(y) = 0 -> []
            |x::xs,y,i when i%Z_numbers.value(y) > 0 ->Z_numbers.make(0)::add_nulti_position_count(x::xs,y,(i+1))
            |x::xs,y,i when i%Z_numbers.value(y) = 0 -> x::add_nulti_position_count(xs,y,(i+1))
            |[],_,_ -> []

let add_position(x,y) = Poly(add_nulti_position_count(x,y,1))

let rec multy_poly_count = function
            |x,[] -> make([0])
            |x,y::ys -> add_position(add_multi(x,y),y) + multy_poly_count(x,ys)
             
let  multy_poly (x,y) = multy_poly_count(x,value(y))

type Polynomier with 
     static member (*) (x:Polynomier,y:Polynomier) = multy_poly(x,y)
        






