
module Polynomier

open Z_numbers

type Polynomier = | Poly of List<N_number>

let rec make_poly_list x  = match x with
         | x    ->   match  x with
                      | [] -> []
                      | x::xs -> make(x)::make_poly_list(xs) 

         | N    ->   match N with
                        | 
           

let rec make_poly_int(x) = Poly(make_poly_list(x))

let rec value_poly = function 
           |  Poly(x) -> List.map( fun x -> Z_numbers.value(x)) x