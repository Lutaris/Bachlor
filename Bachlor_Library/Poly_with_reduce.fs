module Poly_with_reduce
open Field_Int
open Poly_p

    type Polynomial_redus<'p when 'p:> INum and  'p :> Polynomial<'p> 
                     and 'p :(new : unit -> 'p)> (x: int List) =
                     let p = (new 'p ()).GetValue()
                     let poly = (new 'p ()).Value 

                     let  a:'p =  
                     

                             






