namespace Bachlor_lib

open System



module Vector =
    type Vector = V of float * float
    let ( ~-.) (V(x,y))                = V(-x,-y)
    let ( +. )   (V(x1,y1))  (V(x2,y2))  = V(x1+x2,y1+y2) 
    let ( -.)   v1          v2          = v1 +. -. v2
    let ( *.)  a           (V(x,y))    = V(a*x,a*y)
    let ( &.)   (V(x1,y1))  (V(x2,y2))  = x1*x2 + y1*y2
    let norm   (V(x,y))                = sqrt(x*x+y*y)
    let make   (x,y)                   = V(x,y)
    let coord  (V(x,y))                = (x,y)




module gcd =
    let rec gcd = function
            | (0,b) -> b
            | (a,b) -> gcd(b % a,a )
