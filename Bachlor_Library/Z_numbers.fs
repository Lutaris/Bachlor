﻿module Z_numbers
type N_number =  | N of int
                 | Prime of N_number           





//let prime = 12073 // The prime selectet for now
let prime = 7
let set_prime(x) = Prime(N(x)) // prime make function
let nat_prime = Seq.initInfinite ( fun i  -> (i % prime) )

let make(x) = N(Seq.nth x nat_prime) // N make function

let a = 5


let rec value  = function // Get value function
             | N(x) -> x
             | Prime(x) -> value(x) 
            // | Poly(x) -> fun x 
            
//
let rec RemoveNegative = function 
            | (prime,x) -> if x < 0 then RemoveNegative(prime,(x + prime)) else x

let rec RemoveNegative_list = function 
           | (prime,[]) -> []
           | (prime,x::xs)   -> if x < 0 then (x + prime)::RemoveNegative_list(prime,xs) else x::RemoveNegative_list(prime,xs) 
           


type N_number with
        static member (+) (x:N_number,y:N_number) = N(Seq.nth (value(x)+value(y)) nat_prime)
        static member (*) (a,x:N_number)          = N(Seq.nth (a*value(x)) nat_prime)
        static member (*) (x:N_number,y:N_number) = N(Seq.nth (value(x)*value(y)) nat_prime)
        static member (-) (x:N_number,y:N_number) = N(Seq.nth (RemoveNegative(prime,value(x)-value(y))) nat_prime)
        static member (%) (x:N_number,y:N_number) = N(Seq.nth (value(x)%value(y)) nat_prime)
 
let rec Reminder = function a,b -> if (value(a)) > (value(b)) then Reminder(a-b,b) else (a%b)

let largeInt = function
            |(a,b) -> (value(b) <= value(a))

let rec countReduce_start = function
          | (a,b,false) -> 0
          | (a,b,true) -> 1 + countReduce_start(a-b,b,largeInt(a,b))          

let rec countReduce(a,b) =  countReduce_start(a-b,b,largeInt(a,b))

let rec EuclideanSub2 = function   
            |(f_function,g_function,xs,q,s,t,i) when value(g_function) = 0 -> (xs,q,s,t,i)
            |(f_function,g_function,xs,q,(s0::s1::_ as s_list),(t0::t1::_ as t_list),i)  ->  
              EuclideanSub2(g_function,Reminder(f_function,g_function),Reminder(f_function,g_function)::xs,countReduce(f_function,g_function)::q,
               s1-s0*countReduce(f_function,g_function)::s_list,t1-t0*countReduce(f_function,g_function)::t_list,i+1)
            

let EuclideanSub = function  
            | f,g     -> EuclideanSub2(f,g,[g;f],[],[0;1],[1;0],0)

let EuclideanSub_positiv (f,g) = match EuclideanSub(f,g),prime with 
                                            | (h,q,s,t,i),_    -> h,q,RemoveNegative_list(prime,s),RemoveNegative_list(prime,t),i

let modInverseSub(f,g) = match EuclideanSub(f,g)  with
                    | (_,_,_,t0::t1::tx,_) -> t1

let modInverse(f,g) = match modInverseSub(f,g),prime with
                    | f,prime ->RemoveNegative(prime,f)

let Allnumbers = [|0..1..prime-1|]

let Allnumbers_N = Array.map (fun x -> make(x)) Allnumbers

let Array_invers = Array.map (fun x -> modInverseSub(set_prime(prime),x)) Allnumbers_N
                       
type N_number with   
            static member (/) (x:N_number,y:N_number) = x*make(modInverse(set_prime(prime),y))    




// poly

//let rec make_poly_list = function 
//            | [] -> []
//            | x::xs -> make(x)::make_poly_list(xs)
////
//let rec make_poly_int(x) = Poly(make_poly_list(x))
//
//
//
//
//
//type Polynomer with 
//        | static member (+) (x:Polynomer,y:Polynomer) = 
//