

#load "Lib.fs"
#load "Z_numbers.fs"
open Bachlor_lib
open gcd
//open Vector
//let a = make(5.,3.4)
//let b = make(8.,-3.4)
//let c = a +. b

       

let rec gcdTest = function
            | (0,b) -> b
            | (a,b) -> gcdTest(b % a,a )

let c  = gcdTest(6,3);

let d = gcd(5,4)
let e = gcd(125,35)


// file.fsi
//module Vector                       // Vector signature
//type Vector
//val ( ~-.) : Vector -> Vector
//val ( +.)  : Vector -> Vector
//val ( -.)  : Vector -> Vector
//val ( *.)  : Vector -> Vector
//val ( &.)  : Vector -> Vector
//val norm   : Vector -> float
//val make   : float * float -> Vector
//val coord  : Vector -> float * float


// is a Larger then b
let largeInt = function
            |(a,b) -> (b <= a)

// test largeInt
let largeInt_test_a = largeInt(4,3)
let largeInt_test_b = largeInt(3,4)


let rec reduce = function 
        |(a,b) -> (a-b,b)

let reduce_test_a = reduce(4,3)
let reduce_test_b = reduce(3,4)
 
//let rec countReduce_start = function
//          | (a,b,false) -> 0
//          | (a,b,true) -> 1 + countReduce_start(a-b,b,largeInt(a,b))          
//
//let rec countReduce(a,b) =  countReduce_start(a-b,b,largeInt(a,b))

//let rec test = function 
//        | i when i > 5 -> []
//        | i            -> i::test(i+1)

let rec countReduce_start = function
          | (a,b,false) -> 0
          | (a,b,true) -> 1 + countReduce_start(a-b,b,largeInt(a,b))          

let rec countReduce(a,b) =  countReduce_start(a-b,b,largeInt(a,b))

let rec Reminder = function a,b -> if (a > b) then (Reminder(a-b,b)) else a%b
                    

let rec EuclideanSub2 = function   
            |(f_function,g_function,xs,q,s,t,i) when g_function = 0 -> (xs,q,s,t,i)
            |(f_function,g_function,xs,q,(s0::s1::_ as s_list),(t0::t1::_ as t_list),i)  ->  
              EuclideanSub2(g_function,Reminder(f_function,g_function),Reminder(f_function,g_function)::xs,countReduce(f_function,g_function)::q,
              s1-s0*countReduce(f_function,g_function)::s_list,t1-t0*countReduce(f_function,g_function)::t_list,i+1)
            
//let EuclideanSub2 (f,g) = function EuclideanSub 4 5 [] 0 

let rec EuclideanSub = function  
            | f,g     -> EuclideanSub2(f,g,[g;f],[],[0;1],[1;0],0)

EuclideanSub(7,1)
EuclideanSub(7,2)
EuclideanSub(7,3)
EuclideanSub(7,4)
EuclideanSub(7,5)
EuclideanSub(7,6)




let modInverse(f,g) = match EuclideanSub(f,g)  with
                    | (_,_,_,t0::t1::tx,_) -> t1

EuclideanSub(7,2)

let test_modInverse = modInverse(7,6) 


let nat_7 = Seq.initInfinite(fun i  -> i%7 );;

let prime = 12073

open Z_numbers

let n1 = make(10)
let n2 = make(6)
let n3 = make(17)
let n4 = make(42)

let n5 = n1 + n2
let n6 = 3 * n1
let n7 = n1 * n2

let n10 = make(7)
let n11 = make(12)

let n12 = EuclideanSub_positiv(n11,n10)
let n13 = EuclideanSub(set_prime(prime),make(2))


let n14 = set_prime(prime)
let n15 = value(n14)
let n16 = EuclideanSub_positiv(n14,n11)
let n17 = n14 + n10
let n18 = EuclideanSub_positiv(set_prime(prime),make(12))
let n19 = modInverse(set_prime(prime),make(12))





let Allnumbers = [|0..1..prime-1|]

let m = function x -> x*3

let n20 = set_prime(prime)
let test = Array.map(fun s -> s*3)  Allnumbers

let Allnumbers_N = Array.map (fun x -> make(x)) Allnumbers

let Array_invers = Array.map (fun x -> modInverse(set_prime(prime),x)) Allnumbers_N


let rec RemoveNegative = function 
            | (prime,x::xs)   -> if x < 0 then (x + prime)::RemoveNegative(prime,xs) else RemoveNegative(prime,xs)

