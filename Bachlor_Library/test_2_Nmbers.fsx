
#load "Z_numbers.fs"
//#load "Polynomier.fs"
open Z_numbers
open Polynomier

let prime = 7


let n1 = make(10)
let n2 = make(6)
let n3 = make(17)
let n4 = make(42)

let n5 = n1 + n2
let n6 = 3 * n1
let n7 = n1 * n2

let n10 = make(7)
let n11 = make(12)

let n34 = n2 / n3  

let n12 = EuclideanSub_positiv(n11,n10)
let n13 = EuclideanSub(set_prime(prime),make(2))


let n14 = set_prime(prime)
let n15 = value(n14)
let n16 = EuclideanSub_positiv(n14,n11)
let n17 = n14 + n10
let n18 = EuclideanSub_positiv(set_prime(prime),make(12))
let n19 = modInverse(set_prime(prime),make(12))

let n45 = make (1000000000)



let Allnumbers = [|0..1..prime-1|]

let m = function x -> x*3

let n20 = set_prime(prime)
let test = Array.map(fun s -> s*3)  Allnumbers

let Allnumbers_N = Array.map (fun x -> make(x)) Allnumbers

let Array_invers = Array.map (fun x -> modInverse(set_prime(prime),x)) Allnumbers_N

let n21 = make(2)
let n22 = make(6)
let n23 = n22/n21

let p1 = [1..1..9]
let p2 = make_poly_int(p1)

let p3 = [2..1..9]
let p4 = make_poly_int(p3)

let p5 = value_poly(p4)



