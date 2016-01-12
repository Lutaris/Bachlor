
#load "Z_numbers.fs"
#load "Polynomier.fs"
open Z_numbers
open Polynomier




let prime = 7

let n0 = Z_numbers.make(0)
let n1 = Z_numbers.make(10)
let n2 = Z_numbers.make(6)
let n3 = Z_numbers.make(17)
let n4 = Z_numbers.make(42)

let n5 = n1 + n2
let n6 = 3 * n1
let n7 = n1 * n2

let n10 = Z_numbers.make(7)
let n11 = Z_numbers.make(12)

let n34 = n2 / n3  

let n12 = EuclideanSub_positiv(n11,n10)
let n13 = EuclideanSub(set_prime(prime),Z_numbers.make(2))


let n14 = set_prime(prime)
let n15 = value(n14)
let n16 = EuclideanSub_positiv(n14,n11)
let n17 = n14 + n10
let n18 = EuclideanSub_positiv(set_prime(prime),Z_numbers.make(12))
let n19 = modInverse(set_prime(prime),Z_numbers.make(12))

let n45 = Z_numbers.make (1000000000)



let Allnumbers = [|0..1..prime-1|]

let m = function x -> x*3

let n20 = set_prime(prime)
let test = Array.map(fun s -> s*3)  Allnumbers

let Allnumbers_N = Array.map (fun x -> make(x)) Allnumbers

let Array_invers = Array.map (fun x -> modInverse(set_prime(prime),x)) Allnumbers_N

let n21 = make(2)
let n22 = make(6)
let n23 = n22/n21



let p1 = [1..9]
let p2 = Polynomier.make(p1)
let p3 = [1..9]
let p4 = Polynomier.make(p3)
//let p5 = Polynomier.add_poly(p2,p4)
let p6 = p2+p4
let p7 = p2*p4

let a =List.init 3 (fun index -> index * 3);;
let b = a.Length
let c = List.init (a.Length * a.Length) (fun i -> i*0)
let d = List.map (fun i -> i*3) a
let f = [1;1;1;1;1]



let test =add_nulti_position_count(Polynomier.value(p2),n0,1)
