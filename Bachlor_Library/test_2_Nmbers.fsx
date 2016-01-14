
#load "Z_numbers.fs"
#load "Polynomier.fs"
open Z_numbers
open Polynomier




let prime = 7

let n0 = Z_numbers.make(0)
let n1 = Z_numbers.make(10)
let n2 = Z_numbers.make(-1)
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


//let n14 = set_prime(prime)
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
let p8 = p4-p2
let p9 = [0;0;3]
let p10 = Polynomier.make(p9)
let P11 = p10 * p2
let p12 = p2 * p10
let p13 = p10 + p2
let p14 = Polynomier.make([1;2;3])
let p15 = p14-p10
let p16 = p10-p14

let kane = p14.Length

let p17 = reduse_poly(p2,p14)
let p18 = p17*p14
let p19 = p2-p18
let p20 = Poly_Euclid_start(p14,p10)
let p21 = modInverseSub_poly(p14,p10)

let p22 = Polynomier.make([1;1;0;0;0;1])
let p23 = Polynomier.make([1;0;0;1])

let p24 = Poly_Euclid_start(p22,p23)
let p25 = modInverseSub_poly(p22,p23)

let p22 =p10 * p21




let a =List.init 3 (fun index -> index * 3);;
let b = a.Length
let c = List.init (a.Length * a.Length) (fun i -> i*0)
let d = List.map (fun i -> i*3) a
let f = [1;1;1;1;1]



