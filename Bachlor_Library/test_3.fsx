
#load "Field_Int.fs"
#load "Poly.fs"
open Field_Int
open Poly

type N3 () =
  interface INum with
    member i.GetValue () = 3

type N5 () =
  interface INum with
    member i.GetValue () = 5

type N7 () =
  interface INum with
    member i.GetValue () = 7

let rnd = System.Random()


type F3 =  PrimeField<N3>
type F5 =  PrimeField<N5>
type F7 =  PrimeField<N7>
let a = F5.create 5
let b = F5.create 3
let c = a.plus b
let d = Field_Int.sum([c;a;b])
let e = d.Value
let f = F7.create (rnd.Next())
let g = F7.create (rnd.Next())






type P_E5 =  Poly<F5>
let a_p = P_E5.create (List.map (fun x -> F5 (x)) [4;3;5;2;5])
let b_p = a_p + a_p
let c_p = a_p + b_p
let e_p = c_p.Int_list
let f_p = a_p.Int_list

type P_E7 = Poly<F7> 
let a_p7 = P_E7.create (List.map (fun x -> F7 (x)) [3;8;4;3;5;2;5])
let b_p7 = P_E7.create (List.map (fun x -> F7 (x)) [4;3;5;2;5])
let c_p7 = a_p7 + a_p7
let d_p7 = a_p7 + b_p7
let e_p7 = c_p7.Int_list
let f_p7 = a_p7.Int_list




let test1  = c_p7.p_mod b_p7.Value
let test2 = test1.Int_list 
let test3 = test1 * b_p7
let test4 = test3.Int_list 

 let poly_divide_const (xs :'f list , ys :'f list when 'f :> IField<'n>) = 
                match (xs,ys) with
                | [],_ -> failwith "emptylist poly_divide_const x"
                | _,[] -> failwith "emptylist poly_divide_const y"
                | x::xs,y::ys -> x.devide y 
      

      let rec make_zeros(i :int , xs :'f list  when 'f :> IField<'n>) = 
                match (i,xs) with
                    | _,[]    -> failwith "list empty"  
                    | 0,xs    -> []
                    | i,(x::xs as s_list) -> x.zero::make_zeros(i-1,s_list) 

      let poly_divide_const_2 (xs :'f list , ys :'f list when 'f :> IField<'n>) = 
                match (xs,ys) with
                 | (xs,ys) when 0 > (xs.Length - ys.Length) -> Poly([])
                 | (xs,ys) when 0 < (xs.Length - ys.Length) -> Poly(List.rev(poly_divide_const(List.rev(xs),List.rev(ys))::make_zeros(xs.Length-ys.Length,xs)))
      
      let rec p_mod_runner (xs :'f list , ys :'f list , q :'f list when 'f :> IField<'n>) =
                match (xs,ys,q) with 
                   |xs,ys,q  when xs.Length < ys.Length -> Poly(xs),Poly(q)
                   |xs,ys,q                             -> p_mod_runner(Poly(xs)-(Poly(ys)*poly_divide_const_2(xs,ys)),ys,Poly(q)+poly_divide_const_2(xs,ys))

      let p_mod_start (xs :'f list , ys :'f list when 'f :> IField<'n>) =
                match (xs,ys) with
                 |xs,ys when xs.Length < ys.Length -> Poly(xs),poly_divide_const_2(xs,ys)
                 |x::xs as x_list,ys when xs.Length > ys.Length -> p_mod_runner((x_list),(ys),([x.zero]))