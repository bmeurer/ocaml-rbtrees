(*-
 * Copyright (c) 2007, Benedikt Meurer <benedikt.meurer@googlemail.com>
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 *)

(* This is my implementation of Red-Black Trees for OCaml. It is based upon
 * "Red-Black Trees in a Functional Setting", Chris Okasaki in "Functional
 * Pearls".
 * Red-Black Trees are exposed via a map and a set API, which is designed to
 * be compatible with the Map and Set modules in the OCaml standard library
 * (which are implemented using AVL trees). You can use the Rbmap and Rbset
 * modules as drop-in replacement for the Map and Set modules.
 *)

module type OrderedType =
  sig
    type t
    val compare: t -> t -> int
  end

module type S =
  sig
    type key
    type +'a t
    val empty: 'a t
    val is_empty: 'a t -> bool
    val add: key -> 'a -> 'a t -> 'a t
    val find: key -> 'a t -> 'a
    val remove: key -> 'a t -> 'a t
    val mem:  key -> 'a t -> bool
    val iter: (key -> 'a -> unit) -> 'a t -> unit
    val map: ('a -> 'b) -> 'a t -> 'b t
    val mapi: (key -> 'a -> 'b) -> 'a t -> 'b t
    val fold: (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
    val compare: ('a -> 'a -> int) -> 'a t -> 'a t -> int
    val equal: ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
  end

module Make(Ord: OrderedType) =
struct
  type key = Ord.t

  type 'a t =
    | Black of 'a t * key * 'a * 'a t
    | Red of 'a t * key * 'a * 'a t
    | Empty

  type 'a enum =
    | End
    | More of key * 'a * 'a t * 'a enum

  let rec enum m e =
    match m with
      | Empty -> e
      | Black(l, k, x, r) | Red(l, k, x, r) -> enum l (More(k, x, r, e))

  let blackify = function
    | Red(l, k, x, r) -> Black(l, k, x, r), false
    | m -> m, true

  let empty = Empty

  let is_empty = function
    | Empty -> true
    | _ -> false

  let balance_left l kx x r =
    match l, kx, x, r with
      | Red(Red(a, kx, x, b), ky, y, c), kz, z, d
      | Red(a, kx, x, Red(b, ky, y, c)), kz, z, d ->
          Red(Black(a, kx, x, b), ky, y, Black(c, kz, z, d))
      | l, kx, x, r ->
          Black(l, kx, x, r)

  let balance_right l kx x r =
    match l, kx, x, r with
      | a, kx, x, Red(Red(b, ky, y, c), kz, z, d)
      | a, kx, x, Red(b, ky, y, Red(c, kz, z, d)) ->
          Red(Black(a, kx, x, b), ky, y, Black(c, kz, z, d))
      | l, kx, x, r ->
          Black(l, kx, x, r)

  let add kx x m =
    let rec add_aux = function
      | Empty ->
          Red(Empty, kx, x, Empty)
      | Red(l, ky, y, r) ->
          let c = Ord.compare kx ky in
            if c < 0 then
              Red(add_aux l, ky, y, r)
            else if c > 0 then
              Red(l, ky, y, add_aux r)
            else
              Red(l, kx, x, r)
      | Black(l, ky, y, r) ->
          let c = Ord.compare kx ky in
            if c < 0 then
              balance_left (add_aux l) ky y r
            else if c > 0 then
              balance_right l ky y (add_aux r)
            else
              Black(l, kx, x, r)
    in fst (blackify (add_aux m))

  let rec find k = function
    | Empty ->
        raise Not_found
    | Red(l, kx, x, r)
    | Black(l, kx, x, r) ->
        let c = Ord.compare k kx in
          if c < 0 then find k l
          else if c > 0 then find k r
          else x

  let unbalanced_left = function
    | Red(Black(a, kx, x, b), ky, y, c) ->
        balance_left (Red(a, kx, x, b)) ky y c, false
    | Black(Black(a, kx, x, b), ky, y, c) ->
        balance_left (Red(a, kx, x, b)) ky  y c, true
    | Black(Red(a, kx, x, Black(b, ky, y, c)), kz, z, d) ->
        Black(a, kx, x, balance_left (Red(b, ky, y, c)) kz z d), false
    | _ ->
        assert false

  let unbalanced_right = function
    | Red(a, kx, x, Black(b, ky, y, c)) ->
        balance_right a kx x (Red(b, ky, y, c)), false
    | Black(a, kx, x, Black(b, ky, y, c)) ->
        balance_right a kx x (Red(b, ky, y, c)), true
    | Black(a, kx, x, Red(Black(b, ky, y, c), kz, z, d)) ->
        Black(balance_right a kx x (Red(b, ky, y, c)), kz, z, d), false
    | _ ->
        assert false

  let rec remove_min = function
    | Empty
    | Black(Empty, _, _, Black(_)) ->
        assert false
    | Black(Empty, kx, x, Empty) ->
        Empty, kx, x, true
    | Black(Empty, kx, x, Red(l, ky, y, r)) ->
        Black(l, ky, y, r), kx, x, false
    | Red(Empty, kx, x, r) ->
        r, kx, x, false
    | Black(l, kx, x, r) ->
        let l, ky, y, d = remove_min l in
        let m = Black(l, kx, x, r) in
          if d then
            let s, d = unbalanced_right m in m, ky, y, d
          else
            m, ky, y, false
    | Red(l, kx, x, r) ->
        let l, ky, y, d = remove_min l in
        let m = Red(l, kx, x, r) in
          if d then
            let m, d = unbalanced_right m in m, ky, y, d
          else
            m, ky, y, false

  let remove k m =
    let rec remove_aux = function
      | Empty ->
          Empty, false
      | Black(l, kx, x, r) ->
          let c = Ord.compare k kx in
            if c < 0 then
              let l, d = remove_aux l in
              let m = Black(l, kx, x, r) in
                if d then unbalanced_right m else m, false
            else if c > 0 then
              let r, d = remove_aux r in
              let m = Black(l, kx, x, r) in
                if d then unbalanced_left m else m, false
            else
              begin match r with
                | Empty ->
                    blackify l
                | _ ->
                    let r, kx, x, d = remove_min r in
                    let m = Black(l, kx, x, r) in
                      if d then unbalanced_left m else m, false
              end
      | Red(l, kx, x, r) ->
          let c = Ord.compare k kx in
            if c < 0 then
              let l, d = remove_aux l in
              let m = Red(l, kx, x, r) in
                if d then unbalanced_right m else m, false
            else if c > 0 then
              let r, d = remove_aux r in
              let m = Red(l, kx, x, r) in
                if d then unbalanced_left m else m, false
            else
              begin match r with
                | Empty ->
                    l, false
                | _ ->
                    let r, kx, x, d = remove_min r in
                    let m = Red(l, kx, x, r) in
                      if d then unbalanced_left m else m, false
              end
    in fst (remove_aux m)

  let rec mem k = function
    | Empty ->
        false
    | Red(l, kx, x, r)
    | Black(l, kx, x, r) ->
        let c = Ord.compare k kx in
          if c < 0 then mem k l
          else if c > 0 then mem k r
          else true

  let rec iter f = function
    | Empty -> ()
    | Red(l, k, x, r) | Black(l, k, x, r) -> iter f l; f k x; iter f r

  let rec map f = function
    | Empty -> Empty
    | Red(l, k, x, r) -> Red(map f l, k, f x, map f r)
    | Black(l, k, x, r) -> Black(map f l, k, f x, map f r)

  let rec mapi f = function
    | Empty -> Empty
    | Red(l, k, x, r) -> Red(mapi f l, k, f k x, mapi f r)
    | Black(l, k, x, r) -> Black(mapi f l, k, f k x, mapi f r)

  let rec fold f m accu =
    match m with
      | Empty -> accu
      | Red(l, k, x, r) | Black(l, k, x, r) -> fold f r (f k x (fold f l accu))

  let compare cmp m1 m2 =
    let rec compare_aux e1 e2 =
      match e1, e2 with
        | End, End ->
            0
        | End, _ ->
            -1
        | _, End ->
            1
        | More(k1, x1, r1, e1), More(k2, x2, r2, e2) ->
            let c = Ord.compare k1 k2 in
              if c <> 0 then c
              else
                let c = cmp x1 x2 in
                  if c <> 0 then c
                  else compare_aux (enum r1 e1) (enum r2 e2)
    in compare_aux (enum m1 End) (enum m2 End)

  let equal cmp m1 m2 =
    let rec equal_aux e1 e2 =
      match e1, e2 with
        | End, End ->
            true
        | End, _
        | _, End ->
            false
        | More(k1, x1, r1, e1), More(k2, x2, r2, e2) ->
            (Ord.compare k1 k2 = 0
                && cmp x1 x2
                && equal_aux (enum r1 e1) (enum r2 e2))
    in equal_aux (enum m1 End) (enum m2 End)
end
