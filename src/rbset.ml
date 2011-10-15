(*-
 * Copyright (c) 2007, Benedikt Meurer <benedikt.meurer@googlemail.com>
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,
 * and/or sell copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
 * OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
 * IN THE SOFTWARE.
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
    type elt
    type t
    val empty: t
    val is_empty: t -> bool
    val mem: elt -> t -> bool
    val add: elt -> t -> t
    val singleton: elt -> t
    val remove: elt -> t -> t
    val union: t -> t -> t
    val inter: t -> t -> t
    val diff: t -> t -> t
    val compare: t -> t -> int
    val equal: t -> t -> bool
    val subset: t -> t -> bool
    val iter: (elt -> unit) -> t -> unit
    val fold: (elt -> 'a -> 'a) -> t -> 'a -> 'a
    val for_all: (elt -> bool) -> t -> bool
    val exists: (elt -> bool) -> t -> bool
    val filter: (elt -> bool) -> t -> t
    val partition: (elt -> bool) -> t -> t * t
    val cardinal: t -> int
    val elements: t -> elt list
    val min_elt: t -> elt
    val max_elt: t -> elt
    val choose: t -> elt
    val split: elt -> t -> t * bool * t
  end

module Make(Ord: OrderedType) =
struct
  type elt = Ord.t

  type t =
    | Black of t * elt * t
    | Red of t * elt * t
    | Empty

  type enum =
    | End
    | More of elt * t * enum

  let rec enum s e =
    match s with
      | Empty -> e
      | Black(l, x, r) | Red(l, x, r) -> enum l (More(x, r, e))

  let blackify = function
    | Red(l, x, r) -> Black(l, x, r), false
    | s -> s, true

  let empty = Empty

  let is_empty = function
    | Empty -> true
    | _ -> false

  let rec mem x = function
    | Empty ->
        false
    | Red(l, y, r)
    | Black(l, y, r) ->
        let c = Ord.compare x y in
          if c < 0 then mem x l
          else if c > 0 then mem x r
          else true

  let balance_left l x r =
    match l, x, r with
      | Red(Red(a, x, b), y, c), z, d
      | Red(a, x, Red(b, y, c)), z, d ->
          Red(Black(a, x, b), y, Black(c, z, d))
      | l, x, r ->
          Black(l, x, r)

  let balance_right l x r =
    match l, x, r with
      | a, x, Red(Red(b, y, c), z, d)
      | a, x, Red(b, y, Red(c, z, d)) ->
          Red(Black(a, x, b), y, Black(c, z, d))
      | l, x, r ->
          Black(l, x, r)

  let add x s =
    let rec add_aux = function
      | Empty ->
          Red(Empty, x, Empty)
      | Red(l, y, r) as s ->
          let c = Ord.compare x y in
            if c < 0 then
              Red(add_aux l, y, r)
            else if c > 0 then
              Red(l, y, add_aux r)
            else
              s
      | Black(l, y, r) as s ->
          let c = Ord.compare x y in
            if c < 0 then
              balance_left (add_aux l) y r
            else if c > 0 then
              balance_right l y (add_aux r)
            else
              s
    in fst (blackify (add_aux s))

  let singleton x =
    Black(Empty, x, Empty)

  let unbalanced_left = function
    | Red(Black(a, x, b), y, c) -> balance_left (Red(a, x, b)) y c, false
    | Black(Black(a, x, b), y, c) -> balance_left (Red(a, x, b)) y c, true
    | Black(Red(a, x, Black(b, y, c)), z, d) -> Black(a, x, balance_left (Red(b, y, c)) z d), false
    | _ -> assert false

  let unbalanced_right = function
    | Red(a, x, Black(b, y, c)) -> balance_right a x (Red(b, y, c)), false
    | Black(a, x, Black(b, y, c)) -> balance_right a x (Red(b, y, c)), true
    | Black(a, x, Red(Black(b, y, c), z, d)) -> Black(balance_right a x (Red(b, y, c)), z, d), false
    | _ -> assert false

  let rec remove_min = function
    | Empty
    | Black(Empty, _, Black(_)) ->
        assert false
    | Black(Empty, x, Empty) ->
        Empty, x, true
    | Black(Empty, x, Red(l, y, r)) ->
        Black(l, y, r), x, false
    | Red(Empty, x, r) ->
        r, x, false
    | Black(l, x, r) ->
        let l, y, d = remove_min l in
        let s = Black(l, x, r) in
          if d then
            let s, d = unbalanced_right s in s, y, d
          else
            s, y, false
    | Red(l, x, r) ->
        let l, y, d = remove_min l in
        let s = Red(l, x, r) in
          if d then
            let s, d = unbalanced_right s in s, y, d
          else
            s, y, false

  let remove x s =
    let rec remove_aux = function
      | Empty ->
          Empty, false
      | Black(l, y, r) ->
          let c = Ord.compare x y in
            if c < 0 then
              let l, d = remove_aux l in
              let s = Black(l, y, r) in
                if d then unbalanced_right s else s, false
            else if c > 0 then
              let r, d = remove_aux r in
              let s = Black(l, y, r) in
                if d then unbalanced_left s else s, false
            else
              begin match r with
                | Empty ->
                    blackify l
                | _ ->
                    let r, y, d = remove_min r in
                    let s = Black(l, y, r) in
                      if d then unbalanced_left s else s, false
              end
      | Red(l, y, r) ->
          let c = Ord.compare x y in
            if c < 0 then
              let l, d = remove_aux l in
              let s = Red(l, y, r) in
                if d then unbalanced_right s else s, false
            else if c > 0 then
              let r, d = remove_aux r in
              let s = Red(l, y, r) in
                if d then unbalanced_left s else s, false
            else
              begin match r with
                | Empty ->
                    l, false
                | _ ->
                    let r, y, d = remove_min r in
                    let s = Red(l, y, r) in
                      if d then unbalanced_left s else s, false
              end
    in fst (remove_aux s)

  let union s1 s2 =
    let rec union_aux e1 e2 accu =
      match e1, e2 with
        | End, End ->
            accu
        | End, More(x, r, e)
        | More(x, r, e), End ->
            union_aux End (enum r e) (add x accu)
        | (More(x1, r1, e1) as e1'), (More(x2, r2, e2) as e2') ->
            let c = Ord.compare x1 x2 in
              if c < 0 then union_aux (enum r1 e1) e2' (add x1 accu)
              else if c > 0 then union_aux e1' (enum r2 e2) (add x2 accu)
              else union_aux (enum r1 e1) (enum r2 e2) (add x1 accu)
    in union_aux (enum s1 End) (enum s2 End) Empty

  let inter s1 s2 =
    let rec inter_aux e1 e2 accu =
      match e1, e2 with
        | End, _
        | _, End ->
            accu
        | (More(x1, r1, e1) as e1'), (More(x2, r2, e2) as e2') ->
            let c = Ord.compare x1 x2 in
              if c < 0 then inter_aux (enum r1 e1) e2' accu
              else if c > 0 then inter_aux e1' (enum r2 e2) accu
              else inter_aux (enum r1 e1) (enum r2 e2) (add x1 accu)
    in inter_aux (enum s1 End) (enum s2 End) Empty

  let diff s1 s2 =
    let rec diff_aux e1 e2 accu =
      match e1, e2 with
        | End, _ ->
            accu
        | More(x, r, e), End ->
            diff_aux (enum r e) End (add x accu)
        | (More(x1, r1, e1) as e1'), (More(x2, r2, e2) as e2') ->
            let c = Ord.compare x1 x2 in
              if c < 0 then diff_aux (enum r1 e1) e2' (add x1 accu)
              else if c > 0 then diff_aux e1' (enum r2 e2) accu
              else diff_aux (enum r1 e1) (enum r2 e2) accu
    in diff_aux (enum s1 End) (enum s2 End) Empty

  let compare s1 s2 =
    let rec compare_aux e1 e2 =
      match e1, e2 with
        | End, End ->
            0
        | End, _ ->
            -1
        | _, End ->
            1
        | More(x1, r1, e1), More(x2, r2, e2) ->
            let c = Ord.compare x1 x2 in
              if c <> 0 then c else compare_aux (enum r1 e1) (enum r2 e2)
    in compare_aux (enum s1 End) (enum s2 End)

  let equal s1 s2 =
    compare s1 s2 = 0

  let rec subset s1 s2 =
    match s1, s2 with
      | Empty, _ ->
          true
      | _, Empty ->
          false
      | (Black(l1, x1, r1) | Red(l1, x1, r1)), ((Black(l2, x2, r2) | Red(l2, x2, r2)) as s2) ->
          let c = Ord.compare x1 x2 in
            if c = 0 then
              subset l1 l2 && subset r1 r2
            else if c < 0 then
              subset (Black(l1, x1, Empty)) l2 && subset r1 s2
            else
              subset (Black(Empty, x1, r1)) r2 && subset l1 s2

  let rec iter f = function
    | Empty -> ()
    | Black(l, x, r) | Red(l, x, r) -> iter f l; f x; iter f r

  let rec fold f s accu =
    match s with
      | Empty -> accu
      | Black(l, x, r) | Red(l, x, r) -> fold f r (f x (fold f l accu))

  let rec for_all p = function
    | Empty -> true
    | Black(l, x, r) | Red(l, x, r) -> p x && (for_all p l && for_all p r)

  let rec exists p = function
    | Empty -> false
    | Black(l, x, r) | Red(l, x, r) -> p x || (exists p l || exists p r)

  let filter p s =
    let rec filter_aux accu = function
      | Empty -> accu
      | Black(l, x, r) | Red(l, x, r) -> filter_aux (filter_aux (if p x then add x accu else accu) l) r
    in filter_aux Empty s

  let partition p s =
    let rec partition_aux (t, f as accu) = function
      | Empty ->
          accu
      | Black(l, x, r) | Red(l, x, r) ->
          partition_aux (partition_aux (if p x then (add x t, f) else (t, add x f)) l) r
    in partition_aux (Empty, Empty) s

  let rec cardinal = function
    | Empty -> 0
    | Black(l, x, r) | Red(l, x, r) -> 1 + cardinal l + cardinal r

  let rec elements_aux accu = function
    | Empty -> accu
    | Black(l, x, r) | Red(l, x, r) -> elements_aux (x :: elements_aux accu r) l

  let elements s =
    elements_aux [] s

  let rec min_elt = function
    | Empty -> raise Not_found
    | Black(Empty, x, _) | Red(Empty, x, _) -> x
    | Black(l, _, _) | Red(l, _, _) -> min_elt l

  let rec max_elt = function
    | Empty -> raise Not_found
    | Black(_, x, Empty) | Red(_, x, Empty) -> x
    | Black(_, _, r) | Red(_, _, r) -> max_elt r

  let choose = function
    | Empty -> raise Not_found
    | Black(_, x, _) | Red(_, x, _) -> x

  let split x s =
    let rec split_aux y (l, b, r) =
      let c = Ord.compare x y in
        if c < 0 then l, b, add x r
        else if c > 0 then add x l, b, r
        else l, true, r
    in fold split_aux s (Empty, false, Empty)
end
