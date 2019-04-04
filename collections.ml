(*
                                CS 51
                        Problem Set 6: Search

                             Collections

  The COLLECTION module signature is a generic data structure
  generalizing stacks, queues, and priority queues, allowing adding
  and taking elements. This file provides the signature and several
  functors implementing specific collections (stacks, queues,
  etc.).
 *)

module type COLLECTION =
sig

  (* Empty -- Exception indicates attempt to take from an empty
     collection *)
  exception Empty

  (* elements in the collection *)
  type elt

  (* collections themselves *)
  type collection

  (* empty -- the empty collection, collection with no elements *)
  val empty : collection

  (* length col -- Returns number of elements in the collection col *)
  val length : collection -> int

  (* is_empty col -- Returns true if and only if the collection col is
     empty *)
  val is_empty : collection -> bool

  (* add elt col -- Returns a collection like col but with an element
     elt added *)
  val add : elt -> collection -> collection

  (* take col -- Returns a pair of an element from the collection and
     the collection of the remaining elements; raises Empty if the
     collection is empty. Which element is taken is determined by the
     implementation. *)
  val take : collection -> elt * collection

end

(*----------------------------------------------------------------------
  Some useful collections

  To think about: For each of these implementations, what is the time
  complexity for adding and taking elements in this kind of
  collection?  *)

(*......................................................................
  Stacks implemented as lists
 *)

module MakeStackList (Element : sig type t end)
       : (COLLECTION with type elt = Element.t) =
  struct
    exception Empty

    type elt = Element.t
    type collection = elt list

    let empty : collection = []

    let is_empty (d : collection) : bool =
      d = empty

    let length (d : collection) : int =
      List.length d

    let add (e : elt) (d : collection) : collection =
      e :: d

    let take (d : collection) :  elt * collection =
      match d with
      | hd :: tl -> (hd, tl)
      | _ -> raise Empty
end

(*......................................................................
  Queues implemented as lists
 *)

module MakeQueueList (Element : sig type t end)
       : (COLLECTION with type elt = Element.t) =
  struct
    exception Empty

    type elt = Element.t
    type collection = elt list

    let empty : collection = []

    let length (d : collection) : int =
      List.length d

    let is_empty (d : collection) : bool =
      d = empty

    let add (e : elt) (d : collection) : collection =
      d @ [e];;

    let take (d : collection)  :  elt * collection =
      match d with
      | hd :: tl -> (hd, tl)
      | _ -> raise Empty
  end

(*......................................................................
  Queues implemented as two stacks

  In this implementation, the queue is implemented as a pair of stacks
  (s1, s2) where the elements in the queue from highest to lowest
  priority (first to last to be taken) are given by s1 @ s2R (where
  s2R is the reversal of s2). Elements are added (in stack regime) to
  s2, and taken from s1. When s1 is empty, s2 is reversed onto s1. See
  Section 15.2.2 in Chapter 15 for more information on this
  technique. *)

module MakeQueueStack (Element : sig type t end)
       : (COLLECTION with type elt = Element.t) =
  struct
    exception Empty

    module Stack = (MakeStackList(Element)
                    : (COLLECTION with type elt = Element.t))

    type elt = Element.t

    type collection = {front: Stack.collection; revrear: Stack.collection}

    let empty : collection = {front = Stack.empty ; revrear = Stack.empty}

    let length (d : collection) : int =
      (Stack.length d.front) + (Stack.length d.revrear)

    let is_empty (d : collection) : bool =
      (Stack.is_empty d.front) && (Stack.is_empty d.revrear)

    let add (e : elt) (d : collection) : collection =
      {front = d.front; revrear = Stack.add e d.revrear}

    let rec to_rev_list (c : Stack.collection) : Stack.elt list =
      if Stack.is_empty c then []
      else
        let h, t = Stack.take c in
        (to_rev_list t) @ [h]
        
    let rec from_list (lst : Stack.elt list) : Stack.collection =
      match lst with
      | [] -> Stack.empty
      | h :: t -> Stack.add h (from_list t)

    let rec take (d : collection) : elt * collection =
      (* tail recursive conversion of stack to rev list *)
      if Stack.is_empty d.front then
        take {front = from_list (to_rev_list d.revrear);
              revrear = Stack.empty}
      else
        let h, t = Stack.take d.front in
        (h, {front = t; revrear = d.revrear})
  end

(*======================================================================
Time estimate

Please give us an honest (if approximate) estimate of how long (in
minutes) this part of the problem set took you to complete (per person
on average, not in total).  We care about your responses and will use
them to help guide us in creating future assignments.
......................................................................*)
let minutes_spent_collections () : int = 180 ;;
