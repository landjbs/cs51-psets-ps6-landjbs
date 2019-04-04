(*
                                CS 51
                        Problem Set 6: Search

      Puzzle Solving

  This file contains the PUZZLESOLVER signature for modules that solve
  particular puzzles, as well as a higher-order functor,
  MakePuzzleSolver. A PUZZLESOLVER module solves the puzzle by searching for
  a path from the initial state to the goal state.

  The MakePuzzleSolver functor takes a COLLECTION functor and a
  PUZZLEDESCRIPTION and returns a PUZZLESOLVER. The collection specified
  by the functor is used to store the states that have been reached so
  far. Thus, the ordering in which states are delivered by the
  collection (with the take function) determines the order in which
  the states are searched. A stack regime gives depth-first search, a
  queue regime breadth-first search.

  At the bottom of the file are definitions for depth-first search and
  breadth-first search puzzle solvers, partially applied versions of the
  MakePuzzleSolver functor that use certain collections to engender
  different search methods.
 *)

open Set
open Collections
open Puzzledescription

(* PUZZLESOLVER -- a module signature that provides for solving puzzles
   and graphically drawing the results *)

module type PUZZLESOLVER =
  sig
    (* CantReachGoal -- Exception raised by solve when no solution can
       be found *)
    exception CantReachGoal

    (* state -- The possible puzzle states *)
    type state
    (* move -- The possible moves *)
    type move

    (* solve () -- Returns a solution to the puzzle as a pair containing
       a list of moves and a list of states. The moves, when executed
       starting in the initial state, result in a goal state. A list
       of all of the states visited in the solution process in order
       of visiting (useful in visualizing the search process) is
       provided as the returned state list. *)
    val solve : unit -> move list * state list
    (* draw states moves -- Graphically renders a solution given by the moves *)
    val draw : state list -> move list -> unit
    (* print_state state -- Prints a representation of a state on the
       standard output *)
    val print_state: state -> unit
  end

(* MakePuzzleSolver -- a higher-order functor that generates puzzle solvers, with type

     (functor(sig type t end -> COLLECTION)) -> PUZZLEDESCRIPTION -> PUZZLESOLVER

   A functor that given a functor from an element type to a
   COLLECTION, as well as a PUZZLEDESCRIPTION, returns a full PUZZLESOLVER
   module.

   The functor MakeCollection is used for generating the collection
   for storing pending states that have yet to be searched. Using
   different collection regimes -- stacks (MakeStackList), queues
   (MakeQueueList, MakeQueueStack), etc. -- leads to different search
   regimes -- depth-first, breadth-first, etc.
 *)
module MakePuzzleSolver (MakeCollection
   : functor (Element : sig type t end) ->
     (COLLECTION with type elt = Element.t))
  (G : PUZZLEDESCRIPTION)
       : (PUZZLESOLVER with type state = G.state
  and type move = G.move) =
  struct
    exception CantReachGoal

    module Col = MakeCollection(struct
                                  type t = G.state * (G.move list)
                                end)

    module Set = Set.Make(struct
                            type t = G.state
                            let compare = G.compare_states
                          end)

    type state = G.state
    type move = G.move

    let get_nStates (n : (state * move) list) : state list =
      List.map (fun x -> fst x) n

    let rec neighbors_to_add (neighbors : (state * move) list) (s : Set.t)
                             : (state * move) list =
      match neighbors with
      | [] -> []
      | (state, move) :: t ->
            (if not (Set.mem state s) then
              (state, move) :: (neighbors_to_add t s)
             else
              neighbors_to_add t s)

    let rec states_to_set (lst : state list) (s : Set.t) : Set.t =
      match lst with
      | [] -> s
      | [x] -> Set.add x s
      | h :: t ->
          (let s = Set.add h s in
          states_to_set t s)

    let rec neighbors_to_col (lst : (state * move) list) (c : Col.collection)
                             (movList : move list) : Col.collection =
      match lst with
      | [] -> c
      | [(s, m)] -> Col.add (s, (m :: movList)) c
      | (s, m) :: t ->
          (let c = Col.add (s, (m :: movList)) c in
          neighbors_to_col t c movList)

    let solve (unit) : move list * state list =
      let pending = Col.add (G.initial_state, []) Col.empty in
      let visited = Set.singleton G.initial_state in
      let expanded = [] in
      let rec search (pending : Col.collection) (visited : Set.t)
                     (expanded : state list) : move list * state list =
        let current_state, movList = fst (Col.take pending) in

        let pending = snd (Col.take pending) in
        (* add current state to expanded *)
        let expanded = current_state :: expanded in
        (* if current state is a goal, return info *)
        if G.is_goal current_state then
          (movList, expanded)
        else
          let neighbors =  G.neighbors current_state in
          (* add neighbor states to expanded *)
          let expanded = (get_nStates neighbors) @ expanded in
          let to_add = neighbors_to_add neighbors visited in
          let pending = neighbors_to_col (to_add) pending movList in
          let visited = states_to_set (get_nStates neighbors) visited in

          if Col.is_empty pending then raise CantReachGoal
          else
            search pending visited expanded in
      search pending visited expanded

    let draw (sList : state list) (mList : move list) : unit =
      G.draw sList mList

    let print_state (s : state) : unit =
      G.print_state s

  end ;;

(* DFSSolver and BFSSolver: Higher-order functors that take in a
   PUZZLEDESCRIPTION, and will return puzzles that are solved with DFS and
   BFS, respectively. The fast bfs solver uses a better implementation
   of queues for speed. *)

(* DFS faster on maze *)
module DFSSolver = MakePuzzleSolver(MakeStackList) ;;
module BFSSolver = MakePuzzleSolver(MakeQueueList) ;;
module FastBFSSolver = MakePuzzleSolver(MakeQueueStack) ;;


(*======================================================================
Time estimate

Please give us an honest (if approximate) estimate of how long (in
minutes) the problem set took you to complete.  We care about your
responses and will use them to help guide us in creating future
assignments.
......................................................................*)

let minutes_spent_puzzlesolve () : int = 800 ;;
