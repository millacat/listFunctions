module listFunctions

(* Applies f to each element in lst and return
 * a concatenated list of the results *)
let rec collect (fct : 'a -> 'b list) (lst : 'a list) : 'b list =
    match lst with
    | [] -> []
    | x :: xs -> fct x @ collect fct xs

(* Returns true or false depending on whether or not elm is contained in lst. *)
let rec contains (elm : 'a) (lst : 'a list) : bool =
    match lst with
    | [] -> false
    | x :: xs -> if x = elm then true
                 else contains elm xs

(* Returns a new list with all the elements of lst
 * for which fct evaluates to true.*)
let rec filter (fct : ('a -> bool)) (lst : 'a list) : 'a list =
    match lst with
    | [] -> []
    | x :: xs -> if fct x then x :: filter fct xs
                 else filter fct xs

(* Returns the first element of lst for which f is true. *)
let rec find (fct : ('a -> bool)) (lst : 'a list) : 'a =
    match lst with
    | [] -> raise(System.Collections.Generic.KeyNotFoundException
                                "Error: Did not find any element")
    | x :: xs -> if fct x then x
                 else find fct xs

(* Alternative version using a guard *)
let rec find' fct lst : 'a =
    match lst with
    | [] -> raise(System.Collections.Generic.KeyNotFoundException
                                "Error: Did not find any element")
    | x :: xs when fct x -> x
    | _ :: xs -> find fct xs


(* Returns the index of the first element of lst for which f is true.
 * ArgumentException : Thrown if the predicate evaluates to false for all the
 *                     elements of the list.
 *)
let findIndex (fct : ('a -> bool)) (lst : 'a list) : int =
    let rec countIndexes lst cnt =
        match lst with
        | [] -> raise(System.ArgumentException
                       "Error: Predicate evaluates to false for all elms")
        | x :: xs -> if fct x then cnt
                     else countIndexes xs cnt+1
    countIndexes lst 0

(* Updates an accumulator iteratively by applying f to each element in lst.
 * Left fold.
 *
 * fct (... (fct (fct acc lst.[0]) lst.[1]) ...) lst.[n]
 *)
let rec fold (fct : ('s -> 'a -> 's)) (acc : 's) (lst : 'a list) : 's =
    match lst with
    | [] -> acc
    | x :: xs -> fold fct (fct acc x) xs

(* Updates an accumulator iteratively backwards
 * by applying f to each element in lst.
 *
 * fct lst.[0] (fct lst.[1] (...(fct lst.[n] acc) ...))
 * *)
let rec foldBack (fct : ('a -> 's -> 's)) (lst : 'a list) (acc : 's) : 's =
    match lst with
    | [] -> acc
    | x :: xs -> fct x (foldBack fct xs acc)

(* Returns true if all elements in lst are true when f is applied to them. *)
let rec forall (fct : 'a -> bool) (lst : 'a list) : bool =
    match lst with
    | [] -> true
    | x :: xs -> if fct x then forall fct xs
                 else false

(* Returns the first element in lst. If lst is empty an exception is raised *)
let head (lst : 'a list) : 'a =
    match lst with
    | [] -> raise (System.ArgumentException "Error: The list is empty")
    | x :: _ -> x


(* Create a list with m elements
 * and whose value is the result of applying f to the index of the element. *)
let init (m : int) (fct : int -> 'a) : 'a list =
    let rec builder len ind =
        match len with
        | len when len < 0 -> raise (System.ArgumentException
                                      "Error: List cannot be negative length")
        | 0 -> []
        | len -> fct ind :: builder (len-1) (ind+1)
    builder m 0

(* Returns true if lst is empty. *)
let isEmpty (lst : 'a list) : bool =
    match lst with
    | [] -> true
    | _  -> false

(* Applies f to every element in lst.
 * Note it is not the map function, since fct returns unit ().
 *)
let rec iter (fct : 'a -> unit) (lst : 'a list) : unit =
    match lst with
    | [] -> ()
    | x :: [] -> fct x
    | x :: xs -> fct x; iter fct xs

(*Returns a list as a concatenation of applying f to every element of lst.*)
let rec map (fct : ('a -> 'b)) (lst : 'a list) : 'b list =
    match lst with
    | [] -> []
    | x :: xs -> fct x :: map fct xs

(* Returns a new list with the same elements as in lst but in reversed order. *)
let rec rev (lst : 'a list) : 'a list =
    match lst with
    | [] -> []
    | x :: xs -> rev xs @ [x]

(* Returns lst in sorted order. Sorts the list using bubble sort *)
let sort (lst : 'a list) : 'a list =

    let rec bubble lst  : 'a list =
        match lst with
        | [] -> []
        | x :: [] -> [x]
        | x0 :: x1 :: xs -> if x0 <= x1 then x0 :: bubble (x1 :: xs)
                            else x1 :: bubble (x0 :: xs)

    let rec isSorted lst : bool =
        match lst with
        | [] -> true
        | x :: [] -> true
        | x0 :: x1 :: xs -> if x0 <= x1 then isSorted <| x1::xs
                            else false

    let rec run lst : 'a list =
        match isSorted lst with
        | false -> bubble lst |> run
        | true  -> lst

    run lst

(* Returns the tail of the list. This is everything except the first element *)
let tail (lst : 'a list) : 'a list =
    match lst with
    | [] -> []
    | x :: [] -> []
    | x :: xs -> xs

(* Returns a tuple of lists containing the first elements and all the second
 * elements of lst.
 *)
let rec unzip (lst : ('t * 't) list) : ('t list * 't list) =
    match lst with
    | [] -> [],[]
    | (t0,t1) :: ts -> let tail0,tail1 = unzip ts
                       t0 :: tail0, t1 :: tail1

(* Returns a list of pairs,
 * where elements in lst1 and lst2 are iteratively paired. *)
let rec zip (lst1 : 'a list) (lst2 : 'a list) : ('a * 'a) list =
    match lst1, lst2 with
    | [], [] -> []
    | x :: xs, y :: ys -> (x,y) :: (zip xs ys)
    | _ -> raise (System.ArgumentException
                   "Error: Lists are not the same length")

(* Returns the length of the list *)
let length (lst : 'a list) : int =
    let rec counter (lst : 'a list) (cnt : int) : int =
        match lst with
        | [] -> cnt
        | x :: xs -> counter xs (cnt+1)

    counter lst 0



// for collect
let threeOfElm (elm : int) : int list = [elm; elm; elm]
let addAndMul (elm : float) : float list = [elm+elm; elm*elm]

// for filter
let even (elm : int) = elm % 2 = 0
let uneven (elm : int) = elm % 2 = 1

let vocal (elm : char) =
    let vocals = ['a'; 'e'; 'i'; 'o'; 'u'; 'y'; 'æ'; 'ø'; 'å']
    // Basically a List.contains implementation
    let mutable ans = false
    for vocal in vocals do
        if elm = vocal then ans <- true
    ans

let vocal' (elm : char) =
    let vocals = ['a'; 'e'; 'i'; 'o'; 'u'; 'y'; 'æ'; 'ø'; 'å']
    List.contains elm vocals

// for map
let addTwo (elm : int)    = elm + 2
let addStr (elm : string) = elm + "abe"

// simple prints
let prn a = printfn "%A" a
let prnStr s = printfn "%s" s
