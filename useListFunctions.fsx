open listFunctions

prnStr <| "\ncollect:"
printfn "%A" <| collect threeOfElm [1; 2; 3; 4]
printfn "%A" <| collect addAndMul [1.; 2.; 3.; 4.]

prnStr <| "\nmap:"
printfn "%A" <| map addTwo [1; 2; 3]
printfn "%A" <| map addStr ["garage"; "super"; "øl"]

prnStr <| "\nfilter:"
prn <| filter even [1; 2; 3; 4; 5; 6]
prn <| filter uneven [1; 2; 3; 4; 5; 6]
prn <| filter vocal' ['c'; 'a'; 'm'; 'i']

prnStr <| "\ncontains:"
prn <| contains "korreman" ["fren"; "korreman"; "celeste"]

prnStr <| "\nfind:"
prn <| find (fun x -> x = 1) [2; 3; 4; 1; 1]

prnStr <| "\nfindIndex:"
prn <| findIndex (fun x -> x = 1) [2; 1; 4; 5; 5]
prn <| findIndex (fun x -> x = 1) [2; 4; 5; 5; 1]

(* There is no 1s in the array [2;2], so this test throws an error.
 * Both branches in the try with should have the same type.
 * The error message is a string,
 * so the first branch - the try branch - should be casted to a string.
 *)
prn <| try string (findIndex (fun x -> x = 1) [2; 2])
       with ex -> ex.Message

(* There should be no error here, but it is nice to make errorhandling, when
 * you know the code can throw an error
 * *)
prn <| try string (findIndex (fun x -> x = 1) [1; 4; 5; 5; 1])
       with ex -> ex.Message

prnStr <| "\nfold:"
prn <| fold (fun acc x -> x :: acc) [] [1; 2; 3; 4; 5]
prn <| List.fold (fun acc x -> x :: acc) [] [1; 2; 3; 4; 5]

prnStr <| "\nfoldBack:"
prn <| foldBack (fun x acc -> x :: acc) [1; 2; 3; 4; 5] []
prn <| List.foldBack (fun x acc -> x :: acc) [1; 2; 3; 4; 5] []

prnStr <| "\nforall:"
prn <| forall even [2; 4; 6]
prn <| forall even [2; 4; 5; 6]

prnStr <| "\nhead:"
prn <| head ['r'; 'u'; 's'; '<'; '3']
prn <| head [666; 1; 2; 3]
prn <| try string (head []) with ex -> ex.Message

prnStr <| "\ninit:"
prn <| init 10 (fun i -> i*i)
prn <| init 10 (fun i -> i)

prnStr <| "\nisEmpty"

prnStr <| "\niter"
prn <| iter (fun elm -> printfn "a %d" elm) [-2;-1;0;1;2;3]

prnStr <| "\nrev"
prn <| rev [1;2;3;4;5]

prnStr <| "\nsort"
prn <| sort [3;1;0;-1]

prnStr <| "\nunzip:"
prn <| unzip [(1,2);(3,4);(5,6)]

prnStr <| "zip:"
prn <| zip [1;2;3] [1;2;3]
prn <| try string (zip [1;2;3] []) with ex -> ex.Message





