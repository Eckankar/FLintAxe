// This program is used for testing the output.
module SampleProgram

let rec sum = function
   | []      -> 0
   |[x::xs] -> x + sum [xs]