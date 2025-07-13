// For more information see https://aka.ms/fsharp-console-apps
open MyLib.Functions.Functions
open MyLib.Types

[<EntryPoint>]
let main argv =
    let sum = add (30, 3)
    let greeting = greet { Name = "Alex"; Age = sum }
    
    printfn $"{greeting}"
    0
