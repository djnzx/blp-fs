module MyLib.Functions

open MyLib.Types

module Functions =

    let add (a, b) = a + b

    let greet (person: Person) = $"Hello, {person.Name}! ({person.Age})"
