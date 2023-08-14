open Enigma
open System

[<EntryPoint>]
let main args =
    printfn "Enigma Machine CLI"
    printfn "Enter a message to encrypt:"
    let input = Console.ReadLine()

    let encryptedMessage =
        input.ToUpper()
        |> Seq.map (fun char -> if Char.IsLetter char then encryptChar enigmaConfig char else char)
        |> String.Concat

    printfn "Encrypted message: %s" encryptedMessage

    0
