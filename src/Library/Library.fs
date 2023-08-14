module Enigma

type Rotor = { Name: string; Wiring: string }

let rotorI = { Name = "I"; Wiring = "EKMFLGDQVZNTOWYHXUSPAIBRCJ" }
let rotorII = { Name = "II"; Wiring = "AJDKSIRUXBLHWTMCQGZNPYFVOE" }
let rotorIII = { Name = "III"; Wiring = "BDFHJLCPRTXVZNYEIWGAKMUSQO" }

// Enigma machine configuration
type EnigmaMachine = { Rotors: Rotor list; Reflector: string; Plugboard: (char * char) list }

let enigmaConfig = { Rotors = [rotorI; rotorII; rotorIII]; Reflector = "YRUHQSLDPXNGOKMIEBFZCWVJAT"; Plugboard = [('A', 'E'); ('G', 'Q'); ('P', 'V')] }

// Perform rotor substitution
let substitute rotor char =
    let index = int (char - 'A')
    rotor.Wiring.[index]

// Perform rotor reverse substitution
let reverseSubstitute rotor (letter: char) =
    let index = rotor.Wiring.IndexOf(letter)
    index + int 'A' |> char

// Perform plugboard substitution
let plugboardSubstitute plugboard char =
    match List.tryFind (fun (a, b) -> a = char || b = char) plugboard with
    | Some (a, b) -> if a = char then b else a
    | None -> char

// Simulate the Enigma machine encryption
let encryptChar enigma char =
    let substitutedChar = plugboardSubstitute enigma.Plugboard char
    let rotorOutput =
        enigma.Rotors
        |> List.fold (fun char rotor -> substitute rotor char) substitutedChar
        |> substitute { rotorIII with Wiring = enigma.Reflector }
        |> List.foldBack (fun rotor char -> reverseSubstitute rotor char) enigma.Rotors

    plugboardSubstitute enigma.Plugboard rotorOutput
