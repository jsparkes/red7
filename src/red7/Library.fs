namespace red7

/// Documentation for my library
///
/// ## Example
///
///     let h = Library.hello 1
///     printfn "%d" h
///
module Library = 
    open System

    let rng = System.Random()
  
    // In order from lowest to highest
    type CardColor = 
    | Violet
    | Indigo
    | Blue
    | Green
    | Yellow
    | Orange
    | Red

    let CardColors = [  CardColor.Violet; CardColor.Indigo; CardColor.Blue;
                        CardColor.Green; CardColor.Yellow; CardColor.Orange;
                        CardColor.Red ]

    type CardNumber(n : int8) =
        member val Number = n

    type Card = {
        Color : CardColor
        Number : CardNumber
    }

    let initialDeck = 
        [ for n in 1y..7y do
            for c in CardColors ->
                { Color = c; Number = CardNumber(n) } ]
        |> List.sortBy (fun _ -> rng.Next())
