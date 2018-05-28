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

    // https://gist.github.com/kristopherjohnson/aab3b42911e25c3ec352
    let shuffleInPlace (array : 'a[]) =
        let swap i j =
            let temp = array.[i]
            array.[i] <- array.[j]
            array.[j] <- temp
        let random = new Random()
        let len = array.Length
        [0..len-2] |> Seq.iter(fun i -> swap i (random.Next(i, len)))
        array

    let deck = 
        seq {
            for n in 1y..7y do
                for c in CardColors do
                    yield { Color = c; Number = CardNumber(n) }
        } 
        |> Seq.toArray
        |> shuffleInPlace
