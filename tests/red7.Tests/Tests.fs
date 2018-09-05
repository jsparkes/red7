module red7.Tests

open red7
open red7.Library
open NUnit.Framework

let rng = System.Random()

[<Test>]
let ``Dealing A Card`` () =
    let game = Game(1)
    game.Start()
    let card = game.Deck.Head
    Assert.IsFalse(game.Players.Head.HasCard(card))
    game.DealACard(game.Players.Head)
    Assert.IsTrue(game.Players.Head.HasCard(card))

[<Test>]
let ``Sort By Color`` () =
    let cols = CardColors
                |> List.sortBy  (fun _ -> rng.Next())
                |> List.sort
    Assert.AreEqual(cols, CardColors) |> ignore

[<Test>]
let ``Sort By CardColor`` () =
    let deck = CardColors
                |> List.sortBy  (fun _ -> rng.Next())
                |> List.map (fun c -> Some({ Color = c; Number = CardNumber(1y)}))
                // |> List.sort
    ()

[<Test>]
let ``Sort By CardNumber`` () =
    let nums = [1y..7y] |> List.sortBy (fun _ -> rng.Next())
    let cards = [for n in nums do
                    yield { Color = CardColor.Violet; Number = CardNumber n}]
    ()
    //let sorted = List.sort cards
    //let deck = CardColors
    //            |> List.sortBy  (fun _ -> rng.Next())
    //            // |> List.choose (fun c -> Some({ Color = c; Number = CardNumber(1y)}))
    //            |> List.sort
    //printfn "%O" deck


