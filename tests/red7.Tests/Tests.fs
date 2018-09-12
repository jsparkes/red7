module red7.Tests

open red7
open red7.Library
open NUnit.Framework

let rng = System.Random()

[<Test>]
let ``Dealing A Card`` () =
    let game = Game 1
    game.Start()
    let player = game.Players.Head
    let deck = Deck.Random
    let card = deck.Top()
    Assert.IsTrue(deck.Contains(card.Value))
    Assert.IsFalse(player.HasCard(card.Value))
    deck.DealACard(game.Players.Head)
    Assert.IsFalse(deck.Contains(card.Value))
    Assert.IsTrue(player.HasCard(card.Value))

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

[<Test>]
let ``Deck HighestCard`` () =
    let card1 = Deck.Random.HighestCard()
    match card1 with
    | None -> Assert.Fail "No highest card returned" |> ignore
    | Some c -> Assert.AreEqual(7, c.Number.Number)
    let card2 = Deck.Empty.HighestCard()
    match card2 with
    | None -> Assert.Pass
    | Some c -> Assert.Fail
    ()

[<Test>]
let ``Deck LowestCard`` () =
    let card1 = Deck.Random.LowestCard()
    match card1 with
    | None -> Assert.Fail "No highest card returned" |> ignore
    | Some c -> Assert.AreEqual(1, c.Number.Number)
    let card2 = Deck.Empty.LowestCard()
    match card2 with
    | None -> Assert.Pass
    | Some c -> Assert.Fail
    ()
