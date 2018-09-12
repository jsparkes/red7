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
    Assert.AreEqual(7y, card1)
    let card2 = Deck.Empty.HighestCard()
    Assert.AreEqual(0y, card2)

[<Test>]
let ``Deck LowestCard`` () =
    let card1 = Deck.Random.LowestCard()
    Assert.AreEqual(1y, card1)
    let card2 = Deck.Empty.LowestCard()
    Assert.AreEqual(0y, card2)

[<Test>]
let ``Rule CheckHighest`` () =
    let ruleCard = { Color = CardColor.Red; Number = CardNumber 1y }
    let rule = Rule ruleCard
    let game1 = Game 4
    game1.Start()
    let allHighest = 
        [for player in game1.Players do
            yield player.Hand.HighestCard()]
    let max = List.max allHighest
    for player in game1.Players do
        if max = player.Hand.HighestCard() then
            Assert.IsTrue(rule.Check(game1, player))
        else
            Assert.IsFalse(rule.Check(game1, player))

