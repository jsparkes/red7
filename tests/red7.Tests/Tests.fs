module red7.Tests

open red7
open red7.Library
open NUnit.Framework

let rng = System.Random()

[<Test>]
let ``Card Equality`` () =
    let one = Card(CardColor.Red, 5y)
    Assert.AreEqual(one, one)
    let two = Card(CardColor.Red, 5y)
    Assert.AreEqual(one, two)
    let three = Card(CardColor.Blue, 5y)
    Assert.AreNotEqual(one, three)
    let four = Card(CardColor.Red, 1y)
    Assert.AreNotEqual(two, four)
    Assert.AreNotEqual(three, four)

[<Test>]
let ``Card IComparable`` () =
    let deck = Deck.Random.Cards |> List.sort
    let card = List.head deck
    Assert.AreEqual(Card(CardColor.Violet, 1y), card)
    let rev = List.rev deck
    Assert.AreEqual(Card(CardColor.Red, 7y), List.head rev)

[<Test>]
let ``Player IComparable`` () =
    let game = Game(2)
    game.Start()
    let p1 = List.item 0 game.Players
    let p2 = List.item 1 game.Players
    Assert.AreEqual(p1, p1)
    Assert.AreNotEqual(p1, p2)

[<Test>]
let ``Deck Remove A Card`` () =
    let deck = Deck.Random
    let card = Card(CardColor.Orange, 3y)
    Assert.IsTrue(deck.Contains card)
    deck.RemoveCard(card)
    Assert.IsFalse(deck.Contains card)

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
                |> List.map (fun c -> Some(Card(c, 1y)))
                // |> List.sort
    ()

[<Test>]
let ``Sort By CardNumber`` () =
    let nums = [1y..7y] |> List.sortBy (fun _ -> rng.Next())
    let cards = [for n in nums ->
                    Card(CardColor.Violet, n) ]
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
    let ruleCard = Card(CardColor.Red, 1y)
    let rule = Rule ruleCard
    let game1 = Game 4
    game1.Start()
    // We cheat here by using the Hand instead of Tableau
    // to make the test easier to construct.
    let max =
        [for player in game1.Players -> player.Hand.HighestCard()]
        |> List.max
    for player in game1.Players do
        if max = player.Hand.HighestCard() then
            Assert.IsTrue(rule.Check(game1, player))
        else
            Assert.IsFalse(rule.Check(game1, player))

