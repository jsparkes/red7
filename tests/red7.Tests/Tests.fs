module red7.Tests

open red7
open red7.Library
open NUnit.Framework

[<Test>]
let ``Dealing A Card`` () =
    let game = Game(1)
    game.Start()
    let card = game.Deck.Head
    Assert.IsFalse(game.Players.Head.HasCard(card))
    game.DealACard(game.Players.Head)
    Assert.IsTrue(game.Players.Head.HasCard(card))
