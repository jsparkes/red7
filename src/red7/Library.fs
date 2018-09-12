namespace red7

open System.Linq;

// Using the Red 7 game (https://boardgamegeek.com/boardgame/161417/red7)
// to learn about machine learning for games.
// It may actually be a bad choice, since a wrong move immediately ends the game!
// Not much learning there, except to avoid losing moves.  Maybe there's
// enough in the scoring to try maximizing?

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

    type Deck(initial: List<Card>) =
        member val Cards = initial with get, set

        static member Random =
            [ for n in 1y..7y do
                for c in CardColors ->
                    { Color = c; Number = CardNumber(n) } ]
            |> List.sortBy (fun _ -> rng.Next())
            |> Deck

        static member Empty =
            Deck List.empty<Card>

        member x.Top() = 
            match x.Cards with
            | [] -> None
            | c -> Some (List.head x.Cards)

        member x.Contains(card) =
            x.Cards.Contains(card)

        member x.AddCard(card) =
            x.Cards <- List.append x.Cards (List.singleton card)

        member x.DealACard(player: Player) =
            match x.Cards with
            | [] -> x.DeckEmpty()
            | top::rest ->
                x.Cards <- rest
                player.ReceiveCard(top)

        member x.DeckEmpty() = ()

        member x.HighestCard() =
            match x.Cards with
            | [] -> None
            | cards -> Some (cards
                                |> List.sortBy (fun c -> - c.Number.Number)
                                |> List.head)

        member x.LowestCard() =
            match x.Cards with
            | [] -> None
            | cards -> Some (cards
                                |> List.sortBy (fun c -> c.Number.Number)
                                |> List.head)


    and Player(name: string, game: Game) =

        member val Name = name
        member val Game = game
        member val Hand = Deck.Empty
        member val Tableau = List.empty<Card> with get, set

        member x.ReceiveCard(card: Card) =
            x.Hand.AddCard(card)

        member x.HasCard(card: Card) =
            x.Hand.Contains(card)

    and Game(numOfPlayers: int) =

        member val Players = List<Player>.Empty with get, set 
        member val Deck = Deck.Random with get, set
        // This doesn't really have to be a stack.
        // We could just save the topmost card.
        member val Rule = List.empty<Card> with get, set

        member x.Start() =
            x.Players <- [ for i in 1..numOfPlayers ->
                                Player("Player " + i.ToString(), x) ]
            for p in x.Players do
                for j in 0..6 do
                    x.Deck.DealACard p

    and Rule(card: Card) =

        member x.Check(game: Game, player: Player) =
            match card.Color with
            | CardColor.Red -> x.CheckHighest(game, player)
            | CardColor.Orange -> x.CheckMostNumber(game, player)
            | CardColor.Yellow -> x.CheckMostColor(game, player)
            | CardColor.Green -> x.CheckMostEven(game, player)
            | CardColor.Blue -> x.CheckDifferentColors(game, player)
            | CardColor.Indigo -> x.CheckSequence(game, player)
            | CardColor.Violet -> x.CheckBelowFour(game, player)

        member x.CheckHighest(game: Game, player: Player) = false
        member x.CheckMostNumber(game: Game, player: Player) = false
        member x.CheckMostColor(game: Game, player: Player) = false
        member x.CheckMostEven(game: Game, player: Player) = false
        member x.CheckMostOdd(game: Game, player: Player) = false
        member x.CheckDifferentColors(game: Game, player: Player) = false
        member x.CheckSequence(game: Game, player: Player) = false
        member x.CheckBelowFour(game: Game, player: Player) = false
