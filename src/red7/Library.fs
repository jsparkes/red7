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
    | Violet = 1
    | Indigo = 2
    | Blue = 3
    | Green = 4
    | Yellow = 5
    | Orange = 6
    | Red = 7

    let CardColors = [1..7] |> List.map enum<CardColor>

    type CardNumber(n : int8) =
        member val Number = n

    type Card(color: CardColor, num: int8) =
        member val Color = color
        member val Number = CardNumber num

        override x.ToString() =
            x.Color.ToString() + ": " + x.Number.Number.ToString()

        interface IComparable<Card> with
            member x.CompareTo other =
                // Use tuple comparison
                compare (x.Color, x.Number.Number) (other.Color, other.Number.Number)

        interface IComparable with
            member x.CompareTo obj =
                match obj with
                | null -> 1
                | :? Card as other -> compare (x.Color, x.Number.Number) (other.Color, other.Number.Number)
                | _ -> invalidArg "other" "not a Card"

        interface IEquatable<Card> with
            member x.Equals other =
                other.Color = x.Color && other.Number.Number = x.Number.Number

        override x.Equals obj =
            match obj with
            | :? Card as other -> other.Color = x.Color && other.Number.Number = x.Number.Number
            | _ -> false

        override x.GetHashCode () =
            hash (x.Color, x.Number.Number)

    type Deck(initial: List<Card>) =
        member val Cards = initial with get, set

        static member Random =
            [ for n in 1y..7y do
                for c in CardColors ->
                    Card(c, n) ]
            |> List.sortBy (fun _ -> rng.Next())
            |> Deck

        static member Empty =
            Deck List.empty<Card>

        member x.Top() = 
            match x.Cards with
            | [] -> None
            | c -> Some (List.head c)

        member x.Contains(card) =
            x.Cards.Contains(card)

        member x.AddCard(card) =
            x.Cards <- List.append x.Cards (List.singleton card)

        member x.RemoveCard(card) =
            // x.Cards <- x.Cards |> List.except (List.singleton card)
            x.Cards <- x.Cards |> List.filter (fun c -> c <> card)

        member x.DealACard(player: Player) =
            match x.Cards with
            | [] -> x.DeckEmpty()
            | top::rest ->
                x.Cards <- rest
                player.ReceiveCard(top)

        member x.DeckEmpty() = ()

        member x.HighestCard() =
            match x.Cards with
            | [] -> 0y
            | cards -> cards
                            |> List.map (fun c -> - c.Number.Number)
                            |> List.sort
                            |> List.head
                            |> (fun n -> - n)

        member x.LowestCard() =
            match x.Cards with
            | [] -> 0y
            | cards -> cards
                            |> List.map (fun c -> c.Number.Number)
                            |> List.sort
                            |> List.head

        member x.CountDifferentColors() =
            x.Cards |> List.groupBy (fun card -> card.Color) |> List.length

    and Player(name: string, game: Game) =

        member val Name = name
        member val Game = game
        member val Hand = Deck.Empty
        member val Tableau = Deck.Empty with get, set

        member x.ReceiveCard(card: Card) =
            x.Hand.AddCard(card)

        member x.HasCard(card: Card) =
            x.Hand.Contains(card)

        member x.PlayACard(card: Card) =
            x.Hand.RemoveCard card
            x.Tableau.AddCard card

        interface IComparable<Player> with
            member x.CompareTo other =
                // Use tuple comparison
                compare (x.Name) (other.Name)

        interface IComparable with
            member x.CompareTo obj =
                match obj with
                | null -> 1
                | :? Player as other -> compare (x.Name) (other.Name)
                | _ -> invalidArg "other" "not a Player"

        override x.Equals(obj) =
            match obj with
            | :? Player as other -> other.Name = x.Name
            | _ -> false

        override x.GetHashCode() =
            x.Name.GetHashCode()

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
            | _ -> false

        member x.CheckHighest(game: Game, player: Player) =
            // I could have done this in one pipeline, but this is clearer.
            let allTableaus = game.Players |> List.map (fun player -> player.Tableau)
            let max = allTableaus |> List.map (fun deck -> deck.HighestCard()) |> List.max
            max = player.Tableau.HighestCard()
            
        member x.CheckMaxScore(game: Game, player: Player, score: Player -> int) =
            let map =
                [for p in game.Players do
                    yield p, score p]
                |> Map.ofList
            let max = map |> Map.toList |> List.map snd |> List.max
            max = map.[player]

        member private x.CountLargestGroup (player: Player) groupFn =
            match player.Tableau.Cards with
            | [] -> 0
            | cards -> cards
                        |> List.groupBy groupFn
                        |> List.map (fun cards -> List.length (snd cards))
                        |> List.max

        member x.CheckMostNumber(game: Game, player: Player) =
            let score (player: Player) =
                x.CountLargestGroup player (fun card -> card.Number)
            x.CheckMaxScore (game, player, score)

        member x.CheckMostColor(game: Game, player: Player) =
            let score (player: Player) =
                x.CountLargestGroup player (fun card -> card.Color)
            x.CheckMaxScore (game, player, score)

        member x.CheckMostEven(game: Game, player: Player) =
            let isEven (card: Card) = card.Number.Number &&& 1y = 0y
            let score (player: Player) =
                match player.Tableau.Cards with
                | [] -> 0
                | cards -> cards
                            |> List.filter isEven
                            |> List.length
            x.CheckMaxScore (game, player, score)

        member x.CheckMostOdd(game: Game, player: Player) =
            let isOdd (card: Card) = card.Number.Number &&& 1y = 1y
            let score (player: Player) =
                match player.Tableau.Cards with
                | [] -> 0
                | cards -> cards
                            |> List.filter isOdd
                            |> List.length
            x.CheckMaxScore (game, player, score)

        member x.CheckDifferentColors(game: Game, player: Player) =
            let score (player: Player) = player.Tableau.Cards |> List.groupBy (fun card -> card.Color) |> List.length
            x.CheckMaxScore (game, player, score)

        member x.CheckSequence(game: Game, player: Player) = false

        member x.CheckBelowFour(game: Game, player: Player) =
            let score (player: Player) =
                match player.Tableau.Cards with
                | [] -> 0
                | cards -> cards
                            |> List.filter (fun card -> card.Number.Number < 4y)
                            |> List.length
            x.CheckMaxScore (game, player, score)
