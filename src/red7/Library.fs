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

    type CardNumber(n : sbyte) =
        member val Number = n

    type Card(color: CardColor, num: sbyte) =
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
            List.tryHead x.Cards

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
                player.ReceiveCard(top)
                x.Cards <- rest

        member x.DeckEmpty() = ()

        static member ConvertToValues(cards: List<Card>) =
            match cards with
            | [] -> [ 0y ]
            | cs -> cs |> List.map (fun c -> c.Number.Number)

        member x.HighestCard() =
            Deck.ConvertToValues (x.Cards) |> List.max

        member x.LowestCard() =
            Deck.ConvertToValues (x.Cards) |> List.min

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

        static member CountLargestGroup (cards: List<Card>) groupFn =
            match cards with
            | [] -> 0
            | cs -> cs
                        |> List.groupBy groupFn
                        |> List.map (snd >> List.length) // (fun cards -> List.length (snd cards))
                        |> List.max

        member x.CheckMostNumber(game: Game, player: Player) =
            let score (player: Player) =
                Rule.CountLargestGroup player.Tableau.Cards (fun card -> card.Number)
            x.CheckMaxScore (game, player, score)

        member x.CheckMostColor(game: Game, player: Player) =
            let score (player: Player) =
                Rule.CountLargestGroup player.Tableau.Cards (fun card -> card.Color)
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
            let score (player: Player) = 
                match player.Tableau.Cards with 
                | [] -> 0
                | cards -> cards
                            |> List.groupBy (fun card -> card.Color)
                            |> List.length
            x.CheckMaxScore (game, player, score)

        static member LongestSequence (cards: List<Card>) =
            let byNumber (card: Card) = card.Number.Number

            let rec sequenceCount acc (cards: List<Card>) value =
                match cards with
                | [] -> acc
                | cs -> if (List.head cs).Number.Number = value then
                            sequenceCount (acc + 1) (List.tail cs) (value + 1y)
                        else
                            acc

            let rec sequenceCounts (cards: List<Card>) =
                match cards with
                | [] -> [ 0 ]
                | card :: rest ->
                    let count = sequenceCount 1 rest (1y + card.Number.Number)
                    // Skip over the first sequence for the tail
                    count :: sequenceCounts (List.skip (count - 1) rest)

            cards
            |> List.distinctBy byNumber
            |> List.sortBy byNumber
            |> sequenceCounts 
            |> List.max

        member x.CheckSequence(game: Game, player: Player) =
            let score (player: Player) = 
                match player.Tableau.Cards with 
                | [] -> 0
                | cards -> Rule.LongestSequence cards
            x.CheckMaxScore (game, player, score)

        member x.CheckBelowFour(game: Game, player: Player) =
            let score (player: Player) =
                match player.Tableau.Cards with
                | [] -> 0
                | cards -> cards
                            |> List.filter (fun card -> card.Number.Number < 4y)
                            |> List.length
            x.CheckMaxScore (game, player, score)

        member x.IsValidMove(game: Game, player: Player, card: Card) =
            let p = player
            p.PlayACard(card)
            x.Check(game, p)
