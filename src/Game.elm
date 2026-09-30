module Game exposing
    ( allHorses
    , horseColor
    , kitty
    , scratchMultiplier
    , stepsRemaining
    , totalSteps
    , usesDarkText
    , winProbabilities
    , winner
    )

{-| Game rules and win-probability math for the Across the Board horse racing game.

Two dice are rolled each turn. Horse `n` advances one step when the dice total `n`.
Four horses are scratched before the race; rolling a scratched horse pays into the
kitty instead (1st scratch pays 1 × base, 2nd pays 2 × base, and so on). The first
horse to finish its track wins.

-}


allHorses : List Int
allHorses =
    List.range 2 12


{-| Number of ways two dice can total `n` (out of 36).
-}
ways : Int -> Int
ways n =
    6 - abs (7 - n)


{-| Length of each horse's track.
-}
totalSteps : Int -> Int
totalSteps n =
    case n of
        2 -> 3
        3 -> 6
        4 -> 8
        5 -> 11
        6 -> 14
        7 -> 16
        8 -> 14
        9 -> 11
        10 -> 8
        11 -> 6
        12 -> 3
        _ -> 0


horseColor : Int -> String
horseColor n =
    case n of
        2 -> "#a6cee3"
        3 -> "#b2df8a"
        4 -> "#fb9a99"
        5 -> "#fdbf6f"
        6 -> "#cab2d6"
        7 -> "#b15928"
        8 -> "#6a3d9a"
        9 -> "#ff7f00"
        10 -> "#e31a1c"
        11 -> "#33a02c"
        12 -> "#1f78b4"
        _ -> "#999999"


usesDarkText : Int -> Bool
usesDarkText n =
    n <= 6


{-| 1-based position of a horse in the scratch list, which is also its kitty multiplier.
-}
scratchMultiplier : List Int -> Int -> Maybe Int
scratchMultiplier scratches horse =
    scratches
        |> List.indexedMap (\i s -> ( i + 1, s ))
        |> List.filter (\( _, s ) -> s == horse)
        |> List.head
        |> Maybe.map Tuple.first


{-| Each suit of the deck contributes 4 + 3 + 2 + 1 base units up front,
then every roll of a scratched horse adds its multiplier × base.
-}
kitty : Float -> List Int -> List Int -> Float
kitty base scratches rolls =
    let
        initial =
            4 * base * (4 + 3 + 2 + 1)

        fromRolls =
            rolls
                |> List.filterMap (scratchMultiplier scratches)
                |> List.sum
                |> toFloat
    in
    initial + base * fromRolls


stepsRemaining : List Int -> Int -> Int
stepsRemaining rolls horse =
    totalSteps horse - List.length (List.filter ((==) horse) rolls)


activeHorses : List Int -> List Int
activeHorses scratches =
    List.filter (\h -> not (List.member h scratches)) allHorses


winner : List Int -> List Int -> Maybe Int
winner scratches rolls =
    activeHorses scratches
        |> List.filter (\h -> stepsRemaining rolls h <= 0)
        |> List.head


{-| Exact win probability for every active horse.

Instead of Monte Carlo simulation, we "Poissonize" the race: let each horse receive
steps as an independent Poisson process with rate equal to its dice probability.
Merging those processes yields exactly the same sequence of relevant rolls as the
real game, so the race outcome has the same distribution. The time for horse `i` to
finish its `s` remaining steps is then Erlang(s, rate), and

    P(i wins) = ∫ f_i(t) · Π_{j ≠ i} S_j(t) dt

which we evaluate numerically with the midpoint rule.

-}
winProbabilities : List Int -> List Int -> List ( Int, Float )
winProbabilities scratches rolls =
    case winner scratches rolls of
        Just w ->
            List.map
                (\h ->
                    ( h
                    , if h == w then
                        1

                      else
                        0
                    )
                )
                (activeHorses scratches)

        Nothing ->
            let
                horses =
                    activeHorses scratches
                        |> List.map (\h -> { horse = h, steps = stepsRemaining rolls h, rate = toFloat (ways h) / 36 })

                dt =
                    0.1

                -- Poisson terms e^{-rt}(rt)^k/k! for k = 0 .. s-1.
                -- Survival is their sum; density is rate × the last term.
                survivalAndDensity t h =
                    let
                        rt =
                            h.rate * t

                        go k term sum =
                            if k >= h.steps - 1 then
                                ( sum + term, h.rate * term )

                            else
                                go (k + 1) (term * rt / toFloat (k + 1)) (sum + term)
                    in
                    go 0 (e ^ -rt) 0

                integrate t acc =
                    let
                        sd =
                            List.map (survivalAndDensity t) horses

                        survivals =
                            List.map Tuple.first sd

                        allSurvive =
                            List.product survivals

                        contributions =
                            List.indexedMap
                                (\i ( _, dens ) ->
                                    dens
                                        * (survivals
                                            |> List.indexedMap Tuple.pair
                                            |> List.filter (\( j, _ ) -> j /= i)
                                            |> List.map Tuple.second
                                            |> List.product
                                          )
                                        * dt
                                )
                                sd

                        acc2 =
                            List.map2 (+) acc contributions
                    in
                    if allSurvive < 1.0e-12 || t > 5000 then
                        acc2

                    else
                        integrate (t + dt) acc2

                raw =
                    integrate (dt / 2) (List.map (always 0) horses)

                total =
                    List.sum raw
            in
            List.map2 (\h p -> ( h.horse, p / total )) horses raw
