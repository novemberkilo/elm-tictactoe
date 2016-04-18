module TicTacToeModel where


import List exposing ((::), all, drop, filter, head, isEmpty, length, map)
import Maybe exposing (withDefault)


type Player = O | X


type Result = Draw | Winner Player


type alias Field = { col: Int, row: Int }


type alias Move = (Field,Player)


type alias Moves = List Move


type GameState =
      FinishedGame Result Moves
    | NotFinishedGame Player Moves


other : Player -> Player
other player =
    case player of
        X -> O
        O -> X


moves : GameState -> Moves
moves state =
    case state of
        (NotFinishedGame _ moves) -> moves
        (FinishedGame _ moves) -> moves


initialState : GameState
initialState = NotFinishedGame X []


isFieldEmpty : Moves -> Field -> Bool
isFieldEmpty moves field = all (\move -> not (fst move == field)) moves


subsequences : List a -> List (List a)
subsequences lst =
    case lst of
        []  -> [[]]
        h::t -> let st = subsequences t
                in
                    st ++ map (\x -> h::x) st


playerWon : Player -> Moves -> Bool
playerWon player =
    let fieldsAreInLine fields =
            all (\{col} -> col == 1) fields ||
            all (\{col} -> col == 2) fields ||
            all (\{col} -> col == 3) fields ||
            all (\{row} -> row == 1) fields ||
            all (\{row} -> row == 2) fields ||
            all (\{row} -> row == 3) fields ||
            all (\{col,row} -> col == row) fields ||
            all (\{col,row} -> col + row == 4) fields
    in  subsequences
            >> filter (\x -> length x == 3)
            >> filter (all (\(_,p) -> p == player))
            >> map (map fst)
            >> filter fieldsAreInLine
            >> isEmpty
            >> not


addMove : Move -> GameState -> GameState
addMove move state =
    let newMoves = move :: (moves state)
        player = snd move
    in
      if  playerWon player newMoves then
          FinishedGame (Winner player) newMoves
        else if length newMoves == 9 then
               FinishedGame Draw newMoves
        else NotFinishedGame (other player) newMoves


makeComputerMove : GameState -> GameState
makeComputerMove state = case state of
    FinishedGame _ _ -> state
    NotFinishedGame player moves ->
        let fields = [
                {col=2,row=2},
                {col=1,row=1},
                {col=3,row=3},
                {col=1,row=3},
                {col=3,row=1},
                {col=1,row=2},
                {col=2,row=1},
                {col=2,row=3},
                {col=3,row=2}
            ]
            newField = filter (isFieldEmpty moves) fields |> head |> withDefault {col=0,row=0}
            newMoves = (newField, player) :: moves
        in
            addMove (newField, player) state


makeHumanAndComputerMove : Field -> GameState -> GameState
makeHumanAndComputerMove field state =
    case state of
        FinishedGame _ _ -> state
        NotFinishedGame player moves ->
            if isFieldEmpty moves field
                then addMove (field,player) state |> makeComputerMove
                else state


undoMoves : GameState -> GameState
undoMoves state =
    case state of
        NotFinishedGame _ [] -> state
        NotFinishedGame player moves ->
            NotFinishedGame player (moves |> drop 2)
        FinishedGame _ _ -> state


processClick : (Int,Int) -> GameState -> GameState
processClick (x,y) =
    let col = 1 + x // 100
        row = 1 + y // 100
    in
        if col >= 1 && col <= 3 && row >= 1 && row <= 3
            then makeHumanAndComputerMove {col=col,row=row}
            else identity
