module TicTacToeModel where

import List exposing ((::), all, drop, filter, head, isEmpty, length, map, concatMap, sortWith, maximum)
import Maybe exposing (withDefault)
import Debug

type Player = O | X


type Result = Draw | Winner Player


type alias Field = { col: Int, row: Int }


type alias Move = (Field,Player)
type alias ScoredMove = (Move, Int)

type alias Moves = List Move
type alias ScoredMoves = List ScoredMove

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

scoreWin : Result -> Int -> Int
scoreWin result depth =
  case result of
    Draw -> 0
    Winner X -> depth - 10
    Winner O -> 10 - depth

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


getAvailableMoves : GameState -> List Field
getAvailableMoves state = case state of
    FinishedGame _ _ -> []
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
        in
            filter (isFieldEmpty moves) fields


getAvailableGameStates : GameState -> List GameState
getAvailableGameStates state = case state of
  FinishedGame _ _ -> [state]
  NotFinishedGame player moves ->
  let newMoves = getAvailableMoves state
  in
    map (\field -> addMove (field, player) state) newMoves


miniMax : GameState -> Int -> List Int
miniMax state depth = case state of
    FinishedGame result _ -> [scoreWin result depth]
    NotFinishedGame player moves -> concatMap (\state -> miniMax state (depth+1)) (getAvailableGameStates state)


compareScoredStates : (GameState, Int) -> (GameState, Int) -> Order
compareScoredStates (_,score1) (_,score2) = compare score2 score1


nextMove : List (GameState, Int) -> Maybe Move
nextMove scoredStates = case scoredStates of
  [] -> Nothing
  otherwise -> let topScoredState = scoredStates |> sortWith compareScoredStates |> head
                   (topScoringState, topScore) = withDefault ((FinishedGame (Winner X) []),0) topScoredState
               in
                 topScoringState |> moves |> head

makeComputerMove : GameState -> GameState
makeComputerMove state = case state of
    FinishedGame _ _ -> state
    NotFinishedGame player moves ->
    let
      availableMovesScored = getAvailableGameStates state
                           |> map (\st -> (st, withDefault 0 (List.minimum (miniMax st 0))))
      -- TODO: Fix silly withDefault below
      bestMove = withDefault ({col=2,row=1}, player) (nextMove (Debug.log "available moves scored" availableMovesScored))
    in
      addMove bestMove state

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
