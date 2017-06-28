module Step7.Step7 exposing (..)

import Html exposing (..)
import Html.Events as Events
import Html.Attributes exposing (..)
import Html5.DragDrop as DragDrop
import List.Extra exposing (splitWhen, last)
import Markdown


-- Model


type Insert =
    Before Int String
  | Last String


type alias Card =
  { id: Int
  , label: String
  , column: String
  }


type alias Model =
    { cards : List Card
    , dragDrop : DragDrop.Model Int Insert
    , newCardName : String
    , columns : List String
    }


insertBefore : a -> (a -> Bool) -> List a -> List a
insertBefore insert when into =
  case splitWhen when into of
    Just (before, after) -> before ++ insert :: after
    Nothing -> into


insertLast : a -> List a -> List a
insertLast new xs = xs ++ [new]


addCard : String -> List Card -> List Card
addCard name cards =
  let
    newCard = Card (nextCardId cards) name "Todo"
  in
    newCard :: cards


moveCard : Int -> Insert -> List Card -> List Card
moveCard cardIdToMove insert cards =
  let
    (otherCards, movedCards) = List.partition (\card -> card.id /= cardIdToMove) cards
  in
    case movedCards of
      movedCard :: rest ->
        case insert of
          Before id column -> insertBefore {movedCard | column = column} (\card -> card.id == id) otherCards
          Last column -> insertLast {movedCard | column = column} otherCards
      [] -> cards


cardIds cards = List.map (\x -> x.id) cards


cardsInColumn : List Card -> String -> List Card
cardsInColumn cards column = List.filter (\card -> card.column == column) cards


nextCardId : List Card -> Int
nextCardId cards =
  let
    existingIds = cardIds cards
  in
    case List.maximum existingIds of
      Just max -> max + 1
      Nothing -> 0


-- To try out performance with many cards, increase this number
nrOfGeneratedCards = 1


model =
    { cards = List.foldr addCard [] (["A card", "Another card", "Yet another card"] ++ (List.repeat nrOfGeneratedCards "generated card"))
    , dragDrop = DragDrop.init
    , newCardName = ""
    , columns = ["Todo", "Doing", "Done"]
    , report = Nothing
    }


-- Update


type Msg
    = DragDropMsg (DragDrop.Msg Int Insert)
    | AddCard
    | EnterCardName String


doDragDrop msg model =
  let
    ( dragModel, result ) = DragDrop.update msg model.dragDrop
    dragId = DragDrop.getDragId model.dragDrop
  in
    { model
    | dragDrop = dragModel
    , cards =
      case result of
        Nothing ->
          model.cards

        Just (_, insert) ->
          case dragId of
            Nothing -> model.cards
            Just dragId -> moveCard dragId insert model.cards
    } ! []


update msg model =
  case msg of

    DragDropMsg dragMsg -> doDragDrop dragMsg model

    AddCard -> (
      { model
      | cards = addCard model.newCardName model.cards
      , newCardName = ""
      }, Cmd.none)

    EnterCardName newName -> ({ model | newCardName = newName }, Cmd.none)


-- View


cardStyle = style
  [ ("background", "white")
  , ("box-shadow", "0.5px 0.5px 3px #00000080")
  , ("height", "40px")
  , ("margin", "10px")
  , ("padding", "10px")
  , ("border-radius", "2px")
  ]


columnStyle = style
  [ ("background", "#B8C3F0")
  , ("margin", "10px")
  , ("padding", "10px")
  , ("border-radius", "4px")
  , ("width", "250px")
  ]


dropStyle = style
  [ ("top", "50%")
  , ("margin", "10px")
  , ("height", "10px")
  , ("border", "2px dashed black")
  ]


inputCardStyle = style
  [ ("margin", "10px")
  ]


columnsStyle = style
  [ ("display", "flex")
  ]


instructionStyle = style
  [ ("margin", "10px")
  ]


dropZone insert =
  div
    ( dropStyle :: (DragDrop.droppable DragDropMsg insert) )
    []


viewCard card withDropZones =
  let
    draggableAttributes = DragDrop.draggable DragDropMsg card.id
    attributes = cardStyle :: draggableAttributes
    handleDropZone element = if withDropZones then (dropZone (Before card.id card.column) :: element :: []) else [element]
    cardElement = div attributes [ text card.label ]
  in
    div [] (handleDropZone cardElement)


isOneBeforeTheOther : a -> a -> List a -> Bool
isOneBeforeTheOther one other list =
  case list of
    first :: second :: rest ->
      if first == one && second == other then
        True
      else
        isOneBeforeTheOther one other (second :: rest)
    _ -> False


getOneAfterThisOne : List a -> a -> Maybe a
getOneAfterThisOne list thisOne =
  case list of
    first :: second :: rest ->
      if first == thisOne then
        Just second
      else
        getOneAfterThisOne (second :: rest) thisOne
    _ -> Nothing


viewCardInput nameSoFar = div [cardStyle]
  [ input [size 14, Events.onInput EnterCardName, value nameSoFar] []
  , button [Events.onClick AddCard] [text "Add"]
  ]


instructions : Html Msg
instructions =
    Markdown.toHtml [ inputCardStyle ] """
# Step 7 - Extras

No more exercises! The following could be interesting look at, maybe write a few tests for example:

* [elm-spa-example](https://github.com/rtfeldman/elm-spa-example)
* [elm-test](http://package.elm-lang.org/packages/elm-community/elm-test/latest)
  with or without fuzzers (aka property based tests)
* [style-elements](http://package.elm-lang.org/packages/mdgriffith/style-elements/latest)
* [ellie-app](https://ellie-app.com/new) "elm fiddler"
* [elm-live](https://www.npmjs.com/package/elm-live)

[Step 6](../Step6/Step6.elm) [Benchmark](Benchmark.elm)
"""


viewColumn : List Card -> String -> Maybe Int -> Html Msg
viewColumn cards column dragId =
    let
        allCardIds = cardIds cards

        isLastCardDragged =
          Maybe.map2 (\draggedId theLastCard -> draggedId == theLastCard.id) dragId (last cards)
          |> Maybe.withDefault False

        isCardBeforeBeingDragged cardId =
          dragId
          |> Maybe.map (\draggedId -> isOneBeforeTheOther draggedId cardId allCardIds)
          |> Maybe.withDefault False

        showZones cardId =
          case dragId of
            Just id -> cardId /= id && not (isCardBeforeBeingDragged cardId)
            Nothing -> False

        lastDropZone =
          case dragId of
            Just id -> if isLastCardDragged then [] else [dropZone (Last column)]
            Nothing -> []

        viewCards = List.map (\card -> viewCard card (showZones card.id)) cards
    in
        div [columnStyle] (viewCards ++ lastDropZone)


viewColumn2 : List Card -> String -> Maybe Int -> Html Msg
viewColumn2 cards column dragId =
    let
        allCardIds = cardIds cards

        isLastCardDragged =
          Maybe.map2 (\draggedId theLastCard -> draggedId == theLastCard.id) dragId (last cards)
          |> Maybe.withDefault False

        isCardBeforeBeingDragged cardId =
          dragId
          |> Maybe.andThen (getOneAfterThisOne allCardIds)
          |> Maybe.map ((==) cardId)
          |> Maybe.withDefault False

        showZones cardId =
          case dragId of
            Just id -> cardId /= id && not (isCardBeforeBeingDragged cardId)
            Nothing -> False

        lastDropZone =
          case dragId of
            Just id -> if isLastCardDragged then [] else [dropZone (Last column)]
            Nothing -> []

        viewCards = List.map (\card -> viewCard card (showZones card.id)) cards
    in
        div [columnStyle] (viewCards ++ lastDropZone)


view model =
  let
     dragId = DragDrop.getDragId model.dragDrop
     columns =
       model.columns
       |> List.map (\column -> viewColumn2 (cardsInColumn model.cards column) column dragId)
       |> div [columnsStyle]
  in
    div [] [columns, instructions]

main =
    program
        { init = ( model, Cmd.none)
        , update = update
        , view = view
        , subscriptions = always Sub.none
        }

