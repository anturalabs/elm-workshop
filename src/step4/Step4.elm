module Step4 exposing (..)

import Html exposing (..)
import Html.Events as Events
import Html.Attributes exposing (..)
import Html5.DragDrop as DragDrop
import List.Extra exposing (splitWhen)


-- Model


-- Describes where to insert a card
type Insert =
    Before Int
  | Last


type alias Card =
  { id: Int
  , label: String
  }


type alias Model =
    { cards : List Card
    , dragDrop : DragDrop.Model Int Insert
    , newCardName : String
    }


insertBefore : a -> (a -> Bool) -> List a -> List a
insertBefore insert when into =
  case splitWhen when into of
    Just (before, after) -> before ++ insert :: after
    Nothing -> into


insertLast : a -> List a -> List a
insertLast new xs = xs ++ [new]


moveCard : Int -> Insert -> List Card -> List Card
moveCard cardIdToMove insert cards =
  let
    (otherCards, movedCards) = List.partition (\card -> card.id /= cardIdToMove) cards
  in
    case movedCards of
      movedCard :: rest ->
        case insert of
          Before id -> insertBefore movedCard (\card -> card.id == id) otherCards
          Last -> insertLast movedCard otherCards
      [] -> cards


cardIds cards = List.map (\x -> x.id) cards


nextCardId : List Card -> Int
nextCardId cards =
  let
    existingIds = cardIds cards
  in
    case List.maximum existingIds of
      Just max -> max + 1
      Nothing -> 0


model =
    { cards = [ Card 1 "A card (1)", Card 2 "Another card (2)", Card 3 "Yet another card (3)" ]
    , dragDrop = DragDrop.init
    , newCardName = ""
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


addCard cards name =
  let
    newCard = Card (nextCardId cards) name
  in
    newCard :: cards


update msg model =
  case msg of

    DragDropMsg dragMsg -> doDragDrop dragMsg model

    AddCard -> (
      { model
      | cards = addCard model.cards model.newCardName
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
  [ ("background", "#8A86FA")
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
    handleDropZone element = if withDropZones then (dropZone (Before card.id) :: element :: []) else [element]
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


instruction t = p [] [ text t ]


instructions = div [inputCardStyle]
  [ h1 [] [ text "Step 4 - The Text Field" ]
  , instruction "It's time to add cards"
  , a [href "../step5/Step5.elm"] [text "Step 5"]
  ]


viewCardInput nameSoFar = div [cardStyle]
  [ input [size 14, Events.onInput EnterCardName, value nameSoFar] []
  , button [Events.onClick AddCard] [text "Add"]
  ]


view model =
    let
        dragId = DragDrop.getDragId model.dragDrop
        allCardIds = cardIds model.cards

        lastCard =
          model.cards
          |> List.reverse
          |> List.head

        -- Here we use "cases" to act differently depending on if there is a drag operation active
        -- Checking the last card follows the same pattern
        isLastCardDragged =
          case dragId of
            Just id ->
              case lastCard of
                Just theLastCardIsReal -> theLastCardIsReal.id == id
                Nothing -> False
            Nothing -> False

        -- Here is an alternative to 'case' when converting from a Maybe to a Bool,
        -- dragId is a Maybe because there might not be any dragging going on
        isCardBeforeDragged cardId =
          dragId
          |> Maybe.map (\draggedId -> isOneBeforeTheOther draggedId cardId allCardIds)
          |> Maybe.withDefault False

        showZones cardId =
          case dragId of
            Just id -> cardId /= id && not (isCardBeforeDragged cardId)
            Nothing -> False

        lastDropZone =
          case dragId of
            Just id -> if isLastCardDragged then [] else [dropZone Last]
            Nothing -> []

        viewCards = List.map (\card -> viewCard card (showZones card.id)) model.cards
    in
        div [] [ div [columnStyle] ((viewCardInput model.newCardName) :: viewCards ++ lastDropZone), instructions ]


main =
    program
        { init = ( model, Cmd.none )
        , update = update
        , view = view
        , subscriptions = always Sub.none
        }

