module Pages.Jobs exposing (Params, Model, Msg, page)

import Spa.Document exposing (Document)
import Spa.Page as Page exposing (Page)
import Spa.Url as Url exposing (Url)


import Api
import Api.Oban exposing (Job)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
import Html exposing (Html)

page : Page Params Model Msg
page =
    Page.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- INIT


type alias Params =
    ()


type alias Model =
    { jobs : Api.Data (List Job)
    }


init : Url Params -> ( Model, Cmd Msg )
init url =
    ( Model Api.Loading
    , Api.Oban.job
        { onResponse = GotObanJobs
        }
    )


-- UPDATE


type Msg
    = GotObanJobs (Api.Data (List Job))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotObanJobs data ->
            ( { model | jobs = data} 
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Document Msg
view model =
    { title = "Jobs!"
    , body = [viewJobs model.jobs]
    }

viewJobs : Api.Data (List Job) -> Element msg
viewJobs data =
    case data of
        Api.NotAsked ->
            text "Not asked"

        Api.Loading ->
            text "Loading..."

        Api.Failure _ ->
            text "Something went wrong..."

        Api.Success jobs ->
            column [] (List.map viewJob jobs)

viewJob : Job -> Element msg
viewJob job =
    column 
        [explain Debug.todo] 
        [ labelText "Worker" job.worker
        , labelText "Inserted at" job.inserted_at
        , labelText "State" job.state
        , maybeLabelText "Completed at" job.completed_at
        ]

labelText : String -> String -> Element msg
labelText label value =
    row [] [
        el [Font.bold] (text (String.concat [label, ": "]))
        , el [] (text value)
    ]

maybeLabelText : String -> Maybe String -> Element msg
maybeLabelText label value =
    case value of
        Just val ->
            labelText label val
        Nothing ->
            none
