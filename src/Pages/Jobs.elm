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
    { jobs : List Job
    , filteredJobs : List Job
    , filter : Filter
    , status : Api.Data (List Job)
    }

type Filter 
    = All 
     | Completed
     | Discarded


init : Url Params -> ( Model, Cmd Msg )
init url =
    ( Model [] [] All Api.Loading 
    , Api.Oban.job
        { onResponse = GotObanJobs
        }
    )


-- UPDATE


type Msg
    = GotObanJobs (Api.Data (List Job))
    | FilterChange Filter


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FilterChange f ->
            ({ model | filter = f, filteredJobs = (filterJobs f model.jobs) }, Cmd.none)
        GotObanJobs data ->
            case data of
                Api.Success j ->
                    ( { model | jobs = j, filteredJobs = j, status = data} 
                    , Cmd.none
                    )
                _ ->
                    ( { model | status = data} 
                    , Cmd.none
                    )

                


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Document Msg
view model =
    { title = "Jobs!"
    , body = [filterBar model.filter, (viewJobs model.status model.filteredJobs)]
    }

viewJobs : Api.Data (List Job) -> (List Job) -> Element msg
viewJobs status filteredJobs =
    case status of
        Api.NotAsked ->
            text "Not asked"

        Api.Loading ->
            text "Loading..."

        Api.Failure _ ->
            text "Something went wrong..."

        Api.Success _ ->
            column [] (List.map viewJob filteredJobs)

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

filterBar : Filter -> Element Msg
filterBar f =
    column [] [ el [Font.bold] (filterText f)
    , Input.button [] {onPress = Just (FilterChange All), label = text "All"}
    , Input.button [] {onPress = Just (FilterChange Completed), label = text "Completed"}
    , Input.button [] {onPress = Just (FilterChange Discarded), label = text "Discarded"}
    ]




filterText f =
    case f of
        All ->
            text "All"
        Completed ->
            text "Completed"
        Discarded ->
            text "Discarded"

filterJobs : Filter -> List Job -> List Job
filterJobs filter jobs =
    List.filter (filterJob filter) jobs


filterJob : Filter -> Job -> Bool
filterJob filter job =
    case filter of
        Completed ->
            job.state == "completed"
        Discarded ->
            job.state == "discarded"
        All ->
            True
