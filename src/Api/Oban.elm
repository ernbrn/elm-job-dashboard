module Api.Oban exposing
    ( Job
    , job
    )

{-|
@docs Job
@docs jobs
-}

import Api
import Http
import Json.Decode as Decode


type alias Job =
    { worker : String
    , inserted_at : String
    , state : String
    , completed_at : Maybe String
    }


decoder : Decode.Decoder Job
decoder =
    Decode.map4 Job
        (Decode.field "worker" Decode.string)
        (Decode.field "inserted_at" Decode.string)
        (Decode.field "state" Decode.string)
        (Decode.maybe (Decode.field "completed_at" Decode.string))



-- ENDPOINTS


job : { onResponse : Api.Data (List Job) -> msg } -> Cmd msg
job =
    jobs "oban_jobs"


jobs : String -> { onResponse : Api.Data (List Job) -> msg } -> Cmd msg
jobs endpoint options =
    Http.get
        { url = "http://localhost:4000/api/" ++ endpoint
        , expect =
            Api.expectJson options.onResponse
                (Decode.list decoder)
        }