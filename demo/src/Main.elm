module Main exposing (main)

{- This is a starter app which presents a text label, text field, and a button.
   What you enter in the text field is echoed in the label.  When you press the
   button, the text in the label is reverse.
   This version uses `mdgriffith/elm-ui` for the view functions.
-}

import Base64
import Browser
import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import Element.Input as Input
import File exposing (File)
import File.Select as Select
import Github
import Html exposing (Html)
import Http
import SHA1
import Task


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { content : String
    , authToken : String
    , file_sha : String
    , owner : String
    , repo : String
    , branch : String
    , fileName : String
    , headSha : String
    , headUrl : String
    , commit_sha : String
    , tree_sha : String
    , tree_url : String
    , message : String
    , output : String
    }



-- MSG


type Msg
    = NoOp
    | InputAccessToken String
    | InputSha String
    | InputOwner String
    | InputRepo String
    | CreateBlob
    | GitHubFileCreated FileOperation (Result Http.Error { content : { sha : String } })
    | BlobReceived (Result Http.Error { sha : String })
    | LocalFileRequested FileOperation
    | LocalFileLoaded FileOperation File
    | LocalFileContentDecoded FileOperation String
    | GetHeadRef
    | GotHeadRef (Result Http.Error { sha : String, url : String })
    | GotCommitInfo (Result Http.Error { commit_sha : String, tree_sha : String, tree_url : String })


type FileOperation
    = FCreate
    | FUpdate


type alias Flags =
    {}


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { authToken = ""
      , output = ""
      , file_sha = ""
      , owner = "jxxcarlson"
      , repo = "minilatex-docs"
      , branch = "master"
      , headSha = ""
      , headUrl = ""
      , commit_sha = ""
      , tree_sha = ""
      , tree_url = ""
      , message = "No message for now"
      , fileName = ""
      , content = ""
      }
    , Cmd.none
    )


subscriptions model =
    Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        InputAccessToken str ->
            ( { model | authToken = str }, Cmd.none )

        InputSha str ->
            ( { model | file_sha = str }, Cmd.none )

        InputOwner str ->
            ( { model | owner = str }, Cmd.none )

        InputRepo str ->
            ( { model | repo = str }, Cmd.none )

        -- GetBlob ->
        --     ( model
        --     , Task.attempt BlobReceived
        --         (Github.getBlob
        --             { owner = model.owner
        --             , repo = model.repo
        --             , sha = model.file_sha
        --             }
        --         )
        --     )
        GetHeadRef ->
            ( model
            , getHeadRefTask
            )

        GotHeadRef result ->
            case result of
                Ok data ->
                    ( { model
                        | headSha = Debug.log "head, sha" data.sha
                        , headUrl = Debug.log "head, url" data.url
                        , content = "sha: " ++ data.sha ++ ", data: " ++ data.sha ++ "\nurl: " ++ data.url
                      }
                    , getCommitInfo model.owner model.repo data.sha
                    )

                Err err ->
                    ( { model | output = Debug.toString err }, Cmd.none )

        GotCommitInfo result ->
            case result of
                Ok data ->
                    let
                        _ =
                            Debug.log "GotCommitInfo" data
                    in
                    -- TODO createBlob
                    ( { model
                        | commit_sha = data.commit_sha
                        , tree_sha = data.tree_sha
                        , tree_url = data.tree_url
                      }
                    , getTree data.tree_url
                    )

                Err err ->
                    ( { model | output = Debug.toString err }, Cmd.none )

        BlobReceived result ->
            let
                _ =
                    Debug.log "BlobReceived" result
            in
            case result of
                Err errMsg ->
                    ( { model | output = Debug.toString errMsg }, Cmd.none )

                Ok data ->
                    ( { model | file_sha = data.sha }, getHeadRefTask )

        CreateBlob ->
            -- TODO: check out sha field
            ( model
            , createBlob FCreate
                { authToken = model.authToken
                , owner = model.owner
                , repo = model.repo
                , branch = model.branch
                , path = model.fileName
                , sha = ""
                , message = model.message
                , content = model.content
                }
            )

        GitHubFileCreated fileOperation result ->
            let
                _ =
                    Debug.log "GitHubFileCreated" result

                cmd =
                    case fileOperation of
                        FCreate ->
                            Cmd.none

                        FUpdate ->
                            -- Github.getHeadRef { owner = "jxxcarlson", repo = "minilatex-docs", branch = "master" }
                            Cmd.none
            in
            case result of
                Err errMsg ->
                    ( { model | output = Debug.toString errMsg }, cmd )

                Ok reply ->
                    ( { model | output = reply.content.sha, file_sha = reply.content.sha }, cmd )

        LocalFileRequested fileOperation ->
            ( model, Select.file [ "application/text" ] (LocalFileLoaded fileOperation) )

        LocalFileLoaded fileOperation file ->
            let
                _ =
                    Debug.log "FILE" (File.name file)

                _ =
                    Debug.log "SIZE" (File.size file)
            in
            ( { model | fileName = File.name file }, Task.perform (LocalFileContentDecoded fileOperation) (File.toString file) )

        LocalFileContentDecoded fileOperation content ->
            let
                _ =
                    Debug.log "CONTENT" content
            in
            -- TODO checkout sha field
            ( { model | content = content, output = content |> SHA1.fromString |> SHA1.toHex }
            , createBlob fileOperation
                { authToken = model.authToken
                , owner = model.owner
                , repo = model.repo
                , branch = model.branch
                , path = model.fileName
                , sha = ""
                , message = model.message
                , content = content
                }
            )


getTree : String -> Cmd Msg
getTree url =
    Http.get
        { url = url
        , expect = Http.expectString GotTree
        }



-- HELPERS
-- https://api.github.com/repos/jxxcarlson/minilatex-docs/contents/jabberwocky.txt
-- https://api.github.com/repos/jxxcarlson/minilatex-docs/contents/jabberwocky.txt?refs=master
-- http://www.levibotelho.com/development/commit-a-file-with-the-github-api/
-- createBlob :
--     FileOperation
--     ->
--         { authToken : String
--         , repo : String
--         , branch : String
--         , path : String
--         , message : String
--         , content : String
--         }


createBlob fileOperation params =
    let
        sha =
            Debug.log "createBlob, sha"
                (case fileOperation of
                    FCreate ->
                        ""

                    FUpdate ->
                        -- params.content |> SHA1.fromString |> SHA1.toHex
                        "54f9d6da5c91d556e6b54340b1327573073030af"
                )
    in
    case fileOperation of
        FCreate ->
            createFileTask fileOperation params

        FUpdate ->
            createBlobTask fileOperation params


getHeadRefTask =
    Task.attempt GotHeadRef
        (Github.getHeadRef { owner = "jxxcarlson", repo = "minilatex-docs", branch = "master" })


createFileTask fileOperation params =
    Task.attempt (GitHubFileCreated fileOperation)
        (Github.updateFileContents
            { authToken = params.authToken
            , owner = params.owner
            , repo = params.repo
            , branch = params.branch
            , path = params.path
            , sha = ""
            , message = params.message
            , content = Debug.log "createBlob, content" params.content
            }
        )


createBlobTask fileOperation params =
    Task.attempt BlobReceived
        (Github.createBlob
            { authToken = params.authToken
            , owner = params.owner
            , repo = params.repo
            , content = Debug.log "createBlob, content" params.content
            }
        )


getCommitInfo owner repo sha =
    Task.attempt GotCommitInfo
        (Github.getCommitInfo
            { owner = owner
            , repo = repo
            , sha = sha
            }
        )



--
-- VIEW
--


view : Model -> Html Msg
view model =
    Element.layout [] (mainColumn model)


mainColumn : Model -> Element Msg
mainColumn model =
    column mainColumnStyle
        [ column [ padding 40, spacing 20, width (px 600), height (px 600) ]
            [ title "GitHub API"
            , inputAccessToken model
            , inputSha model
            , inputOwner model
            , inputRepo model
            , row [ spacing 12, Element.moveRight 4 ] [ createBlobButton, updateBlobButton, getHeadRefButton ]
            , el [ Font.size 14 ] (text ("sha: " ++ model.output))
            , outputDisplay model
            ]
        ]


title : String -> Element msg
title str =
    row [ Font.bold, Font.size 24, paddingXY 0 12 ] [ text str ]


outputDisplay : Model -> Element msg
outputDisplay model =
    row
        [ paddingXY 0 12
        , Font.size 14
        , Background.color (Element.rgb 1 1 1)
        , width (px 600)
        , height (px 200)
        , padding 12
        , Element.scrollbarY
        , Element.scrollbarX
        ]
        [ el [ Element.alignTop ] (text model.content) ]


inputAccessToken : Model -> Element Msg
inputAccessToken model =
    Input.text []
        { onChange = InputAccessToken
        , text = model.authToken
        , placeholder = Just (Input.placeholder [] (el [] (text "access token")))
        , label = Input.labelLeft [] <| el [] (text "")
        }


inputSha : Model -> Element Msg
inputSha model =
    Input.text []
        { onChange = InputSha
        , text = model.file_sha
        , placeholder = Just (Input.placeholder [] (el [] (text "sha")))
        , label = Input.labelLeft [] <| el [] (text "")
        }


inputOwner : Model -> Element Msg
inputOwner model =
    Input.text []
        { onChange = InputOwner
        , text = model.owner
        , placeholder = Just (Input.placeholder [] (el [] (text "owner")))
        , label = Input.labelLeft [] <| el [] (text "")
        }


inputRepo : Model -> Element Msg
inputRepo model =
    Input.text []
        { onChange = InputRepo
        , text = model.repo
        , placeholder = Just (Input.placeholder [] (el [] (text "repo")))
        , label = Input.labelLeft [] <| el [] (text "")
        }


getHeadRefButton : Element Msg
getHeadRefButton =
    row [ centerX ]
        [ Input.button buttonStyle
            { onPress = Just GetHeadRef
            , label = el [ width (px 100), centerX, centerY ] (text "HEAD ref")
            }
        ]



-- getBlobButton : Element Msg
-- getBlobButton =
--     row [ centerX ]
--         [ Input.button buttonStyle
--             { onPress = Just GetBlob
--             , label = el [ width (px 100), centerX, centerY ] (text "Get blob")
--             }
--         ]


createBlobButton : Element Msg
createBlobButton =
    row [ centerX ]
        [ Input.button buttonStyle
            { onPress = Just (LocalFileRequested FCreate)
            , label = el [ width (px 100), centerX, centerY ] (text "Create file")
            }
        ]


updateBlobButton : Element Msg
updateBlobButton =
    row [ centerX ]
        [ Input.button buttonStyle
            { onPress = Just (LocalFileRequested FUpdate)
            , label = el [ width (px 100), centerX, centerY ] (text "Update file")
            }
        ]



--
-- STYLE
--


mainColumnStyle =
    [ centerX
    , centerY
    , Background.color (rgb255 240 240 240)
    , paddingXY 20 20
    , width (px 700)
    , height (px 700)
    ]


buttonStyle =
    [ Background.color (rgb255 40 40 40)
    , Font.color (rgb255 255 255 255)
    , paddingXY 15 8
    ]
