module Main exposing (main)

{- This app show how one can use the package avh4/elm-github-v3 to
   add files to a github repo, then update them as needed along with
   a commit message.  The app has two buttons "Create file" and "Update file,"
   both of which bring up file browser to choose a file to be added to the
   repository or to be updated.

   You can view the latest version of the file this way:

        https://github.com/:owner/:repo/blob/master/:filename

   The segment :filename should be understood as :path.  While this app
   is set up to use the master branch, that can also be set by the user.

   To use the app, you will need an `authToken`.  This can be a personal
   access tokn or an OAuth token: go to your Github page, to Settings, choose
   Developer Settings, and you wll find it.
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
    , owner : String
    , repo : String
    , branch : String
    , fileName : String
    , message : String
    , output : String
    }



-- MSG


type Msg
    = NoOp
      -- INPUT
    | InputAccessToken String
    | InputOwner String
    | InputRepo String
    | InputMessage String
      -- FILE
    | LocalFileRequested FileOperation
    | LocalFileLoaded FileOperation File
    | LocalFileContentDecoded FileOperation String
      -- REPO
    | GitHubFileCreated (Result Http.Error { content : { sha : String } })
    | RefUpdated (Result Http.Error { sha : String })


type FileOperation
    = FCreate
    | FUpdate


type alias Flags =
    {}


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { authToken = ""
      , output = ""
      , owner = "jxxcarlson"
      , repo = "minilatex-docs"
      , branch = "master"
      , message = ""
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

        -- INPUT
        InputAccessToken str ->
            ( { model | authToken = str }, Cmd.none )

        InputOwner str ->
            ( { model | owner = str }, Cmd.none )

        InputRepo str ->
            ( { model | repo = str }, Cmd.none )

        InputMessage str ->
            ( { model | message = str }, Cmd.none )

        -- FILE
        LocalFileRequested fileOperation ->
            ( model, Select.file [ "application/text" ] (LocalFileLoaded fileOperation) )

        LocalFileLoaded fileOperation file ->
            ( { model | fileName = File.name file }, Task.perform (LocalFileContentDecoded fileOperation) (File.toString file) )

        -- REPO
        LocalFileContentDecoded fileOperation content ->
            ( { model | content = content, output = content |> SHA1.fromString |> SHA1.toHex }
            , createBlobCmd fileOperation
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

        GitHubFileCreated result ->
            case result of
                Err errMsg ->
                    ( { model | output = Debug.toString errMsg }, Cmd.none )

                Ok reply ->
                    ( { model | output = reply.content.sha }, Cmd.none )

        RefUpdated result ->
            case result of
                Ok reply ->
                    ( { model | output = reply.sha }, Cmd.none )

                Err err ->
                    ( { model | output = Debug.toString err }, Cmd.none )



-- HELPERS


createAndCommitFile : { a | authToken : String, owner : String, repo : String, branch : String, path : String, message : String, content : String } -> Cmd Msg
createAndCommitFile params =
    Task.attempt GitHubFileCreated
        (Github.updateFileContents
            { authToken = params.authToken
            , owner = params.owner
            , repo = params.repo
            , branch = params.branch
            , path = params.path
            , sha = ""
            , message = params.message
            , content = params.content
            }
        )


createBlobCmd : FileOperation -> { a | authToken : String, owner : String, repo : String, branch : String, path : String, message : String, content : String } -> Cmd Msg
createBlobCmd fileOperation params =
    case fileOperation of
        FCreate ->
            createAndCommitFile params

        FUpdate ->
            Task.attempt RefUpdated
                (Github.updateAndCommit
                    { authToken = params.authToken
                    , owner = params.owner
                    , repo = params.repo
                    , fileName = params.path
                    , content = params.content
                    , message = params.message
                    }
                )



-- VIEW


view : Model -> Html Msg
view model =
    Element.layout [] (mainColumn model)


mainColumn : Model -> Element Msg
mainColumn model =
    column mainColumnStyle
        [ column [ padding 40, spacing 20, width (px 600), height (px 600) ]
            [ title "GitHub API"
            , inputAccessToken model
            , inputOwner model
            , inputRepo model
            , inputMessage model
            , row [ spacing 12, Element.moveRight 4 ] [ createBlobButton, updateBlobButton ]
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


inputMessage : Model -> Element Msg
inputMessage model =
    Input.text []
        { onChange = InputMessage
        , text = model.message
        , placeholder = Just (Input.placeholder [] (el [] (text "commit message")))
        , label = Input.labelLeft [] <| el [] (text "")
        }


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



-- STYLE


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
