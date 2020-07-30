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
    , accessToken : String
    , sha : String
    , owner : String
    , repo : String
    , fileName : String
    , headSha : String
    , headUrl : String
    , output : String
    }



-- MSG


type Msg
    = NoOp
    | InputAccessToken String
    | InputSha String
    | InputOwner String
    | InputRepo String
    | CreateGithubFile
    | GitHubFileCreated (Result Http.Error { content : { sha : String } })
    | GetBlob
    | BlobReceived (Result Http.Error String)
    | LocalFileRequested
    | LocalFileLoaded File
    | LocalFileContentDecoded String
    | GetHeadRef
    | GotHeadRef (Result Http.Error { sha : String, url : String })


type alias Flags =
    {}


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { accessToken = "dc739157b778090b18841a6a6dd02adfebff85e9"
      , output = ""
      , sha = ""
      , owner = "jxxcarlson"
      , repo = "minilatex-docs"
      , headSha = ""
      , headUrl = ""
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
            ( { model | accessToken = str }, Cmd.none )

        InputSha str ->
            ( { model | sha = str }, Cmd.none )

        InputOwner str ->
            ( { model | owner = str }, Cmd.none )

        InputRepo str ->
            ( { model | repo = str }, Cmd.none )

        GetBlob ->
            ( model
            , Task.attempt BlobReceived
                (Github.getBlob
                    { owner = model.owner
                    , repo = model.repo
                    , sha = model.sha
                    }
                )
            )

        GetHeadRef ->
            ( model
            , Task.attempt GotHeadRef
                (Github.getHeadRef { owner = "jxxcarlson", repo = "minilatex-docs", branch = "master" })
            )

        GotHeadRef result ->
            case result of
                Ok data ->
                    ( { model
                        | headSha = Debug.log "head, sha" data.sha
                        , headUrl = Debug.log "head, url" data.url
                        , content = "sha: " ++ data.sha ++ ", data: " ++ data.sha ++ "\nurl: " ++ data.url
                      }
                    , Cmd.none
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

                Ok str ->
                    case Base64.decode (String.trim str) of
                        Err _ ->
                            ( { model | output = "Error: " ++ str }, Cmd.none )

                        Ok content ->
                            ( { model | content = content }, Cmd.none )

        CreateGithubFile ->
            ( model
            , createGithubFile model model.fileName model.content
            )

        GitHubFileCreated result ->
            let
                _ =
                    Debug.log "GitHubFileCreated" result
            in
            case result of
                Err errMsg ->
                    ( { model | output = Debug.toString errMsg }, Cmd.none )

                Ok reply ->
                    ( { model | output = reply.content.sha }, Cmd.none )

        LocalFileRequested ->
            ( model, Select.file [ "application/text" ] LocalFileLoaded )

        LocalFileLoaded file ->
            let
                _ =
                    Debug.log "FILE" (File.name file)

                _ =
                    Debug.log "SIZE" (File.size file)
            in
            ( { model | fileName = File.name file }, Task.perform LocalFileContentDecoded (File.toString file) )

        LocalFileContentDecoded content ->
            let
                _ =
                    Debug.log "CONTENT" content
            in
            ( { model | content = content, output = content |> SHA1.fromString |> SHA1.toHex }
            , createGithubFile model model.fileName content
            )



-- HELPERS
-- https://api.github.com/repos/jxxcarlson/minilatex-docs/contents/jabberwocky.txt
-- https://api.github.com/repos/jxxcarlson/minilatex-docs/contents/jabberwocky.txt?refs=master
-- http://www.levibotelho.com/development/commit-a-file-with-the-github-api/


createGithubFile model fileName content =
    Task.attempt GitHubFileCreated
        (Github.updateFileContents
            { authToken = model.accessToken
            , owner = model.owner
            , repo = model.repo
            , branch = "master"
            , path = fileName
            , sha = content |> SHA1.fromString |> SHA1.toHex
            , message = "First commit"
            , content = content
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
            , row [ spacing 12, Element.moveRight 4 ] [ getBlobButton, createBlobButton, getHeadRefButton ]
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
        , text = model.accessToken
        , placeholder = Just (Input.placeholder [] (el [] (text "access token")))
        , label = Input.labelLeft [] <| el [] (text "")
        }


inputSha : Model -> Element Msg
inputSha model =
    Input.text []
        { onChange = InputSha
        , text = model.sha
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


getBlobButton : Element Msg
getBlobButton =
    row [ centerX ]
        [ Input.button buttonStyle
            { onPress = Just GetBlob
            , label = el [ width (px 100), centerX, centerY ] (text "Get blob")
            }
        ]


createBlobButton : Element Msg
createBlobButton =
    row [ centerX ]
        [ Input.button buttonStyle
            { onPress = Just LocalFileRequested
            , label = el [ width (px 100), centerX, centerY ] (text "Create file")
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


strr =
    """
Twas brillig, and the slithy toves
Did gyre and gimble in the wabe;
All mimsy were the borogoves,
And the mome raths outgrabe.

Beware the Jabberwock, my son!
The jaws that bite, the claws that catch!
Beware the Jubjub bird, and shun
The frumious Bandersnatch!

He took his vorpal sword in hand:
Long time the manxome foe he sought —
So rested he by the Tumtum tree,
And stood awhile in thought.

And as in uffish thought he stood,
The Jabberwock, with eyes of flame,
Came whiffling through the tulgey wood,
And burbled as it came!

One, two! One, two! And through and through
The vorpal blade went snicker-snack!
He left it dead, and with its head
He went galumphing back.

And hast thou slain the Jabberwock?
Come to my arms, my beamish boy!
O frabjous day! Callooh! Callay!
He chortled in his joy.

Twas brillig, and the slithy toves
Did gyre and gimble in the wabe;
All mimsy were the borogoves,
And the mome raths outgrabe.
"""
