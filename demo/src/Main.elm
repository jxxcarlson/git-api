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
import Json.Decode
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
    , new_tree_sha : String
    , new_commit_sha : String
    , commit_message : String
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
    | GitHubFileCreated (Result Http.Error { content : { sha : String } })
    | BlobReceived (Result Http.Error { sha : String })
    | LocalFileRequested FileOperation
    | LocalFileLoaded FileOperation File
    | LocalFileContentDecoded FileOperation String
    | GetHeadRef
    | GotHeadRef (Result Http.Error { sha : String, url : String })
    | GotCommitInfo (Result Http.Error { commit_sha : String, tree_sha : String, tree_url : String })
    | GotTree (Result Http.Error String)
    | TreeCreated (Result Http.Error { sha : String })
    | NewCommitCreated (Result Http.Error { sha : String })
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
      , file_sha = ""
      , owner = "jxxcarlson"
      , repo = "minilatex-docs"
      , branch = "master"
      , headSha = ""
      , headUrl = ""
      , commit_sha = ""
      , tree_sha = ""
      , tree_url = ""
      , new_tree_sha = ""
      , new_commit_sha = ""
      , commit_message = "This is a test"
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

        -- INPUT
        InputAccessToken str ->
            ( { model | authToken = str }, Cmd.none )

        InputSha str ->
            ( { model | file_sha = str }, Cmd.none )

        InputOwner str ->
            ( { model | owner = str }, Cmd.none )

        InputRepo str ->
            ( { model | repo = str }, Cmd.none )

        -- FILE
        LocalFileRequested fileOperation ->
            ( model, Select.file [ "application/text" ] (LocalFileLoaded fileOperation) )

        LocalFileLoaded fileOperation file ->
            ( { model | fileName = File.name file }, Task.perform (LocalFileContentDecoded fileOperation) (File.toString file) )

        LocalFileContentDecoded fileOperation content ->
            ( { model | content = content, output = content |> SHA1.fromString |> SHA1.toHex }
            , createBlob2Cmd fileOperation
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

        -- COMMIT CHANGE
        GetHeadRef ->
            ( model
            , getHeadRefCmd
            )

        GotHeadRef result ->
            case result of
                Ok data ->
                    ( { model
                        | headSha = Debug.log "head, sha" data.sha
                        , headUrl = Debug.log "head, url" data.url
                        , content = "sha: " ++ data.sha ++ ", data: " ++ data.sha ++ "\nurl: " ++ data.url
                      }
                    , getCommitInfoCmd model.owner model.repo data.sha
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
                    , getTreeCmd data.tree_url
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
                    ( { model | file_sha = data.sha }, getHeadRefCmd )

        GitHubFileCreated result ->
            case result of
                Err errMsg ->
                    ( { model | output = Debug.toString errMsg }, Cmd.none )

                Ok reply ->
                    ( { model | output = reply.content.sha, file_sha = reply.content.sha }, Cmd.none )

        GotTree sha ->
            let
                _ =
                    Debug.log "GotTree" sha
            in
            ( model, createTreeCmd model )

        TreeCreated result ->
            let
                _ =
                    Debug.log "MAKE NEW TREE"
            in
            case result of
                Ok reply ->
                    ( { model | new_tree_sha = Debug.log "@@! TreeCreated, new_tree_sha" reply.sha }
                    , createCommitCmd model reply.sha
                    )

                Err err ->
                    ( { model | output = Debug.toString err }, Cmd.none )

        NewCommitCreated result ->
            let
                _ =
                    Debug.log "@@! NewCommitCreated" result
            in
            case result of
                Ok reply ->
                    ( { model | new_commit_sha = reply.sha }, updateRefCmd model reply.sha )

                Err err ->
                    ( { model | output = Debug.toString err }, Cmd.none )

        RefUpdated result ->
            let
                _ =
                    Debug.log "11: End (RefUpdated)"
            in
            case result of
                Ok reply ->
                    ( { model | output = "REF: " ++ reply.sha }, Cmd.none )

                Err err ->
                    ( { model | output = "!!!!" ++ Debug.toString err }, Cmd.none )



-- Cmds


updateRefCmd : { a | authToken : String, owner : String, repo : String, branch : String } -> String -> Cmd Msg
updateRefCmd model newCommitSha =
    Task.attempt RefUpdated
        (Github.updateRef
            { authToken = model.authToken
            , owner = model.owner
            , repo = model.repo
            , branch = model.branch
            , force = True
            , sha = newCommitSha
            }
        )


createCommitCmd : { a | authToken : String, owner : String, repo : String, commit_message : String, headSha : String } -> String -> Cmd Msg
createCommitCmd model new_tree_sha =
    let
        _ =
            Debug.log "@@@" "9: createCommitTask"

        _ =
            Debug.log "@@! new_tree_sha" new_tree_sha
    in
    Task.attempt NewCommitCreated
        (Github.createCommit
            { authToken = model.authToken
            , owner = model.owner
            , repo = model.repo
            , message = Debug.log "@@! MESS" model.commit_message
            , tree = Debug.log "@@! NTSH" new_tree_sha
            , parents = Debug.log "@@! PARENTS" [ model.headSha ]
            }
        )


createTreeCmd : { a | authToken : String, owner : String, repo : String, tree_sha : String, file_sha : String, fileName : String } -> Cmd Msg
createTreeCmd model =
    let
        _ =
            Debug.log "@@@" "8: createTreeTask"
    in
    Task.attempt TreeCreated
        (Github.createTree
            { authToken = model.authToken
            , owner = model.owner
            , repo = model.repo
            , tree_sha = model.tree_sha
            , file_sha = model.file_sha
            , path = model.fileName
            }
        )


getHeadRefCmd : Cmd Msg
getHeadRefCmd =
    let
        _ =
            Debug.log "@@@" "5: getHeadRefTask"
    in
    Task.attempt GotHeadRef
        (Github.getHeadRef { owner = "jxxcarlson", repo = "minilatex-docs", branch = "master" })


createAndCommitFile : FileOperation -> { a | authToken : String, owner : String, repo : String, branch : String, path : String, message : String, content : String } -> Cmd Msg
createAndCommitFile fileOperation params =
    Task.attempt GitHubFileCreated
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


initiateUpdateAndCommit : a -> { b | authToken : String, owner : String, repo : String, content : String } -> Cmd Msg
initiateUpdateAndCommit fileOperation params =
    let
        _ =
            Debug.log "@@@" "4: createBlobTask"
    in
    Task.attempt BlobReceived
        (Github.createBlob
            { authToken = params.authToken
            , owner = params.owner
            , repo = params.repo
            , content = Debug.log "createBlob, content" params.content
            }
        )



-- HELPERS


getTreeCmd : String -> Cmd Msg
getTreeCmd url =
    let
        _ =
            Debug.log "@@@" "7: getTree"
    in
    Http.get
        { url = url
        , expect = Http.expectJson GotTree (Json.Decode.field "sha" Json.Decode.string)
        }


createBlob2Cmd : FileOperation -> { a | authToken : String, owner : String, repo : String, branch : String, path : String, message : String, content : String } -> Cmd Msg
createBlob2Cmd fileOperation params =
    case fileOperation of
        FCreate ->
            createAndCommitFile fileOperation params

        FUpdate ->
            initiateUpdateAndCommit fileOperation params


getCommitInfoCmd : String -> String -> String -> Cmd Msg
getCommitInfoCmd owner repo sha =
    let
        _ =
            Debug.log "@@@" "6: getCommitInfoTask"
    in
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
