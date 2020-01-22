module Main exposing (Model, Msg(..), init, main, subscriptions, update, view)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Json.Decode exposing (Decoder, float, int, nullable, string)
import Json.Decode.Pipeline as DP
import String.Extra



-- MAIN


main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { translation : Result Json.Decode.Error Language }


type alias Language =
    { portfolio : String
    , firstName : String
    , welcomeHeaderText : String
    , welcomeTitle : String
    , welcomeButton : String
    , student : String
    , schoolKehraText : String
    , juniorSoftwareDev : String
    , tallinnPolytechnicText : String
    , juniorLogIT : String
    , tthkText : String
    , aboutMe : String
    , skillsText : String
    , skills : String
    , websiteOfGames : String
    , websiteOfLaptops : String
    , websiteOfCelebrationPlanner : String
    , ticTacToe : String
    , rockPaperScissors : String
    , becomeAHacker : String
    , freeCodeCamp : String
    , contact : String
    , sendAMail : String
    , footerText : String
    }


decodeTranslations : Json.Decode.Value -> Result Json.Decode.Error Language
decodeTranslations englishTranslation =
    Json.Decode.decodeValue langDecoder englishTranslation


langDecoder : Json.Decode.Decoder Language
langDecoder =
    Json.Decode.succeed Language
        |> DP.required "portfolio" Json.Decode.string
        |> DP.required "firstName" Json.Decode.string
        |> DP.required "welcomeHeaderText" Json.Decode.string
        |> DP.required "welcomeTitle" Json.Decode.string
        |> DP.required "welcomeButton" Json.Decode.string
        |> DP.required "student" Json.Decode.string
        |> DP.required "schoolKehraText" Json.Decode.string
        |> DP.required "juniorSoftwareDev" Json.Decode.string
        |> DP.required "tallinnPolytechnicText" Json.Decode.string
        |> DP.required "juniorLogIT" Json.Decode.string
        |> DP.required "tthkText" Json.Decode.string
        |> DP.required "aboutMe" Json.Decode.string
        |> DP.required "skillsText" Json.Decode.string
        |> DP.required "skills" Json.Decode.string
        |> DP.required "websiteOfGames" Json.Decode.string
        |> DP.required "websiteOfLaptops" Json.Decode.string
        |> DP.required "websiteOfCelebrationPlanner" Json.Decode.string
        |> DP.required "ticTacToe" Json.Decode.string
        |> DP.required "rockPaperScissors" Json.Decode.string
        |> DP.required "becomeAHacker" Json.Decode.string
        |> DP.required "freeCodeCamp" Json.Decode.string
        |> DP.required "contact" Json.Decode.string
        |> DP.required "sendAMail" Json.Decode.string
        |> DP.required "footerText" Json.Decode.string


decodePortfolio : Json.Decode.Value -> Result Json.Decode.Error String
decodePortfolio englishTranslation =
    Json.Decode.decodeValue oneDecoder englishTranslation


oneDecoder : Json.Decode.Decoder String
oneDecoder =
    Json.Decode.field "firstName" Json.Decode.string


init : Json.Decode.Value -> ( Model, Cmd Msg )
init englishTranslation =
    let
        translations : Result Json.Decode.Error Language
        translations =
            decodeTranslations englishTranslation

        _ =
            Debug.log "Log" <| decodeTranslations englishTranslation
    in
    ( Model translations, Cmd.none )



-- UPDATE


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model
            , Cmd.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


educationGrid : String -> String -> String -> String -> Html Msg
educationGrid columnClass agesText headerText paragraphText =
    div [ class columnClass ]
        [ h3 [] [ text agesText ]
        , hr [ class "educationLine" ] []
        , h4 [] [ text headerText ]
        , p [] [ text paragraphText ]
        ]


portfolioTile : String -> String -> String -> Html Msg
portfolioTile site imageSrc title =
    div [ class "project-tile" ]
        [ a [ class "project", href site, target "_blank" ]
            [ img [ class "project-image", src imageSrc ] []
            , p [ class "project-title" ]
                [ span [ class "code" ] [ text "<" ]
                , text title
                , span [ class "code" ] [ text ">" ]
                ]
            ]
        ]


navbarList : String -> Html Msg
navbarList logoName =
    let
        linkList : List String
        linkList =
            [ "home"
            , "about"
            , "skills"
            , "portfolio"
            , "contact"
            ]

        navigationList : List (Html Msg)
        navigationList =
            linkList
                |> List.map (\a -> li [ href ("#" ++ a) ] [ Html.a [] [ text <| String.Extra.toTitleCase a ] ])
    in
    ul [ class "menu touch" ]
        (List.append
            [ li [] [ a [ href "#home", class "logo" ] [ text <| String.Extra.toTitleCase logoName ] ] ]
            navigationList
        )


skillsList : List (Html Msg)
skillsList =
    let
        list : List String
        list =
            [ "HTML"
            , "CSS"
            , "JS"
            , "PHP"
            , "MySQL/SQL"
            , "Bootstrap"
            , "C#"
            , "React"
            ]
    in
    List.map (\a -> li [] [ Html.a [] [ text a ] ]) list


navbarView : String -> Html Msg
navbarView logoName =
    nav [ id "nav" ]
        [ input [ type_ "checkbox", id "checkbox-menu" ] []
        , label [ for "checkbox-menu" ]
            [ navbarList logoName
            , span [ class "toggle" ] [ text "☰" ]
            ]
        ]


homeView : String -> String -> String -> Html Msg
homeView headerText welcomeTitle buttonText =
    div [ id "home" ]
        [ h1 [ class "welcome" ] [ text headerText ]
        , p [ class "welcomeTitle" ] [ text welcomeTitle ]
        , a [ href "#about", class "btn buttonMore" ] [ text buttonText ]
        , a [ href "#about", class "downButton" ] [ i [ class "fa fa-angle-double-down" ] [] ]
        ]


titleShadow : String -> List (Html Msg)
titleShadow title =
    [ div [ class <| String.Extra.decapitalize <| String.Extra.camelize <| String.Extra.unsurround "" title ++ "Title" ] [ text title ]
    , div [ class <| String.Extra.decapitalize <| String.Extra.camelize <| String.Extra.unsurround "" title ++ "TitleShadow" ] [ text title ]
    ]


skillDevicons : List (Html Msg)
skillDevicons =
    let
        skills : List String
        skills =
            [ "devicon-css3-plain-wordmark"
            , "devicon-javascript-plain"
            , "devicon-php-plain"
            , "devicon-mysql-plain"
            , "devicon-bootstrap-plain"
            , "devicon-react-original-wordmark"
            , "devicon-csharp-plain"
            ]
    in
    List.map (\a -> li [ class a ] []) skills


view : Model -> Browser.Document Msg
view model =
    case model.translation of
        Ok lang ->
            { title = lang.portfolio
            , body =
                [ div []
                    [ header []
                        [ navbarView lang.firstName
                        ]
                    ]
                , div [ class "homeBackground" ]
                    [ homeView lang.welcomeHeaderText lang.welcomeTitle lang.welcomeButton
                    ]
                , div [ class "aboutBackground" ]
                    [ div [ class "container" ]
                        [ div [ id "about" ]
                            ([ div [ class "educationGrid" ]
                                [ educationGrid "educationFirst" "2008 - 2016" lang.student lang.schoolKehraText
                                , educationGrid "educationSecond" "2016 - 2019" lang.juniorSoftwareDev lang.tallinnPolytechnicText
                                , educationGrid "educationThree" "2019 - 2021" lang.juniorLogIT lang.tthkText
                                ]
                             ]
                                |> List.append (titleShadow lang.aboutMe)
                            )
                        ]
                    ]
                , div [ class "skillsBackground" ]
                    [ div [ class "container" ]
                        [ div [ id "skills" ]
                            ([ div [ class "skillsText" ]
                                [ p [] [ text lang.skillsText ]
                                , ul [] <| skillsList
                                ]
                             ]
                                |> List.append (titleShadow lang.skills)
                            )
                        , div [ class "skillsGrid" ]
                            skillDevicons
                        ]
                    ]
                , div [ class "portfolioBackground" ]
                    [ div [ class "container" ]
                        [ div [ id "portfolio" ]
                            ([ div [ class "portfolioGrid" ]
                                [ portfolioTile "http://nitram278.000webhostapp.com/" "img/mGames.png" lang.websiteOfGames
                                , portfolioTile "http://method27.000webhostapp.com/laptops/" "img/laptops.png" lang.websiteOfLaptops
                                , portfolioTile "https://martceelebrate.000webhostapp.com" "img/celebrationPlanner.png" lang.websiteOfCelebrationPlanner
                                , portfolioTile "https://play.google.com/store/apps/details?id=com.nitram.tictactoeMS" "img/ticTacToe.png" lang.ticTacToe
                                , portfolioTile "https://play.google.com/store/apps/details?id=com.nitram278.rockPaperScissors" "img/rockPaperScissors.png" lang.rockPaperScissors
                                , portfolioTile "https://play.google.com/store/apps/details?id=com.MSANDevs.Becomeahacker" "img/becomeAHacker.png" lang.becomeAHacker
                                , portfolioTile "https://codepen.io/Matrix27/pens/public" "https://www.npofocus.nl/thumbs/i/14000/mod_media_image/14105.w1913.0.c9cc1fa.png" lang.freeCodeCamp
                                ]
                             ]
                                |> List.append (titleShadow lang.portfolio)
                            )
                        ]
                    ]
                ]
            }

        Err err ->
            { title = "Portfolio"
            , body = [ div [] [ text "Error" ] ]
            }
