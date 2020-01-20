module Main exposing (Model, Msg(..), init, main, subscriptions, update, view)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import String.Extra
import Task
import Time



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
    { zone : Time.Zone
    , time : Time.Posix
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model Time.utc (Time.millisToPosix 0)
    , Task.perform AdjustTimeZone Time.here
    )



-- UPDATE


type Msg
    = ShowTime Time.Posix
    | AdjustTimeZone Time.Zone


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ShowTime time ->
            ( { model | time = time }
            , Cmd.none
            )

        AdjustTimeZone newZone ->
            ( { model | zone = newZone }
            , Cmd.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every 1000 ShowTime



-- VIEW


viewGrid : String -> String -> String -> String -> Html Msg
viewGrid columnClass agesText headerText paragraphText =
    div [ class columnClass ]
        [ h3 [] [ text agesText ]
        , hr [ class "educationLine" ] []
        , h4 [] [ text headerText ]
        , p [] [ text paragraphText ]
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
        textList : List String
        textList =
            [ "HTML", "CSS", "JS", "PHP", "MySQL/SQL", "Bootstrap", "C#", "React" ]
    in
    List.map (\a -> li [] [ Html.a [] [ text a ] ]) textList


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


view : Model -> Browser.Document Msg
view model =
    { title = "Portfolio"
    , body =
        [ div []
            [ header []
                [ navbarView "Martin"
                ]
            ]
        , div [ class "homeBackground" ]
            [ homeView "Welcome to my portfolio" "Nice to meet you" "More about me"
            ]
        , div [ class "aboutBackground" ]
            [ div [ class "container" ]
                [ div [ id "about" ]
                    ([ div [ class "educationGrid" ]
                        [ viewGrid "educationFirst" "2008 - 2016" "Student" "The first school days were in Kehra Gymnasium for 9 years. After 9 class i went to Tallinn Polytechnic."
                        , viewGrid "educationSecond" "2016 - 2019" "Junior Software Developer" "In Tallinn Polytechnic i studied for Junior Software Developer, where i learned how to do websites, programms and apps for mobiles. The languages and technologies HTML, CSS, JS, Bootstrap, PHP, SQL, C# were learned here."
                        , viewGrid "educationThree" "2019 - 2021" "Junior Logistics IT Systems Specialist" "For now i study for Logistics IT Systems Specialist."
                        ]
                     ]
                        |> List.append (titleShadow "About me")
                    )
                ]
            ]
        , div [ class "skillsBackground" ]
            [ div [ class "container" ]
                [ div [ id "skills" ]
                    ([ p [] [ text "I love to learn new technologies. Next to, you can see some of those technologies that i have learned at my software developer path." ]
                     , ul []
                        skillsList
                     ]
                        |> List.append (titleShadow "Skills")
                    )
                ]
            ]
        ]
    }
