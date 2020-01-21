module Main exposing (Model, Msg(..), init, main, subscriptions, update, view)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
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
    { skills : List String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model [ "HTML", "CSS", "JS", "PHP", "MySQL/SQL", "Bootstrap", "C#", "React" ]
    , Cmd.none
    )



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


skillsList : List String -> List (Html Msg)
skillsList skills =
    List.map (\a -> li [] [ Html.a [] [ text a ] ]) skills


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



--, i [ class "devicon-css3-plain-wordmark" ] []


skillsDevicon : List (Html Msg)
skillsDevicon =
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
                    [ div [ class "skillsText" ]
                        ([ p [] [ text "I love to learn new technologies. Next to, you can see some of those technologies that i have learned at my software developer path." ]
                         , ul [] <| skillsList model.skills
                         ]
                            |> List.append (titleShadow "Skills")
                        )
                    ]
                , div [ class "skillsGrid" ]
                    skillsDevicon
                ]
            ]
        , div [ class "portfolio" ]
            [ div [ class "container" ]
                [ div [ id "portfolio" ]
                    ([ div [ class "portfolioGrid" ]
                        []
                     ]
                        |> List.append (titleShadow "Portfolio")
                    )
                ]
            ]
        ]
    }
