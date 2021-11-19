module Common exposing (..)

import Browser as B
import ElmEscapeHtml as EEH
import Html as H
import Html.Attributes as Attr


type alias Project =
    { id : String
    , name : String
    }


type alias Todo =
    { id : String
    , projectId : String
    , todo : String
    , status : TodoStatus
    }


type TodoStatus
    = Done
    | Pending


type alias HasBack =
    Bool


viewHeader : HasBack -> String -> H.Html a
viewHeader hasback string =
    H.div
        [ Attr.class "flex items-center text-2xl pb-3 mb-3 border-b border-black"
        ]
        [ H.div [] [ viewBackBtn hasback ]
        , H.h1 [ Attr.class "" ] [ H.text string ]
        ]


viewBackBtn : HasBack -> H.Html a
viewBackBtn hasBack =
    if hasBack then
        H.a [ Attr.href "/" ] [ H.text (EEH.unescape "&#x2190;") ]

    else
        H.span [] []


renderPage : { title : String, body : H.Html a } -> B.Document a
renderPage details =
    { title = details.title
    , body =
        [ H.div [ Attr.class "p-10 w-1/3" ] [ details.body ]
        ]
    }
