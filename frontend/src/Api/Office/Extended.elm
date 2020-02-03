module Api.Office.Extended exposing (toString)

import Api exposing (Office(..))

toString : Office -> String
toString office = 
    case office of
       Home -> "Home"
       Rennes -> "Rennes"
       Poool -> "Poool"
       OutOfOffice -> "Out of office"
