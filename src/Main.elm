import Life exposing (init, update, view, subscriptions)

import Html.App as App


main =
  App.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }
