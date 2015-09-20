import Environment exposing (init, update, view)
--import Cell exposing (init, update, view)

import StartApp.Simple exposing (start)


main =
  start
    { model = init
    , update = update
    , view = view
    }