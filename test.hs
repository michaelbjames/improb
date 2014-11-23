{-# LANGUAGE QuasiQuotes #-}

import Improb.Quote

[improb|
tempo: 120

:piano:
=> (R,4)
(R,4) => (R,2)
(R,4) => (R,3)
(R,2) => (R,4)

:violin:
=> (R,3)
(R,3) => (R,5)
(R,5) => (R,3)
(R,3) => (R,4)
|]

