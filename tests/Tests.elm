module Tests exposing (..)

import Test exposing (..)
import Expect
import Fuzz exposing (list, int, tuple, string)
import String

import Bread exposing (..)

bindOperations = 
  let start = read [0xaf, 0xff]
      skip = bitNum 2
      getTup = tuple2 (readBitString 2) (bitNum 2)
      getBitNum = (bitNum 2)
    
      (result, _) = start <| 
                    skip >>| 
                    getTup >>= \tupData -> 
                    getBitNum >>= \x ->
                    return ("data", x, tupData)      
  in
    result

all : Test
all =
    describe "Sample Test Suite"
        [ describe "test composition"
            [ test "Mixed output" <|
                \() ->
                    Expect.equal ("data", 2, ([On, On], 2)) bindOperations

            ]
        ]