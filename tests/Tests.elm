module Tests exposing (..)

import Test exposing (..)
import Expect

import Bread exposing (..)

type BitFlag =
    Negative
  | Overflow
  | Unused
  | Break
  | Decimal
  | InterruptDisable
  | Carry
  | Zero

fields = 
  [ Negative
  , Overflow
  , Unused
  , Break
  , Decimal
  , InterruptDisable
  , Carry
  , Zero ]

bitSet = (read [0x11] <| bitField fields)
         |> Tuple.first

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
    describe "Bread Test Suite"
        [ describe "test composition"
            [ test "Mixed output" <|
                \() ->
                  Expect.equal ("data", 2, ([On, On], 2)) bindOperations
                    

            ]
        , describe "test bitfields"
          [ test "basic bitfield" <|
              \() ->
                Expect.true "Should have the decimal bit" 
                            (hasBit Decimal bitSet)
          , test "basic bitfield" <|
              \() ->
                Expect.false "Should not have the unused bit" 
                            (hasBit Unused bitSet)
          ]
        ]