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

fields : List BitFlag
fields = 
  [ Negative
  , Overflow
  , Unused
  , Break
  , Decimal
  , InterruptDisable
  , Carry
  , Zero ]

bitSet : BitField BitFlag
bitSet = (read [0x11] <| bitField fields)
         |> toResult
         |> Result.withDefault (BitField [])

bindOperations : Result String ( String, Int, ( BitString, Int ) )
bindOperations = 
  let start = read [0xaf, 0xff]
      skip = bitNum 2
      getTup = tuple2 (readBitString 2) (bitNum 2)
      getBitNum = (bitNum 2)
    
      (result, _) = start <| 
                    skip >>| 
                    getTup >>= \tupData -> 
                    getBitNum >>= \x ->
                    succeed ("data", x, tupData)      
  in
    result

read9Bits : DecodeContext
read9Bits =
  read [0xaf, 0xf0] (bitNum 9)
  |> Tuple.second

overflow : Result String String
overflow =
  let reader =
    (bitNum 24) >>| succeed "this should never return"
  in 
    read [0xaf, 0xf0] reader
    |> toResult


all : Test
all =
    describe "Bread Test Suite"
        [ describe "test composition"
            [ test "Mixed output" <|
                \() ->
                  Expect.equal (Ok ("data", 2, ([On, On], 2))) bindOperations                  
            ]
        , describe "bytes should advance"
            [ test "requesting >8 bits advances the current byte" <|
                \()  -> Expect.equal read9Bits.currentByte 0xf0 
            , test "requestion >8 bits pops the head byte" <|
                \() -> Expect.equal read9Bits.bytes []
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
        , describe "failures"
          [ test "overflows" <|
            \() ->
              Expect.equal 
                (Err "Buffer overflow")
                overflow]
        ]