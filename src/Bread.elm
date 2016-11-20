module Bread exposing (Bit (On, Off), BitString, BitField (BitField), DecodeContext, 
                       intToBitString, bitStringToInt, readBitString, bitNum,
                       bitField, read, hasBit, (>>=), (>>|), succeed, return,
                       toResult, tuple2, tuple3)

{-| A library for handling binary data in a manner somewhat similar to Elm's
JSON decoder. It works on lists of bytes (represented as Ints) and has an
especial focus on sub-byte decoding (for things such as bitfields or tightly
packed data). 

The state of the reader is returned from each function which
makes it possible to change what you read depending on the content. For
example, a binary format might define the length of data in some sort of
header, so we need to first extract the length value, and then use it
for the remaining data. Using functions such as (>>= and >>|) we can
chain together these operations while capturing their values in a very
convenient manner. For example:

```
bindOperations = 
  -- define some of our read 'operations' for clarity
  let skip = bitNum 2 
      getTup = tuple2 (readBitString 2) (bitNum 2)
      getBitNum = (bitNum 2)
  in 
    read [0xaf, 0xff] <| 
    skip >>| 
    getTup >>= \tupData ->        -- capture tuple value in `tupData`
    getBitNum >>= \x ->           -- capture bit num value in `x`
    succeed ("data", x, tupData)  -- gather values into a tuple 
    |> toResult
```

# types 
@docs BitField, Bit, BitString, DecodeContext

# reading
@docs read, toResult

# Bit operations
@docs bitStringToInt, intToBitString, readBitString, bitNum, bitField, hasBit

# Composing readers
@docs (>>=), (>>|), succeed, return, tuple2, tuple3
-}


import Bitwise

-- Types

{-| Represents the state off a bit, i.e. 1 = On, 0 = Off -}
type Bit = On | Off

{-| A string of bits which may converted to a number -}
type alias BitString = List Bit

{-| Bitfields allow specifing values to substitute for numbered bits -}
type BitField a = BitField (List a)

type alias DecoderFunc a = (DecodeContext -> (DecodeValue a, DecodeContext)) 
type alias DecodeValue a = Result String a

{-| The current state of a read operation -}
type alias DecodeContext = 
  { bitPosition : Int
  , bytes : List Int
  , currentByte : Int     
  }

{-| Convert an integer into its component bits -}
intToBitString : Int -> List Bit
intToBitString int =
  List.map (\i -> if (Bitwise.and (Bitwise.shiftLeftBy i 1) int) > 0 then 
                    On 
                  else 
                    Off) 
           (List.range 0 7)
          
{-| Join a string of bits into its representative number -}  
bitStringToInt : List Bit -> Int
bitStringToInt bitString =
  bitString
  |> List.indexedMap (\shift bit -> if bit == On then
                                      Bitwise.shiftLeftBy shift 1
                                    else
                                      0)
  |> List.sum

advanceByte : DecodeContext -> (DecodeValue (), DecodeContext)
advanceByte ctx =
  case ctx.bytes of
    byte :: rest ->
      (Ok (), { bytes = rest, currentByte = byte, bitPosition = 0 })
    [] -> (Err "Buffer overflow", ctx) 

takeBit : DecodeContext -> (DecodeValue Bit, DecodeContext)
takeBit ctx =
  let val = Bitwise.shiftLeftBy ctx.bitPosition 1
      (status, ctx_) = if ctx.bitPosition == 8 then
              advanceByte ctx
            else
              (Ok (), ctx)         
  in
    case status of
      Ok () ->
        ( if (Bitwise.and val ctx_.currentByte) > 0 then
            Ok On
          else
            Ok Off
        , { ctx_ | bitPosition = ctx_.bitPosition + 1 } )
      Err err -> (Err err, ctx_)

readBitString_ : Int -> DecodeContext -> BitString -> (DecodeValue BitString, DecodeContext)
readBitString_ amount ctx bits =
  if amount == 0 then
    (Ok <| List.reverse bits, ctx)
  else
    case takeBit ctx of
      (Ok bit, ctx_) ->
        readBitString_ (amount - 1) ctx_ <| bit :: bits
      (Err err, ctx_) -> (Err err, ctx_)
      

{-| Read a given number of bits into a bit string -}
readBitString : Int -> DecodeContext -> (DecodeValue BitString, DecodeContext)
readBitString amount ctx =
  readBitString_ amount ctx []
  
{-| Take a given number of bits and get their combined integer value -}  
bitNum : Int -> DecodeContext -> ( DecodeValue Int, DecodeContext )  
bitNum amount =
  readBitString amount >>= \x -> succeed (bitStringToInt x)

{-| Map bits to a list of values, where bits are on -}
bitField : List a -> DecodeContext -> (DecodeValue (BitField a), DecodeContext)
bitField bits ctx =
  let 
      gatherBits vals ctx acc =
        case vals of
          [] -> (Ok acc, ctx)
          h :: tail ->
            case takeBit ctx of
              (Err err, ctx_) -> (Err err, ctx_)
              (Ok On, ctx_) -> gatherBits tail ctx_ (h :: acc)
              (Ok Off, ctx_) -> gatherBits tail ctx_ acc

      (bitfield, ctx_) = gatherBits bits ctx []
  in 
    case bitfield of
      Ok bf -> (Ok <| BitField bf, ctx_)
      Err err -> (Err err, ctx_)

{-| Determine whether a bit value is set in a bitfield -}
hasBit : a -> BitField a -> Bool
hasBit bit (BitField bf) =
  List.any ((==) bit) bf

{-| Decode a list of ints using the provided decoder function -}  
read : List Int -> DecoderFunc a -> (DecodeValue a, DecodeContext)
read bytes decoder =
  { bitPosition = 0
  , bytes = bytes
  , currentByte = 0 }
  |> advanceByte >>| decoder


{-| Takes a pair of `DecoderFunc`s and pairs the results in a tuple -}
tuple2 : DecoderFunc a ->
         DecoderFunc b ->
         DecodeContext ->
         (DecodeValue (a, b), DecodeContext)
tuple2 d1 d2 ctx =
  let (r1, ctx1) = d1 ctx
      (r2, ctx2) = d2 ctx1
      res = Result.map2 (,) r1 r2
  in
    (res, ctx2)

{-| Takes three `DecoderFunc`s and combines the results in a tuple -}
tuple3 : DecoderFunc a ->
         DecoderFunc b ->
         DecoderFunc c ->
         DecodeContext ->
         (DecodeValue (a, b, c), DecodeContext)
tuple3 d1 d2 d3 ctx =
  let (r1, ctx1) = d1 ctx
      (r2, ctx2) = d2 ctx1
      (r3, ctx3) = d3 ctx2
      res = Result.map3 (,,) r1 r2 r3
  in    
    (res, ctx3)
    

{-| Monadic bind - useful for chaining reads and aggregating the returned
    values -}
(>>=) :  (DecodeContext -> ( DecodeValue a, DecodeContext )) 
      -> (a -> DecodeContext -> ( DecodeValue b, DecodeContext )) 
      -> DecodeContext 
      -> ( DecodeValue b, DecodeContext )
(>>=) action1 action2 world0 =
    case action1 world0 of
      (Ok r, world1) -> 
        let (b, world2) = action2 r world1
        in (b, world2)
      (Err err, world1) -> (Err err, world1)

{-| Monadic bind ignoring value - the result of the left function
    is not passed to the right -}
(>>|) : (DecodeContext -> ( DecodeValue a, DecodeContext )) 
      -> (DecodeContext -> ( DecodeValue b, DecodeContext )) 
      -> DecodeContext 
      -> ( DecodeValue b, DecodeContext )        
(>>|) action1 action2 world0 =
  case action1 world0 of
    (Ok _, world1) ->
      let (b, world2) = action2 world1
      in (b, world2)
    (Err err, world1) -> (Err err, world1)
  
{-| -}
return : DecodeValue a -> DecodeContext -> ( DecodeValue a, DecodeContext )
return value world =
  ( value, world )

{-| Take the result of a read operation and turn it into a Result -}
toResult : (DecodeValue a, DecodeContext) -> DecodeValue a
toResult (val, _)=
  val

{-| Raise a value that always succeeds -}
succeed : a -> DecodeContext -> ( DecodeValue a, DecodeContext )
succeed a = 
  return <| Ok a