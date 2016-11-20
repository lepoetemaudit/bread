module Bread exposing (..)

import Bitwise

type Bit = On | Off
type alias BitString = List Bit
type alias DecoderFunc a = (DecodeContext -> (DecodeValue a, DecodeContext)) 
type BitField a = BitField (List a)
type alias DecodeValue a = Result String a

type alias DecodeContext = 
  { bitPosition : Int
  , bytes : List Int
  , currentByte : Int     
  }

intToBitString : Int -> List Bit
intToBitString int =
  List.map (\i -> if (Bitwise.and (Bitwise.shiftLeftBy i 1) int) > 0 then 
                    On 
                  else 
                    Off) 
           (List.range 0 7)
          
  
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
      

readBitString : Int -> DecodeContext -> (DecodeValue BitString, DecodeContext)
readBitString amount ctx =
  readBitString_ amount ctx []
  
bitNum : Int -> DecodeContext -> ( DecodeValue Int, DecodeContext )  
bitNum amount =
  readBitString amount >>= \x -> succeed (bitStringToInt x)

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

hasBit : a -> BitField a -> Bool
hasBit bit (BitField bf) =
  List.any ((==) bit) bf

  
read : List Int -> DecoderFunc a -> (DecodeValue a, DecodeContext)
read bytes decoder =
  { bitPosition = 0
  , bytes = bytes
  , currentByte = 0 }
  |> advanceByte >>| decoder


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
  
return : DecodeValue a -> DecodeContext -> ( DecodeValue a, DecodeContext )
return value world =
  ( value, world )

toResult : (DecodeValue a, DecodeContext) -> DecodeValue a
toResult (val, _)=
  val

succeed : a -> DecodeContext -> ( DecodeValue a, DecodeContext )
succeed a = 
  return <| Ok a