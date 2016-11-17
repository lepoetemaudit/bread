module Bread exposing (..)

import Bitwise

type Bit = On | Off
type alias BitString = List Bit
type alias DecoderFunc a = (DecodeContext -> (a, DecodeContext)) 

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

takeBit : DecodeContext -> (DecodeContext, Bit)
takeBit ctx =
  let val = Bitwise.shiftLeftBy ctx.bitPosition 1
  in
    ( { ctx | bitPosition = ctx.bitPosition + 1 }
    , if (Bitwise.and val ctx.currentByte) > 0 then
        On
      else
        Off )

readBitString_ : Int -> DecodeContext -> BitString -> (BitString, DecodeContext)
readBitString_ amount ctx bits =
  if amount == 0 then
    (List.reverse bits, ctx)
  else
    let (ctx_, bit) = takeBit ctx
        bits_ = bit :: bits
    in 
      readBitString_ (amount - 1) ctx_ bits_

readBitString : Int -> DecodeContext -> (BitString, DecodeContext)
readBitString amount ctx =
  readBitString_ amount ctx []

bitNum : Int -> DecodeContext -> (Int, DecodeContext)
bitNum amount ctx =
  let (bits, ctx_) = readBitString amount ctx
  in 
    (bitStringToInt bits, ctx_) 

type BitField a = BitField (List a)

bitField : List a -> DecodeContext -> ((BitField a), DecodeContext)
bitField bits ctx =
  let (bitfield, ctx_) = 
    List.foldl (\acc (bs, ctx) -> 
                 let (ctx_, bit) = takeBit ctx 
                 in
                   case bit of
                     On -> (acc :: bs, ctx_)
                     Off -> (bs, ctx_))

               ([], ctx) bits
  in 
    (BitField bitfield, ctx) 

hasBit : a -> BitField a -> Bool
hasBit bit (BitField bf) =
  List.any ((==) bit) bf

  
read : List Int -> DecoderFunc a -> (a, DecodeContext)
read bytes decoder =
  decoder { bitPosition = 0
          , bytes = bytes
          , currentByte = 0xaf }


tuple2 : DecoderFunc a ->
         DecoderFunc b ->
         DecodeContext ->
         ((a, b), DecodeContext)
tuple2 d1 d2 ctx =
  let (r1, ctx1) = d1 ctx
      (r2, ctx2) = d2 ctx1
  in
    ((r1, r2), ctx2)

tuple3 : DecoderFunc a ->
         DecoderFunc b ->
         DecoderFunc c ->
         DecodeContext ->
         ((a, b, c), DecodeContext)
tuple3 d1 d2 d3 ctx =
  let (r1, ctx1) = d1 ctx
      (r2, ctx2) = d2 ctx1
      (r3, ctx3) = d3 ctx2
  in
    ((r1, r2, r3), ctx3)
    
    
(>>=) :  (DecodeContext -> ( a, DecodeContext )) 
      -> (a -> DecodeContext -> ( b, DecodeContext )) 
      -> DecodeContext 
      -> ( b, DecodeContext )
(>>=) action1 action2 world0 =
  let (a, world1) = action1 world0
      (b, world2) = action2 a world1
  in (b, world2)
  
(>>|) action1 action2 world0 =
  let (_, world1) = action1 world0
      (b, world2) = action2 world1
  in (b, world2)
  
return : a -> DecodeContext -> ( a, DecodeContext )
return value world =
  ( value, world )
