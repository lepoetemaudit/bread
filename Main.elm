import Html
import Bitwise

type Bit = On | Off
type alias BitString = List Bit

type alias DecodeContext = 
  { bitPosition : Int
  , bytes : List Int
  , currentByte : Int     
  }

intToBitString : Int -> List Bit
intToBitString int =
  List.map (\i -> if (Bitwise.and (Bitwise.shiftLeft 1 i) int) > 0 then 
                    On 
                  else 
                    Off) 
           [0..7]
  
bitStringToInt : List Bit -> Int
bitStringToInt bitString =
  bitString
  |> List.indexedMap (\shift bit -> if bit == On then
                                      Bitwise.shiftLeft 1 shift
                                    else
                                      0)
  |> List.sum

takeBit : DecodeContext -> (DecodeContext, Bit)
takeBit ctx =
  let val = Bitwise.shiftLeft 1 ctx.bitPosition
  in
    ( { ctx | bitPosition = ctx.bitPosition + 1 }
    , if (Bitwise.and val ctx.currentByte) > 0 then
        On
      else
        Off )

readBitString' : Int -> DecodeContext -> BitString -> (BitString, DecodeContext)
readBitString' amount ctx bits =
  if amount == 0 then
    (List.reverse bits, ctx)
  else
    let (ctx, bit) = takeBit ctx
        bits' = bit :: bits
    in 
      readBitString' (amount - 1) ctx bits'    

readBitString : Int -> DecodeContext -> (BitString, DecodeContext)
readBitString amount ctx =
  readBitString' amount ctx []

bitNum : Int -> DecodeContext -> (Int, DecodeContext)
bitNum amount ctx =
  let (bits, ctx') = readBitString amount ctx
  in 
    (bitStringToInt bits, ctx') 
  
read : List Int -> DecoderFunc a -> (a, DecodeContext)
read bytes decoder =
  decoder { bitPosition = 0
          , bytes = bytes
          , currentByte = 17 }

type alias DecoderFunc a = (DecodeContext -> (a, DecodeContext)) 

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
    
getTup : DecoderFunc (Int, Int, Int)
getTup = tuple3 (bitNum 2) (bitNum 3) (bitNum 1)

result : ((Int, Int, Int), DecodeContext)
result = 
  read [17, 4] getTup

main : Html.Html a
main =
  Html.text <| toString result