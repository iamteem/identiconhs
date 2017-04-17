module Identicon where

import qualified Crypto.Hash.MD5 as MD5
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack, unpack)
import Data.Char (ord)
import Data.Word (Word8)
import Data.List (concatMap, init, take, zip, reverse, any)
import Data.List.Split (chunksOf)
import qualified Codec.Picture as Juicy

type Hash = [Int]
type Color = (Int, Int, Int)
type Code = Int
type Index = Int
type PixelMap = [(Code, Index)]

run :: String -> IO ()
run s = do
  let hash = getHash s
  let color = getColor hash
  let pixelMap = getEvenPixels $ buildPixelMap hash
  let coords = translatePixels pixelMap
  putStrLn $ show coords
  let image = createImage color coords
  saveImage image s

getHash :: String -> Hash
getHash s = map ord md5
  where md5 = unpack $ (MD5.hash . pack) s

getColor :: Hash -> Color
getColor h = (r, g, b)
  where (r:g:b:_) = h

buildPixelMap :: Hash -> PixelMap
buildPixelMap h = let rows = init $ chunksOf 3 h
                      mirrored = concatMap mirrorRow rows
                      mirrorRow row = row ++ (reverse $ take 2 row)
                  in zip mirrored [(0 :: Int)..]

getEvenPixels :: PixelMap -> PixelMap
getEvenPixels = filter (even . fst)

type SquareCoordinates = ((Integer, Integer), (Integer, Integer))

translatePixels :: PixelMap -> [SquareCoordinates]
translatePixels = map translatePixel

translatePixel :: (Code, Index) -> SquareCoordinates
translatePixel (_, index) = (topLeft, bottomRight)
  where i = fromIntegral index
        x = (i `rem` 5) * 50
        y = (i `div` 5) * 50
        topLeft = (x, y)
        bottomRight = (x + 49, y + 49)

type Image = Juicy.Image Juicy.PixelRGB8

createImage :: Color -> [SquareCoordinates] -> Image
createImage (r, g, b) coords = Juicy.generateImage imageGenerator 250 250
  where imageGenerator x y = if (any (within x y) coords) then coloredPixel else whitePixel
        coloredPixel = Juicy.PixelRGB8 (fromIntegral r :: Word8) (fromIntegral g :: Word8) (fromIntegral b :: Word8)
        whitePixel = Juicy.PixelRGB8 255 255 255

saveImage :: Image -> String -> IO ()
saveImage image string = Juicy.writePng (string ++ ".png") image

within :: Int -> Int -> SquareCoordinates -> Bool
within x y ((topLeftX, topLeftY), (bottomRightX, bottomRightY)) =
  let x' = fromIntegral x
      y' = fromIntegral y
  in
      x' >= topLeftX && x' <= bottomRightX && y' >= topLeftY && y' <= bottomRightY
