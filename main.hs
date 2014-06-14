import System.IO

cutoff0 = 55991
tax0 = 0.52 :: Double

cutoff1 = 33363
tax1 = 0.42 :: Double

cutoff2 = 19645
tax2 = 0.1085 :: Double

cutoff3 = 0
tax3 = 0.0585 :: Double

tax x | x > cutoff0 = (x - cutoff0) * tax0 + tax (cutoff0)
      | x > cutoff1 = (x - cutoff1) * tax1 + tax (cutoff1)
      | x > cutoff2 = (x - cutoff2) * tax2 + tax (cutoff2)
      | x > cutoff3 = (x - cutoff3) * tax3 + tax (cutoff3)
      | x == 0      = 0

readLine amount = do
  end <- isEOF
  if end then
    putStrLn (show $ tax (amount) )
  else do
    input <- getLine
    readLine (amount + (read input :: Double))

main = do
  readLine(0 :: Double)
