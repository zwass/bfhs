import Data.Array

type Memory = Array Int

data BFState = BFState { dataMemory :: Memory Int
                       , dataPointer :: Int
                       , insnMemory :: Memory Char
                       , insnPointer :: Int
                       , input :: String
                       , output :: String }
             deriving (Show)

memMax :: Int
memMax = 29999

initialDataMemory :: Memory Int
initialDataMemory = listArray (0, memMax) (repeat 0)

initialInsnMemory :: Memory Char
initialInsnMemory = listArray (0, memMax) $ "++++++++++[>+++++++>++++++++++>+++>+<<<<-]>++.>+.+++++++..+++.>++.<<+++++++++++++++.>.+++.------.--------.>+.>." ++ repeat ' '

initialBFState :: BFState
initialBFState = BFState initialDataMemory 0 initialInsnMemory 0 "" ""

jumpToMatchingCloseBrace :: BFState -> BFState
jumpToMatchingCloseBrace s = case insnMemory s ! insnPointer s of
  ']' -> s
  _   -> jumpToMatchingCloseBrace $ s {insnPointer = insnPointer s + 1}

jumpToMatchingOpenBrace :: BFState -> BFState
jumpToMatchingOpenBrace s = case insnMemory s ! insnPointer s of
  '[' -> s
  _   -> jumpToMatchingOpenBrace $ s {insnPointer = insnPointer s - 1}

evaluate :: BFState -> IO BFState
evaluate s | insnPointer s > memMax = return s
evaluate s =
  let s' = case iMem ! iPtr of
        '>' -> return $ s {dataPointer = dPtr + 1}
        '<' -> return $ s {dataPointer = dPtr - 1}
        '+' -> return $ s {dataMemory = dMem // [(dPtr, dVal + 1)]}
        '-' -> return $ s {dataMemory = dMem // [(dPtr, dVal - 1)]}
        '.' -> putStr [toEnum dVal] >> return s
        ',' -> do c <- getChar
                  return $ s {dataMemory = dMem // [(dPtr, fromEnum c)] }
        '[' | dVal == 0 -> return $ jumpToMatchingCloseBrace s
        ']' | dVal /= 0 -> return $ jumpToMatchingOpenBrace s
        _ -> return s
  in do s'' <- s'
        evaluate s'' {insnPointer = insnPointer s'' + 1}
  where dMem = dataMemory s
        dPtr = dataPointer s
        dVal = dMem ! dPtr
        iMem = insnMemory s
        iPtr = insnPointer s
