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


evaluate :: BFState -> BFState
evaluate s | insnPointer s > memMax = s
evaluate s =
  let s' = case iMem ! iPtr of
        '>' -> s {dataPointer = dPtr + 1}
        '<' -> s {dataPointer = dPtr - 1}
        '+' -> s {dataMemory = dMem // [(dPtr, dVal + 1)]}
        '-' -> s {dataMemory = dMem // [(dPtr, dVal - 1)]}
        '.' -> s {output = output s ++ [toEnum dVal]}
        ',' -> error "no input yet supported"
        '[' | dVal == 0 -> jumpToMatchingCloseBrace s
        ']' | dVal /= 0 -> jumpToMatchingOpenBrace s
        _ -> s
  in evaluate $ s' {insnPointer = insnPointer s' + 1}
  where dMem = dataMemory s
        dPtr = dataPointer s
        dVal = dMem ! dPtr
        iMem = insnMemory s
        iPtr = insnPointer s
