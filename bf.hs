import Data.Array

type Memory = Array Int

data BFState = BFState { dataMemory :: Memory Int
                       , dataPointer :: Int
                       , insnMemory :: Memory Char
                       , insnPointer :: Int
                       , input :: String
                       , output :: String }
             deriving (Show)

initialDataMemory :: Memory Int
initialDataMemory = listArray (0, 29999) (repeat 0)

initialInsnMemory :: Memory Char
initialInsnMemory = listArray (0, 29999) $ "++++++++++[>+++++++>++++++++++>+++>+<<<<-]>++.>+.+++++++..+++.>++.<<+++++++++++++++.>.+++.------.--------.>+.>." ++ repeat ' '

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
evaluate s = if insnPointer s > 29999 then s else
  let s' = case insnMemory s ! insnPointer s of
        '>' -> s {dataPointer = dataPointer s + 1}
        '<' -> s {dataPointer = dataPointer s - 1}
        '+' -> s {dataMemory = dataMemory s // [(dataPointer s, (dataMemory s ! dataPointer s) + 1)]}
        '-' -> s {dataMemory = dataMemory s // [(dataPointer s, (dataMemory s ! dataPointer s) - 1)]}
        '.' -> s {output = output s ++ [toEnum (dataMemory s ! dataPointer s)]}
        ',' -> error "no input yet supported"
        '[' | dataMemory s ! dataPointer s == 0 -> jumpToMatchingCloseBrace s
        ']' | dataMemory s ! dataPointer s /= 0 -> jumpToMatchingOpenBrace s
        _ -> s
  in evaluate $ s' {insnPointer = insnPointer s' + 1}
