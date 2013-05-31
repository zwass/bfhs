import Prelude hiding (catch)
import Data.Array
import System.Environment (getArgs)
import System.IO
import System.IO.Error (isEOFError)
import Control.Exception (catch)

type Memory = Array Int

data BFState = BFState { dataMemory :: Memory Int
                       , dataPointer :: Int
                       , insnMemory :: Memory Char
                       , insnPointer :: Int }
             deriving (Show)

memMax :: Int
memMax = 30000

initialDataMemory :: Memory Int
initialDataMemory = listArray (0, memMax) (repeat 0)

initialInsnMemory :: Memory Char
initialInsnMemory = mkInsnMemory ">,[>,]<[.<]"

mkInsnMemory :: String -> Memory Char
mkInsnMemory s = listArray (0, memMax) $ s ++ repeat ' '

initialBFState :: BFState
initialBFState = BFState initialDataMemory 0 initialInsnMemory 0

jumpToMatchingCloseBrace :: BFState -> BFState
jumpToMatchingCloseBrace = jumpToMatchingCloseBrace' (-1) where
  jumpToMatchingCloseBrace' n s = case insnMemory s ! insnPointer s of
    ']' | n == 0 -> s
    ']' -> jumpToMatchingCloseBrace' (n - 1) $ s'
    '[' -> jumpToMatchingCloseBrace' (n + 1) $ s'
    _   -> jumpToMatchingCloseBrace' n $ s'
    where s' = s {insnPointer = insnPointer s + 1}

jumpToMatchingOpenBrace :: BFState -> BFState
jumpToMatchingOpenBrace = jumpToMatchingOpenBrace' (-1) where
  jumpToMatchingOpenBrace' n s = case insnMemory s ! insnPointer s of
    '[' | n == 0 -> s
    '[' -> jumpToMatchingOpenBrace' (n - 1) $ s'
    ']' -> jumpToMatchingOpenBrace' (n + 1) $ s'
    _   -> jumpToMatchingOpenBrace' n $ s'
    where s' = s {insnPointer = insnPointer s - 1}

getChar' :: IO Char
getChar' =  getChar `catch` eofHandler where
  eofHandler e = if isEOFError e then return '\0' else ioError e

evaluate :: BFState -> IO BFState
evaluate s | insnPointer s > memMax = return s
evaluate s =
  let s' = case iMem ! iPtr of
        '>' -> return $ s {dataPointer = dPtr + 1}
        '<' -> return $ s {dataPointer = dPtr - 1}
        '+' -> return $ s {dataMemory = dMem // [(dPtr, dVal + 1)]}
        '-' -> return $ s {dataMemory = dMem // [(dPtr, dVal - 1)]}
        '.' -> putStr [toEnum dVal] >> return s
        ',' -> do c <- getChar'
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

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  args <- getArgs
  if length args /= 1
    then putStrLn "No input program provided. Exiting."
    else do
    code <- readFile $ head args
    let initState = initialBFState {insnMemory = mkInsnMemory code}
    _ <- evaluate initState
    return ()
