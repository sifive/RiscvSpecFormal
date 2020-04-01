import qualified Simulator as S
import qualified RegisterFile as R
import Instance
import qualified Data.Map.Strict as M
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import qualified Data.BitVector as BV 
import System.Exit
import System.IO
import System.Environment
import Data.List
import Data.Maybe
import Control.Monad
import Data.Text.Read
import qualified Data.Text as T (Text, pack, unpack, null)

import Data.BitVector as BV
import Control.Exception
import Data.IORef
import UartDev
import SimLib
import Syntax (separateModRemove, getRules, getAllRegisters, Kind(..), Signature)

hex_to_integer :: T.Text -> Integer
hex_to_integer txt = case hexadecimal txt of
    Left str -> error $ "Formatting error: " ++ str
    Right (x,str) -> if T.null str then x else error $ "Formatting error, extra text: " ++ T.unpack str

isa_size :: IO Int
isa_size = do
  args <- getArgs
  let ps = catMaybes $ map (binary_split '@') args
  case lookup "xlen" ps of
    Just n -> return (read n :: Int)
    Nothing -> return 32

getArgVal :: String -> Int -> IO Int
getArgVal name n = do
    args <- getArgs

    let ps = catMaybes $ map (binary_split ':') args

    case find (\(x,y) -> x == name) ps of
        Just (_,y) -> return $ fromIntegral $ hex_to_integer $ T.pack y
        Nothing -> error $ "Argument value " ++ name ++ " not supplied."

mem_file :: String
mem_file = "pMemFile"

console_read :: IO String
console_read = do
  console_has_input <- try (hReady stdin) :: IO (Either IOError Bool)
  case console_has_input of
    Left _ -> return ""
    Right has_input -> if has_input then getLine else return ""

binary_split :: Eq a => a -> [a] -> Maybe ([a],[a])
binary_split x xs = go xs [] where
    go [] _ = Nothing
    go (y:ys) acc = if x == y then Just (Data.List.reverse acc, ys) else go ys (y:acc)

process_args :: [String] -> [(String,String)]
process_args = catMaybes . map (binary_split '=')

timeout :: Int
timeout = 200000

{-
proc_core_readUART :: (BV.BV, (BV.BV, ())) -> fileState -> regs -> IO BV.BV
proc_core_readUART (addr, (size, _)) _ _ = do
    result <- foldM
                (\acc offset -> do
                  currUARTState <- readIORef $ consoleUART simEnv
                  (result, nextUARTState) <-
                    return $ uart_read currUARTState
                      (offset + (BV.nat $ addr))
                  writeIORef (consoleUART simEnv) nextUARTState
                  return $ (acc <<. (bitVec 4 8)) .|. (bitVec 64 result))
                (bitVec 64 0)
                (Data.List.reverse [0 .. (2 ^ (BV.nat $ size) - 1)])
    return (simEnv, result)

proc_core_writeUART :: (BV.BV, (BV.BV, (BV.BV, ()))) -> fileState -> regs -> SimEnvironment -> IO (SimEnvironment, BV.BV)
proc_core_writeUART (addr, (val, (size, _))) _ _ simEnv = do
    mapM_
      (\offset -> do
          currUARTState <- readIORef $ consoleUART simEnv
          writeIORef (consoleUART simEnv) $
            uart_write currUARTState
              (offset + (fromIntegral $ BV.nat $ addr))
              (fromIntegral $ BV.nat $ BV.least 8 (val >>. (bitVec 7 (offset * 8)))))
      [0 .. (2 ^ (BV.nat $ size) - 1)]
    return (simEnv, BV.nil)
-}

externalInterruptMethod :: R.Any -> S.KamiState -> IO R.Any
externalInterruptMethod _ _ = return $ unsafeCoerce False

debugInterruptMethod :: R.Any -> S.KamiState -> IO R.Any
debugInterruptMethod _ _ = return $ unsafeCoerce False

methods :: M.Map String (Syntax.Signature,S.Coq_meth_sig)
methods = M.fromList [
    ("proc_core_externalInterrupt", ((Syntax.Bit 0,Syntax.Bool), externalInterruptMethod)),
    ("proc_core_debugInterrupt", ((Syntax.Bit 0,Syntax.Bool), debugInterruptMethod))
  ]

data Stream a = (:+) a (Stream a) deriving Show

unwind_list :: [a] -> Stream a
unwind_list xs = go xs xs

  where

    go xs [] = go xs xs
    go xs (y:ys) = y :+ go xs ys

data Outcome = Pass | Fail | Neither

check_tohost :: S.KamiState -> IO Outcome
check_tohost s = do
  sz <- isa_size
  tohost_addr <- getArgVal "tohost_address" sz
  val <- getRegFileVal s mem_file tohost_addr
  let val' = unsafeCoerce val :: BV.BV
  return $ if val' == 1 then Pass else if val' > 1 then Fail else Neither

main :: IO ()
main = do
  args <- getArgs
  let files = process_args args
  sz <- isa_size
  ct <- newIORef 0
  ruleCounter <- newIORef 0
  let mod = if sz == 32 then model32 else model64
  let (_,(rfs,basemod)) = separateModRemove mod
  let rules = unwind_list $ getRules basemod
  let numRules = length $ getRules basemod
  state <- initialize mod files
  go numRules 0 0 rules state

    where

      go numRules ct ruleCounter (r :+ rules') state = do
        let nextCt = if ruleCounter == numRules then ct + 1 else ct
        let nextRuleCounter = if ruleCounter == numRules then 0 else ruleCounter + 1
        when (nextRuleCounter == 0) $ putStrLn $ "[sim] current cycle count: " ++ show ct
        when (currCt == timeout) $ do
          hPutStrLn stderr "TIMEOUT TIMEOUT TIMEOUT TIMEOUT TIMEOUT TIMEOUT TIMEOUT TIMEOUT TIMEOUT."
          exitFailure
        state' <- simulate methods r state
        outcome <- check_tohost state'
        case outcome of
          Neither -> go numRules nextCt nextRuleCounter rules' state'
          Pass -> do
            hPutStrLn stderr "PASSED."
            exitSuccess
          Fail -> do
            hPutStrLn stderr "FAILED FAILED FAILED FAILED FAILED FAILED FAILED FAILED FAILED."
            exitFailure
