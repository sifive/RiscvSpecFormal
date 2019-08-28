
module Main where

import qualified Data.List
import Simulator.All

import qualified Data.HashMap as M
import qualified Data.Vector as V
import qualified Data.BitVector as BV

import Data.String
import Data.List (find)
import Data.Maybe (isJust, catMaybes)
import Control.Monad
import Data.IORef
import System.Exit
import System.IO
import System.Random (randomIO)
import System.Environment (getArgs)
import Text.Read
import Control.Exception
import UART

import HaskellTarget as T

timeout :: Int
timeout = 50000

isa_size :: IO Int
isa_size = do
  args <- getArgs
  let ps = catMaybes $ map (binary_split '@') args
  case lookup "xlen" ps of
    Just n -> return (read n :: Int)
    Nothing -> return 32

kami_model :: Int -> ([RegFileBase] , BaseModule)
kami_model 32 = T.kami_model32
kami_model 64 = T.kami_model64

regfiles :: Int -> [RegFileBase]
regfiles n = fst $ kami_model n

basemod :: Int -> BaseModule
basemod n = snd $ kami_model n

mem_file :: String
mem_file = "proc_core_mem_reg_file"

float_file :: String
float_file = "proc_core_float_reg_file"

int_file :: String
int_file = "proc_core_int_data_reg"

-- 0x80000000
offset :: Int
offset = 2147483648

data Environment = Environment {
  steps       :: Int,
  counter     :: Int,
  consoleUART :: IORef UART_NS16550A
}

mkEnv :: IO Environment
mkEnv = do
  uartRef <- newIORef mkUART
  return $ Environment 0 0 uartRef

console_read :: IO String
console_read = do
  -- putStrLn "[console_read]"
  console_has_input <- try (hReady stdin) :: IO (Either IOError Bool)
  case console_has_input of
    Left isEOFError -> return ""
    Right has_input
      -> if has_input
              then do
                -- putStrLn "[console_read] read input."
                b <- getChar
                bs <- console_read
                return (b : bs)
              else do
                -- putStrLn "[console_read] did not read any input."
                return ""
{-
  input <- try (getLine) :: IO (Either IOError String)
  return $
    case input of
      Left isEOFError -> ""
      Right content -> content
-}
instance AbstractEnvironment Environment where
  envPost env filestate regstate ruleName = return env
  envPre env filestate regstate ruleName = do
    -- I. update console state
    console_input <- console_read
    uart_state_init <- readIORef $ consoleUART env
    let (console_output, uart_state_final) =
          uart_deq_output $ uart_enq_input uart_state_init console_input in do
      if console_output /= ""
        then putStrLn $ "[console out] > " ++ console_output
        else return ()
      writeIORef (consoleUART env) uart_state_final
    -- II. update simulation state
    putStrLn $ "[main] rule name: " ++ ruleName
    if ruleName /= "proc_core_pipeline"
      then return env
      else
        let currCounter = counter env
            currSteps = steps env in do
          isaSize <- isa_size
{-
          tohost_addr <- getArgVal "tohost_address" isaSize
          when (currCounter > timeout) $ do
              hPutStrLn stdout "TIMEDOUT TIMEDOUT TIMEDOUT TIMEDOUT TIMEDOUT TIMEDOUT"
              hPutStrLn stderr "TIMEDOUT TIMEDOUT TIMEDOUT TIMEDOUT TIMEDOUT TIMEDOUT"
              exitFailure
          case M.lookup mem_file (arrs filestate) of
              Nothing -> error $ "File " ++ mem_file ++ " not found."
              Just v -> let val = v V.! (fromIntegral $ BV.nat $ bvCoerce tohost_addr) in 
                  if bvCoerce val == 1 then do
                      args <- getArgs
                      let ps = catMaybes $ map (binary_split '@') args
                      case lookup "signature" ps of
                          Nothing -> return ()
                          Just filename -> case lookup "sign_size" ps of
                              Nothing -> hPutStrLn stderr "sign_size expected but not supplied"
                              Just x -> let sign_size = read x in
                                  case M.lookup mem_file (arrs filestate) of
                                      Nothing -> hPutStrLn stderr $ "File " ++ mem_file ++ " not found."
                                      Just v -> do
                                          let sz = V.length v
                                          let indices = reverse [(sz-sign_size)..(sz-1)]
                                          let vals = map (\i -> ppr_hex (v V.! i)) indices
                                          let spliced = (chunksOf 4 vals) :: [[String]]
                                          let newlined = (map (\t -> concat (t ++ [['\n']])) spliced) :: [String]
                                          let reversed = (reverse newlined) :: [String]
                                          writeFile filename $ concat reversed
                      hPutStrLn stdout "Passed"
                      hPutStrLn stderr "Passed"
                      exitSuccess
                  else if bvCoerce val > 1 then do
                          hPutStrLn stdout "FAILED FAILED FAILED FAILED FAILED FAILED FAILED FAILED FAILED"
                          hPutStrLn stderr "FAILED FAILED FAILED FAILED FAILED FAILED FAILED FAILED FAILED"
                          exitFailure
                  else do
-}
          nextEnv <- io_stuff filestate regstate
                       env {
                         counter = (currCounter + 1),
                         steps = if currSteps > 0
                                   then currSteps - 1
                                   else currSteps
                       }
          return env

io_stuff :: FileState -> M.Map String Val -> Environment -> IO Environment
io_stuff filestate regstate env =
  let currSteps = steps env :: Int in do
    interactive <- interactive_mode
    if interactive && currSteps == 0
      then do 
        putStr "% "
        hFlush stdout
        input <- getLine
        case words input of
            ["Step",num] -> case readMaybe num of
                Nothing -> do
                    putStrLn "Formatting error."
                    io_stuff filestate regstate env
                Just n -> return $ env {steps = n}
            [reg] -> do 
                print_reg regstate $ "proc_core_" ++ reg
                io_stuff filestate regstate env
            ["Float",addr] -> do
                case hex_to_maybe_integer_str addr of
                    Just n -> print_file_reg filestate float_file $ fromInteger n
                    Nothing -> putStrLn "Formatting error."
                io_stuff filestate regstate env
            ["Int",addr] -> do
                case hex_to_maybe_integer_str addr of
                    Just n -> case n == 0 of
                        True -> case M.lookup int_file (files filestate) of
                            Nothing -> putStrLn $ "File " ++ int_file ++ " not found."
                            Just r -> let k = kind r in putStrLn $ ppr_hex $ defVal k
                        False -> print_file_reg filestate int_file $ fromInteger n
                    Nothing -> putStrLn "Formatting error."
                io_stuff filestate regstate env
            ["Mem",addr] -> do
                case hex_to_maybe_integer_str addr of
                    Just n -> print_file_reg filestate mem_file $ fromInteger n - offset
                    Nothing -> putStrLn "Formatting error."
                io_stuff filestate regstate env
            [] -> io_stuff filestate regstate env
            _ -> do
                putStrLn "Formatting error."
                io_stuff filestate regstate env
      else return env


proc_core_readUART :: Environment -> Val -> FileState -> M.Map String Val -> IO (Environment, Val)
proc_core_readUART env v filestate regstate = do
    putStrLn "[proc_core_readUART]"
    uart_state_init <- readIORef $ consoleUART env
    (result, uart_state_final) <- return $ uart_read uart_state_init $ BV.nat $ bvCoerce v
    writeIORef (consoleUART env) uart_state_final
    return (env, BVVal $ BV.bitVec 8 result)

proc_core_writeUART :: Environment -> Val -> FileState -> M.Map String Val -> IO (Environment, Val)
proc_core_writeUART env v filestate regstate = do
    putStrLn "[proc_core_writeUART]"
    uart_state_init <- readIORef $ consoleUART env
    writeIORef (consoleUART env) $
      uart_write uart_state_init
        (fromIntegral $ BV.nat $ bvCoerce $ struct_field_access "addr" v)
        (fromIntegral $ BV.nat $ bvCoerce $ struct_field_access "data" v)
    return (env, BVVal BV.nil)

io_meth :: Environment -> Val -> FileState -> M.Map String Val -> IO (Environment, Val)
io_meth env v filestate regstate = do
  consoleUART <- readIORef $ consoleUART env
  return (env, BoolVal $ uart_has_interrupt consoleUART)

meths :: [(String, Environment -> Val -> FileState -> M.Map String Val -> IO (Environment, Val))]
meths =
  [("proc_core_ext_interrupt_pending", io_meth),
   ("proc_core_readUART", proc_core_readUART),
   ("proc_core_writeUART", proc_core_writeUART)]

chunksOf :: Int -> [a] -> [[a]]
chunksOf n [] = []
chunksOf n ys =
  let (val, rest) = splitAt n ys in
  (val : chunksOf n rest)

-- TODO: merge this into envStep
proc_core_meth :: Environment -> Val -> FileState -> M.Map String Val -> IO (Environment, Val)
proc_core_meth env _ _ _  = return (env, tt)

main :: IO()
main = do
  hSetBuffering stdout NoBuffering
  env <- mkEnv
  envRef <- newIORef env
  n <- isa_size
  putStrLn "[main] starting the simulation"
  simulate_module 0 round_robin_rules envRef (map fst $ getRules (basemod n)) meths (regfiles n) (basemod n)
  return ()
