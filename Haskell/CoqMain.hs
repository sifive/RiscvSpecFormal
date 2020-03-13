import qualified Simulator as S
import qualified RegisterFile as R
import Instance
import qualified Data.Map as M
import qualified Data.Vector as V
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
import UART

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
mem_file = "proc_core_mem_reg_file"

data SimEnvironment = SimEnvironment {
  consoleUART :: IORef UART_NS16550A
}

mkInitSimEnv :: IO SimEnvironment
mkInitSimEnv = do
  uartRef <- newIORef mkUART
  return $ SimEnvironment uartRef

console_read :: IO String
console_read = do
  console_has_input <- try (hReady stdin) :: IO (Either IOError Bool)
  case console_has_input of
    Left _ -> return ""
    Right has_input -> if has_input then getLine else return ""

simEnvPre :: SimEnvironment -> IO ()
simEnvPre simEnv = do
  consoleInput <- console_read
  currUARTState <- readIORef $ consoleUART simEnv
  let (consoleOutput, nextUARTState) = uart_deq_output (uart_enq_input currUARTState consoleInput) in do
    when (consoleOutput /= "") $ do
      consoleWriteRes <- try (putStrLn $ "[console out] > " ++ consoleOutput) :: IO (Either IOError ())
      case consoleWriteRes of
        Left errorMsg -> putStrLn $ "[Main] Error: an IO error occured while trying to print output to the console. " ++ show errorMsg
        Right _ -> return ()
    writeIORef (consoleUART simEnv) nextUARTState 

env :: S.Environment SimEnvironment
env = S.Build_Environment 
    (\simEnv _ _ _ -> unsafeCoerce $ do
      simEnvPre simEnv
      return simEnv :: IO SimEnvironment)
    (\simEnv (R.Build_FileState methods int_regs files) _ ruleName -> unsafeCoerce $ do
        when (ruleName /= "proc_core_pipeline") (return ()) -- TODO: LLEE: increment timeout counter.
        case M.lookup mem_file (unsafeCoerce files) of
            Nothing -> error "File not found."
            Just (R.Build_RegFile _ _ _ _ _ _ _ v) -> do
                sz <- isa_size
                tohost_addr <- getArgVal "tohost_address" sz
                let val = (unsafeCoerce v) V.! tohost_addr
                let val' = ((unsafeCoerce val) :: BV.BV)
                if val' == 1 then do
                    hPutStrLn stderr "PASSED."
                    exitSuccess
                else if val' > 1 then do
                    hPutStrLn stderr "FAILED."
                    exitFailure
                else return simEnv)

binary_split :: Eq a => a -> [a] -> Maybe ([a],[a])
binary_split x xs = go xs [] where
    go [] _ = Nothing
    go (y:ys) acc = if x == y then Just (Data.List.reverse acc, ys) else go ys (y:acc)

process_args :: [String] -> [(String,String)]
process_args = catMaybes . map (binary_split '=')

timeout :: Int
timeout = 200000

proc_core_readUART :: (BV.BV, (BV.BV, ())) -> fileState -> regs -> SimEnvironment -> IO (SimEnvironment, BV.BV)
proc_core_readUART (addr, (size, _)) _ _ simEnv = do
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

main :: IO()
main = do
    args <- getArgs
    let files = process_args args
    sz <- isa_size
    let sim = unsafeCoerce (if sz == 32 then coqSim_32 else coqSim_64)
    initSimEnv <- mkInitSimEnv
    sim env initSimEnv files timeout
      (\_ _ _ _ -> return (initSimEnv, False) :: IO (SimEnvironment, Bool)) -- proc_core_pipeline
      proc_core_readUART
      proc_core_writeUART
