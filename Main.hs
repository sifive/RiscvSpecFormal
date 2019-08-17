
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

import HaskellTarget as T

handle :: Handle
handle = stdout

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

-- intersperse_at :: Int -> a -> [a] -> [a]
-- intersperse_at n x xs = let (ys,zs) = splitAt n xs in
--     case zs of
--     [] -> ys
--     _ -> ys ++ (x:intersperse_at n x zs)

meths :: IORef Int -> IORef Int -> [(String, Val -> FileState -> M.Map String Val -> IO Val)]
meths steps counter =
  [("proc_core_pc", proc_core_meth steps counter),
   ("proc_core_ext_interrupt_pending", io_meth steps counter)]

io_meth :: IORef Int -> IORef Int -> Val -> FileState -> M.Map String Val -> IO Val
io_meth steps counter v filestate regstate = do
   args <- getArgs
   if isJust $ find (\arg -> arg == "--enable-ext-interrupts") args then do
     putStrLn "external interrupts enabled"
     result <- randomIO
     if result then do putStrLn "signalling an external interrupt"; return tt else return tt
     return $ BoolVal result
   else return $ BoolVal False

chunksOf :: Int -> [a] -> [[a]]
chunksOf n [] = []
chunksOf n ys =
  let (val, rest) = splitAt n ys in
  (val : chunksOf n rest)

proc_core_meth :: IORef Int -> IORef Int -> Val -> FileState -> M.Map String Val -> IO Val
proc_core_meth steps counter v filestate regstate = do
    isaSize <- isa_size
    tohost_addr <- getArgVal "tohost_address" isaSize
    n <- readIORef counter
    when (n > timeout) $ do
        hPutStrLn handle "TIMEDOUT TIMEDOUT TIMEDOUT TIMEDOUT TIMEDOUT TIMEDOUT"
        hPutStrLn stderr "TIMEDOUT TIMEDOUT TIMEDOUT TIMEDOUT TIMEDOUT TIMEDOUT"
        exitFailure
    writeIORef counter (n+1)
    currSteps <- readIORef steps
    when (currSteps > 0) (writeIORef steps (currSteps-1))
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
                hPutStrLn handle "Passed"
                hPutStrLn stderr "Passed"
                exitSuccess

            else if bvCoerce val > 1 then do
                    hPutStrLn handle "FAILED FAILED FAILED FAILED FAILED FAILED FAILED FAILED FAILED"
                    hPutStrLn stderr "FAILED FAILED FAILED FAILED FAILED FAILED FAILED FAILED FAILED"
                    exitFailure
            else do
                io_stuff
                return tt

    where 

    io_stuff = do
        currSteps <- readIORef steps
        interactive <- interactive_mode
        when (interactive && currSteps == 0) $ do
        putStr "% "
        hFlush stdout
        input <- getLine
        case words input of
            ["Step",num] -> case readMaybe num of
                Nothing -> do
                    putStrLn "Formatting error."
                    io_stuff
                Just n -> writeIORef steps n
            [reg] -> do 
                print_reg regstate $ "proc_core_" ++ reg
                io_stuff
            ["Float",addr] -> do
                case hex_to_maybe_integer_str addr of
                    Just n -> print_file_reg filestate float_file $ fromInteger n
                    Nothing -> putStrLn "Formatting error."
                io_stuff
            ["Int",addr] -> do
                case hex_to_maybe_integer_str addr of
                    Just n -> case n == 0 of
                        True -> case M.lookup int_file (files filestate) of
                            Nothing -> putStrLn $ "File " ++ int_file ++ " not found."
                            Just r -> let k = kind r in putStrLn $ ppr_hex $ defVal k
                        False -> print_file_reg filestate int_file $ fromInteger n
                    Nothing -> putStrLn "Formatting error."
                io_stuff
            ["Mem",addr] -> do
                case hex_to_maybe_integer_str addr of
                    Just n -> print_file_reg filestate mem_file $ fromInteger n - offset
                    Nothing -> putStrLn "Formatting error."
                io_stuff
            [] -> io_stuff
            _ -> do
                putStrLn "Formatting error."
                io_stuff

main :: IO()
main = do
    counter <- newIORef 0
    steps <- newIORef 0
    n <- isa_size
    simulate_module 0 round_robin_rules (map fst $ getRules (basemod n)) (meths steps counter) (regfiles n) (basemod n)
    return ()
