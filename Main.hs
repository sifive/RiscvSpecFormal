
module Main where

import Simulator.All
import HaskellTarget as T

import Control.Monad
import Data.IORef
import System.Exit
import System.IO

handle :: Handle
handle = stdout

timeout :: Int
timeout = 1000

isa_size :: Int
isa_size = snd T.kami_model

regfiles :: [T.RegFileBase]
regfiles = fst $ fst T.kami_model

basemod :: T.BaseModule
basemod = snd $ fst T.kami_model

meths :: IORef Int -> [(String, Val -> IO Val)]
meths counter = [("proc_core_pc", proc_core_meth counter)]

--from riscv-tests/rv65ui-p-add.dump
passaddr :: Val
passaddr = BitvectorVal $ hex_to_bv isa_size "60c"

failaddr :: Val
failaddr = BitvectorVal $ hex_to_bv isa_size "5f8"

proc_core_meth :: IORef Int -> Val -> IO Val
proc_core_meth counter v = do
    n <- readIORef counter
    when (n > timeout) $ do
        hPutStrLn handle "TIMEOUT"
        exitFailure
    writeIORef counter (n+1)
    if v == passaddr then do
        hPutStrLn handle "Pass"
        exitSuccess
    else if v == failaddr then do
        hPutStrLn handle "Fail"
        exitFailure
    else return tt

main :: IO()
main = do
    counter <- newIORef 0
    simulate_module 0 round_robin_rules (map fst $ T.getRules basemod) (meths counter) regfiles basemod
    return ()
