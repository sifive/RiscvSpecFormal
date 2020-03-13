import qualified HaskellTarget as T

import Simulator.All

import Data.IORef
import System.Environment

instance AbstractEnvironment () where
    envPre _ _ _ _ = return ()
    envPost _ _ _ _ = return ()

getMod :: IO (Bool,T.Mod)
getMod = do
    args <- getArgs
    if "Reg" `elem` args then return (False,T.testReg)
    else
        if "Async" `elem` args then return (False,T.testAsync)
        else
            if "SyncIsAddr" `elem` args then return (False,T.testSyncIsAddr)
            else
                if "SyncNotIsAddr" `elem` args then return (False,T.testSyncNotIsAddr)
                else
                    if "Native" `elem` args then return (True,T.testNative)
                    else error "Please supply a test as a command-line argument."

main :: IO()
main = do
    e <- newIORef ()
    (b,mod) <- getMod
    let rules = if b then user_rules else round_robin_rules
    let (rfs,basemod) = snd $ T.separateModRemove mod
    simulate_module 0 rules e (map fst $ T.getRules basemod) [] rfs [] basemod
    return ()
