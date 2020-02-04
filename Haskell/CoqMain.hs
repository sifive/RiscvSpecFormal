
module CoqMain where

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

env :: S.Environment a b c d ()
env = S.Build_Environment 
    (\_ _ _ _ -> unsafeCoerce $ (return () :: IO ()))
    (\_ (R.Build_FileState methods int_regs files) _ ruleName -> unsafeCoerce $ do
        when (ruleName /= "proc_core_pipeline") (return ())
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
                else return ()  

        )

binary_split :: Eq a => a -> [a] -> Maybe ([a],[a])
binary_split x xs = go xs [] where
    go [] _ = Nothing
    go (y:ys) acc = if x == y then Just (reverse acc, ys) else go ys (y:acc)

process_args :: [String] -> [(String,String)]
process_args = catMaybes . map (binary_split '=')

timeout :: Int
timeout = 10000

main :: IO()
main = do
    args <- getArgs
    let files = process_args args
    sz <- isa_size
    let sim = if sz == 32 then coqSim_32 else coqSim_64
    sim env () files timeout (\_ _ _ _ -> return ((), False))
