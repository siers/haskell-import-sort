{-# LANGUAGE OverloadedStrings #-}

{- cabal install process-extras string -}

import Prelude ()

import Control.Applicative
import Control.Monad
import Data.Function
import Data.Functor
import Data.List
import Data.List.Split
import Data.Maybe
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import System.Environment
import System.Process.ByteString
import GHC.Base

snd3 (_,b,_) = b

bsplitOn bs = map B.pack . splitOn (B.unpack bs) . B.unpack

strip = rs . rs
    where rs = B.reverse . B.dropWhile (== 10)

respace from to = B.intercalate to . bsplitOn from

process :: [String] -> B.ByteString -> IO B.ByteString
process (a:as) input = readProcessWithExitCode a as input >>= return . snd3

nullify :: B.ByteString -> B.ByteString
nullify = respace "\n" "\0"

denullify :: B.ByteString -> B.ByteString
denullify = respace "\0" "\n"

unpara :: [String] -> (B.ByteString -> B.ByteString) -> B.ByteString -> IO B.ByteString
unpara mapArgs pre = fmap smash . mapM (process mapArgs . pre) . bsplitOn "\n\n"
    where smash = B.intercalate "\n" . map nullify

para = B.intercalate "\n\n" . map denullify . bsplitOn "\n"

main = do
    args <- getArgs

    let [mapArgs:processArgs:_] = splitWhen (== "--") args

    B.getContents >>= unpara ["cat"] strip
