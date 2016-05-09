{-# LANGUAGE OverloadedStrings #-}

{- cabal install process-extras string -}

import Control.Applicative
import Control.Monad
import Data.Function
import Data.Functor
import Data.List
import Data.List.Split
import Data.Maybe
import qualified Data.ByteString as B
import System.Environment
import System.Process.ByteString

snd3 (_,b,_) = b

bsplitOn bs = map B.pack . splitOn (B.unpack bs) . B.unpack

strip = rs . rs
    where rs = B.reverse . B.dropWhile (== 10)

respace from to = B.intercalate to . bsplitOn from

nullify :: B.ByteString -> B.ByteString
nullify = respace "\n" "\0"

denullify :: B.ByteString -> B.ByteString
denullify = respace "\0" "\n"

unpara pre = B.intercalate "\n" . map (nullify . pre) . bsplitOn "\n\n"
para = B.intercalate "\n\n" . map denullify . bsplitOn "\n"

process :: B.ByteString -> IO B.ByteString
process input = getArgs >>= (\as -> readProcessWithExitCode
    (head as) (tail as) input) >>= return . snd3

main = B.putStr . eoln . para . strip =<< process . eoln . unpara strip =<< B.getContents
    where eoln = flip B.append "\n"
