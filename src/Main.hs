-- yell, a library for automating Facebook conversations.
-- Copyright (C) 2016  Boo Mew Mew

-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.

-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.

-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.

-- Address correspondence about this library to boomewmew@gmail.com.

{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text          as T
import qualified Facebook           as FB
import qualified System.Environment as E
import qualified System.IO          as IO

type Content  = T.Text
type Contents = [Content]
type Person   = Content
type Dialog   = Content
type Line     = (Person,Dialog)

delimiter :: Content
delimiter = ":"

redirectUrl :: FB.RedirectUrl
redirectUrl = "https://www.google.com"

parseArgs :: [String] -> IO.FilePath
parseArgs []         = error $ "No argument given. Please provide " ++
                       "conversation file as command-line argument."
parseArgs [fileName] = fileName
parseArgs _          = error "Too many arguments."

recombine :: Contents -> Line
recombine []            = error "Empty line in conversation file."
recombine [content]     = error $ T.unpack $ "Line \"" `T.append` content
                          `T.append` "\" contains no \"" `T.append` delimiter
                          `T.append` " character."
recombine (person:tail) = (person,T.intercalate delimiter tail)

parseConversation :: Content -> [Line]
parseConversation = map (recombine . T.splitOn delimiter) . T.lines

main :: IO ()
main = do
    args        <- E.getArgs
    fileContent <- IO.readFile $ parseArgs args
    putStrLn $ show $ map (\(p,d) -> (T.unpack p,T.unpack d)) $
        parseConversation $ T.pack fileContent
    return ()

-- state <- FB.getUserAccessTokenStep1 redirectUrl ["publish_actions"]
