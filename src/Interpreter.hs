import Core
import Parser
import Evaluator
import Eval
import System.Environment (getArgs)
import System.IO

import Control.Monad.Trans.Class

import qualified Data.Map.Lazy as Map

main :: IO Config
main = do
  args <- getArgs
  c <- runParams args
  return c

runParams :: [String] -> IO Config

runParams [] = do
  putStr version
  (c,()) <- runEvaluatorT evalInteractive initConfig
  return c

runParams ("--help":[]) = do
  putStr help
  return initConfig

runParams ("--version":[]) = do
  putStr $ version ++ license
  return initConfig

runParams (f:[]) = do
  p' <- parseFromFile program f 
  case p' of
    Left x -> do
      putStrLn $ show x
      return initConfig
    Right p -> do
      (c,()) <- runEvaluatorT (evalImports p >> evalProg p) initConfig 
      return c

runParams _ = runParams $ "--help":[]

evalImports :: Program -> Eval ()
evalImports p = foldr (>>) (return ()) (map evalImport $ programImports p)

evalImport :: String -> Eval ()
evalImport f = do
  p' <- lift $ parseFromFile program f
  case p' of
    Left x -> do
      lift.putStrLn $ show x
      return ()
    Right p -> do
      c <- config
      mixs <- pure $ configDecls c
      pmixs <- pure $ programMixins p
      mixs' <- pure $ mixs ++ (filter (\x -> not $ elem x mixs) pmixs)
      put c { configDecls = mixs' }
      evalImports p
      return ()  

evalInteractive :: Eval ()
evalInteractive = do
  lift $ putStr " >  "
  lift $ hFlush stdout
  line <- lift getLine
  instr <- pure $ (parse importStmt "" line, parse instruction "" line)
  case instr of
    (Right x, _) -> do evalImport x
                       evalInteractive
                       return ()
    (_,Right x) -> do evalInstr x
                      evalInteractive
                      return ()

    (Left x, Left y) -> do lift.putStrLn $ (show x) ++ (show y)
                           return ()
                                          
initConfig :: Config
initConfig = Config initHeap initEnv initCtx initDefs
  where
  initHeap = Map.fromList [(0,Object [] Map.empty)]
  initEnv  = Left Map.empty
  initCtx  = Top 
  initDefs = [Mixin "Object" [] [] [], mixinInteger 0, mixinBoolean False, mixinString ""]

help = "    magda <filename> | --help | --version\n"

version = " HI Magda v.1.0 \n" ++
          " An Haskell Interpreter for the Magda Language. \n" ++
          "    https://gitlab.com/magda-lang/hi-magda \n" ++
          "  -------------------------------------------\n"
             
license = " Haskell Interpreter for Magda \n\
          \ Copyright (C) 2019  Magda Language \n\
          \ This program is free software: you can redistribute it and/or modify \n\
          \ it under the terms of the GNU General Public License as published by \n\
          \ the Free Software Foundation, either version 3 of the License, or \n\
          \ (at your option) any later version. \n\
          \ \n\
          \ This program is distributed in the hope that it will be useful, \n\
          \ but WITHOUT ANY WARRANTY; without even the implied warranty of \n\
          \ MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the \n\
          \ GNU General Public License for more details. \n\
          \ You should have received a copy of the GNU General Public License \n\
          \ along with this program.  If not, see <http://www.gnu.org/licenses/>. \n"
