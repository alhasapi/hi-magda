import Core
import Parser
import Evaluator
import Eval
import System.Environment (getArgs)
import System.IO

import Control.Monad.Trans.Class

import qualified Data.Map.Lazy as Map

-- main :: IO ()
-- main = do args <- getArgs
--           case args of
--             [] -> do interactive initConfig
--                      return ()
--             (p:[]) -> case parseParam p of
--                         Just x -> runParam x
--                         otherwise -> do evalFile p
--                                         return ()
--             otherwise -> runParam "help"
--   where
--     evalFile :: String -> IO (Config)
--     evalFile f = do p <- parseFromFile program f
--                     case p of
--                       Left x   -> error $ show x
--                       Right p' -> do initMixins <- fmap (++ programMixins p') $ evalImports (programImports p')
--                                      (cc@(Config a b c d),_) <- pure $ eval (evalProg p' {programMixins=initMixins}) initConfig --naming config fields forces evaluation
--                                      return cc

--     parseParam :: String -> Maybe String
--     parseParam ('-':'-':p) = Just p
--     parseParam _ = Nothing
  
--     runParam :: String -> IO ()
--     runParam "version" = putStrLn $ version ++ license
--     runParam "help" = putStrLn help
--     runParam x = putStrLn $ "Unknown parameter " ++ x

-- evalImports :: [String] -> IO [Mixin]
-- evalImports [] = return []
-- evalImports (f:fs) = do p <- parseFromFile program f
--                         case p of
--                           Left x -> error $ show x
--                           Right p' -> do defs <- pure $ programMixins p'
--                                          imps <- pure $ programImports p'
--                                          ps <- evalImports (fs ++ imps)
--                                          return $ defs ++ ps

main :: IO Config
main = do
  putStr version
  (c,()) <- runEvaluatorT evalInteractive initConfig
  return c
  
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
  
-- interactive :: Config -> IO (Config)
-- interactive c = do
--   putStr " > "
--   hFlush stdout
--   c' <- readEval c
--   c'' <- interactive c'
--   return c''
--   where
--     readEval c =
--       do l <- getLine
--          c' <- execInstr c l
--          case c' of
--            Just c'' -> return c''
--            Nothing -> return c
  
--     execInstr :: Config -> String -> IO (Maybe Config)
--     execInstr _ "" = return Nothing
--     execInstr c l = let parsel x = parse x "" l in
--                       case (parsel instruction,parsel importStmt) of
--                         (Right x,_) -> do (c'@(Config c1 c2 c3 c4),_) <- pure $ eval (evalInstr x) c
--                                           return $ Just c'
--                         (_,Right x) -> do ms <- fmap (++ configDecls c) (evalImports [x])
--                                           return $ Just c {configDecls=ms}
--                         (Left x,Left y) -> do putStrLn $ (show y) ++ (show x)
--                                               return Nothing
                                          
initConfig :: Config
initConfig = Config initHeap initEnv initCtx initDefs
  where
  initHeap = Map.fromList [(0,Object [] Map.empty)]
  initEnv  = Left Map.empty
  initCtx  = Top 
  initDefs = [Mixin "Object" [] [] [], mixinInteger 0, mixinBoolean False, mixinString ""]

help = "\tmagda <filename> | --help | --version"

version = " HI Magda v.1.0 \n" ++
          " An Haskell Interpreter for the Magda Language. \n" ++
          "    https://gitlab.com/magda-lang/hi-magda \n"
             
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
