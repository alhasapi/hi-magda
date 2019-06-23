import Core
import Parser
import Eval
import System.Environment (getArgs)
import System.IO

import qualified Data.Map.Lazy as Map

main :: IO ()
main = do args <- getArgs
          case length args of
            0 -> do interactive initConfig
                    return ()
            1 -> case parseParam $ head args of
                   Just x -> runParam x
                   otherwise -> do evalFile $ head args
                                   return ()
            otherwise -> putStrLn "Wrong arguments number"
  where
    evalFile :: String -> IO (Config)
    evalFile f = do p <- parseFromFile program f
                    case p of
                      Left x   -> error $ show x
                      Right p' -> do initMixins <- fmap (++ programMixins p') $ evalImports (programImports p')
                                     (cc@(Config a b c d),_) <- pure $ eval (evalProg p' {programMixins=initMixins}) initConfig --naming config fields forces evaluation
                                     return cc

    parseParam :: String -> Maybe String
    parseParam ('-':'-':p) = Just p
    parseParam _ = Nothing
  
    runParam :: String -> IO ()
    runParam "version" = putStrLn $ version ++ license
    runParam x = putStrLn $ "Unknown parameter " ++ x

evalImports :: [String] -> IO [Mixin]
evalImports [] = return []
evalImports (f:fs) = do p <- parseFromFile program f
                        case p of
                          Left x -> error $ show x
                          Right p' -> do defs <- pure $ programMixins p'
                                         imps <- pure $ programImports p'
                                         ps <- evalImports (fs ++ imps)
                                         return $ defs ++ ps

interactive :: Config -> IO (Config)
interactive c = do
  putStr " > "
  hFlush stdout
  c' <- readEval c
  c'' <- interactive c'
  return c''
  where
    readEval c =
      do l <- getLine
         c' <- execInstr c l
         case c' of
           Just c'' -> return c''
           Nothing -> return c
  
    execInstr :: Config -> String -> IO (Maybe Config)
    execInstr c l =
      case parse importStmts "" l of
        Right fs -> do ms' <- evalImports fs
                       ms <- pure $ configDecls c
                       c' <- pure $ c {configDecls = ms ++ ms'}
                       return $ Just c'
        Left _ -> case parse instruction "" l of
                    Right i -> do (c'@(Config c1 c2 c3 c4),_) <- pure $ eval (evalInstr i) c
                                  return $ Just c'
                    Left x -> do print x
                                 return Nothing
        
initConfig :: Config
initConfig = Config initHeap initEnv initCtx initDefs
  where
  initHeap = Map.fromList [(0,Object [] Map.empty)]
  initEnv  = Left Map.empty
  initCtx  = Top 
  initDefs = [Mixin "Object" [] [] [], mixinInteger 0, mixinBoolean False, mixinString ""]

version = " HI Magda v.0 \n\
          \ An Haskell Interpreter for the Magda Language. \n\n\
          \ \t https://gitlab.com/magda-lang/hi-magda \n\n"
             
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
