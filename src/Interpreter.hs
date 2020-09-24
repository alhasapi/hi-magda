import Core
import Parser
import Evaluator
import Eval
import TypeCheck
import TypeChecker
import System.Environment (getArgs)
import System.IO

import System.Console.Haskeline

import Control.Monad.Trans.Class

import qualified Data.Map.Lazy as Map

main :: IO Config
main = do
  args <- getArgs
  c <- runParams args
  return c

typeCheck :: TypeCheck a -> IO Bool
typeCheck tc =  case runTypeChecker tc initContext of
  (c,Left x) -> do putStrLn x
                   putStrLn $ show c
                   return False
  otherwise -> return True
  
runParams :: [String] -> IO Config
runParams [] = do
  putStr version
  putStrLn disclaimer
  (c,()) <- runEvaluatorT evalInteractive initConfig
  return c

runParams ("--help":[]) = do
  putStr help
  return initConfig

runParams ("--version":[]) = do
  putStr $ version ++ license
  return initConfig

runParams ("--ast":[]) = runParams ["--help"]

runParams ("--ast":f:[]) = do
  p' <- parseFromFile program f
  case p' of
    Left x -> do
      putStrLn $ show x
      return initConfig
    Right x -> do
      putStrLn $ show x      
      return initConfig

runParams (f:[]) = do
  p' <- parseFromFile program f 
  case p' of
    Left x -> do
      putStrLn $ show x
      return initConfig
    Right p -> do
      
      tc <- typeCheck (tcheckProgram p)
      putStrLn("RIUSCITO");
      --putStrLn(show $ programMain p)
      if tc            
        then do (c,()) <- runEvaluatorT (evalImports p >> evalProg p) initConfig 
                putStrLn("RIUSCITO2");
                return c
        else return initConfig

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
  iline <- lift $ runInputT defaultSettings $ getInputLine " >  "
  case iline of
    Nothing -> lift.putStrLn $ " Bye!"
    Just "" -> evalInteractive
    Just line -> do
      instr <- pure $ ( parse importStmt "<stdin>" line
                      , parse instruction "<stdin>" line
                      , parse localId "<stdin>" line )
      case instr of
        (Right x, _, _) -> do
          evalImport x
          evalInteractive

        (_,Right i, _) -> do
          --tc <- lift.typeCheck $ tcheckInstr i
          if True -- tc
            then do evalInstr i
                    evalInteractive
            else evalInteractive

        (_, _, Right (Identifier x _)) -> do
          c <- config
          Left env <- pure $ configEnv c
          env' <- pure.(Left) $ Map.insert x ObjNull env
          put $ c { configEnv = env' }
          evalInteractive

        (Left x, Left y, Left z) -> do
          lift.putStrLn $ (show x) ++ (show y) ++ (show z)
          evalInteractive
                                          
initConfig :: Config
initConfig = Config initHeap initEnv initCtx initDecls
  where
  initHeap = Map.fromList [(0,Object [] Map.empty)]
  initEnv  = Left Map.empty
  initCtx  = Top 

initDecls :: [Mixin]
initDecls = [Mixin "Object" [] [] [], mixinBoolean, mixinInteger, mixinString]

initContext :: TypeCheckContext
initContext = TypeCheckContext initDecls [] (Left ())
    
help = "    magda [ <filename> | --help | --version | --ast <filename> ]\n"

version = " HI Magda v.1.1 \n" ++
          " An Haskell Interpreter for the Magda Language. \n" ++
          "    https://gitlab.com/magda-lang/hi-magda    \n" ++
          "  ------------------------------------------  \n"

disclaimer = " Haskell Interpreter for Magda  Copyright (C) 2019  Magda Language \n\
             \ This program comes with ABSOLUTELY NO WARRANTY. \n\
             \ This is free software, and you are welcome to redistribute it \n\
             \ under certain conditions. Run `magda --version` for details. \n"
          
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
