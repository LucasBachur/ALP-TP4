module Eval3
  ( eval
  , Env
  )
where

import           AST
import           Monads
import qualified Data.Map.Strict               as M
import           Data.Maybe
import           Data.Strict.Tuple
import           Control.Monad                  ( liftM
                                                , ap
                                                )

-- Entornos
type Env = M.Map Variable Int

-- Entorno nulo
initEnv :: Env
initEnv = M.empty

-- Ejercicio 3.a: Proponer una nueva m\'onada que  
-- lleve una traza de ejecución (además de manejar errores y estado).
-- y dar su instancia de mónada. Llamarla |StateErrorTrace|. 
newtype StateErrorTrace a =
  SET {runSET:: Env -> (Either Error (Pair a Env), String)}

instance Functor StateErrorTrace where
  fmap = liftM

instance Applicative StateErrorTrace where
  pure  = return
  (<*>) = ap

instance Monad StateErrorTrace where
  return x = SET (\s -> (Right (x :!: s),""))
  m >>= f = SET (\s -> case runSET m s of
                            (Left e, t) -> (Left e, t)
                            (Right (x :!: s'), t) -> 
                               let (m, t') = runSET (f x) s' in (m, t++t')
                          )

-- Ejercicio 3.b: Resolver en Monad.hs


-- Ejercicio 3.c: Dar una instancia de MonadTrace para StateErrorTrace.
instance MonadTrace StateErrorTrace where
  addTrace st = SET (\s -> (Right (() :!: s), st)) 

-- Ejercicio 3.d: Dar una instancia de MonadError para StateErrorTrace.
instance MonadError StateErrorTrace where
  throw e = SET (\s -> (Left e, ""))

-- Ejercicio 3.e: Dar una instancia de MonadState para StateErrorTrace.
instance MonadState StateErrorTrace where
  lookfor v = SET (\s -> case M.lookup v s of
                              Just n -> (Right (n :!: s), "")
                              _ -> (Left UndefVar, ""))
  update v i = SET (\s -> (Right (() :!: M.insert v i s), ""))

-- Ejercicio 3.f: Implementar el evaluador utilizando la monada StateErrorTrace.
-- Evalua un programa en el estado nulo

eval :: Comm -> Either (Error, String) (Env, String)
eval p = case runSET (stepCommStar p) initEnv of
            (Right (_ :!: s), t) -> Right (s, t)
            (Left e, t) -> Left (e, t)

-- Evalua multiples pasos de un comando, hasta alcanzar un Skip
stepCommStar :: (MonadState m, MonadError m, MonadTrace m) => Comm -> m ()
stepCommStar Skip = return ()
stepCommStar c    = stepComm c >>= \c' -> stepCommStar c'

-- Evalua un paso de un comando
stepComm :: (MonadState m, MonadError m, MonadTrace m) => Comm -> m Comm
stepComm Skip = return Skip
stepComm (Let v exp) = do n <- evalExp exp
                          update v n
                          addTrace (v ++ " = " ++ show n ++ "\n")
                          return Skip
stepComm (Seq Skip c2) = return c2
stepComm (Seq c1 c2) = do c' <- stepComm c1
                          return (Seq c' c2)
stepComm (IfThenElse b c1 c2) = evalExp b >>= \r -> if r then return c1 else return c2
stepComm loop@(While b c) = evalExp b >>= \r -> if r then return (Seq c loop) else return Skip

-- Evalua una expresion 
evalExp :: (MonadState m, MonadError m, MonadTrace m) => Exp a -> m a
evalExp (Const n) = return n
evalExp (Var v) = lookfor v
evalExp (UMinus exp) = evalExp exp >>= \n -> return (-n)
evalExp (Plus exp1 exp2) = do n1 <- evalExp exp1
                              n2 <- evalExp exp2
                              return (n1+n2)
evalExp (Minus exp1 exp2) = do n1 <- evalExp exp1
                               n2 <- evalExp exp2
                               return (n1-n2)
evalExp (Times exp1 exp2) = do n1 <- evalExp exp1
                               n2 <- evalExp exp2
                               return (n1*n2)
evalExp (Div exp1 exp2) = do n1 <- evalExp exp1
                             n2 <- evalExp exp2
                             case n2 of
                               0 -> throw DivByZero
                               _ -> return (div n1 n2)
evalExp (EAssgn var exp) = do n <- evalExp exp
                              update var n
                              return n
evalExp (ESeq exp1 exp2) = evalExp exp1 >> evalExp exp2

evalExp BTrue = return True
evalExp BFalse = return False
evalExp (Lt exp1 exp2) = do b1 <- evalExp exp1
                            b2 <- evalExp exp2
                            return (b1<b2)
evalExp (Gt exp1 exp2) = do b1 <- evalExp exp1
                            b2 <- evalExp exp2
                            return (b1>b2)
evalExp (And exp1 exp2) = do b1 <- evalExp exp1
                             b2 <- evalExp exp2
                             return (b1 && b2)
evalExp (Or exp1 exp2) = do b1 <- evalExp exp1
                            b2 <- evalExp exp2
                            return (b1 || b2)
evalExp (Not exp) = evalExp exp >>= \b -> return (not b)
evalExp (Eq exp1 exp2) = do b1 <- evalExp exp1
                            b2 <- evalExp exp2
                            return (b1 == b2)
evalExp (NEq exp1 exp2) = do b1 <- evalExp exp1
                             b2 <- evalExp exp2
                             return (b1 /= b2)
