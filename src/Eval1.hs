module Eval1
  ( eval
  , Env
  )
where

import           AST
import           Monads
import qualified Data.Map.Strict               as M
import           Data.Maybe
import           Prelude                 hiding ( fst
                                                , snd
                                                )
import           Data.Strict.Tuple
import           Control.Monad                  ( liftM
                                                , ap
                                                , liftM2
                                                )

-- Entornos
type Env = M.Map Variable Int

-- Entorno nulo
initEnv :: Env
initEnv = M.empty

-- Mónada estado
newtype State a = State { runState :: Env -> Pair a Env }

instance Monad State where
  return x = State (\s -> (x :!: s))
  m >>= f = State (\s -> let (v :!: s') = runState m s in runState (f v) s')

-- Para calmar al GHC
instance Functor State where
  fmap = liftM

instance Applicative State where
  pure  = return
  (<*>) = ap

instance MonadState State where
  lookfor v = State (\s -> (lookfor' v s :!: s))
    where lookfor' v s = fromJust $ M.lookup v s
  update v i = State (\s -> (() :!: update' v i s)) where update' = M.insert

-- Ejercicio 1.b: Implementar el evaluador utilizando la monada State

-- Evalua un programa en el estado nulo
eval :: Comm -> Env
eval p = snd (runState (stepCommStar p) initEnv)

-- Evalua multiples pasos de un comando, hasta alcanzar un Skip
stepCommStar :: MonadState m => Comm -> m ()
stepCommStar Skip = return ()
stepCommStar c    = stepComm c >>= \c' -> stepCommStar c'

-- Evalua un paso de un comando
stepComm :: MonadState m => Comm -> m Comm
stepComm (Seq Skip c2) = return c2
stepComm (Seq c1 c2) = do e1 <- stepComm c1
                          return (Seq e1 c2)
stepComm (IfThenElse e c1 c2) = do v1 <- evalExp e
                                   if v1 then return c1 else return c2
stepComm w@(While bool e) = do bval <- evalExp bool
                               if bval then return (Seq e w) else return Skip
stepComm (Let x e) = do val <- evalExp e
                        update x val
                        return Skip

-- Evalua una expresion
evalExp :: MonadState m => Exp a -> m a
evalExp (Const v) = return v
evalExp (Var x) = lookfor x
evalExp (UMinus e) = liftM negate (evalExp e)
evalExp (Plus e1 e2) = liftM2 (+) (evalExp e1) (evalExp e2)
evalExp (Minus e1 e2) = liftM2 (-) (evalExp e1) (evalExp e2)
evalExp (Times e1 e2) = liftM2 (*) (evalExp e1) (evalExp e2)
evalExp (Div e1 e2) = liftM2 (div) (evalExp e1) (evalExp e2)
evalExp (BTrue) = return True
evalExp (BFalse) = return False
evalExp (Lt e1 e2) = liftM2 (<) (evalExp e1) (evalExp e2)
evalExp (Gt e1 e2) = liftM2 (>) (evalExp e1) (evalExp e2)
evalExp (Eq e1 e2) = liftM2 (==) (evalExp e1) (evalExp e2)
evalExp (NEq e1 e2) = liftM2 (/=) (evalExp e1) (evalExp e2)
evalExp (And e1 e2) = liftM2 (&&) (evalExp e1) (evalExp e2)
evalExp (Or e1 e2) = liftM2 (||) (evalExp e1) (evalExp e2)
evalExp (Not e1) = liftM (not) (evalExp e1)
