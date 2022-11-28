module Eval2
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
                                                , liftM2
                                                )

-- Entornos
type Env = M.Map Variable Int

-- Entorno nulo
initEnv :: Env
initEnv = M.empty

-- Mónada estado, con manejo de errores
newtype StateError a =
  StateError { runStateError :: Env -> Either Error ( Pair a Env) }


-- Para calmar al GHC
instance Functor StateError where
  fmap = liftM

instance Applicative StateError where
  pure  = return
  (<*>) = ap

-- Ejercicio 2.a: Dar una instancia de Monad para StateError:
instance Monad StateError where
  return x = StateError (\env -> Right (x :!: env))
  m >>= f = StateError (\env -> let r = (runStateError m) env
                                in case r of
                                    Left err -> Left err
                                    Right (x' :!: env') -> runStateError (f x') env')

-- Ejercicio 2.b: Dar una instancia de MonadError para StateError:
instance MonadError StateError where
  throw err = StateError (\_ -> Left err)

-- Ejercicio 2.c: Dar una instancia de MonadState para StateError:
instance MonadState StateError where
  lookfor v = StateError (\s -> case M.lookup v s of
                              Nothing -> Left UndefVar
                              Just val -> Right (val :!: s))
  update v i = StateError (\s -> Right (() :!: update' v i s)) where update' = M.insert

-- Ejercicio 2.d: Implementar el evaluador utilizando la monada StateError.
-- Evalua un programa en el estado nulo
eval :: Comm -> Either Error Env
eval p = case runStateError (stepCommStar p) initEnv of
          Left err -> Left err
          Right (_ :!: env) -> Right env

-- Evalua multiples pasos de un comando, hasta alcanzar un Skip
stepCommStar :: (MonadState m, MonadError m) => Comm -> m ()
stepCommStar Skip = return ()
stepCommStar c    = stepComm c >>= \c' -> stepCommStar c'

-- Evalua un paso de un comando
stepComm :: (MonadState m, MonadError m) => Comm -> m Comm
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
evalExp :: (MonadState m, MonadError m) => Exp a -> m a
evalExp (Const v) = return v
evalExp (Var x) = lookfor x
evalExp (UMinus e) = liftM negate (evalExp e)
evalExp (Plus e1 e2) = liftM2 (+) (evalExp e1) (evalExp e2)
evalExp (Minus e1 e2) = liftM2 (-) (evalExp e1) (evalExp e2)
evalExp (Times e1 e2) = liftM2 (*) (evalExp e1) (evalExp e2)
evalExp (Div e1 e2) = do v2 <- evalExp e2
                         if v2 == 0 then throw DivByZero 
                                    else (do v1 <- evalExp e1
                                             return (div v1 v2))
evalExp (BTrue) = return True
evalExp (BFalse) = return False
evalExp (Lt e1 e2) = liftM2 (<) (evalExp e1) (evalExp e2)
evalExp (Gt e1 e2) = liftM2 (>) (evalExp e1) (evalExp e2)
evalExp (Eq e1 e2) = liftM2 (==) (evalExp e1) (evalExp e2)
evalExp (NEq e1 e2) = liftM2 (/=) (evalExp e1) (evalExp e2)
evalExp (And e1 e2) = liftM2 (&&) (evalExp e1) (evalExp e2)
evalExp (Or e1 e2) = liftM2 (||) (evalExp e1) (evalExp e2)
evalExp (Not e1) = liftM (not) (evalExp e1)
evalExp (EAssgn x e) = do v <- evalExp e
                          update x v
                          return v
evalExp (ESeq e1 e2) = do evalExp e1
                          evalExp e2