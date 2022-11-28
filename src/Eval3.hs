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
                                                , liftM2
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
  StateErrorTrace { runStateErrorTrace :: Env -> Pair (Either Error (Pair a Env)) Trace }

instance Monad StateErrorTrace where 
  return x = StateErrorTrace (\s -> ((Right (x :!: s)) :!: ""))
  m >>= f = StateErrorTrace (\s -> let (e :!: t) = runStateErrorTrace m s
                                   in case e of 
                                      Left err -> (Left err :!: t)
                                      Right (a :!: s') -> let (e' :!: t') = runStateErrorTrace (f a) s'
                                                          in (e' :!: t ++ t'))

-- Recuerde agregar las siguientes instancias para calmar al GHC:
instance Functor StateErrorTrace where
  fmap = liftM

instance Applicative StateErrorTrace where
  pure  = return
  (<*>) = ap

-- Ejercicio 3.b: Resolver en Monad.hs

-- Ejercicio 3.c: Dar una instancia de MonadTrace para StateErrorTrace.
instance MonadTrace StateErrorTrace where
  addTrace tr = StateErrorTrace (\s -> ((Right (() :!: s)) :!: tr))

-- Ejercicio 3.d: Dar una instancia de MonadError para StateErrorTrace.
instance MonadError StateErrorTrace where
  throw err = StateErrorTrace (\_ -> ((Left err) :!: ""))

-- Ejercicio 3.e: Dar una instancia de MonadState para StateErrorTrace.
instance MonadState StateErrorTrace where
  lookfor v = StateErrorTrace (\s -> case M.lookup v s of
                              Nothing -> Left UndefVar :!: ("Variable " ++ show v ++ " not defined\n\n")
                              Just val -> Right (val :!: s) :!: "")
  update v i = StateErrorTrace (\s -> (Right (() :!: update' v i s)) :!: "") where update' = M.insert

-- Ejercicio 3.f: Implementar el evaluador utilizando la monada StateErrorTrace.
-- Evalua un programa en el estado nulo
eval :: Comm -> Pair (Either Error Env) Trace
eval p = let (ev :!: tr) = runStateErrorTrace (stepCommStar p) initEnv
         in case ev of
            Left err -> ((Left err) :!: tr)
            Right (_ :!: env) -> ((Right env) :!: tr)

-- Evalua multiples pasos de un comando, hasta alcanzar un Skip
stepCommStar :: (MonadState m, MonadError m, MonadTrace m) => Comm -> m ()
stepCommStar Skip = return ()
stepCommStar c    = stepComm c >>= \c' -> stepCommStar c'

-- Evalua un paso de un comando
stepComm :: (MonadState m, MonadError m, MonadTrace m) => Comm -> m Comm
stepComm (Seq Skip c2) = return c2
stepComm (Seq c1 c2) = do e1 <- stepComm c1
                          return (Seq e1 c2)
stepComm (IfThenElse e c1 c2) = do v1 <- evalExp e
                                   addTrace ("In if " ++ show e ++ " evaluated " ++ show v1 ++ "\n\n")
                                   if v1 then return c1 else return c2
stepComm w@(While bool e) = do bval <- evalExp bool
                               addTrace ("In while " ++ show bool ++ " evaluated " ++ show bval ++ "\n\n")
                               if bval then return (Seq e w) else return Skip
stepComm (Let x e) = do val <- evalExp e
                        addTrace ("let " ++ show x ++ " = " ++ show val ++ "\n\n")
                        update x val
                        return Skip

-- Evalua una expresion 
evalExp :: (MonadState m, MonadError m, MonadTrace m) => Exp a -> m a
evalExp (Const v) = return v
evalExp (Var x) = lookfor x
evalExp (UMinus e) = liftM negate (evalExp e)
evalExp (Plus e1 e2) = liftM2 (+) (evalExp e1) (evalExp e2)
evalExp (Minus e1 e2) = liftM2 (-) (evalExp e1) (evalExp e2)
evalExp (Times e1 e2) = liftM2 (*) (evalExp e1) (evalExp e2)
evalExp d@(Div e1 e2) = do v2 <- evalExp e2
                           if v2 == 0 then (do addTrace ("In " ++ show d ++ " a division by zero ocurred")
                                               throw DivByZero)
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
