module Lance.IRCompile.IRCompile
  ( compileProgram,
    IRValue (..),
    IRInstr (..),
    Label,
    LabelIndex,
    Register,
    IRProgram,
  )
where

import Control.Monad (foldM)
import Control.Monad.State.Strict
  ( MonadState (get, put),
    State,
    modify',
    runState,
  )
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Vector qualified as V
import Data.Word (Word16, Word8)
import Lance.Resolve.ResolvedExpr (RExpr (..), RPrimitiveCall (RPrimitiveCallPure), RValue (RNumber))

type Register = Word8

type ConstantIndex = Word16

type Label = Word16

type LabelIndex = Word16

data IRValue
  = IRInt Int
  | IRBool Bool
  deriving (Show, Eq, Ord)

data IRInstr
  = IRLoadConstant Register ConstantIndex
  | IRAdd Register Register Register
  | IRMove Register Register
  | IREqual Register Register Register
  | IRJump LabelIndex
  | IRJumpIfFalse Register LabelIndex
  deriving (Show, Eq, Ord)

type IRProgram = V.Vector IRInstr

data IRCompileState = IRCompileState
  { nextRegister :: Register,
    labels :: Map Label LabelIndex,
    nextLabel :: LabelIndex,
    constants :: [IRValue],
    nextConstantIndex :: ConstantIndex,
    instructions :: [IRInstr],
    pc :: Word16
  }
  deriving (Show, Eq)

type IRCompileM a = State IRCompileState a

newRegister :: IRCompileM Register
newRegister = do
  s <- get
  let r = nextRegister s
  put s {nextRegister = r + 1}
  return r

emit :: IRInstr -> IRCompileM ()
emit i = modify' $ \st -> st {instructions = instructions st ++ [i], pc = pc st + 1}

addConstant :: IRValue -> IRCompileM ConstantIndex
addConstant v = do
  s <- get
  let idx = nextConstantIndex s
  put s {nextConstantIndex = idx + 1, constants = constants s ++ [v]}
  return idx

addLabel :: IRCompileM LabelIndex
addLabel = do
  s <- get
  let lbl = nextLabel s
  put s {nextLabel = lbl + 1, labels = Map.insert lbl 0 (labels s)}
  return lbl

markLabel :: Label -> IRCompileM ()
markLabel lbl = do
  pc <- pc <$> get
  modify' $ \s -> s {labels = Map.insert lbl pc (labels s)}

compileExpr :: RExpr -> IRCompileM Register
compileExpr (RValue (RNumber n)) = do
  idx <- addConstant (IRInt (fromIntegral n))
  reg <- newRegister
  emit (IRLoadConstant reg idx)
  return reg
compileExpr (RPrimitiveCall (RPrimitiveCallPure "+" _ exprs)) = do
  regs <- mapM compileExpr exprs
  V.fold1M
    ( \r1 r2 -> do
        dst <- newRegister
        emit (IRAdd dst r1 r2)
        return dst
    )
    (V.fromList regs)
compileExpr (RPrimitiveCall (RPrimitiveCallPure "=" _ [e1, e2])) = do
  r1 <- compileExpr e1
  r2 <- compileExpr e2
  reg <- newRegister
  emit (IREqual reg r1 r2)
  return reg
compileExpr (RIf cond thenBranch elseBranch) = do
  condReg <- compileExpr cond
  dst <- newRegister

  labelElse <- addLabel
  labelEnd <- addLabel

  emit (IRJumpIfFalse condReg labelElse)

  thenReg <- compileExpr thenBranch
  emit (IRMove dst thenReg)
  emit (IRJump labelEnd)

  markLabel labelElse

  elseReg <- compileExpr elseBranch
  emit (IRMove dst elseReg)
  markLabel labelEnd

  return dst
compileExpr e = error $ "compileExpr: not implemented for " ++ show e

compileProgram :: [RExpr] -> (V.Vector IRValue, IRProgram, Map Label LabelIndex, Maybe Register)
compileProgram exprs =
  let initSt =
        IRCompileState
          { nextRegister = 0,
            labels = Map.empty,
            nextLabel = 0,
            constants = [],
            nextConstantIndex = 0,
            instructions = [],
            pc = 0
          }

      step :: Maybe Register -> RExpr -> IRCompileM (Maybe Register)
      step _ e = Just <$> compileExpr e

      (lastReg, st) = runState (foldM step Nothing exprs) initSt
   in ( V.fromList (constants st),
        V.fromList (instructions st),
        labels st,
        lastReg
      )
