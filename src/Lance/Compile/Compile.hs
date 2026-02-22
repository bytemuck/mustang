module Lance.Compile.Compile
  ( lowerIR,
    Program (..),
  )
where

import Data.Map (Map)
import Data.Map qualified as Map
import Data.Vector qualified as V
import Data.Word (Word8)
import Lance.IRCompile.IRCompile
  ( IRInstr (..),
    IRValue (..),
    Label,
    LabelIndex,
  )

data Value = VInt Int | VBool Bool deriving (Show, Eq)

data Program
  = BCLoadConst Word8 Word8 Word8
  | BCAdd Word8 Word8 Word8
  | BCMove Word8 Word8 Word8
  | BCEqual Word8 Word8 Word8
  | BCJump Word8 Word8 Word8
  | BCJumpIfFalse Word8 Word8 Word8
  deriving (Show, Eq)

-- | Encode a PC (Word16) into two bytes
encodePC :: LabelIndex -> (Word8, Word8)
encodePC pc = (fromIntegral $ pc `div` 256, fromIntegral $ pc `mod` 256)

lowerInstr :: Map LabelIndex Label -> IRInstr -> Program
lowerInstr labelMap ir =
  case ir of
    IRLoadConstant dst cIx ->
      BCLoadConst dst (fromIntegral cIx) 0
    IRAdd dst a b ->
      BCAdd dst a b
    IRMove dst src ->
      BCMove dst src 0
    IREqual dst a b ->
      BCEqual dst a b
    IRJump lbl ->
      let (hi, lo) = encodePC (lookupLabel lbl)
       in BCJump hi lo 0
    IRJumpIfFalse reg lbl ->
      let (hi, lo) = encodePC (lookupLabel lbl)
       in BCJumpIfFalse reg hi lo
  where
    lookupLabel l =
      case Map.lookup l labelMap of
        Just pc -> pc
        Nothing -> error $ "unresolved label: " ++ show l

lowerIR :: Map LabelIndex Label -> (V.Vector IRValue, V.Vector IRInstr) -> (V.Vector Value, V.Vector Program)
lowerIR labelMap (consts, instrs) = (V.map lowerValue consts, V.map (lowerInstr labelMap) instrs)
  where
    lowerValue :: IRValue -> Value
    lowerValue (IRInt n) = VInt n
    lowerValue (IRBool b) = VBool b