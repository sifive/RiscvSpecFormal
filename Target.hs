module Target (module Syntax, module Rtl, module Word, module Fin, module EclecticLib, module PeanoNat, rtlMod) where
import EclecticLib
import PeanoNat
import Fin
import Instance
import Rtl
import Syntax hiding (unsafeCoerce)
import Word
rtlMod :: RtlModule
rtlMod =
  model32
  model32
  model32
