
module Numeric.Decimal.OperationSpec (spec) where

import Test.Hspec

import Numeric.Decimal
import Numeric.Decimal.Arithmetic

import qualified Numeric.Decimal.Operation as Op

spec :: Spec
spec = do
  describe "Special values" $ do
    it "Infinity + 1 = Infinity" $
      op2 Op.add "Infinity" "1" `shouldBe` "Infinity"
    it "NaN + 1 = NaN" $
      op2 Op.add "NaN" "1" `shouldBe` "NaN"
    it "NaN + Infinity = NaN" $
      op2 Op.add "NaN" "Infinity" `shouldBe` "NaN"
    it "1 - Infinity = -Infinity" $
      op2 Op.subtract "1" "Infinity" `shouldBe` "-Infinity"
    it "-1 * Infinity = -Infinity" $
      op2 Op.multiply "-1" "Infinity" `shouldBe` "-Infinity"
    it "-0 - 0 = -0" $
      op2 Op.subtract "-0" "0" `shouldBe` "-0"
    it "-1 * 0 = -0" $
      op2 Op.multiply "-1" "0" `shouldBe` "-0"
    it "1 / 0 = Infinity" $
      op2 Op.divide "1" "0" `shouldBe` "Infinity"
    it "1 / -0 = -Infinity" $
      op2 Op.divide "1" "-0" `shouldBe` "-Infinity"
    it "-1 / 0 = -Infinity" $
      op2 Op.divide "-1" "0" `shouldBe` "-Infinity"

  describe "add" $ do
    it "add('12', '7.00')        ==>  '19.00'" $
      op2 Op.add "12" "7.00" `shouldBe` "19.00"
    it "add('1E+2', '1E+4')      ==>  '1.01E+4'" $
      op2 Op.add "1E+2" "1E+4" `shouldBe` "1.01E+4"

  describe "subtract" $ do
    it "subtract('1.3', '1.07')  ==>  '0.23'" $
      op2 Op.subtract "1.3" "1.07" `shouldBe` "0.23"
    it "subtract('1.3', '1.30')  ==>  '0.00'" $
      op2 Op.subtract "1.3" "1.30" `shouldBe` "0.00"
    it "subtract('1.3', '2.07')  ==>  '-0.77'" $
      op2 Op.subtract "1.3" "2.07" `shouldBe` "-0.77"

  describe "minus" $ do
    it "minus('1.3')   ==>  '-1.3'" $
      op1 Op.minus "1.3" `shouldBe` "-1.3"
    it "minus('-1.3')  ==>  '1.3'" $
      op1 Op.minus "-1.3" `shouldBe` "1.3"

  describe "plus" $ do
    it "plus('1.3')    ==>  '1.3'" $
      op1 Op.plus "1.3" `shouldBe` "1.3"
    it "plus('-1.3')   ==>  '-1.3'" $
      op1 Op.plus "-1.3" `shouldBe` "-1.3"

  describe "multiply" $ do
    it "multiply('1.20', '3')         ==>  '3.60'" $
      op2 Op.multiply "1.20" "3" `shouldBe` "3.60"
    it "multiply('7', '3')            ==>  '21'" $
      op2 Op.multiply "7" "3" `shouldBe` "21"
    it "multiply('0.9', '0.8')        ==> '0.72'" $
      op2 Op.multiply "0.9" "0.8" `shouldBe` "0.72"
    it "multiply('0.9', '-0')         ==> '-0.0'" $
      op2 Op.multiply "0.9" "-0" `shouldBe` "-0.0"
    it "multiply('654321', '654321')  ==>  '4.28135971E+11'" $
      op2 Op.multiply "654321" "654321" `shouldBe` "4.28135971E+11"

  describe "exp" $ do
    it "exp('-Infinity')    ==> '0'" $
      op1 Op.exp "-Infinity" `shouldBe` "0"
    it "exp('-1')           ==> '0.367879441'" $
      op1 Op.exp "-1" `shouldBe` "0.367879441"
    it "exp('0')            ==> '1'" $
      op1 Op.exp "0" `shouldBe` "1"
    it "exp('1')            ==> '2.71828183'" $
      op1 Op.exp "1" `shouldBe` "2.71828183"
    it "exp('0.693147181')  ==> '2.00000000'" $
      op1 Op.exp "0.693147181" `shouldBe` "2.00000000"
    it "exp('+Infinity')    ==> 'Infinity'" $
      op1 Op.exp "+Infinity" `shouldBe` "Infinity"

  describe "fusedMultiplyAdd" $ do
    it ("fused-multiply-add('3', '5', '7')                             ==>  " ++
        "'22'") $
      op3 Op.fusedMultiplyAdd "3" "5" "7" `shouldBe` "22"
    it ("fused-multiply-add('3', '-5', '7')                            ==>  " ++
        "'-8'") $
      op3 Op.fusedMultiplyAdd "3" "-5" "7" `shouldBe` "-8"
    it ("fused-multiply-add('888565290', '1557.96930', '-86087.7578')  ==>  " ++
        "'1.38435736E+12'") $
      op3 Op.fusedMultiplyAdd "888565290" "1557.96930" "-86087.7578" `shouldBe`
      "1.38435736E+12"

  describe "ln" $ do
    it "ln('0')           ==> '-Infinity'" $
      op1 Op.ln "0" `shouldBe` "-Infinity"
    it "ln('1.000')       ==> '0'" $
      op1 Op.ln "1.000" `shouldBe` "0"
    it "ln('2.71828183')  ==> '1.00000000'" $
      op1 Op.ln "2.71828183" `shouldBe` "1.00000000"
    it "ln('10')          ==> '2.30258509'" $
      op1 Op.ln "10" `shouldBe` "2.30258509"
    it "ln('+Infinity')   ==> 'Infinity'" $
      op1 Op.ln "+Infinity" `shouldBe` "Infinity"

  describe "log10" $ do
    it "log10('0')          ==>  '-Infinity'" $
      op1 Op.log10 "0" `shouldBe` "-Infinity"
    it "log10('0.001')      ==>  '-3'" $
      op1 Op.log10 "0.001" `shouldBe` "-3"
    it "log10('1.000')      ==>  '0'" $
      op1 Op.log10 "1.000" `shouldBe` "0"
    it "log10('2')          ==>  '0.301029996'" $
      op1 Op.log10 "2" `shouldBe` "0.301029996"
    it "log10('10')         ==>  '1'" $
      op1 Op.log10 "10" `shouldBe` "1"
    it "log10('70')         ==>  '1.84509804'" $
      op1 Op.log10 "70" `shouldBe` "1.84509804"
    it "log10('+Infinity')  ==>  'Infinity'" $
      op1 Op.log10 "+Infinity" `shouldBe` "Infinity"

  describe "divide" $ do
    it "divide('1', '3'  )      ==>  '0.333333333'" $
      op2 Op.divide "1" "3" `shouldBe` "0.333333333"
    it "divide('2', '3'  )      ==>  '0.666666667'" $
      op2 Op.divide "2" "3" `shouldBe` "0.666666667"
    it "divide('5', '2'  )      ==>  '2.5'" $
      op2 Op.divide "5" "2" `shouldBe` "2.5"
    it "divide('1', '10' )      ==>  '0.1'" $
      op2 Op.divide "1" "10" `shouldBe` "0.1"
    it "divide('12', '12')      ==>  '1'" $
      op2 Op.divide "12" "12" `shouldBe` "1"
    it "divide('8.00', '2')     ==>  '4.00'" $
      op2 Op.divide "8.00" "2" `shouldBe` "4.00"
    it "divide('2.400', '2.0')  ==>  '1.20'" $
      op2 Op.divide "2.400" "2.0" `shouldBe` "1.20"
    it "divide('1000', '100')   ==>  '10'" $
      op2 Op.divide "1000" "100" `shouldBe` "10"
    it "divide('1000', '1')     ==>  '1000'" $
      op2 Op.divide "1000" "1" `shouldBe` "1000"
    it "divide('2.40E+6', '2')  ==>  '1.20E+6'" $
      op2 Op.divide "2.40E+6" "2" `shouldBe` "1.20E+6"

  describe "abs" $ do
    it "abs('2.1')    ==>  '2.1'" $
      op1 Op.abs "2.1" `shouldBe` "2.1"
    it "abs('-100')   ==>  '100'" $
      op1 Op.abs "-100" `shouldBe` "100"
    it "abs('101.5')  ==>  '101.5'" $
      op1 Op.abs "101.5" `shouldBe` "101.5"
    it "abs('-101.5') ==>  '101.5'" $
      op1 Op.abs "-101.5" `shouldBe` "101.5"

  describe "compare" $ do
    it "compare('2.1', '3')     ==>  '-1'" $
      op2 compare' "2.1" "3" `shouldBe` "-1"
    it "compare('2.1', '2.1')   ==>  '0'" $
      op2 compare' "2.1" "2.1" `shouldBe` "0"
    it "compare('2.1', '2.10')  ==>  '0'" $
      op2 compare' "2.1" "2.10" `shouldBe` "0"
    it "compare('3', '2.1')     ==>  '1'" $
      op2 compare' "3" "2.1" `shouldBe` "1"
    it "compare('2.1', '-3')    ==>  '1'" $
      op2 compare' "2.1" "-3" `shouldBe` "1"
    it "compare('-3', '2.1')    ==>  '-1'" $
      op2 compare' "-3" "2.1" `shouldBe` "-1"

  describe "max" $ do
    it "max('3', '2')    ==>  '3'" $
      op2 Op.max "3" "2" `shouldBe` "3"
    it "max('-10', '3')  ==>  '3'" $
      op2 Op.max "-10" "3" `shouldBe` "3"
    it "max('1.0', '1')  ==>  '1'" $
      op2 Op.max "1.0" "1" `shouldBe` "1"
    it "max('7', 'NaN')  ==>  '7'" $
      op2 Op.max "7" "NaN" `shouldBe` "7"

  describe "min" $ do
    it "min('3', '2')    ==>  '2'" $
      op2 Op.min "3" "2" `shouldBe` "2"
    it "min('-10', '3')  ==>  '-10'" $
      op2 Op.min "-10" "3" `shouldBe` "-10"
    it "min('1.0', '1')  ==>  '1.0'" $
      op2 Op.min "1.0" "1" `shouldBe` "1.0"
    it "min('7', 'NaN')  ==>  '7'" $
      op2 Op.min "7" "NaN" `shouldBe` "7"

  describe "power" $ do
    it "power('2', '3')             ==>  '8'" $
      op2 Op.power "2" "3" `shouldBe` "8"
    it "power('-2', '3')            ==>  '-8'" $
      op2 Op.power "-2" "3" `shouldBe` "-8"
    it "power('2', '-3')            ==>  '0.125'" $
      op2 Op.power "2" "-3" `shouldBe` "0.125"
    it "power('1.7', '8')           ==>  '69.7575744'" $
      op2 Op.power "1.7" "8" `shouldBe` "69.7575744"
    it "power('10', '0.301029996')  ==>  '2.00000000'" $
      op2 Op.power "10" "0.301029996" `shouldBe` "2.00000000"
    it "power('Infinity', '-1')     ==>  '0'" $
      op2 Op.power "Infinity" "-1" `shouldBe` "0"
    it "power('Infinity', '0')      ==>  '1'" $
      op2 Op.power "Infinity" "0" `shouldBe` "1"
    it "power('Infinity', '1')      ==>  'Infinity'" $
      op2 Op.power "Infinity" "1" `shouldBe` "Infinity"
    it "power('-Infinity', '-1')    ==>  '-0'" $
      op2 Op.power "-Infinity" "-1" `shouldBe` "-0"
    it "power('-Infinity', '0')     ==>  '1'" $
      op2 Op.power "-Infinity" "0" `shouldBe` "1"
    it "power('-Infinity', '1')     ==>  '-Infinity'" $
      op2 Op.power "-Infinity" "1" `shouldBe` "-Infinity"
    it "power('-Infinity', '2')     ==>  'Infinity'" $
      op2 Op.power "-Infinity" "2" `shouldBe` "Infinity"
    it "power('0', '0')             ==>  'NaN'" $
      op2 Op.power "0" "0" `shouldBe` "NaN"

  describe "quantize" $ do
    it "quantize('2.17', '0.001')        ==>  '2.170'" $
      op2 Op.quantize "2.17" "0.001" `shouldBe` "2.170"
    it "quantize('2.17', '0.01')         ==>  '2.17'" $
      op2 Op.quantize "2.17" "0.01" `shouldBe` "2.17"
    it "quantize('2.17', '0.1')          ==>  '2.2'" $
      op2 Op.quantize "2.17" "0.1" `shouldBe` "2.2"
    it "quantize('2.17', '1e+0')         ==>  '2'" $
      op2 Op.quantize "2.17" "1e+0" `shouldBe` "2"
    it "quantize('2.17', '1e+1')         ==>  '0E+1'" $
      op2 Op.quantize "2.17" "1e+1" `shouldBe` "0E+1"
    it "quantize('-Inf'  'Infinity')     ==>  '-Infinity'" $
      op2 Op.quantize "-Inf" "Infinity" `shouldBe` "-Infinity"
    it "quantize('2',    'Infinity')     ==>  'NaN'" $
      op2 Op.quantize "2" "Infinity" `shouldBe` "NaN"
    it "quantize('-0.1', '1'  )          ==>  '-0'" $
      op2 Op.quantize "-0.1" "1" `shouldBe` "-0"
    it "quantize('-0',   '1e+5')         ==>  '-0E+5'" $
      op2 Op.quantize "-0" "1e+5" `shouldBe` "-0E+5"
    it "quantize('+35236450.6', '1e-2')  ==>  'NaN'" $
      op2 Op.quantize "+35236450.6" "1e-2" `shouldBe` "NaN"
    it "quantize('-35236450.6', '1e-2')  ==>  'NaN'" $
      op2 Op.quantize "-35236450.6" "1e-2" `shouldBe` "NaN"
    it "quantize('217',  '1e-1')         ==>  '217.0'" $
      op2 Op.quantize "217" "1e-1" `shouldBe` "217.0"
    it "quantize('217',  '1e+0')         ==>  '217'" $
      op2 Op.quantize "217" "1e+0" `shouldBe` "217"
    it "quantize('217',  '1e+1')         ==>  '2.2E+2'" $
      op2 Op.quantize "217" "1e+1" `shouldBe` "2.2E+2"
    it "quantize('217',  '1e+2')         ==>  '2E+2'" $
      op2 Op.quantize "217" "1e+2" `shouldBe` "2E+2"

  describe "reduce" $ do
    it "reduce('2.1')     ==>  '2.1'" $
      op1 Op.reduce "2.1" `shouldBe` "2.1"
    it "reduce('-2.0')    ==>  '-2'" $
      op1 Op.reduce "-2.0" `shouldBe` "-2"
    it "reduce('1.200')   ==>  '1.2'" $
      op1 Op.reduce "1.200" `shouldBe` "1.2"
    it "reduce('-120')    ==>  '-1.2E+2'" $
      op1 Op.reduce "-120" `shouldBe` "-1.2E+2"
    it "reduce('120.00')  ==>  '1.2E+2'" $
      op1 Op.reduce "120.00" `shouldBe` "1.2E+2"
    it "reduce('0.00')    ==>  '0'" $
      op1 Op.reduce "0.00" `shouldBe` "0"

  describe "roundToIntegralExact" $ do
    it "round-to-integral-exact('2.1')      ==>  '2'" $
      op1 Op.roundToIntegralExact "2.1" `shouldBe` "2"
    it "round-to-integral-exact('100')      ==>  '100'" $
      op1 Op.roundToIntegralExact "100" `shouldBe` "100"
    it "round-to-integral-exact('100.0')    ==>  '100'" $
      op1 Op.roundToIntegralExact "100.0" `shouldBe` "100"
    it "round-to-integral-exact('101.5')    ==>  '102'" $
      op1 Op.roundToIntegralExact "101.5" `shouldBe` "102"
    it "round-to-integral-exact('-101.5')   ==>  '-102'" $
      op1 Op.roundToIntegralExact "-101.5" `shouldBe` "-102"
    it "round-to-integral-exact('10E+5')    ==>  '1.0E+6'" $
      op1 Op.roundToIntegralExact "10E+5" `shouldBe` "1.0E+6"
    it "round-to-integral-exact('7.89E+77') ==>  '7.89E+77'" $
      op1 Op.roundToIntegralExact "7.89E+77" `shouldBe` "7.89E+77"
    it "round-to-integral-exact('-Inf')     ==>  '-Infinity'" $
      op1 Op.roundToIntegralExact "-Inf" `shouldBe` "-Infinity"

  describe "squareRoot" $ do
    it "square-root('0')     ==> '0'" $
      op1 Op.squareRoot "0" `shouldBe` "0"
    it "square-root('-0')    ==> '-0'" $
      op1 Op.squareRoot "-0" `shouldBe` "-0"
    -- The following example is a corrected version of that found in the
    -- specification; confirmed with Mike Cowlishaw on 2016-08-02.
    it "square-root('0.39')  ==> '0.624499800'" $
      op1 Op.squareRoot "0.39" `shouldBe` "0.624499800"
    it "square-root('100')   ==> '10'" $
      op1 Op.squareRoot "100" `shouldBe` "10"
    it "square-root('1')     ==> '1'" $
      op1 Op.squareRoot "1" `shouldBe` "1"
    it "square-root('1.0')   ==> '1.0'" $
      op1 Op.squareRoot "1.0" `shouldBe` "1.0"
    it "square-root('1.00')  ==> '1.0'" $
      op1 Op.squareRoot "1.00" `shouldBe` "1.0"
    it "square-root('7')     ==> '2.64575131'" $
      op1 Op.squareRoot "7" `shouldBe` "2.64575131"
    it "square-root('10')    ==> '3.16227766'" $
      op1 Op.squareRoot "10" `shouldBe` "3.16227766"

  describe "and" $ do
    it "and('0', '0')        ==>  '0'" $
      op2 Op.and "0" "0" `shouldBe` "0"
    it "and('0', '1')        ==>  '0'" $
      op2 Op.and "0" "1" `shouldBe` "0"
    it "and('1', '0')        ==>  '0'" $
      op2 Op.and "1" "0" `shouldBe` "0"
    it "and('1', '1')        ==>  '1'" $
      op2 Op.and "1" "1" `shouldBe` "1"
    it "and('1100', '1010')  ==>  '1000'" $
      op2 Op.and "1100" "1010" `shouldBe` "1000"
    it "and('1111', '10')    ==>  '10'" $
      op2 Op.and "1111" "10" `shouldBe` "10"

  describe "or" $ do
    it "or('0', '0')        ==>  '0'" $
      op2 Op.or "0" "0" `shouldBe` "0"
    it "or('0', '1')        ==>  '1'" $
      op2 Op.or "0" "1" `shouldBe` "1"
    it "or('1', '0')        ==>  '1'" $
      op2 Op.or "1" "0" `shouldBe` "1"
    it "or('1', '1')        ==>  '1'" $
      op2 Op.or "1" "1" `shouldBe` "1"
    it "or('1100', '1010')  ==>  '1110'" $
      op2 Op.or "1100" "1010" `shouldBe` "1110"
    it "or('1110', '10')    ==>  '1110'" $
      op2 Op.or "1110" "10" `shouldBe` "1110"

  describe "xor" $ do
    it "xor('0', '0')        ==>  '0'" $
      op2 Op.xor "0" "0" `shouldBe` "0"
    it "xor('0', '1')        ==>  '1'" $
      op2 Op.xor "0" "1" `shouldBe` "1"
    it "xor('1', '0')        ==>  '1'" $
      op2 Op.xor "1" "0" `shouldBe` "1"
    it "xor('1', '1')        ==>  '0'" $
      op2 Op.xor "1" "1" `shouldBe` "0"
    it "xor('1100', '1010')  ==>  '110'" $
      op2 Op.xor "1100" "1010" `shouldBe` "110"
    it "xor('1111', '10')    ==>  '1101'" $
      op2 Op.xor "1111" "10" `shouldBe` "1101"

  describe "invert" $ do
    it "invert('0')          ==>  '111111111'" $
      op1 Op.invert "0" `shouldBe` "111111111"
    it "invert('1')          ==>  '111111110'" $
      op1 Op.invert "1" `shouldBe` "111111110"
    it "invert('111111111')  ==>  '0'" $
      op1 Op.invert "111111111" `shouldBe` "0"
    it "invert('101010101')  ==>  '10101010'" $
      op1 Op.invert "101010101" `shouldBe` "10101010"

  describe "canonical" $
    it "canonical('2.50')  ==>  '2.50'" $
      op1 Op.canonical "2.50" `shouldBe` "2.50"

  describe "class'" $ do
    it "class('Infinity')   ==>  \"+Infinity\"" $
      op1 Op.class' "Infinity" `shouldBe` "+Infinity"
    it "class('1E-10')      ==>  \"+Normal\"" $
      op1 Op.class' "1E-10" `shouldBe` "+Normal"
    it "class('2.50')       ==>  \"+Normal\"" $
      op1 Op.class' "2.50" `shouldBe` "+Normal"
    it "class('0.1E-999')   ==>  \"+Subnormal\"" $
      op1 Op.class' "0.1E-999" `shouldBe` "+Subnormal"
    it "class('0')          ==>  \"+Zero\"" $
      op1 Op.class' "0" `shouldBe` "+Zero"
    it "class('-0')         ==>  \"-Zero\"" $
      op1 Op.class' "-0" `shouldBe` "-Zero"
    it "class('-0.1E-999')  ==>  \"-Subnormal\"" $
      op1 Op.class' "-0.1E-999" `shouldBe` "-Subnormal"
    it "class('-1E-10')     ==>  \"-Normal\"" $
      op1 Op.class' "-1E-10" `shouldBe` "-Normal"
    it "class('-2.50')      ==>  \"-Normal\"" $
      op1 Op.class' "-2.50" `shouldBe` "-Normal"
    it "class('-Infinity')  ==>  \"-Infinity\"" $
      op1 Op.class' "-Infinity" `shouldBe` "-Infinity"
    it "class('NaN')        ==>  \"NaN\"" $
      op1 Op.class' "NaN" `shouldBe` "NaN"
    it "class('-NaN')       ==>  \"NaN\"" $
      op1 Op.class' "-NaN" `shouldBe` "NaN"
    it "class('sNaN')       ==>  \"sNaN\"" $
      op1 Op.class' "sNaN" `shouldBe` "sNaN"

  describe "compareTotal" $ do
    it "compare-total('12.73', '127.9')   ==>  '-1'" $
      op2 compareTotal' "12.73" "127.9" `shouldBe` "-1"
    it "compare-total('-127',  '12')      ==>  '-1'" $
      op2 compareTotal' "-127" "12" `shouldBe` "-1"
    it "compare-total('12.30', '12.3')    ==>  '-1'" $
      op2 compareTotal' "12.30" "12.3" `shouldBe` "-1"
    it "compare-total('12.30', '12.30')   ==>  '0'" $
      op2 compareTotal' "12.30" "12.30" `shouldBe` "0"
    it "compare-total('12.3',  '12.300')  ==>  '1'" $
      op2 compareTotal' "12.3" "12.300" `shouldBe` "1"
    it "compare-total('12.3',  'NaN')     ==>  '-1'" $
      op2 compareTotal' "12.3" "NaN" `shouldBe` "-1"

  describe "copy" $ do
    it "copy('2.1')    ==>  '2.1'" $
      op1 Op.copy "2.1" `shouldBe` "2.1"
    it "copy('-1.00')  ==>  '-1.00'" $
      op1 Op.copy "-1.00" `shouldBe` "-1.00"

  describe "copyAbs" $ do
    it "copy-abs('2.1')   ==>  '2.1'" $
      op1 Op.copyAbs "2.1" `shouldBe` "2.1"
    it "copy-abs('-100')  ==>  '100'" $
      op1 Op.copyAbs "-100" `shouldBe` "100"

  describe "copyNegate" $ do
    it "copy-negate('101.5')   ==>  '-101.5'" $
      op1 Op.copyNegate "101.5" `shouldBe` "-101.5"
    it "copy-negate('-101.5')  ==>  '101.5'" $
      op1 Op.copyNegate "-101.5" `shouldBe` "101.5"

  describe "copySign" $ do
    it "copy-sign( '1.50',  '7.33')  ==>  '1.50'" $
      op2 Op.copySign  "1.50"  "7.33" `shouldBe` "1.50"
    it "copy-sign('-1.50',  '7.33')  ==>  '1.50'" $
      op2 Op.copySign "-1.50"  "7.33" `shouldBe` "1.50"
    it "copy-sign( '1.50', '-7.33')  ==>  '-1.50'" $
      op2 Op.copySign  "1.50" "-7.33" `shouldBe` "-1.50"
    it "copy-sign('-1.50', '-7.33')  ==>  '-1.50'" $
      op2 Op.copySign "-1.50" "-7.33" `shouldBe` "-1.50"

  describe "isCanonical" $
    it "is-canonical('2.50')  ==>  '1'" $
      pred1 Op.isCanonical "2.50" `shouldBe` "1"

  describe "isFinite" $ do
    it "is-finite('2.50')  ==>  '1'" $
      pred1 Op.isFinite "2.50" `shouldBe` "1"
    it "is-finite('-0.3')  ==>  '1'" $
      pred1 Op.isFinite "-0.3" `shouldBe` "1"
    it "is-finite('0')     ==>  '1'" $
      pred1 Op.isFinite "0" `shouldBe` "1"
    it "is-finite('Inf')   ==>  '0'" $
      pred1 Op.isFinite "Inf" `shouldBe` "0"
    it "is-finite('NaN')   ==>  '0'" $
      pred1 Op.isFinite "NaN" `shouldBe` "0"

  describe "isInfinite" $ do
    it "is-infinite('2.50')  ==>  '0'" $
      pred1 Op.isInfinite "2.50" `shouldBe` "0"
    it "is-infinite('-Inf')  ==>  '1'" $
      pred1 Op.isInfinite "-Inf" `shouldBe` "1"
    it "is-infinite('NaN')   ==>  '0'" $
      pred1 Op.isInfinite "NaN" `shouldBe` "0"

  describe "isNaN" $ do
    it "is-NaN('2.50')   ==>  '0'" $
      pred1 Op.isNaN "2.50" `shouldBe` "0"
    it "is-NaN('NaN')    ==>  '1'" $
      pred1 Op.isNaN "NaN" `shouldBe` "1"
    it "is-NaN('-sNaN')  ==>  '1'" $
      pred1 Op.isNaN "-sNaN" `shouldBe` "1"

  describe "isNormal" $ do
    it "is-normal('2.50')      ==>  '1'" $
      pred1 Op.isNormal "2.50" `shouldBe` "1"
    it "is-normal('0.1E-999')  ==>  '0'" $
      pred1 Op.isNormal "0.1E-999" `shouldBe` "0"
    it "is-normal('0.00')      ==>  '0'" $
      pred1 Op.isNormal "0.00" `shouldBe` "0"
    it "is-normal('-Inf')      ==>  '0'" $
      pred1 Op.isNormal "-Inf" `shouldBe` "0"
    it "is-normal('NaN')       ==>  '0'" $
      pred1 Op.isNormal "NaN" `shouldBe` "0"

  describe "isQNaN" $ do
    it "is-qNaN('2.50')  ==>  '0'" $
      pred1 Op.isQNaN "2.50" `shouldBe` "0"
    it "is-qNaN('NaN')   ==>  '1'" $
      pred1 Op.isQNaN "NaN" `shouldBe` "1"
    it "is-qNaN('sNaN')  ==>  '0'" $
      pred1 Op.isQNaN "sNaN" `shouldBe` "0"

  describe "isSigned" $ do
    it "is-signed('2.50')  ==>  '0'" $
      pred1 Op.isSigned "2.50" `shouldBe` "0"
    it "is-signed('-12')   ==>  '1'" $
      pred1 Op.isSigned "-12" `shouldBe` "1"
    it "is-signed('-0')    ==>  '1'" $
      pred1 Op.isSigned "-0" `shouldBe` "1"

  describe "isSNaN" $ do
    it "is-sNaN('2.50')  ==>  '0'" $
      pred1 Op.isSNaN "2.50" `shouldBe` "0"
    it "is-sNaN('NaN')   ==>  '0'" $
      pred1 Op.isSNaN "NaN" `shouldBe` "0"
    it "is-sNaN('sNaN')  ==>  '1'" $
      pred1 Op.isSNaN "sNaN" `shouldBe` "1"

  describe "isSubnormal" $ do
    it "is-subnormal('2.50')      ==>  '0'" $
      pred1 Op.isSubnormal "2.50" `shouldBe` "0"
    it "is-subnormal('0.1E-999')  ==>  '1'" $
      pred1 Op.isSubnormal "0.1E-999" `shouldBe` "1"
    it "is-subnormal('0.00')      ==>  '0'" $
      pred1 Op.isSubnormal "0.00" `shouldBe` "0"
    it "is-subnormal('-Inf')      ==>  '0'" $
      pred1 Op.isSubnormal "-Inf" `shouldBe` "0"
    it "is-subnormal('NaN')       ==>  '0'" $
      pred1 Op.isSubnormal "NaN" `shouldBe` "0"

  describe "isZero" $ do
    it "is-zero('0')      ==>  '1'" $
      pred1 Op.isZero "0" `shouldBe` "1"
    it "is-zero('2.50')   ==>  '0'" $
      pred1 Op.isZero "2.50" `shouldBe` "0"
    it "is-zero('-0E+2')  ==>  '1'" $
      pred1 Op.isZero "-0E+2" `shouldBe` "1"

  describe "logb" $ do
    it "logb('250')   ==>  '2'" $
      op1 Op.logb "250" `shouldBe` "2"
    it "logb('2.50')  ==>  '0'" $
      op1 Op.logb "2.50" `shouldBe` "0"
    it "logb('0.03')  ==>  '-2'" $
      op1 Op.logb "0.03" `shouldBe` "-2"
    it "logb('0')     ==>  '-Infinity'" $
      op1 Op.logb "0" `shouldBe` "-Infinity"

  describe "scaleb" $ do
    it "scaleb('7.50', '-2')  ==>  '0.0750'" $
      op2 Op.scaleb "7.50" "-2" `shouldBe` "0.0750"
    it "scaleb('7.50', '0')   ==>  '7.50'" $
      op2 Op.scaleb "7.50" "0" `shouldBe` "7.50"
    it "scaleb('7.50', '3')   ==>  '7.50E+3'" $
      op2 Op.scaleb "7.50" "3" `shouldBe` "7.50E+3"

  describe "radix" $
    it "radix()  ==>  '10'" $
      op0 Op.radix `shouldBe` "10"

  describe "sameQuantum" $ do
    it "samequantum('2.17', '0.001')  ==>  '0'" $
      pred2 Op.sameQuantum "2.17" "0.001" `shouldBe` "0"
    it "samequantum('2.17', '0.01')   ==>  '1'" $
      pred2 Op.sameQuantum "2.17" "0.01" `shouldBe` "1"
    it "samequantum('2.17', '0.1')    ==>  '0'" $
      pred2 Op.sameQuantum "2.17" "0.1" `shouldBe` "0"
    it "samequantum('2.17', '1')      ==>  '0'" $
      pred2 Op.sameQuantum "2.17" "1" `shouldBe` "0"
    it "samequantum('Inf', '-Inf')    ==>  '1'" $
      pred2 Op.sameQuantum "Inf" "-Inf" `shouldBe` "1"
    it "samequantum('NaN', 'NaN')     ==>  '1'" $
      pred2 Op.sameQuantum "NaN" "NaN" `shouldBe` "1"

  describe "shift" $ do
    it "shift('34', '8')          ==>  '400000000'" $
      op2 Op.shift "34" "8" `shouldBe` "400000000"
    it "shift('12', '9')          ==>  '0'" $
      op2 Op.shift "12" "9" `shouldBe` "0"
    it "shift('123456789', '-2')  ==>  '1234567'" $
      op2 Op.shift "123456789" "-2" `shouldBe` "1234567"
    it "shift('123456789', '0')   ==>  '123456789'" $
      op2 Op.shift "123456789" "0" `shouldBe` "123456789"
    it "shift('123456789', '+2')  ==>  '345678900'" $
      op2 Op.shift "123456789" "+2" `shouldBe` "345678900"

  describe "rotate" $ do
    it "rotate('34', '8')          ==>  '400000003'" $
      op2 Op.rotate "34" "8" `shouldBe` "400000003"
    it "rotate('12', '9')          ==>  '12'" $
      op2 Op.rotate "12" "9" `shouldBe` "12"
    it "rotate('123456789', '-2')  ==>  '891234567'" $
      op2 Op.rotate "123456789" "-2" `shouldBe` "891234567"
    it "rotate('123456789', '0')   ==>  '123456789'" $
      op2 Op.rotate "123456789" "0" `shouldBe` "123456789"
    it "rotate('123456789', '+2')  ==>  '345678912'" $
      op2 Op.rotate "123456789" "+2" `shouldBe` "345678912"

exceptionError :: Exception p r -> a
exceptionError = error . show . exceptionSignal

type BasicArith = Arith P9 RoundHalfUp

pred1 :: (BasicDecimal -> BasicArith Bool) -> String -> String
pred1 op x = either exceptionError (show . fromBool) $
  evalArith arith newContext
  where arith = op (read x)

pred2 :: (BasicDecimal -> BasicDecimal -> BasicArith Bool) -> String -> String
      -> String
pred2 op x y = either exceptionError (show . fromBool) $
  evalArith arith newContext
  where arith = op (read x) (read y)

op0 :: Show a => BasicArith a -> String
op0 op = either exceptionError show $ evalArith op newContext

op1 :: Show a => (BasicDecimal -> BasicArith a) -> String -> String
op1 op x = either exceptionError show $ evalArith arith newContext
  where arith = op (read x)

op2 :: Show a => (BasicDecimal -> BasicDecimal -> BasicArith a) -> String
    -> String -> String
op2 op x y = either exceptionError show $ evalArith arith newContext
  where arith = read x `op` read y

op3 :: Show a => (BasicDecimal -> BasicDecimal -> BasicDecimal -> BasicArith a)
    -> String -> String -> String -> String
op3 op x y z = either exceptionError show $ evalArith arith newContext
  where arith = op (read x) (read y) (read z)

compare' :: Decimal a b -> Decimal c d -> Arith p r (Decimal p r)
compare' x y = either id fromOrdering <$> Op.compare x y

compareTotal' :: Decimal a b -> Decimal c d -> Arith p r (Decimal p r)
compareTotal' x y = fromOrdering <$> Op.compareTotal x y
