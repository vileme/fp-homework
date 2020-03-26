import Block1Task1Spec
import Block1Task2Spec
import Block1Task3Spec
import Block4Task1Spec
import Block2Task2Spec
import Block3Task1Spec
import Block3Task2Spec
import Block5Task1Spec
import Block6Spec
import Test.Hspec (hspec)

main :: IO ()
main =
  hspec $ do
    Block1Task1Spec.nextDayTest
    Block1Task1Spec.afterDaysTest
    Block1Task1Spec.isWeekendTest
    Block1Task1Spec.daysToPartyTest
    Block1Task2Spec.fromIntTest
    Block1Task2Spec.addTest
    Block1Task2Spec.multiplyTest
    Block1Task2Spec.checkEqualityTest
    Block1Task2Spec.subtractionTest
    Block1Task3Spec.fromListToTreeTest
    Block1Task3Spec.isEmptyTest
    Block1Task3Spec.countElementsTest
    Block1Task3Spec.searchTest
    Block1Task3Spec.insertXTest
    Block1Task3Spec.deleteTest
    Block2Task2Spec.splitOnTest
    Block3Task1Spec.maybeConcatTest
    Block3Task2Spec.nonEmptySemigroup
    Block3Task2Spec.thisOrThatTest
    Block4Task1Spec.stringSumTest
    Block5Task1Spec.evalTest
    Block6Spec.eofTest
    Block6Spec.okTest
    Block6Spec.satisfyTest
    Block6Spec.elementTest
    Block6Spec.streamTest
    Block6Spec.parseBracketsTest
