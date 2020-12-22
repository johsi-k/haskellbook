import Foundations
import Test.Hspec

main :: IO ()
main = hspec $ do

  let parseA = pCharWrapped 'A'
  let parseB = pCharWrapped 'B'

  describe "Encapsulating the parsing function" $ do
    it "can parse \"ABC\"" $ do
      let r = run parseA "ABC"
      print r
      r `shouldBe` Success ('A', "BC")

    it "can parse \"ZBC\"" $ do
      let r = run parseA "ZBC"
      print r
      r `shouldBe` Failure "Expecting 'A'. Got 'Z'"

  let parseAThenB = parseA `andThen` parseB

  describe "andThen Parsing" $ do
    it "can parse \"ABC\"" $ do
      let r = run parseAThenB "ABC"
      print r
      r `shouldBe` Success (('A', 'B'), "C")

    it "can parse \"ZBC\"" $ do
      let r = run parseAThenB "ZBC"
      print r
      r `shouldBe` Failure "Expecting 'A'. Got 'Z'"

    it "can parse \"AZC\"" $ do
      let r = run parseAThenB "AZC"
      print r
      r `shouldBe` Failure "Expecting 'B'. Got 'Z'"

  let parseAOrElseB = parseA `orElse` parseB

  describe "orElse Parsing" $ do
    it "can parse \"AZZ\"" $ do
      let r = run parseAOrElseB "AZZ"
      print r
      r `shouldBe` Success ('A', "ZZ")

    it "can parse \"BZZ\"" $ do
      let r = run parseAOrElseB "BZZ"
      print r
      r `shouldBe` Success ('B', "ZZ")

    it "can parse \"CZZ\"" $ do
      let r = run parseAOrElseB "CZZ"
      print r
      r `shouldBe` Failure "Expecting 'B'. Got 'C'"

  let parseC = pCharWrapped 'C'
  let bOrElseC = parseB `orElse` parseC
  let aAndThenBorC = parseA `andThen` bOrElseC

  describe "andThenOr Parsing" $ do
    it "can parse \"ABZ\"" $ do
      let r = run aAndThenBorC "ABZ"
      print r
      r `shouldBe` Success (('A', 'B'), "Z")

    it "can parse \"ACZ\"" $ do
      let r = run aAndThenBorC "ACZ"
      print r
      r `shouldBe` Success (('A', 'C'), "Z")

    it "cannot parse \"QBZ\"" $ do
      let r = run aAndThenBorC "QBZ"
      print r
      r `shouldBe` Failure "Expecting 'A'. Got 'Q'"

    it "cannot parse \"AQZ\"" $ do
      let r = run aAndThenBorC "AQZ"
      print r
      r `shouldBe` Failure "Expecting 'C'. Got 'Q'"

  let parseLowercase = anyOf ['a'..'z']
  let parseDigit = anyOf ['0'..'9']

  describe "Lowercase Parsing" $ do
    it "can parse \"aBC\"" $ do
      let r = run parseLowercase "aBC"
      print r
      r `shouldBe` Success ('a', "BC")

    it "cannot parse \"ABC\"" $ do
      let r = run parseLowercase "ABC"
      print r
      -- r `shouldBe` Failure "Expecting 'z'. Got 'A'"
      r `shouldBe` Failure "ABC"

  describe "Digit Parsing" $ do
    it "can parse \"1ABC\"" $ do
      let r = run parseDigit "1ABC"
      print r
      r `shouldBe` Success ('1', "ABC")

    it "can parse \"9ABC\"" $ do
      let r = run parseDigit "9ABC"
      print r
      r `shouldBe` Success ('9', "ABC")

    it "cannot parse \"|ABC\"" $ do
      let r = run parseDigit "|ABC"
      print r
      -- r `shouldBe` Failure "Expecting '9'. Got '|'"
      r `shouldBe` Failure "|ABC"
