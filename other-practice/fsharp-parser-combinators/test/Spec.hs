import Test.Hspec
import Foundations
import BuildingALib

main :: IO ()
main = hspec $ do

  let parseA = pChar 'A'
  let parseB = pChar 'B'

  describe "Encapsulating the parsing function" $ do
    it "can parse \"ABC\"" $ do
      let res = run parseA "ABC"
      print res
      res `shouldBe` Success ('A', "BC")

    it "can parse \"ZBC\"" $ do
      let res = run parseA "ZBC"
      print res
      res `shouldBe` Failure "Expecting 'A'. Got 'Z'"

  let parseAThenB = parseA `andThen` parseB
  describe "andThen Parsing" $ do
    it "can parse \"ABC\"" $ do
      let res = run parseAThenB "ABC"
      print res
      res `shouldBe` Success (('A', 'B'), "C")

    it "can parse \"ZBC\"" $ do
      let res = run parseAThenB "ZBC"
      print res
      res `shouldBe` Failure "Expecting 'A'. Got 'Z'"

    it "can parse \"AZC\"" $ do
      let res = run parseAThenB "AZC"
      print res
      res `shouldBe` Failure "Expecting 'B'. Got 'Z'"

  let parseAOrElseB = parseA `orElse` parseB

  describe "orElse Parsing" $ do
    it "can parse \"AZZ\"" $ do
      let res = run parseAOrElseB "AZZ"
      print res
      res `shouldBe` Success ('A', "ZZ")

    it "can parse \"BZZ\"" $ do
      let res = run parseAOrElseB "BZZ"
      print res
      res `shouldBe` Success ('B', "ZZ")

    it "can parse \"CZZ\"" $ do
      let res = run parseAOrElseB "CZZ"
      print res
      res `shouldBe` Failure "Expecting 'A'. Got 'C'; Expecting 'B'. Got 'C'"

  let parseC = pChar 'C'
  let bOrElseC = parseB `orElse` parseC
  let aAndThenBorC = parseA `andThen` bOrElseC

  describe "andThenOr Parsing" $ do
    it "can parse \"ABZ\"" $ do
      let res = run aAndThenBorC "ABZ"
      print res
      res `shouldBe` Success (('A', 'B'), "Z")

    it "can parse \"ACZ\"" $ do
      let res = run aAndThenBorC "ACZ"
      print res
      res `shouldBe` Success (('A', 'C'), "Z")

    it "cannot parse \"QBZ\"" $ do
      let res = run aAndThenBorC "QBZ"
      print res
      res `shouldBe` Failure "Expecting 'A'. Got 'Q'"

    it "cannot parse \"AQZ\"" $ do
      let res = run aAndThenBorC "AQZ"
      print res
      res `shouldBe` Failure "Expecting 'B'. Got 'Q'; Expecting 'C'. Got 'Q'"

  let parseLowercase = anyOf ['a'..'z']

  describe "Lowercase Parsing" $ do
    it "can parse \"aBC\"" $ do
      let res = run parseLowercase "aBC"
      print res
      res `shouldBe` Success ('a', "BC")

    it "cannot parse \"ABC\"" $ do
      let res = run parseLowercase "ABC"
      print res
      res `shouldBe` Failure "Expecting 'a'. Got 'A'; Expecting 'b'. Got 'A'; Expecting 'c'. Got 'A'; Expecting 'd'. Got 'A'; Expecting 'e'. Got 'A'; Expecting 'f'. Got 'A'; Expecting 'g'. Got 'A'; Expecting 'h'. Got 'A'; Expecting 'i'. Got 'A'; Expecting 'j'. Got 'A'; Expecting 'k'. Got 'A'; Expecting 'l'. Got 'A'; Expecting 'm'. Got 'A'; Expecting 'n'. Got 'A'; Expecting 'o'. Got 'A'; Expecting 'p'. Got 'A'; Expecting 'q'. Got 'A'; Expecting 'r'. Got 'A'; Expecting 's'. Got 'A'; Expecting 't'. Got 'A'; Expecting 'u'. Got 'A'; Expecting 'v'. Got 'A'; Expecting 'w'. Got 'A'; Expecting 'x'. Got 'A'; Expecting 'y'. Got 'A'; Expecting 'z'. Got 'A'; ABC"

  describe "Digit Parsing" $ do
    it "can parse \"1ABC\"" $ do
      let res = run parseDigit "1ABC"
      print res
      res `shouldBe` Success ('1', "ABC")

    it "can parse \"9ABC\"" $ do
      let res = run parseDigit "9ABC"
      print res
      res `shouldBe` Success ('9', "ABC")

    it "cannot parse \"|ABC\"" $ do
      let res = run parseDigit "|ABC"
      print res
      res `shouldBe` Failure "Expecting '0'. Got '|'; Expecting '1'. Got '|'; Expecting '2'. Got '|'; Expecting '3'. Got '|'; Expecting '4'. Got '|'; Expecting '5'. Got '|'; Expecting '6'. Got '|'; Expecting '7'. Got '|'; Expecting '8'. Got '|'; Expecting '9'. Got '|'; |ABC"

  let parseABC = pString "ABC"
  describe "String Parsing" $ do
    it "can parse \"ABCDE\"" $ do
      let res = run parseABC "ABCDE"
      print res
      res `shouldBe` Success ("ABC","DE")

    it "cannot parse \"A|CDE\"" $ do
      let res = run parseABC "A|CDE"
      print res
      res `shouldBe` Failure "Expecting 'B'. Got '|'"

    it "cannot parse \"AB|DE\"" $ do
      let res = run parseABC "AB|DE"
      print res
      res `shouldBe` Failure "Expecting 'C'. Got '|'"

  let manyA = many (pChar 'A')
  describe "many Parsing with pChar" $ do
    it "can parse \"ABCD\"" $ do
      let res = run manyA "ABCD"
      print res
      res `shouldBe` Success ("A", "BCD")

    it "can parse \"AACD\"" $ do
      let res = run manyA "AACD"
      print res
      res `shouldBe` Success ("AA", "CD")

    it "can parse \"AAAD\"" $ do
      let res = run manyA "AAAD"
      print res
      res `shouldBe` Success ("AAA", "D")

    -- case with no matches
    it "can parse \"|BCD\"" $ do
      let res = run manyA "|BCD"
      print res
      res `shouldBe` Success ("", "|BCD")

  let manyAB = many (pString "AB")

    -- matching whitespace
  let whitespaceChar = anyOf " \t\n"
  let whitespace = many whitespaceChar

  describe "many Parsing with pString" $ do
    it "can parse \"ABCD\"" $ do
      let res = run manyAB "ABCD"
      print res
      res `shouldBe` Success (["AB"], "CD")

    it "can parse \"ABABCD\"" $ do
      let res = run manyAB "ABABCD"
      print res
      res `shouldBe` Success (["AB", "AB"], "CD")

    it "can parse \"ZCD\"" $ do
      let res = run manyAB "ZCD"
      print res
      res`shouldBe` Success ([], "ZCD")

    it "can parse \"AZCD\"" $ do
      let res = run manyAB "AZCD"
      print res
      res`shouldBe` Success ([], "AZCD")

    it "can parse \"ABC\"" $ do
      let res = run whitespace "ABC"
      print res
      res `shouldBe` Success ([], "ABC")

    it "can parse \" ABC\"" $ do
      let res = run whitespace " ABC"
      print res
      res `shouldBe` Success (" ", "ABC")

    it "can parse \"\\tABC\"" $ do
      let res = run whitespace "\tABC"
      print res
      res `shouldBe` Success ("\t", "ABC")

    let digit = anyOf ['0'..'9']
    let digits = many1 digit

    it "can parse \"1ABC\"" $ do
      let res = run digits "1ABC"
      print res
      res `shouldBe` Success ("1", "ABC")

    it "can parse \"12BC\"" $ do
      let res = run digits "12BC"
      print res
      res `shouldBe` Success ("12", "BC")

    it "can parse \"123C\"" $ do
      let res = run digits "123C"
      print res
      res `shouldBe` Success ("123", "C")

    it "can parse \"1234\"" $ do
      let res = run digits "1234"
      print res
      res `shouldBe` Success ("1234", "")

    it "cannot parse \"ABC\"" $ do
      let res = run digits "ABC"
      print res
      res `shouldBe` Failure "Expecting '0'. Got 'A'; Expecting '1'. Got 'A'; Expecting '2'. Got 'A'; Expecting '3'. Got 'A'; Expecting '4'. Got 'A'; Expecting '5'. Got 'A'; Expecting '6'. Got 'A'; Expecting '7'. Got 'A'; Expecting '8'. Got 'A'; Expecting '9'. Got 'A'; ABC"

  describe "Integer Parsing" $ do
    it "can parse \"1ABC\"" $ do
      let res = run pInt' "1ABC"
      print res
      res `shouldBe` Success (1, "ABC")

    it "can parse \"12BC\"" $ do
      let res = run pInt' "12BC"
      print res
      res `shouldBe` Success (12, "BC")

    it "can parse \"123C\"" $ do
      let res = run pInt' "123C"
      print res
      res `shouldBe` Success (123, "C")

    it "can parse \"1234\"" $ do
      let res = run pInt' "1234"
      print res
      res `shouldBe` Success (1234, "")

    it "cannot parse \"ABC\"" $ do
      let res = run pInt' "ABC"
      print res
      res `shouldBe` Failure "Expecting '0'. Got 'A'; Expecting '1'. Got 'A'; Expecting '2'. Got 'A'; Expecting '3'. Got 'A'; Expecting '4'. Got 'A'; Expecting '5'. Got 'A'; Expecting '6'. Got 'A'; Expecting '7'. Got 'A'; Expecting '8'. Got 'A'; Expecting '9'. Got 'A'; ABC"

  let digit = anyOf ['0'..'9']
  let digitThenSemicolon =
        digit `andThen` opt (pChar ';')
  describe "opt Parsing" $ do
    it "can parse \"1;\"" $ do
      let res = run digitThenSemicolon "1;"
      print res
      res `shouldBe` Success (('1', Just ';'), "")

    it "can parse \"1\"" $ do
      let res = run digitThenSemicolon "1"
      print res
      res `shouldBe` Success (('1', Nothing), "")

  describe "Optional Negative Integer Parsing" $ do
    it "can parse \"-123C\"" $ do
      let res = run pInt "-123C"
      print res
      res `shouldBe` Success (-123, "C")

    it "can parse \"123C\"" $ do
      let res = run pInt "123C"
      print res
      res `shouldBe` Success (123, "C")

  let digitThenSemicolon' =
        digit `keepLeft` opt (pChar ';')

  let ab = pString "AB"
  let cd = pString "CD"
  let abcd =
        (ab `keepLeft` whitespace) `andThen` cd

  let pDoubleQuote = pChar '"'
  let quotedInteger =
        between pDoubleQuote pInt pDoubleQuote

  describe "Throwaway Parsing" $ do
    it "can parse \"1;\"" $ do
      let res = run digitThenSemicolon' "1;"
      print res
      res `shouldBe` Success ('1', "")

    it "can parse \"1\"" $ do
      let res = run digitThenSemicolon' "1"
      print res
      res `shouldBe` Success ('1', "")

    it "can parse \"AB \\t\\nCD\"" $ do
      let res = run abcd "AB \t\nCD"
      print res
      res `shouldBe` Success (("AB", "CD"), "")

    it "can parse \"\"1234\"\"" $ do
      let res = run quotedInteger "\"1234\""
      print res
      res `shouldBe` Success (1234, "")

    it "cannot parse \"1234\"" $ do
      let res = run quotedInteger "1234"
      print res
      res `shouldBe` Failure "Expecting '\"'. Got '1'"

  let comma = pChar ','
  let oneOrMoreDigitList = sepBy1 digit comma

  describe "sepBy1 Parsing" $ do
    it "can parse \"1;\"" $ do
      let res = run oneOrMoreDigitList "1;"
      print res
      res `shouldBe` Success ("1", ";")

    it "can parse \"1,2;\"" $ do
      let res = run oneOrMoreDigitList "1,2;"
      print res
      res `shouldBe` Success ("12", ";")

    it "can parse \"1,2,3;\"" $ do
      let res = run oneOrMoreDigitList "1,2,3;"
      print res
      res `shouldBe` Success ("123", ";")

    it "can parse \"Z;\"" $ do
      let res = run oneOrMoreDigitList "Z;"
      print res
      res `shouldBe` Failure "Expecting '0'. Got 'Z'; Expecting '1'. Got 'Z'; Expecting '2'. Got 'Z'; Expecting '3'. Got 'Z'; Expecting '4'. Got 'Z'; Expecting '5'. Got 'Z'; Expecting '6'. Got 'Z'; Expecting '7'. Got 'Z'; Expecting '8'. Got 'Z'; Expecting '9'. Got 'Z'; Z;"


  let zeroOrMoreDigitList = sepBy digit comma

  describe "sepBy Parsing" $ do
    it "can parse \"1;\"" $ do
      let res = run zeroOrMoreDigitList "1;"
      print res
      res `shouldBe` Success ("1", ";")

    it "can parse \"1,2;\"" $ do
      let res = run zeroOrMoreDigitList "1,2;"
      print res
      res `shouldBe` Success ("12", ";")

    it "can parse \"1,2,3;\"" $ do
      let res = run zeroOrMoreDigitList "1,2,3;"
      print res
      res `shouldBe` Success ("123", ";")

    it "can parse \"Z;\"" $ do
      let res = run zeroOrMoreDigitList "Z;"
      print res
      res `shouldBe` Success ([], "Z;")
