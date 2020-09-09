-- Constructing values

data GuessWhat =
  Chickenbutt deriving (Eq, Show)

data Id a =
  MkId a deriving (Eq, Show)

data Product a b =
  Product a b deriving (Eq, Show)

data Sum a b =
    First a
  | Second b
  deriving (Eq, Show)

data RecordProduct a b =
  RecordProduct { pfirst :: a
                , psecond :: b }
                deriving (Eq, Show)

newtype NumCow =
  NumCow Int
  deriving (Eq, Show)

newtype NumPig =
  NumPig Int
  deriving (Eq, Show)

-- Farmhouse and Farmhouse' are equivalent
data Farmhouse =
  Farmhouse NumCow NumPig
  deriving (Eq, Show)

type Farmhouse' = Product NumCow NumPig


-- For an example with 3 values in the product instead of 2, we can take advantage of the fact that Product takes 2 args, one of which can be another Product of values
newtype NumSheep =
  NumSheep Int
  deriving (Eq, Show)

data BigFarmhouse =
  BigFarmhouse NumCow NumPig NumSheep
  deriving (Eq, Show)

type BigFarmhouse' =
  Product NumCow (Product NumCow NumPig)


-- We can use a similar trick with Sum
type Name = String
type Age = Int
type LovesMud = Bool

type PoundsOfWool = Int

data CowInfo =
  CowInfo Name Age
  deriving (Eq, Show)

data PigInfo =
  PigInfo Name Age LovesMud
  deriving (Eq, Show)

data SheepInfo =
  SheepInfo Name Age PoundsOfWool
  deriving (Eq, Show)

data Animal =
    Cow CowInfo
  | Pig PigInfo
  | Sheep SheepInfo
  deriving (Eq, Show)

-- alternatively,
type Animal' =
  Sum CowInfo (Sum PigInfo SheepInfo)

-- using First and Second to pattern match on the data constructors of Sum
bess' = CowInfo "Bess" 4
bess = First bess' :: Animal'

e' = Second (SheepInfo "elmer" 5 5)
elmer = Second e' :: Animal'
-- elmer (without Animal') :: Sum a1 (Sum a2 SheepInfo)
-- which matches the type signature of Animal'
-- type Animal' = Sum CowInfo (Sum PigInfo SheepInfo)

-- making a mistake
elmo' = Second (SheepInfo "Elmo" 5 5)
elmo = First elmo' -- :: Animal'
-- translates to
-- elmo (without Animal') :: Sum (Sum a SheepInfo) b
-- which is not compatible with the type signature of Animal'
-- type Animal' = Sum CowInfo (Sum PigInfo SheepInfo)
-- Animal' expects CowInfo as its second arg but is given (Sum a SheepInfo) instead

sheep = SheepInfo "Baaaaa" 5 5
-- First (Second sheep) :: Sum (Sum a SheepInfo) b


-- Constructing values
trivialValue :: GuessWhat
trivialValue = Chickenbutt

-- MkId is defined above
idInt :: Id Integer
idInt = MkId 10

-- type synonyms for clarity
type Awesome = Bool
-- type Name = String -- defined above

person :: Product Name Awesome
person = Product "Simon" True

data Twitter =
  Twitter deriving (Eq, Show)

data AskFm =
  AskFm deriving (Eq, Show)

socialNetwork :: Sum Twitter AskFm
socialNetwork = First Twitter

-- The choice of data constructor (First or Second) is determined by the assertions in the type
type SN = Sum Twitter AskFm
twitter = Second Twitter -- :: SN
-- twitter :: Sum a Twitter
-- incompatible with type SN = Sum Twitter AskFm
-- SN expects AskFm as a second argument to Sum but gets Twitter instead

askfm = First AskFm -- :: Sum Twitter AskFm
-- askFM :: Sum AskFm b
-- incompatible with the type signature Sum Twitter AskFm
-- Sum Twitter AskFm expects Twitter as a first argument to Sum but gets AskFm instead

-- We could also assert ordering directly with a datatype like this:
data SocialNetwork =
    Twitter'
  | AskFm'
  deriving (Eq, Show)

-- Now with type synonyms
type TwitterStr = String
type AskFmStr = String

twitterStr :: Sum TwitterStr AskFmStr
twitterStr = First "Twitter"

askfmStr :: Sum TwitterStr AskFmStr
askfmStr = First "AskFm"
-- although we meant Second "AskFm", the typechecker doesn't know something went worng
-- because we used type synonyms instead of defining datatypes

-- Finally with record syntax
-- RecordProduct :: a -> b -> RecordProduct a b
-- Product :: a -> b -> Product a b

myRecord :: RecordProduct Integer Float
myRecord = RecordProduct 42 0.00001

myRecord' :: RecordProduct Integer Float
myRecord' =
  RecordProduct { pfirst = 42
                , psecond = 0.00001 }


data OperatingSystem =
  GnuPlusLinux
  | OpenBSDPlusNevermindJustBSDStill
  | Mac
  | Windows
  deriving (Eq, Show)

data ProgLang =
    Haskell
  | Agda
  | Idris
  | PureScript
  deriving (Eq, Show)

data Programmer =
  Programmer { os :: OperatingSystem
             , lang :: ProgLang }
  deriving (Eq, Show)

nineToFive :: Programmer
nineToFive = Programmer { os = Mac
                        , lang = Haskell }

feelingWizardly :: Programmer
feelingWizardly =
  Programmer { lang = Agda
             , os = GnuPlusLinux }


-- Exercise: Programmers
-- Write a function that generates all possible values of Programmer. Use the provided lists of OperatingSystem and ProgLang.

allOperatingSystems :: [OperatingSystem]
allOperatingSystems =
  [ GnuPlusLinux
  , OpenBSDPlusNevermindJustBSDStill
  , Mac
  , Windows
  ]

allLanguages :: [ProgLang]
allLanguages =
  [Haskell, Agda, Idris, PureScript]

allProgrammers :: [Programmer]
allProgrammers = [Programmer { os = oses, lang = langs } | oses <- allOperatingSystems, langs <- allLanguages ]

-- Accidental bottoms from records
-- constructing a value using record syntax but forgetting a field - do not do this!
-- partialAf = Programmer { os = GnuPlusLinux }
-- yields a 'missing field in record construction lang' exception

-- Use partial application of the data constructor instead:
data ThereYet =
  There Float Int Bool
  deriving (Eq, Show)

notYet :: Int -> Bool -> ThereYet
-- notYet = nope 25.5 --how does this typecheck?
notYet = There 25.5 -- why not this

notQuite :: Bool -> ThereYet
notQuite = notYet 10

yusssss :: ThereYet
yusssss = notQuite False
