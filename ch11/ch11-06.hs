-- ch11-06.hs
module OSPractice where

data OperatingSystem =
    GnuPlusLinux
  | BSD
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
  Programmer { os   :: OperatingSystem
             , lang :: ProgLang }
  deriving (Eq, Show)

nineToFive :: Programmer
nineToFive = Programmer { os = Mac
                        , lang = Haskell }

feelingWizardly :: Programmer
feelingWizardly = 
  Programmer { lang = Agda
             , os = GnuPlusLinux }

allOperatingSystems :: [OperatingSystem]
allOperatingSystems =
  [ GnuPlusLinux
  , BSD
  , Mac
  , Windows
  ]

allLanguages :: [ProgLang]
allLanguages = 
  [Haskell, Agda, Idris, PureScript]

allProgrammers :: [Programmer]
allProgrammers = 
  [ Programmer { os = a, lang = b }
  | a <- allOperatingSystems,
    b <- allLanguages
  ]
