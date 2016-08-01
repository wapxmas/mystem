{-# LANGUAGE OverloadedStrings #-}

module NLP.Mystem.Types where

  import           Control.Monad (forM_)
  import qualified Data.Char     as C
  import           Data.Default
  import qualified Data.Text     as T
  import qualified Data.Text.IO  as TIO
  import           Text.Read

  type WordT = T.Text
  type SWord = WordT
  type RWords = [RWord]

  data MSRes = MSRes SWord RWords deriving Show

  data RWord = RWord { rword :: WordT, pos :: Pos, grams :: Maybe Grams, cases :: [Grams] } deriving Show

  data Grams = Grams VerbsTime Case Spf RIVerb FAdj DComp PVerb GG VAspect APVoice IAN TIVerb OG deriving Show

  newtype Pos = Pos (Maybe PosValue) deriving Show
  data PosValue = A | ADV | ADVPRO | ANUM | APRO | COM | CONJ |
    INTJ | NUM | PART | PR | S | SPRO | V deriving (Show, Read)

  newtype VerbsTime = VerbsTime (Maybe VerbsTimeValue) deriving Show
  data VerbsTimeValue = PRAES | INPRAES | PRAET deriving (Show, Read)

  newtype Case = Case (Maybe CaseValue) deriving Show
  data CaseValue = NOM | GEN | DAT | ACC | INS |
    ABL | PARTC | LOC | VOC deriving (Show, Read)

  newtype Spf = Spf (Maybe SpfValue) deriving Show
  data SpfValue = SG | PL deriving (Show, Read)

  newtype RIVerb = RIVerb (Maybe RIVerbValue) deriving Show
  data RIVerbValue = GER | INF | PARTCP | INDIC | IMPER deriving (Show, Read)

  newtype FAdj = FAdj (Maybe FAdjValue) deriving Show
  data FAdjValue = BREV | PLEN | POSS deriving (Show, Read)

  newtype DComp = DComp (Maybe DCompValue) deriving Show
  data DCompValue = SUPR | COMP deriving (Show, Read)

  newtype PVerb = PVerb (Maybe PVerbValue) deriving Show
  data PVerbValue = P1 | P2 | P3 deriving (Show, Read)

  newtype GG = GG (Maybe GGValue) deriving Show
  data GGValue = M | F | N deriving (Show, Read)

  newtype VAspect = VAspect (Maybe VAspectValue) deriving Show
  data VAspectValue = IPF | PF deriving (Show, Read)

  newtype APVoice = APVoice (Maybe APVoiceValue) deriving Show
  data APVoiceValue = ACT | PASS deriving (Show, Read)

  newtype IAN = IAN (Maybe IANValue) deriving Show
  data IANValue = ANIM | INAN deriving (Show, Read)

  newtype TIVerb = TIVerb (Maybe TIVerbValue) deriving Show
  data TIVerbValue = TRAN | INTR deriving (Show, Read)

  newtype OG = OG (Maybe OGValue) deriving Show
  data OGValue = PARENTH | GEO | AWKW | PERSN | DIST | MF | OBSC | PATRN |
    PRAED | INFORM | RARE | ABBR | OBSOL | FAMN deriving (Show, Read)

  instance Default Grams where
    def = Grams (VerbsTime Nothing) (Case Nothing) (Spf Nothing) (RIVerb Nothing)
      (FAdj Nothing) (DComp Nothing) (PVerb Nothing) (GG Nothing) (VAspect Nothing)
      (APVoice Nothing) (IAN Nothing) (TIVerb Nothing) (OG Nothing)

  fillGrams :: Grams -> [String] -> Grams
  fillGrams g [] = g
  fillGrams (Grams vt cs sp ri fa dc pv gg va ap ia ti og) (s:xs) =
    fillGrams (Grams (fill vt s) (fill cs s) (fill sp s) (fill ri s) (fill fa s)
      (fill dc s) (fill pv s) (fill gg s) (fill va s) (fill ap s) (fill ia s)
      (fill ti s) (fill og s)) xs

  fill :: FillGramm a => a -> String -> a
  fill g = fillGramm g . map C.toUpper

  class (Read a) => Grammeme a where
    readG :: String -> Maybe a
    readG = readMaybe

  class FillGramm a where
    fillGramm :: a -> String -> a

  instance Grammeme PosValue
  instance Grammeme VerbsTimeValue
  instance Grammeme CaseValue where
    readG "PART" = readG "PARTC"
    readG s = readMaybe s
  instance Grammeme SpfValue
  instance Grammeme RIVerbValue
  instance Grammeme FAdjValue
  instance Grammeme DCompValue
  instance Grammeme PVerbValue
  instance Grammeme GGValue
  instance Grammeme VAspectValue
  instance Grammeme APVoiceValue
  instance Grammeme IANValue
  instance Grammeme TIVerbValue
  instance Grammeme OGValue

  instance FillGramm Pos where
    fillGramm (Pos Nothing) s = Pos . readG $ s
    fillGramm g _ = g
  instance FillGramm VerbsTime where
    fillGramm (VerbsTime Nothing) s = VerbsTime . readG $ s
    fillGramm g _ = g
  instance FillGramm Case where
    fillGramm (Case Nothing) s = Case . readG $ s
    fillGramm g _ = g
  instance FillGramm Spf where
    fillGramm (Spf Nothing) s = Spf . readG $ s
    fillGramm g _ = g
  instance FillGramm RIVerb where
    fillGramm (RIVerb Nothing) s = RIVerb . readG $ s
    fillGramm g _ = g
  instance FillGramm FAdj where
    fillGramm (FAdj Nothing) s = FAdj . readG $ s
    fillGramm g _ = g
  instance FillGramm DComp where
    fillGramm (DComp Nothing) s = DComp . readG $ s
    fillGramm g _ = g
  instance FillGramm PVerb where
    fillGramm (PVerb Nothing) s = PVerb . readG $ s
    fillGramm g _ = g
  instance FillGramm GG where
    fillGramm (GG Nothing) s = GG . readG $ s
    fillGramm g _ = g
  instance FillGramm VAspect where
    fillGramm (VAspect Nothing) s = VAspect . readG $ s
    fillGramm g _ = g
  instance FillGramm APVoice where
    fillGramm (APVoice Nothing) s = APVoice . readG $ s
    fillGramm g _ = g
  instance FillGramm IAN where
    fillGramm (IAN Nothing) s = IAN . readG $ s
    fillGramm g _ = g
  instance FillGramm TIVerb where
    fillGramm (TIVerb Nothing) s = TIVerb . readG $ s
    fillGramm g _ = g
  instance FillGramm OG where
    fillGramm (OG Nothing) s = OG . readG $ s
    fillGramm g _ = g
