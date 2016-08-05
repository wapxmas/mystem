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

  data MSRes = MSRes SWord RWords deriving (Eq, Show)

  data RWord = RWord { rword :: WordT, pos :: Pos, grams :: Maybe Grams, cases :: [Grams] } deriving (Eq, Show)

  data Grams = Grams VerbsTime Case Spf RIVerb FAdj DComp PVerb GG VAspect APVoice IAN TIVerb OG deriving (Eq, Show)

  newtype Pos = Pos (Maybe PosValue) deriving (Eq, Show)
  data PosValue = A | ADV | ADVPRO | ANUM | APRO | COM | CONJ |
    INTJ | NUM | PART | PR | S | SPRO | V deriving (Eq, Show, Read)

  newtype VerbsTime = VerbsTime (Maybe VerbsTimeValue) deriving (Eq, Show)
  data VerbsTimeValue = PRAES | INPRAES | PRAET deriving (Eq, Show, Read)

  newtype Case = Case (Maybe CaseValue) deriving (Eq, Show)
  data CaseValue = NOM | GEN | DAT | ACC | INS |
    ABL | PARTC | LOC | VOC deriving (Eq, Show, Read)

  newtype Spf = Spf (Maybe SpfValue) deriving (Eq, Show)
  data SpfValue = SG | PL deriving (Eq, Show, Read)

  newtype RIVerb = RIVerb (Maybe RIVerbValue) deriving (Eq, Show)
  data RIVerbValue = GER | INF | PARTCP | INDIC | IMPER deriving (Eq, Show, Read)

  newtype FAdj = FAdj (Maybe FAdjValue) deriving (Eq, Show)
  data FAdjValue = BREV | PLEN | POSS deriving (Eq, Show, Read)

  newtype DComp = DComp (Maybe DCompValue) deriving (Eq, Show)
  data DCompValue = SUPR | COMP deriving (Eq, Show, Read)

  newtype PVerb = PVerb (Maybe PVerbValue) deriving (Eq, Show)
  data PVerbValue = P1 | P2 | P3 deriving (Eq, Show, Read)

  newtype GG = GG (Maybe GGValue) deriving (Eq, Show)
  data GGValue = M | F | N deriving (Eq, Show, Read)

  newtype VAspect = VAspect (Maybe VAspectValue) deriving (Eq, Show)
  data VAspectValue = IPF | PF deriving (Eq, Show, Read)

  newtype APVoice = APVoice (Maybe APVoiceValue) deriving (Eq, Show)
  data APVoiceValue = ACT | PASS deriving (Eq, Show, Read)

  newtype IAN = IAN (Maybe IANValue) deriving (Eq, Show)
  data IANValue = ANIM | INAN deriving (Eq, Show, Read)

  newtype TIVerb = TIVerb (Maybe TIVerbValue) deriving (Eq, Show)
  data TIVerbValue = TRAN | INTR deriving (Eq, Show, Read)

  newtype OG = OG (Maybe OGValue) deriving (Eq, Show)
  data OGValue = PARENTH | GEO | AWKW | PERSN | DIST | MF | OBSC | PATRN |
    PRAED | INFORM | RARE | ABBR | OBSOL | FAMN deriving (Eq, Show, Read)

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

  isFamname :: MSRes -> Bool
  isFamname (MSRes _ (RWord _ _ (Just (Grams _ _ _ _ _ _ _ _ _ _ _ _ (OG (Just FAMN)))) _ : _)) = True
  isFamname _ = False

  isNoun :: MSRes -> Bool
  isNoun (MSRes _ (RWord _ (Pos (Just S)) _ _ : _)) = True
  isNoun _ = False

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
