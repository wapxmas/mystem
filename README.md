# mystem library
Bindings for [Mystem](https://tech.yandex.ru/mystem/) morphological analyzer executabe.

# Usage
```haskell
main :: IO ()
main = do
  res <- getStems $ T.words "Съешь ещё этих мягких французских булок"
  forM_ res printMSRes -- sample output
```

## mystem / (lib types) output comparation
### mystem:
```
Съешь{съедать=V=(inpraes,sg,indic,2p,pf|sg,imper,2p,pf)}
ещё{еще=ADV=|еще=PART=}
этих{этот=APRO=(abl,pl|gen,pl|acc,pl,anim)}
мягких{мягкий=A=(abl,pl,plen|acc,pl,plen,anim|gen,pl,plen)}
французских{французский=A=(abl,pl,plen|acc,pl,plen,anim|gen,pl,plen)}
булок{булка=S,f,inan=gen,pl}
```
### lib types:
#### on Windows you need to change console codepage to utf8 with `chcp 65001`
```
Съешь
съедать
Pos (Just V)
Grams (VerbsTime (Just INPRAES)) (Case Nothing) (Spf (Just SG)) (RIVerb (Just INDIC)) (FAdj Nothing) (DComp Nothing) (PVerb Nothing) (GG Nothing) (VAspect (Just PF)) (APVoice Nothing) (IAN Nothing) (TIVerb Nothing) (OG Nothing)
Grams (VerbsTime Nothing) (Case Nothing) (Spf (Just SG)) (RIVerb (Just IMPER)) (FAdj Nothing) (DComp Nothing) (PVerb Nothing) (GG Nothing) (VAspect (Just PF)) (APVoice Nothing) (IAN Nothing) (TIVerb Nothing) (OG Nothing)
ещё
еще
Pos (Just ADV)
еще
Pos (Just PART)
этих
этот
Pos (Just APRO)
Grams (VerbsTime Nothing) (Case (Just ABL)) (Spf (Just PL)) (RIVerb Nothing) (FAdj Nothing) (DComp Nothing) (PVerb Nothing) (GG Nothing) (VAspect Nothing) (APVoice Nothing) (IAN Nothing) (TIVerb Nothing) (OG Nothing)
Grams (VerbsTime Nothing) (Case (Just GEN)) (Spf (Just PL)) (RIVerb Nothing) (FAdj Nothing) (DComp Nothing) (PVerb Nothing) (GG Nothing) (VAspect Nothing) (APVoice Nothing) (IAN Nothing) (TIVerb Nothing) (OG Nothing)
Grams (VerbsTime Nothing) (Case (Just ACC)) (Spf (Just PL)) (RIVerb Nothing) (FAdj Nothing) (DComp Nothing) (PVerb Nothing) (GG Nothing) (VAspect Nothing) (APVoice Nothing) (IAN (Just ANIM)) (TIVerb Nothing) (OG Nothing)
мягких
мягкий
Pos (Just A)
Grams (VerbsTime Nothing) (Case (Just ABL)) (Spf (Just PL)) (RIVerb Nothing) (FAdj (Just PLEN)) (DComp Nothing) (PVerb Nothing) (GG Nothing) (VAspect Nothing) (APVoice Nothing) (IAN Nothing) (TIVerb Nothing) (OG Nothing)
Grams (VerbsTime Nothing) (Case (Just ACC)) (Spf (Just PL)) (RIVerb Nothing) (FAdj (Just PLEN)) (DComp Nothing) (PVerb Nothing) (GG Nothing) (VAspect Nothing) (APVoice Nothing) (IAN (Just ANIM)) (TIVerb Nothing) (OG Nothing)
Grams (VerbsTime Nothing) (Case (Just GEN)) (Spf (Just PL)) (RIVerb Nothing) (FAdj (Just PLEN)) (DComp Nothing) (PVerb Nothing) (GG Nothing) (VAspect Nothing) (APVoice Nothing) (IAN Nothing) (TIVerb Nothing) (OG Nothing)
французских
французский
Pos (Just A)
Grams (VerbsTime Nothing) (Case (Just ABL)) (Spf (Just PL)) (RIVerb Nothing) (FAdj (Just PLEN)) (DComp Nothing) (PVerb Nothing) (GG Nothing) (VAspect Nothing) (APVoice Nothing) (IAN Nothing) (TIVerb Nothing) (OG Nothing)
Grams (VerbsTime Nothing) (Case (Just ACC)) (Spf (Just PL)) (RIVerb Nothing) (FAdj (Just PLEN)) (DComp Nothing) (PVerb Nothing) (GG Nothing) (VAspect Nothing) (APVoice Nothing) (IAN (Just ANIM)) (TIVerb Nothing) (OG Nothing)
Grams (VerbsTime Nothing) (Case (Just GEN)) (Spf (Just PL)) (RIVerb Nothing) (FAdj (Just PLEN)) (DComp Nothing) (PVerb Nothing) (GG Nothing) (VAspect Nothing) (APVoice Nothing) (IAN Nothing) (TIVerb Nothing) (OG Nothing)
булок
булка
Pos (Just S)
Grams (VerbsTime Nothing) (Case Nothing) (Spf Nothing) (RIVerb Nothing) (FAdj Nothing) (DComp Nothing) (PVerb Nothing) (GG (Just F)) (VAspect Nothing) (APVoice Nothing) (IAN (Just INAN)) (TIVerb Nothing) (OG Nothing)
Grams (VerbsTime Nothing) (Case (Just GEN)) (Spf (Just PL)) (RIVerb Nothing) (FAdj Nothing) (DComp Nothing) (PVerb Nothing) (GG Nothing) (VAspect Nothing) (APVoice Nothing) (IAN Nothing) (TIVerb Nothing) (OG Nothing)
```
