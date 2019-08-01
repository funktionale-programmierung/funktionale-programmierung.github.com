---
layout: post
title: "Testen von Generiertem Code in Haskell"
author: timo-vonholtz
tags: ["Haskell","Optimierung","Inspection Testing"]
---

Funktionale Tests, also Tests die das Ergebnis einer Berechnung überprüfen, und Benchmarks sind fester Bestandteil moderner Softwareentwicklung. Manchmal möchte man jedoch sicherstellen, dass der Compiler bestimmte Optimierungen durchführt oder zeigen, dass der generierte Code genau so ist wie erwartet.

Für GHC, dem Standard-Compiler für Haskell, gibt es genau zu diesem Zweck die Library [inspection-testing](https://github.com/nomeata/inspection-testing). Hierzu gab es auf der letzten BOB-Konferenz in Berlin auch einen [Vortrag](https://bobkonf.de/2019/breitner.html).

<!-- more start -->

## Motivierendes Beispiel

```haskell
module Simple where

import Test.Inspection
import Data.Maybe

isJust1, isJust2 :: Maybe a -> Bool
isJust1 = not . isNothing
isJust2 Nothing = False
isJust2 (Just _) = True
```

Die beiden Funktionen geben zurück ob der übergebene Wert den `Just` Konstruktor verwendet. Diese Funktion existiert auch in der Standard-Library als [isJust](https://www.stackage.org/haddock/lts-13.18/base-4.12.0.0/Data-Maybe.html#v:isJust).

Ob die beiden Funktionen äquivalent sind, könnte man jetzt z. B. mit QuickCheck testen. Wir würden aber gerne wissen, ob GHC in beiden Fällen exakt den gleichen Code generiert damit wir lesbaren komponierten Code schreiben können, aber dafür nicht mit schlechterer Performance bezahlen müssen.

Diesen Test kann man jetzt mittels inspection-testing realisieren:

```haskell
{-# LANGUAGE TemplateHaskell #-}
module Simple where

import Test.Inspection
import Data.Maybe

isJust1, isJust2 :: Maybe a -> Bool
isJust1 = not . isNothing
isJust2 Nothing = False
isJust2 (Just _) = True

inspect $ 'isJust1 === 'isJust2
```

Mit `'isJust1 === 'isJust2` haben wir eine [Obligation](https://hackage.haskell.org/package/inspection-testing-0.4.1.2/docs/Test-Inspection.html#t:Obligation) eingeführt die sicherstellt, dass beide Implementierungen identisch sind. Wir sorgen mit `inspect` dafür, dass bei Misserfolg das Kompilieren mit einem Fehler abgebrochen wird. Wenn wir das Modul jetzt kompilieren müssen wir leider feststellen, dass die beiden Implementierungen nicht identisch sind:

```
$ stack ghc --package inspection-testing inspection-testing.hs
[1 of 1] Compiling Simple           ( inspection-testing.hs, inspection-testing.o )

inspection-testing.hs: warning:
    Test.Inspection: Compilation without -O detected. Expect
    optimizations to fail.
inspection-testing.hs:13:1: isJust1 === isJust2 failed:
    LHS:
        isJust1 :: forall a. Maybe a -> Bool
        [LclIdX,
         Unf=Unf{Src=<vanilla>, TopLvl=True, Value=False, ConLike=False,
                 WorkFree=False, Expandable=False, Guidance=IF_ARGS [] 30 0}]
        isJust1 = \ (@ a) -> . @ Bool @ Bool @ (Maybe a) not (isNothing @ a)
        
    RHS:
        isJust2 :: forall a. Maybe a -> Bool
        [LclIdX,
         Arity=1,
         Unf=Unf{Src=<vanilla>, TopLvl=True, Value=True, ConLike=True,
                 WorkFree=True, Expandable=True,
                 Guidance=ALWAYS_IF(arity=1,unsat_ok=True,boring_ok=True)}]
        isJust2
          = \ (@ a) (ds_d6ap :: Maybe a) ->
              case ds_d6ap of {
                Nothing -> False;
                Just ds_d6av -> True
              }
        

inspection-testing.hs: error:
    inspection testing unsuccessful
         unexpected failures: 1
```

In der Ausgabe sehen wir welche Obligation fehlgeschlagen ist und für beide Seiten der Gleichung (`LHS` und `RHS`) jeweils den generierten Code. Die Ausgabe stellt hier kein Haskell dar, sondern Core, die interne Repräsentation eines Haskell-Moduls im GHC. Damit enthält sie aber auch viele Annotationen, die es schwer machen den eigentlichen Unterschied der beiden Fälle zu sehen. Man kann den Großteil dessen ausblenden:

```
$ stack ghc --package inspection-testing inspection-testing.hs -- -dsuppress-idinfo -dsuppress-coercions -dsuppress-type-applications -dsuppress-module-prefixes -dsuppress-type-signatures -dsuppress-uniques
[1 of 1] Compiling Simple           ( inspection-testing.hs, inspection-testing.o ) [Optimisation flags changed]

inspection-testing.hs: warning:
    Test.Inspection: Compilation without -O detected. Expect
    optimizations to fail.
inspection-testing.hs:12:1: isJust1 === isJust2 failed:
    LHS:
        isJust1
        isJust1 = \ @ a -> . not isNothing
        
    RHS:
        isJust2
        isJust2
          = \ @ a ds ->
              case ds of {
                Nothing -> False;
                Just ds -> True
              }
        

inspection-testing.hs: error:
    inspection testing unsuccessful
         unexpected failures: 1
```

Jetzt sieht man deutlich, dass bei `isJust1` nichts optimiert wurde. Hier gibt es zum Glück eine hilfreiche Warnung, dass GHC ohne Optimierungen gelaufen ist und es möglicherweise daran liegt. Mit Optimierungen ist das Ergebnis dann wie erwartet:

```
$ stack ghc --package inspection-testing inspection-testing.hs -- -O
[1 of 1] Compiling Simple           ( inspection-testing.hs, inspection-testing.o )
inspection-testing.hs:12:1: lhs === rhs passed.
inspection testing successful
      expected successes: 1
```

## Reale Anwendungsbeispiele

In Haskell ist es relativ einfach Code mit [Template-Haskell](https://wiki.haskell.org/Template_Haskell) oder [GHC Generics](https://wiki.haskell.org/GHC.Generics) für beliebige Datentypen zu generieren. Dies passiert z. B. in den Libraries [hashable](https://github.com/tibbe/hashable), [aeson](https://github.com/bos/aeson). Bei diesen beiden Libraries ist es wichtig, dass Compileroptimierungen korrekt funktionieren, da besonders bei GHC-Genierics der Code sonst möglicherweise deutlich langsamer wäre. Dies kann man natürlich mit mäßigem Aufwand einmalig verifizieren, es geht aber z. B. bei neuen Compilerversionen schnell wieder kaputt. Mithilfe von inspection-testing kann man einfach neue Versionen in die Testsuite aufnehmen und damit die Annahmen verifizieren.

Bei [Checkpad MED](https://www.checkpad.de/) verwenden wir intern die Library [large-hashable](https://github.com/factisresearch/large-hashable), um zu überprüfen, ob sich das Ergebnis von einer Berechnung geändert hat ohne dafür das alte Ergebnis vorhalten zu müssen. Die Library stellt sowohl für Template-Haskell als auch für GHC-Generics Funktionen bereit, um den nötigen Boilerplate-Code zu generieren. Hier ist es uns wichtig, dass beide Implementierungen identisch funktionieren, damit man sie ohne Sorge austauschen kann. Hier war die ursprüngliche Implementierung nicht äquivalent und wird mit diesem [Pull Request](https://github.com/factisresearch/large-hashable/pull/14) austauschbar.
