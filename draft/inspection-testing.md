---
layout: post
title: "Testen von Generiertem Code in Haskell"
author: timo-vonholtz
tags: ["Haskell","Optimierung","Inspection Testing"]
---

Funktionale Tests, also tests die das Ergebnis einer Berechnung überprüfen, und Benchmarks sind fester Bestandteil moderner Softwareentwicklung. Manchmal möchte man jedoch sicherstellen dass der Compiler bestimmte Optimierungen durchführt oder zeigen dass der genierierte Code genau so ist wie erwartet.

Für GHC gibt es genau zu diesem Zweck die Library [inspection-testing](https://github.com/nomeata/inspection-testing).

## Motivierendes Beispiel

```haskell
module Simple where

import Test.Inspection
import Data.Maybe

lhs, rhs :: Maybe a -> Bool
lhs = not . isNothing
rhs Nothing = False
rhs (Just _) = True
```

Testen ob die beiden Funktionen äquivalent sind könnte man jetzt z.B. mit QuickCheck. Wir würden aber gerne wissen, ob GHC in beiden Fällen exakt den gleichen Code generiert damit wir lesbaren komponierten Code schreiben können, aber dafür nicht mit schlechterer Performance bezahlen müssen.

Diesen Test kann man jetzt mittels inspection-testing realisieren:

```haskell
{-# LANGUAGE TemplateHaskell #-}
module Simple where

import Test.Inspection
import Data.Maybe

lhs, rhs :: Maybe a -> Bool
lhs = not . isNothing
rhs Nothing = False
rhs (Just _) = True

inspect $ 'lhs === 'rhs
```

Wenn wir das obige Modul jetzt kompilieren müssen wir leider feststellen, dass die beiden Implementierungen nicht identisch sind:

```
$ stack ghc --package inspection-testing inspection-testing.hs
[1 of 1] Compiling Simple           ( inspection-testing.hs, inspection-testing.o )

inspection-testing.hs: warning:
    Test.Inspection: Compilation without -O detected. Expect
    optimizations to fail.
inspection-testing.hs:13:1: lhs === rhs failed:
    LHS:
        lhs :: forall a. Maybe a -> Bool
        [LclIdX,
         Unf=Unf{Src=<vanilla>, TopLvl=True, Value=False, ConLike=False,
                 WorkFree=False, Expandable=False, Guidance=IF_ARGS [] 30 0}]
        lhs = \ (@ a) -> . @ Bool @ Bool @ (Maybe a) not (isNothing @ a)
        
    RHS:
        rhs :: forall a. Maybe a -> Bool
        [LclIdX,
         Arity=1,
         Unf=Unf{Src=<vanilla>, TopLvl=True, Value=True, ConLike=True,
                 WorkFree=True, Expandable=True,
                 Guidance=ALWAYS_IF(arity=1,unsat_ok=True,boring_ok=True)}]
        rhs
          = \ (@ a) (ds_d6ap :: Maybe a) ->
              case ds_d6ap of {
                Nothing -> False;
                Just ds_d6av -> True
              }
        

inspection-testing.hs: error:
    inspection testing unsuccessful
         unexpected failures: 1
```

Die Ausgabe hier ist kein Haskell sondern Core, die interne Repräsentation eines Haskell-Moduls im GHC. Damit enthält sie aber auch viele Annotationen die es schwer machen den eigentlichen Unterschied der beiden Fälle zu sehen. Man kann aber den Großteil dessen ausblenden:

```
$ stack ghc --package inspection-testing inspection-testing.hs -- -dsuppress-idinfo -dsuppress-coercions -dsuppress-type-applications -dsuppress-module-prefixes -dsuppress-type-signatures -dsuppress-uniques
[1 of 1] Compiling Simple           ( inspection-testing.hs, inspection-testing.o ) [Optimisation flags changed]

inspection-testing.hs: warning:
    Test.Inspection: Compilation without -O detected. Expect
    optimizations to fail.
inspection-testing.hs:12:1: lhs === rhs failed:
    LHS:
        lhs
        lhs = \ @ a -> . not isNothing
        
    RHS:
        rhs
        rhs
          = \ @ a ds ->
              case ds of {
                Nothing -> False;
                Just ds -> True
              }
        

inspection-testing.hs: error:
    inspection testing unsuccessful
         unexpected failures: 1
```

Jetzt sieht man deutlich, dass bei `lhs` nichts optimiert wurde. Hier gibt es aber zum Glück eine hilfreiche Warnung, dass GHC ohne Optimierungen gelaufen ist und es möglicherweise daran liegt. Mit Optimierungen ist das Ergebnis dann wie erwartet:

```
$ stack ghc --package inspection-testing inspection-testing.hs -- -O
[1 of 1] Compiling Simple           ( inspection-testing.hs, inspection-testing.o )
inspection-testing.hs:12:1: lhs === rhs passed.
inspection testing successful
      expected successes: 1
```

## Fazit

In Haskell ist es relativ einfach code mit [Template-Haskell](https://wiki.haskell.org/Template_Haskell) oder [GHC Generics](https://wiki.haskell.org/GHC.Generics) für beliebige Datentypen Code zu generieren. Dies passiert z.B. in den Libraries [hashable](https://github.com/tibbe/hashable), [aeson](https://github.com/bos/aeson). Um den generierten Code zu verifizieren kann man also für einen konkreten Typen definieren was generiert werden soll und dann mittels inspection-testing verifizieren dass der Code genau wie erwartet auch generiert wird.

Bei [Checkpad MED](https://www.checkpad.de/) verwenden wir intern die Library [large-hashable](https://github.com/factisresearch/large-hashable) um zu überprüfen ob sich das Ergebnis von einer Berechnung geändert hat ohne dafür das alte Ergebnis vorhalten zu müssen. Die Library stellt sowohl für Template-Haskell als auch für GHC-Generics funktionen bereit um den nötigen Boilerplate-Code zu generieren. Hier ist uns wichtig, dass beide Implementierungen identisch funktionieren damit man sie ohne Sorge austauschen kann. Hier war die Ursprüngliche Implementierung nicht ächivalent, ist mit diesem [PR](https://github.com/factisresearch/large-hashable/pull/14) aber austauschbar.
