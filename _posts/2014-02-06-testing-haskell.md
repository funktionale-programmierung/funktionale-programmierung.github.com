---
layout: post
description: Testen mit Haskell
title: "Testen mit Haskell"
author: stefan-wehr
tags: ["Testing", "Haskell"]
---

Tests sind für gute Softwarequalität unerlässlich. Obwohl wir hier
in diesem Blog immer wieder über die Vorzüge funktionaler Programmierung
in Bezug auf Softwarequalität und geringe Fehlerraten berichten,
gilt dies natürlich auch für mit funktionalen Sprachen realisierte
Projekte. Um Softwaretests zu schreiben stehen den Entwicklern und Testern 
in funktionalen Sprachen dieselben
Mittel wie z.B. in objekt-orientierten Sprachen zur Verfügung. Allerdings
gestaltet sich das Testen in funktionalen Sprachen oftmals deutlich einfacher,
da Zustand [explizit gehandhabt](/2013/03/12/rein-funktional.html) wird 
(siehe auch folgende Blogartikel: [1](/2013/03/20/warum-funktional.html),
[2](/2013/06/21/persistente-datenstrukturen.html), [3](/2013/08/23/was-ist-funktionale-programmierung.html)),
wodurch Fehler leichter zu reproduzieren
sind und aufwändiges Initialisieren von zu testenden Objekte oft entfällt.

In diesem Artikel möchte ich ein [Framework](http://hackage.haskell.org/package/HTF) 
vorstellen, mit dem wir
bei uns in der Firma sämtliche Tests auf Codeebene für unsere
Haskell-Software organisieren. Das Framework integriert dabei
verschiedene Testmethoden (Unit-Tests, randomisierte Tests mit
[QuickCheck](http://hackage.haskell.org/package/QuickCheck), auch diskutiert in [diesem Blogartikel](/2013/07/10/randomisierte-tests-mit-quickcheck.html)),
ermöglicht schnelles Hinzufügen von neuen Testfällen und bereite
Fehlermeldung so auf, dass die Ursache eines Fehlers einfach
lokalisierbar ist. Das Framework steht unter einer Open-Source-Lizenz.

<!-- more start -->

Angenommen, wir möchten folgende (inkorrekte) Funktion zum Umdrehen einer Liste testen:

{% highlight haskell %}
myReverse :: [a] -> [a]
myReverse []     = []
myReverse [x]    = [x]
myReverse (x:xs) = myReverse xs
{% endhighlight %}

Dazu installieren wir zunächst das [HTF-Paket](/2013/08/23/was-ist-funktionale-programmierung.html) mittels
`cabal install HTF` und fügen dann ganz oben in die Quelldatei das Pragma 

{% highlight haskell %}
{-# OPTIONS_GHC -F -pgmF htfpp #-}
{% endhighlight %}

ein. Dadurch wird der [GHC-Compiler](http://haskell.org/ghc) angewiesen, die Datei
vor dem Kompilieren durch den `htfpp`-Präprozessor zu jagen. Dieser Präprozessor
ist Teil des [Haskell-Test-Frameworks](http://hackage.haskell.org/package/HTF),
kurz HTF, und ermöglicht automatisches Aufsammeln von Testfällen sowie 
einfaches Auffinden von fehlgeschlagene Tests durch das Annotieren
von Fehlermeldungen mit Dateienamen und Zeilennummern.

Wir benötigen auch noch eine Import-Deklaration:

{% highlight haskell %}
import Test.Framework
{% endhighlight %}

Wir starten jetzt mit zwei einfachen Unit-Tests für unsere `reverse`-Funktion:

{% highlight haskell %}
test_nonEmpty = 
    do assertEqual [1] (myReverse [1])
       assertEqual [3,2,1] (myReverse [1,2,3])

test_empty = assertEqual ([] :: [Int]) (myReverse [])
{% endhighlight %}

Die Namen von Unit-Tests beginnen immer mit `test_`, dadurch
werden die Testdefinitionen automatisch gefunden. Mit `assertEqual expected real` drücken
wir aus, dass das Ergebnis des Ausdrucks `real` gleich dem Ausdruck `expected` sein 
muss. Die API von HTF stellt auch noch eine [ganze Reihe](http://hackage.haskell.org/package/HTF-0.11.1.1/docs/Test-Framework-HUnitWrapper.html) 
weiterer Assertions zur Verfügung, mit denen Erwartungen an die Ergebnisse von
Funktionsaufrufen ausgedrückt werden können. Benutzer anderer
Unit-Test-Frameworks werden hier viele bekannte Assertions finden.

Die Definition von [QuickCheck](http://hackage.haskell.org/package/QuickCheck)-Eigenschaften ist
ähnlich einfach, hier verwenden wir das Präfix `prop_`, damit auch diese vom Testframework
automatisch gefunden werden.

{% highlight haskell %}
prop_reverse :: [Int] -> Bool
prop_reverse xs = 
    xs == (myReverse (myReverse xs))
{% endhighlight %}

Wie bereits in einem [früheren Artikel](/2013/07/10/randomisierte-tests-mit-quickcheck.html) beschrieben, 
testet QuickCheck die angegebene Eigenschaft wiederholt mit immer neuen, randomisierten Werten. In
unserem Fall wird also für zufällige Listen von `Int`s getestet, ob das zweimalige Umdrehen
einer Liste wieder die Ausgangsliste liefert.

Jetzt brauchen wir nur noch eine `main`-Funktion, um die Tests in unserem kleinen Beispiel
auszuführen:

{% highlight haskell %}
main =
    do args <- getArgs
       runTestWithArgs args htf_thisModulesTests
{% endhighlight %}

Mit `htf_thisModulesTests` referenzieren wir dabei auf alle im aktuellen Modul
definierten Tests. Der Code in der `main`-Funktion benötigt noch zwei weitere Imports:

{% highlight haskell %}
import Test.Framework
import Test.Framework.TestManager
{% endhighlight %}

Jetzt können wir die Tests ausführen, z.B. interaktiv mit dem `ghci`. Sie finden
den kompletten Code des Beispiels auch [hier](files/testing-haskell/Reverse.hs). Das Ausführen produziert folgende
Ausgabe:

![Ausgaben von ghci](/files/testing-haskell/HTF.png)

Oh, es gab Fehler! Wir bemerken zwei Dinge an der Ausgabe von HTF.
Zum einen wird für fehlgeschlagene Assertions ein Diff zwischen der erwarteten
und der wirklichen Ausgabe angezeigt. In unserem konkreten Fall brauchen
wir das Diff wohl kaum um den Unterschied zwischen `[3, 2, 1]` und `[3]` zu
erkennen, aber bei großen Ausgaben ist ein Diff sehr wertvoll, um subtile Unterschiede
schnell zu erkennen. Zum anderen wird bei der fehlgeschlagenen QuickCheck-Eigenschaft
ein sogenanntes "Replay-Argument" angezeigt. Damit können wir genau den fehlgeschlagenen
Test deterministisch wiederholen, um ihn z.B. später als Regressionstest in unser 
Repository mit aufzunehmen.

So, jetzt korrigieren wir aber die Definition von `reverse`:

{% highlight haskell %}
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]
{% endhighlight %}

Damit bekommen wir folgende Ausgabe:

![Ausgaben von ghci](/files/testing-haskell/HTF2.png)

Zum Abschluss möchte ich noch kurz skizzieren, wie man mit HTF Tests für ein größeres
Projekt organisiert. In unserem Beispiel oben haben wir ja alle Tests im selben Modul
definiert. Normalerweise sind Tests aber über viele verschiedene Module verstreut.
HTF macht es einfach, alle diese Tests zu einer großen Testsuite zu vereinigen.

Zunächst müssen wir aus jedem Modul, welches Tests definiert, das Symbol
`htf_thisModulesTests` exportieren. Dann können wir ein Hauptmodul
schreiben, welches alle Testmodule importiert und über eine `main`-Funktion
ausführbar macht:

{% highlight haskell %}
{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Main where

import {-@ HTF_TESTS @-} MyPkg.A
import {-@ HTF_TESTS @-} MyPkg.B

main = htfMain htf_importedTests
{% endhighlight %}

Hier werden die in `MyPkg.A` und `MyPkg.B` definierten Tests durch das spezielle Pragma `HTF_TESTS`
importiert. Die `main`-Funktion führt dann alle in diesen Modulen definierten Tests aus.
Über Kommandozeilenoptionen kann man aber auch nur eine bestimmte Menge von Tests
ausführen. Hier sind alle unterstützen Kommandozeilenparameter:

    USAGE: COMMAND [OPTION ...] PATTERN ...
    
      where PATTERN is a posix regular expression matching
      the names of the tests to run.
    
      -q          --quiet             only display errors
      -n PATTERN  --not=PATTERN       tests to exclude
      -l          --list              list all matching tests
      -j[N]       --threads[=N]       run N tests in parallel, default N=1
                  --deterministic     do not shuffle tests when executing them in parallel.
      -o FILE     --output-file=FILE  name of output file
                  --json              output results in machine-readable JSON format (incremental)
                  --xml=FILE          output results in junit-style XML format
                  --split             splits results in separate files to avoid file locking (requires -o/--output-file)
                  --colors=BOOL       use colors or not
      -h          --help              display this message

Insbesondere kann man Tests
auch parallel ausführen oder maschinell lesbare Ausgaben erzeugen. Letzteres benutzen
wir, um die Ausgaben der Tests in [jenkins](http://jenkins-ci.org/), einem Server für
Continuous-Integration, einzubetten.

So, das war's für heute. Wir haben einen praktischen und pragmatischen Ansatz kennengelernt,
im Tests auf Codeebene in Haskell einfach zu organisieren. Das HTF-Tool ist bei uns in
der Firma täglich um Einsatz, vor allem um die Tests für unser [Checkpad](/2013/07/17/medizin-funktional.html)-Projekt
zu organisieren und ausführen.
