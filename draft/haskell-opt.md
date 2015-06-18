---
layout: post
description: Haskell optimieren
title: "Optimierung von Haskellprogrammen"
author: alex-gremm
tags: ["Haskell", "Optimierung", "Performance"]
---

Geht es um funktionalle Programmierung denkt der eine sofort an Kombinatoren wie _map_, _fold_ und 
_zipWith_, der andere an Rekursion und Seiteneffekte, und wieder ein anderer an Monaden und 
Typsysteme. Selten, wenn überhaupt, bringt man funktionale Programmierung mit dem Höchstleistungsrechnen
in Verbindung. In diesem Blogeintrag werden wir an einem kleinen Beispiel zeigen, worauf man achten muss, 
wenn man effiziente Programme in Haskell schreiben möchte.
<!-- more start -->

<!-- Das ist auch die Syntax für Kommentare, die im HTML nachher
auftauchen. -->

## Zahlen aufsummieren ##

Nehmen wir an, wir benötigen eine Funktion die alle Elemente einer List aufsummiert. 
Eine Möglichkeit `sum` zu implementieren ist folgende:
 
{% highlight haskell %}
sum :: Num a -> [a] -> a
sum [] = 0
sum (x:xs) = x + sum xs 
{% endhighlight %}
Das Programm arbeitet die Liste rekursiv ab und summiert die jeweiligen Elemente auf. Erste
Tests mit kleinen Listen bestätigen die Korrektheit unserer Implementierung. Bei langen Listen fällt 
uns jedoch der hohe Speicherverbrauch auf und bei sehr langen terminiert unser Programm 
mit der Meldung `Stack space overflow: current size 33632 bytes.
Use '+RTS -Ksize -RTS' to increase it.` Erhöhen wir den vorgeschlagenen Wert gibt unser Programm 
wieder die korrekte Summe aus. Jedoch terminiert es bei noch größeren Listen wieder mit einem 
"stack overflow". Das Problem ist dabei, dass die Rekursionstiefe für den vorhandenen Stack zu hoch ist. Veranschaulichen wir uns die 
Auswertung eines Aufrufes von `sum` in folgendem Beispiel:

{% highlight haskell %}
sum [1,2,3]
= 
1 + sum [2,3]
=
1 + (2 + sum [3])
=
1 + (2 + (3 + sum []))
=
1 + (2 + (3 + 0))
= 
1 + (2 + 3)
=
1 + 5
=
6
{% endhighlight %}
Bevor wir also mit dem Aufsummieren anfangen können, müssen wir die Liste komplett durchlaufen haben.
Dadurch korreliert die Rekursionstiefe mit der Länge der Liste. Der geübte funktionale Programmierer 
wird sofort vorschlagen die Funktion in eine _endrekursive_ zu transformieren. Diese ist nun in 
der Funktion `go` realisiert.
{% highlight haskell %}
sum' :: Num a -> [a] -> a
sum' = go 0
  where
    go acc [] = acc
    go acc (x:xs) = go (acc + x) xs
{% endhighlight %}
Überraschenderweise treten jedoch auch bei dieser Implementierung Speicherprobleme auf. 
In einer Sprache mit strikter Evaluation wie z.B. ML würde diese Funktion aber den gewünschten Effekt, mit konstant großem Stackspeicher 
zu operieren, haben.
Da Haskell jedoch eine nicht-strikte Sprache ist, wird die Addition des Akkumulators erst ausgeführt wenn dieser benötigt wird.
Um also den gewünschten Effekt zu erzielen müssen wir die Auswertung der Addition in jedem Rekursionsschritt erzwingen.

## Berechnungen erzwingen ##

Mit der Funktion `seq :: a -> b -> b` wird die Auswertung von `a` erzwungen und `b` zurückgegeben. 
{% highlight haskell %}
sum'' :: Num a -> [a] -> a
sum'' = go 0
  where
    go acc [] = acc
    go acc (x:xs) = 
      let acc' = acc + x
      in acc' `seq` go acc' xs
{% endhighlight %}
Die angepasste Funktion `sum''` läuft nun wie erwartet als endrekursive Funktion mit konstantem 
Stackverbrauch. Eine syntaktisch weniger aufwendige Methode biete die GHC Erweiterung 
`BangPatterns`. Durch diese ist es möglich die strikt zu evaluierenden Werte mit einem `!` zu 
markieren wie in `sum'''` zu sehen ist.
{% highlight haskell %}
sum''' :: Num a -> [a] -> a
sum''' = go 0
  where
    go acc [] = acc
    go !acc (x:xs) = go (acc + x) xs
{% endhighlight %}

## Weitere Optimierung durch Typ-Annotationen ##

Um die Implementierungen der Funktionen so allgemein wie möglich zu halten
sind diese durch die Typklasse `Num a` auf die Typen beschränkt,
die `Num` instantieren. Diese sind unter anderen `Integer` und `Int`,
 die beiden Typen für Ganzzahlen. Dabei kann `Integer` beliebig 
große Ganzzahlen darstellen und `Int` wird direkt auf die Wortgröße
der CPU-Architektur gemappt.

Bei einem Aufruf der Funktion `sum` wie in folgendem Kontext ohne 
Typannotationen 
{% highlight haskell %}
main :: IO ()
main = print $ sum''' [1,2,3]
{% endhighlight %}
wird die Liste `[1,2,3]` als Liste aus `Integer`-Werten interpretiert. Da alle in der Liste enthaltenen Werte jedoch problemlos in ein 
Maschinenwort passen, kann (und sollte man!) einen der Werte als `Int` annotieren. Dadurch wird `sum` zur Kompilierzeit auf den Typen `Int` spezialisiert wodurch die effiziente Addition auf 
Maschinenworten benutzt wird anstatt der langsameren Addition auf 
beliebig großen Ganzzahlen. Die Ausführung von `sum [0..1000000000]` benötigt 21 Sekunden wohingegen `sum [0..1000000000 :: Int]` in schon 9 Sekunden
das korrekte Ergebnis liefert.

Woher wir das wissen und noch viel mehr bald in Teil 2!

<!-- more end -->

