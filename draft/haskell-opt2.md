---
layout: post
description: Haskell optimieren
title: "Optimierung von Haskellprogrammen - Teil 2"
author: alex-gremm
tags: ["Haskell", "Optimierung", "Performance"]
---

Im vorherigen [Teil](http://funktionale-programmierung.de/2015/06/18/haskell-opt.html) unserer Serie haben 
wir uns Optimierungen angeschaut, die durch das Erzwingen von Berechnungen und Typ-Annotationen
das Speicherverhalten und die Laufzeit von Haskellprogrammen verbesserten. Um den Wahrheitsgehalt solcher 
Behauptungen zu überprüfen, messen wir einfach das Verhalten der Programme. 
Um aber auch die Auswirkungen der gemachten Veränderungen zu verstehen und sicher zu gehen, dass 
tatsächlich das passiert was passieren soll, müssen wir uns den vom 
Compiler generierten Code anschauen. 
Was bei Low-Level Sprachen wie C und C++ Assemblercode und bei JVM-Spachen Java-Bytecode wäre, ist bei 
dem Glasgow Haskell Compiler (GHC) *Core*.

## Laufzeitverhalten von Haskellprogrammen messen ##

Über [Kommandozeilenparameter](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/runtime-control.html) ist es möglich die GHC Laufzeitumgebung eines Programmes zu manipulieren. 
Man kann die Stack- und Heapgröße verändern, den Garbage Collector anpassen, oder auch einfach nur 
eine Zusammenfassung über das Laufverhalten eines Programmes am Ende ausgeben lassen. Dies 
geschieht wenn man es mit `+RTS -s -RTS` aufruft.

{% highlight haskell %}
sum :: Num a => [a] -> a
sum [] = 0
sum (x:xs) = x + sum xs

main :: IO () 
main = do
  let n = 1000000 -- 1.000.000
  print $ sum [1 .. n]

-- Stark gekürzte Ausgabe von "$MAIN +RTS -s -RTS"
  89 MB total memory in use (0 MB lost due to fragmentation)
  Total   time    0.23s  (0.26s elapsed)
  %GC     time    77.9%  (77.8% elapsed)
  Productivity  22.1% of total user, 19.8% of total elapsed
{% endhighlight %}
Wir sehen also eine Zusammenfassung über das Speicherverhalten sowie über die Laufzeit des Programmes.
Der höchste Speicherverbrauch des Programmes lag bei 89MB, es lief 0,23 Sekunden lang, verbrachte 22,1% der 
Laufzeit mit Berechnungen und 77,9% der Zeit lief der Garbage Collector. Den Großteil der Zeit 
war unser Programm also damit beschäftigt nicht mehr gebrauchten Speicherplatz
frei zu geben anstatt zu Rechnen. Unser Programm hat ein [space leak](https://wiki.haskell.org/Memory_leak).
Erhöhen wir den Wert von `n` um das Zehnfache wird es noch schlimmer:
{% highlight haskell %}
n = 10.000.000:
  971 MB total memory in use (0 MB lost due to fragmentation)
  Total   time    2.09s  (2.32s elapsed)
  %GC     time    79.7%  (80.4% elapsed)
  Productivity  20.3% of total user, 18.3% of total elapsed
{% endhighlight %}

Hier dagegen das Verhalten der endrekursiven strikten Variante für beide Werte von `n`:
{% highlight haskell %}
sum''' :: Num a => [a] -> a
sum''' = go 0
  where
    go !acc [] = acc
    go !acc (x:xs) = go (acc + x) xs

-- Stark gekürzte Ausgaben von "$MAIN +RTS -s -RTS"
n = 1.000.000:
  1 MB total memory in use (0 MB lost due to fragmentation)
  Total   time    0.06s  (0.07s elapsed)
  %GC     time    1.0%   (2.7% elapsed)
  Productivity  99.0% of total user, 96.1% of total elapsed

n = 10.000.000:
  1 MB total memory in use (0 MB lost due to fragmentation)
  Total   time    0.36s  (0.36s elapsed)
  %GC     time    0.9%   (2.4% elapsed)
  Productivity  99.1% of total user, 98.4% of total elapsed
{% endhighlight %}
Eindeutig verbrauchen beide Varianten konstant wenig Speicher (1MB) und verbringen 
99% der Laufzeit mit der Berechnung. Genau so wie es sein soll!

Wir hatten zudem behauptet, dass die verschiedenen Ganzzahlentypen Einfluss auf die Laufzeit haben.
Hier also die Ergebnisse für `Integer` und `Int` getypte Eingabelisten:
{% highlight haskell %}
n = 1.000.000.000 (Integer)
  1 MB total memory in use (0 MB lost due to fragmentation)
  Total   time  28.93s  (29.27s elapsed)
  %GC     time  0.6%    (2.3% elapsed)
  Productivity  99.4% of total user, 98.3% of total elapsed

n = 1.000.000.000 (Int)
  1 MB total memory in use (0 MB lost due to fragmentation)
  Total   time  18.13s  (18.50s elapsed)
  %GC     time  0.5%    (3.5% elapsed)
  Productivity  99.5% of total user, 97.5% of total elapsed
{% endhighlight %}

Somit haben wir unsere Vermutungen durch Messen bestätigt. 
Um sicher zu gehen, dass unsere Optimierungen anschlagen und nicht irgendetwas anderes 
für die verbesserte Laufzeit verantwortlich ist, schauen wir uns den von GHC generierten Code an.

## GHC Core ##

[Core](https://ghc.haskell.org/trac/ghc/wiki/Commentary/Compiler/CoreSynType) ist der Name der 
Zwischensprache, auf die GHC Haskell im ersten Schritt transformiert. Interessant ist Core für uns, 
da es sich dabei um eine explizit getypte funktionale Sprache handelt, bei der man zusätzlich noch 
den Auswertungszeitpunkt von Ausdrücken ablesen kann. Somit ist es uns also möglichen unsere gemachten 
Behauptungen bezüglich der vorgenommen Optimierung durch die Analyse des erzeugten Core-Codes zu 
überprüfen. 
 
{% highlight haskell %}
-- von https://ghc.haskell.org/trac/ghc/wiki/Commentary/Compiler/CoreSynType 
type CoreExpr = Expr Var

data Expr b -- "b" for the type of binders, 
  = Var   Id
  | Lit   Literal
  | App   (Expr b) (Arg b)
  | Lam   b (Expr b)
  | Let   (Bind b) (Expr b)
  | Case  (Expr b) b Type [Alt b]
  | Cast  (Expr b) Coercion
  | Tick  (Tickish Id) (Expr b)
  | Type  Type

type Arg b = Expr b
type Alt b = (AltCon, [b], Expr b)

data AltCon = DataAlt DataCon | LitAlt  Literal | DEFAULT

data Bind b = NonRec b (Expr b) | Rec [(b, (Expr b))]
{% endhighlight %}
An der Definition von Core kann man die typischen Konstrukte funktionaler Programmiersprachen erkennen. 
Es gibt unter anderem `Lam`bda Abstraktionen, `Let` bindings, und Funktions-`App`likationen. Von besonderem
Interesse für uns ist jedoch das `Case` Konstrukt, denn nur hier findet eine Auswertung statt. `Case` ist, im Gegensatz zu Haskell selbst, strikt! Veranschaulichen wir uns dessen Bedeutung:

{% highlight haskell %}
sum :: Num a => [a] -> a
sum [] = 0
sum (x:xs) = x + sum xs

-- Vereinfachter Core von sum
-- Generiert durch `ghc -ddump-simpl Main.hs`
sum =
  \ ($num_a :: Num a)
    (list :: [a]) ->
    case list of _ {
      [] -> fromInteger $num_a (__integer 0);
      : x xs -> + $num_a x (sum $num_a xs)
    }
{% endhighlight %}
Der generierte Core-Code lässt sich wie folgt interpretieren: `sum` ist eine rekursive Funktion,
 welche zwei Parameter hat: `$num_a` ist die `Num`-Instanz vom Typen `a` und `list` unsere Eingabeliste. 
Typklassen werden also auf Core-Ebene explizit als zusätzlicher Parameter zur ursprünglichen Funktion übergeben. Das 
`case` in der nächsten Zeile inspiziert die Form von `list`. Sie wird so weit ausgewertet wie nötig
um auf die leere Liste bzw. Kopf und Schwanz matchen zu können. 
Im nicht-leeren Fall wird die Addition aus der entsprechenden `Num`-Instanz auf den Kopf und dem 
rekursivem Aufruf ausgeführt. Zur Erinnerung: Das ist der Code mit dem Stackoverflow Speicherproblem. 
Im Vergleich dazu die strikte, endrekursive Variante:

{% highlight haskell %}
sum''' :: Num a => [a] -> a
sum''' = go 0
  where
    go !acc [] = acc
    go !acc (x:xs) = go (acc + x) xs

-- Vereinfachter Core von go in sum'''
go =
  \ (acc :: a) (list :: [a]) ->
    case acc of acc1 { __DEFAULT ->
      case list of _  {
        [] -> acc1;
        : x xs ->
          go (+ $num_a x acc1) xs
      }
    }
{% endhighlight %}
Der Hauptunterschied zum vorherigen Core ist das `case` auf den Akkumulator `acc`. Dieses dient nicht wie
 das Anschließende auf die Liste zur Fallunterscheidung zwischen leerer und nicht-leerer Liste, sondern 
ausschließlich der Auswertung von `acc`. Dies erkennt man daran, dass es nur den `__DEFAULT`-Fall 
gibt. Das Ergebnis der Auswertung von `acc` wird an `acc1` gebunden und an den rekursiven Aufruf von 
`go` übergeben. Der Ausdruck `(+ $num_a x acc1)` wird also im darauffolgendem Rekursionsschritt 
reduziert. Somit können wir also sicher sein, dass wir keine Kette aufbauen, die noch aus auszuführenden 
Additionen besteht, und dementsprechend mit konstant großem Stackspeicherverbrauch operieren.

## Typ-Annotationen und deren Auswirkungen ##
Die vorherigen Beispiele zeigen den generierten Core-Code der Funktionen in Isolation. 
Da aber bei der Verwendung der Funktion mehr Informationen zum Übersetzungszeitpunkt zur Verfügung stehen,
kann GHC die Funktion auf ihre konkrete Anwedung optimieren.

{% highlight haskell %}
main :: IO ()
main = print $ sum''' [1, 2, 3]

-- Vereinfachter Core
main = print
  ...
  go =
    \ (acc :: Integer)
      (list :: [Integer]) ->
      case acc of acc1 { __DEFAULT ->
        case list of _ {
          [] -> acc1;
          : x xs ->
            go (+ $num_Integer x acc1) xs
        }
  }
{% endhighlight %}
Hier hat GHC nun eine Kopie von `sum'''` in `main` eingefügt. Dadurch sparen wir schon mal den Funktionsaufruf
selbst, wichtiger jedoch ist, dass der Compiler die Funktion inklusive ihrer Argumente zu sehen bekommt 
und dadurch weitere Optimierung durchführen kann. Hier zum Beispiel wurde die Liste `[1, 2, 3]` als Liste 
aus `Integer`-Werten interpretiert. Dadurch wurde der `Num`-Typenklassen Parameter gelöscht, und `go` auf
`Integer` spezialisiert.

Noch besseren Code können wir im Moment nur erzeugen, indem wir unsere Liste explizit auf den Typen 
`[Int]` setzen. Der Beweis folgt wie immer im Core:
{% highlight haskell %}
main :: IO ()
main = print $ sum''' [1, 2, 3 :: Int]

-- Vereinfachter Core
main = print
  ...
  go =
    \ (acc :: Int) (list :: [Int]) ->
      case acc of acc1 { I# acc2 ->
        case list of _ {
          [] -> acc1;
          : x xs ->
            go (+ $num_Int x acc1) xs
        }
  }
{% endhighlight %}
Wie erwartet sind der Akkumulator `acc` sowie die Elemente der Liste vom Typ `Int`. Neu ist jedoch,
dass das `case` auf `acc` nicht mehr auf `__DEFAULT` sondern auf `I# acc2` ableitet.
Was es damit auf sich hat und wie wir unser Beispiel noch weiter optimieren können dann im nächsten Teil!
