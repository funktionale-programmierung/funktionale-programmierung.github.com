---
layout: post
description: "A prettier printer - implementation details"
title: "Pretty-Printing II - Laziness FTW"
author: kaan-sahin
tags: ["pretty", "printing", "haskell", "wadler", "laziness", "lazy"]
---

Um ein (verschachteltes) Objekt komfortabel untersuchen zu können, wird eine
gute Darstellung desselben in Form von Text benötigt. Pretty-Printer versuchen
genau das: Dinge so auf den Bildschirm zu drucken, dass die interne Struktur auf
einen Blick ersichtlich ist. 

Im letzten Blogpost [Aufgehübscht! -
Pretty-Printing](https://funktionale-programmierung.de/2020/08/06/prettier-printer.html)
haben wir uns den Pretty Printer von [Philip
Wadler](http://homepages.inf.ed.ac.uk/wadler/)s Paper "A prettier printer"
(1997) angeschaut und verstanden, wie wir allein mit seinen sechs Operatoren
eine Dokumentensprache beschreiben und damit schöne Pretty-Prints erstellen
können. In diesem Post wollen wir uns die eigentliche Implementierung ansehen
und verstehen, warum &ndash; trotz der Erzeugung von vielen möglichen Layouts
&ndash; der Algorithmus sehr effizient ist.

<!-- more start -->

*Hinweis: Es ist sinnvoll, den vorherigen Blogpost [Aufgehübscht! -
Pretty-Printing](https://funktionale-programmierung.de/2020/08/06/prettier-printer.html)
gelesen zu haben. Begleitender Quellcode zum Pretty-Printer ist auf
[Github](https://github.com/kaaninho/a-prettier-printer) zu finden.*

## Wie funktioniert die Dokumentensprache?

Zur Erinnerung hier noch einmal die sechs Operatoren, mit denen wir Dokumente
beschreiben und rendern können:

```haskell
(<>)       :: Doc -> Doc -> Doc
nil        :: Doc
text       :: String -> Doc
line       :: Doc
nest       :: Int -> Doc -> Doc

layout     :: Doc -> String
```

Im vorherigen Blogpost haben wir uns die Verwendung der Operatoren angeschaut,
aber übergangen, was `Doc`, also das zusammengebaute Dokument, eigentlich ist.

### Was ist ein Dokument?

Wenn wir uns die simpelste Definition der Struktur eines Textes überlegen,
könnten wir bei Folgendem herauskommen:

> Ein Text besteht aus Zeichenketten und Umbrüchen mit eventuellen Einrückungen.

In Haskell kann man `Doc` durch einen gemischten Datentyp aus `Text` und `Line`
ausdrücken. Zusätzlich kommt noch, aus technischem Grund, das "leere" Dokument
`Nil` hinzu:

```haskell
Doc = Nil
    | Text String Doc 
    | Line Int Doc
```

Dabei besteht `Text` wiederum aus zwei Teilen: einem `String` (das ist eine
Zeichenkette auf einer Zeile) und einem darauf folgenden weiteren Dokument. Ein
`Line`-Element besteht ebenso aus zwei Teilen: einer Zahl `n`, die die
Einrückung um `n` Zeichen beschreibt und einem weiteren Dokument. Wie wir sehen,
ist die Definition von `Doc` rekursiv! Damit die Rekursion ein Ende finden kann,
benötigen wir das leere Dokument `Nil`. Dieses kennzeichnet demnach das Ende
eines Textes. Der folgende Text

```verbatim
Hallo!

Wir schreiben schöne Texte, die
  - eingerückt sind
  - und Zeilenumbrüche
    - enthalten
```

sieht als Dokument `Doc` so aus:

```haskell
doc = Text "Hallo!" 
      (Line 0
       (Line 0
        (Text "Wir schreiben schöne Texte, die"
         (Line 2
          (Text "- eingerückt sind"
           (Line 2 
            (Text "- und Zeilenumbrüche"
             (Line 4
              (Text "- enthalten" Nil)))))))))
```

### Operatoren unter der Lupe

Da die Beschreibung eines Dokuments so simpel ist, ist es auch genauso simpel,
diese in eine ausdruckbare Zeichenkette zu überführen. `layout` muss nur
wissen, wie es aus `Text`-, `Line`- und `Nil`-Werten einen String macht:

```haskell
layout :: Doc -> String
layout Nil               = ""
layout (Text string doc) = string ++ layout doc
layout (Line indent doc) = "\n" ++ replicate indent ' ' ++ layout doc
```

Die drei Operatoren `nil`, `text` und `line` erzeugen entsprechende Dokumente so:

```haskell
nil      = Nil
text str = Text str Nil
line     = Line 0 Nil
```

`line` ist in der Dokumentensprache nur für Umbrüche zuständig. `nest` übernimmt
die Aufgabe der Einrückung und ist komfortabler als nur `Line` dafür zu
benutzen, da das Verschachteln von `nest`s funktioniert, wie wir in der vierten
Zeile sehen:

```haskell
nest :: Int -> Doc -> Doc
nest n Nil               = Nil
nest n (Text str doc)    = Text str (nest n doc)
nest n (Line indent doc) = Line (n + indent) (nest n doc)
```

Der geneigten Leserin fällt in der dritten Zeile eine auf den ersten Blick
falsch erscheinende Definition auf: `nest` erzeugt im `Text`-Fall kein
`Line`-Element, rückt also `Text` nicht ein! Das heißt wir benötigen einen
expliziten Umbruch vor `Text`, um diesen tatsächlich einzurücken.

Um zwei Dokumente aneinanderzufügen, müssen wir schlussendlich noch `<>`
definieren. Hier sehen wir, wie aus den "flachen" (assoziativen)
Operatoren-Verknüpfungen die verschachtelten Konstruktoraufrufe werden:

```haskell
(<>) :: Doc -> Doc -> Doc
Text str doc <> doc2 = Text str (doc <> doc2)
Line n doc <> doc2   = Line n (doc <> doc2)
Nil <> doc           = doc
```

Der Beispielabsatz von oben sieht mit Operatoren so aus:

```haskell
doc3 = text "Hallo" <> line <> line
       <> text "Wir schreiben schöne Texte, die"
       <> nest 2 (line <> (text "- eingerückt sind")
                  <> text "- und Zeilenumbrüche"
                  <> nest 2 (line <> text "- enthalten"))
```

### Von einem Dokument zu mehreren &ndash; `group`

Die Operatoren zur Beschreibung eines Dokuments wären geschafft! Wie im
vorherigen Blogpost erwähnt, spielt `group` eine wichtige Rolle: mit `group`
lassen sich alternative Layouts eines Dokuments erstellen. Später wird dann aus
diesen verschiedenen Layouts das beste ausgewählt. Für alternative Layouts haben
wir damals die Definition von `Doc`, ohne es im Code aufzuschreiben, erweitert:
Ein Dokument kann jetzt auch eine Sammlung mehrerer (äquivalenter!) Dokumente
sein.

Um das in unserer Definition widerzuspiegeln, bedarf es glücklicherweise nur
einer weiteren Zeile:

```haskell
Doc = Nil
    | Text String Doc 
    | Line Int Doc
    | Union Doc Doc
```

Da `Doc` sich geändert hat, müssen wir alle Funktionen erweitern, die darauf
operieren:

```haskell
nest n (Union doc1 doc2) = Union (nest n doc1) (nest n doc2)

(Union doc1 doc2) <> doc = Union (doc1 <> doc) (doc2 <> doc)
doc <> (Union doc1 doc2) = Union (doc <> doc1) (doc <> doc2)
```

`group` nimmt ein Dokument entgegen und gibt eine Sammlung bestehend aus dem
übergebenen Dokument sowie einem zum übergebenen *äquivalenten* Dokument zurück.
Im zweiten Dokument werden alle Umbrüche und Einrückungen entfernt und durch
Leerzeichen ersetzt. "Äquivalent" meint hier also "gleich bis auf
Umbrüche/Einrückung". Wir implementieren `group` mit zwei Hilfsfunktionen:

```haskell
(<|>) :: Doc -> Doc -> Doc
flatten :: Doc -> Doc
```

`<|>` übernimmt dabei den Job, zwei Dokumente zu einer Vereinigung von
Dokumenten zu machen. Hierbei ist wichtig zu beachten: 

> Die Dokumente haben eine Reihenfolge: Links steht immer das Dokument, das in
> der ersten Zeile *mehr* (oder gleich viele) Zeichen stehen hat als das
> andere.

Warum wir diese Ordnung benötigen, wird später ersichtlich. Die Implementierung
ist denkbar einfach:

```haskell
doc1 <|> doc2 = Union doc1 doc2
```

`flatten` ersetzt in einem Dokument die Umbrüche und Einrückung durch
Leerzeichen:

```haskell
flatten Nil               = Nil
flatten (Text str doc)    = Text str (flatten doc)
flatten (Line n doc)      = Text " " (flatten doc)
flatten (Union doc1 doc2) = flatten doc1
```

Die letzte Zeile ist besonders einfach, da wir ja oben gefordert haben, dass nur
äquivalente Dokumente zu einer Sammlung von Dokumenten zusammengefasst werden
dürfen. Damit gilt für zwei Dokumente aus einem `Union doc1 doc2`:

```haskell
flatten doc1 = flatten doc2
```

Nun können wir `group` definieren:

```haskell
group :: Doc -> Doc
group doc = flatten doc <|> doc
```

Hier bekommen wir direkt ein Geschenk: Dadurch, dass wir `flatten` und `<|>` der
Endnutzerin nicht zur Verfügung stellen, und `group` durch seine Definition die
obige Bedingung an `Union` erfüllt, muss der Endnutzer beim Benutzen des
Pretty-Printers der Bedingung keine Acht geben.

## Aus der Masse die Klasse -- das Beste unter vielen

Durch `group` entstehen also viele Layouts mit verschiedenen Anzahlen an
Umbrüchen und Einrückungen. Jetzt müssen wir nur noch das Beste aus diesen
auswählen:

> Das _Beste_ hierbei ist das, das jede Zeile möglichst gut ausnutzt,
> aber dennoch die vorgegebene Breite nicht überschreitet.

Die Funktion `best` macht genau das. Sie benötigt als Parameter die vorgegebene
Maximalbreite und die Anzahl an Zeichen, die auf der aktuellen Zeile schon
verbraucht sind:

```haskell
best :: Int -> Int -> Doc -> Doc
best width charsUsed Nil               = Nil
best width charsUsed (Text str doc)    = Text str (best width (charsUsed + length str) doc)
best width charsUsed (Line n doc)      = Line n (best width (width - n) doc)
best width charsUsed (Union doc1 doc2) = better width charsUsed
                                                (best width charsUsed doc1)
                                                (best width charsUsed doc2)
```

Wir sehen, dass `best` höchst rekursiv ist, die Dokumente auseinandernimmt und
jedes Objekt der Dokumentensprache untersucht. Interessant ist vor allem der
`Union`-Fall. Hier wird mithilfe von `better` zwischen zwei möglichen Layouts
entschieden. `better` ist eine einfache Verzweigung, die als Vergleichsoperator
`fits` benutzt:

```haskell
better :: Int -> Int -> Doc -> Doc -> Doc
better width charsUsed doc1 doc2 =
  if fits (width - charsUsed) doc1 then doc1 else doc2

fits :: Int -> Doc -> Bool
fits charsLeft _ | charsLeft < 0 = False
fits _ Nil                       = True
fits _ (Line _ doc)              = True
fits charsLeft (Text str doc)    = fits (charsLeft - length str) doc
```

Damit haben wir nun alle Zutaten, um den Pretty-Printer fertigzustellen. Um ein
Dokument schön auszudrucken, können wir `pretty` benutzen:

```
pretty :: Int -> Doc -> Doc
pretty width doc = best width 0 doc
```

Sehr gut! Aber halt mal, der `best`-Algorithmus untersucht viele Dokumente und
schaut darin auf jedes einzelne Objekt und steigt rekursiv ab und auf und ab und
auf ... ist das nicht höchst ineffizient?

## Faul aber clever

Tatsächlich ist der Algorithmus aus zwei Gründen dennoch effizient:

1. die geschickte Implementierung von Dokumenten bzw. `Union`
2. (Haskells) Laziness

Zu Punkt 1: In der Definition von `better` entscheiden wir uns beim `if` direkt
für das erste Dokument, wenn es auf die Maximalbreite passt. Warum ist das
korrekt? Das allererste ("linkeste") Dokument ist das Dokument, das am meisten
Platz auf den Zeilen ausnutzt, da wir bei `<|>` oben eben genau das gefordert
hatten! In gängigen Programmiersprachen ist `if` als "Kurzschlussauswertung"
(short-circuit evaluation) implementiert, das heißt, wenn die Bedingung wahr
ist, wird die Alternative erst gar nicht ausgewertet. Damit verschwinden bei uns
auf einen Schlag etliche Dokumente, die gar nicht erst beachtet werden.

Zu Punkt 2: Haskell macht nicht nur Kurzschlussauswertung, sondern geht noch
viel weiter: es wertet alle Daten nur nach Bedarf aus. Der Fachbegriff
dazu lautet *Lazy Evaluation*. Das heißt bei uns, dass Dokumente oft gar nicht vollständig
aufgebaut werden, sondern &ndash; da schon unterwegs klar ist, dass sie nicht auf die
verfügbare Breite passen &ndash; direkt verworfen. Das können wir zum Beispiel bei
`better` bzw. bei `fits` im Fall von `Text` sehen: wenn `(charsLeft - length)`
kleiner als Null ist, wird `doc` nicht weiter ausgewertet, sondern direkt
`False` zurückgegeben, und mit dem rechten Dokument weitergemacht.

### Wir zählen `best`-Aufrufe

Um einen Eindruck zu bekommen, wie viel Ersparnis sich durch die Laziness
ergibt, schauen wir uns einen `pretty`-Aufruf mit eingeschalteter und
ausgeschalteter Laziness an. Hierfür nehmen wir folgende, wirklich
überschaubare, Clojure-Map her:

```clojure
{:aaa {:bb 1 :cccc {:aa 12}}
 :ddd 24
 :eeeee 122
 :ff 12}
```

und pretty-printen sie mit einer Maximalbreite von 20 Zeichen. Heraus kommt:

```clojure
{:aaa {:bb 1
       :cccc {:aa 12 }
      }:ddd 24
 :eeeee 122 :ff 12 }
```

Bei strikter Auswertung wurden hierfür 1640 rekursive Aufrufe von `best`
benötigt! Mit Lazy Evaluation nur 120. Die große Map

```clojure
{:a {:a 0, :b 1, :c 2, :d 3, :e 4},
 :b {:a 0, :b 1, :c 2, :d 3, :e 4},
 :c {:a 0, :b 1, :c 2, :d 3, :e 4},
 :d {:a 0, :b 1, :c 2, :d 3, :e 4},
 :e {:a 0, :b 1, :c 2, :d 3, :e 4}}
```

zwingt bei strikter Auswertung das System sogar in die Knie und stürzt ab. Mit
Lazy Evaluation sind es nur 636 rekursive Aufrufe.

## Fazit

Im heutigen Blogpost haben wir uns die Implementierung der algebraischen
Operatoren des Pretty Printers von Philip Wadler angeschaut. Die rekursive
Dokumentenstruktur hat es uns ermöglicht, kurzen, prägnanten, gut verständlichen
Code zu schreiben. Haskells Lazy Evaluation und die geschickte Wahl von
Bedingungen an die Dokumentensammlungen machen den Algorithmus dabei nicht nur
schlank, sondern auch noch sehr effizient.
<!-- more end -->
