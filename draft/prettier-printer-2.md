---
layout: post
description: "A prettier printer -- implementation details"
title: "Pretty-Printing -- Laziness FTW"
author: kaan-sahin
tags: ["pretty", "print", "printing", "haskell", "wadler"]
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
und verstehen, warum -- trotz der Erzeugung von Tausenden bis Millionen
möglichen Layouts -- der Algorithmus sehr effizient ist.

<!-- more start -->

*Hinweis: Es ist sinnvoll, den vorherigen Blogpost [Aufgehübscht! -
Pretty-Printing](https://funktionale-programmierung.de/2020/08/06/prettier-printer.html)
gelesen zu haben. Begleitender Quellcode zum Pretty-Printer ist auf
[Github](https://github.com/kaaninho/a-prettier-printer) zu finden.*

## Wie funktioniert die Dokumentensprache?

Zur Erinnerung hier noch einmal die sechs Operatoren, mit denen wir Dokumente
beschreiben bzw. rendern können:

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

In Haskell-Code übertragen bedeutet das, dass `Doc` ein gemischter Datentyp aus
`Text` und `Line` ist. Zusätzlich kommt noch, aus technischem Grund, das "leere"
Dokument `Nil` hinzu:

```haskell
Doc = Nil
    | Text String Doc 
    | Line Int Doc
```

Dabei besteht `Text` wiederum aus zwei Teilen: einem `String` (das ist eine
Zeichenkette auf einer Zeile) und einem darauf folgendem weiteren Dokument. Ein
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

### Operatoren unter die Lupe

Da die Beschreibung eines Dokuments so simpel ist, ist es auch genau so simpel,
diese in tatsächlichen Text -- also eine ausdruckbare Zeichenkette -- zu
überführen. `layout` muss nur wissen, wie es aus `Text`-, `Line`- und
`Nil`-Objekten (?Werten?) einen String macht:

```haskell
layout :: Doc -> String
layout Nil               = ""
layout (Text string doc) = string ++ layout doc
layout (Line indent doc) = "\n" ++ (take indent $ repeat ' ') ++ layout doc
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
falsch erscheinende Definition auf: `nest` rückt `Text` nicht ein! Dies bringt
den technischen Vorteil mit sich, nur einen Verknüpfungsoperator zu benötigen.
Dafür benötigen wir jedoch einen expliziten Umbruch vor `Text`, um diesen
tatsächlich einzurücken.

Um zwei Dokumente aneinanderzufügen, müssen wir schlussendlich noch `<>`
definieren. Hier sehen wir, wie aus den "flachen" (assoziativen)
Operatoren-Verknüpfungen die verschachtelten Konstruktoraufrufe werden:

```haskell
(<>) :: Doc -> Doc -> Doc
Text str doc <> doc2 = Text str (doc <> doc2)
Line n doc <> doc2   = Line n (doc <> doc2)
Nil <> doc           = doc
```

Der Beispielparagraph von oben sieht mit Operatoren so aus:

```haskell
doc3 = text "Hallo" <> line <> line
       <> text "Wir schreiben schöne Texte, die"
       <> nest 2 (line <> (text "- eingerückt sind")
                  <> text "- und Zeilenumbrüche"
                  <> nest 2 (line <> text "- enthalten"))
```

### Von einem Dokument zu mehreren -- `group`

Die Operatoren zur Beschreibung eines Dokuments wären geschafft! Wie im
vorherigen Blogpost erwähnt, spielt `group` eine wichtige Rolle: Mit `group`
lassen sich alternative Layouts eines Dokuments erstellen. Dafür haben wir
damals die Definition von `Doc`, ohne es im Code aufzuschreiben, erweitert: Ein
Dokument kann jetzt auch eine Sammlung mehrerer (äquivalenter!) Dokumente sein.

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

`group` nimmt ein Dokument entgegen, und gibt eine Sammlung bestehend aus dem
übergebenen Dokument, und einem dem übergebenen *äquivalenten* Dokument zurück.
Im zweiten Dokument werden alle Umbrüche und Einrückungen entfernt und durch
Leerzeichen ersetzt. "Äquivalent" meint hier also "gleich bis auf
Umbrüche/Einrückung". Wir implementieren `group` mit Hilfe zweier
Hilfsfunktionen:

```haskell
(<|>) :: Doc -> Doc -> Doc
flatten :: Doc -> Doc
```

`<|>` übernimmt dabei den Job, zwei Dokumente zu einer Vereinigung von
Dokumenten zu machen. Hierbei ist wichtig zu beachten: 

> Die Dokumente haben eine Reihenfolge, links steht immer das Dokument, das in
> der ersten Zeile *mehr* (oder gleich viele) Zeichen stehen hat, als das
> andere.

Warum wir diese Ordnung benötigen, wird später ersichtlich. Die Implementierung
ist denkbar einfach:

```haskell
doc1 <|> doc2 = Union doc1 doc2
```

`flatten` ersetzt in einem Dokument die Umbrüche und Einrückung durch
Leerzeichen:

```haskell
flatten Nil = Nil
flatten (Text str doc) = Text str (flatten doc)
flatten (Line n doc) = Text " " (flatten doc)
flatten (Union doc1 doc2) = flatten doc1
```

Die letzte Zeile ist besonders einfach, da wir ja oben gefordert haben, dass nur
äquivalente Dokumente zu einer Sammlung von Dokumenten zusammengefasst werden
dürfen. Damit gilt:

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
obige Bedingung an `Union` erfüllt, muss beim Benutzen des Pretty-Printers der
Bedingung keine Achtung gegeben werden.

## Aus der Masse die Klasse

Um ein Gefühl dafür zu bekommen, wie so eine große Anzahl an Dokumenten, die
erzeugt werden, zustande kommt, hier ein einfaches Rechenbeispiel. Im letzten
Blogpost haben wir uns die Funktion `xmlToDOC` angeschaut, die aus der
Repräsentation eines XML-Elements eine Sammlung von Dokumenten erstellt. Hier
sind zwei `group`-Aufrufe zu sehen und in `tagToDOC` versteckt sich noch ein
dritter:

```haskell
xmlToDOC :: XML -> DOC
xmlToDOC (Element name attributes xmls)
      = group (tagToDOC name attributes
               </> nest 2 (group line <> concatDOCs (map xmlToDOC xmls)) 
               </> text "</" <> text name <> text ">")
```

Nehmen wir den einfachsten Fall an: Nur ein Element, das keine Kinder hat
(`xmls` ist also eine leere Liste). Dann erzeugt `xmlToDOC` 6 Dokumente. Ein
weiterer Fall: Ein Element mit zwei Kinderknoten: 

Die Funktion `pretty` nimmt eine Maximalbreite und eine Dokumentensammlung
entgegen und gibt das bestmögliche Dokument zurück (im Sinne von: überschreitet
nicht die Maximalbreite aber nutzt die Zeilen größtmöglich aus). Dazu benötigen
wir `best`, `better` und `fits`:

```haskell
best :: Int -> Int -> Doc -> Doc
best width charsLeft Nil               = Nil
best width charsLeft (Text str doc)    = Text str (best width (charsLeft - (length str)) doc)
best width charsLeft (Line n doc)      = Line n (best width (width - n) doc)
best width charsleft (Union doc1 doc2) = better (best width charsLeft doc1)
                                                (best width charsLeft doc2)
```


<!-- more end -->
