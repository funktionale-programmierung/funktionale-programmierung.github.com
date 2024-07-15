---
layout: post
description: "A prettier printer"
title: "Aufgehübscht! - Pretty-Printing"
author: kaan-sahin
tags: ["Haskell", "DSLs"]
---

Um ein (verschachteltes) Objekt komfortabel untersuchen zu können, wird eine
gute Darstellung desselben in Form von Text benötigt. Pretty-Printer versuchen
genau das: Dinge so auf den Bildschirm zu drucken, dass die interne Struktur auf
einen Blick ersichtlich ist. Wir schauen uns heute den Pretty-Printer aus
[Philip Wadlers](http://homepages.inf.ed.ac.uk/wadler/) Paper ["A prettier
printer"](http://homepages.inf.ed.ac.uk/wadler/papers/prettier/prettier.pdf)
(1997) an, dessen Besonderheit die Anwedung von Algebra auf Design und
Implementierung ist. Daran werden wir ein weiteres Mal sehen, dass
Datenmodellierung und Abstraktion der funktionalen Programmierung zu wunderbar
einfachem und prägnantem Quellcode führen, der komplexe Domänen elegant
beschreibt.

<!-- more start -->

*Hinweis: Der komplette Code des Pretty-Printers ist auf
[Github](https://github.com/kaaninho/a-prettier-printer)
zu finden. Viel Spaß beim Ausprobieren!

## The Good, the Bad and the Ugly

Wir werden im Folgenden LISP-Ausdrücke betrachten. Bei ihnen ist es von
besonderer Wichtigkeit, die interne Struktur ersichtlich zu machen und damit den
Sinn von guten Pretty-Printern zu motivieren. Hier ein gelungenes, ein nicht
ganz gelungenes und ein ganz und gar nicht gelungenes (Pretty-)Printing der
Fakultätsfunktion:

```lisp
(DEFUN FACTORIAL (X)
       (COND ((= X 0) 1)
             ((* (FACTORIAL (- X 1))
                 X))))

(DEFUN 
 FACTORIAL
 (X)
 (COND 
  ((= X 0) 1)
  ((* (FACTORIAL 
       (- X 1))
      X))))

(DEFUN FACTORIAL (X) (COND ((=
X 0) 1) ((* (FACTORIAL (- X 1)
) X))))
```

Fangen wir unten an: Es wurde kein Pretty-Printer verwendet, sondern der Code
einfach auf die verfügbare Breite von 30 Zeichen gedruckt. Sehr ungeschickt ist,
dass wegen fehlender Einrückung nicht ersichtlich ist, welcher Ausdruck Teil
eines anderen Ausdrucks ist. Im zweiten Beispiel ist dies schon wesentlich
besser, da verschachtelte Ausdrücke jeweils um ein Zeichen eingerückt sind.
Zudem erzeugen die Zeilenumbrüche noch mehr Struktur. Hier ist jedoch zu
bemängeln, dass zu "aggressiv" umgebrochen wird. Das hat schlechtere Lesbarkeit
und mehr Zeilen im Pretty-Print zur Folge. Das erste Beispiel lässt den Kopf und
Rumpf einer Funktionsdefinition besonders gut erkennen.

Als weiteres Beispiel XML-Code in drei geprinteten Varianten (dieses Mal mit 30
Zeichen Maximalbreite):

```xml
<p
  color="red" font="Times"
  size="10"
>
  Here is some
  <em> emphasized </em> text.
  Here is a
  <a href="http://ww.eg.com" >
    link
  </a>
  elsewhere
</p>

<p
  color=
    "red" 
  font=
    "Times"
  size=
    "10"
>
Here is some
<em> 
  emphasized 
</em> 
text. Here is a
<a
  href="http://www.eg.com/"
> 
link 
</a>
elsewhere.
</p>

<p color="red" font="Times"
size="10" > Here is some <em>
emphasized </em> text. Here is
a <a href="http://www.eg.com/"
> link </a> elsewhere. </p>

```

Das erste Beispiel zeigt wieder den Idealfall: Der gesamte `em`-Ausdruck wird in
eine Zeile in den Fließtext eingebettet. Es ist kein Umbruch nötig, da die
Maximalbreite nicht überschritten wird. Die Attribute des `p`-Elements werden
zunächst nebeneinander, so lang es Platz hat, und dann untereinander sortiert.
Hier wird also nicht nur stur ein "Vorgehen" befolgt ("breche pro
Attribut-Wert-Paar um"), sondern der vorhandene Platz sinnvoll ausgenutzt. Die
beiden anderen Beispiele sind aus obigen Gründen keine gelungenen Layouts.

Obwohl die Beispiele aus zwei unterschiedlichen Domänen kommen
(Programmiersprache Common Lisp und Auszeichnungssprache XML), können beide mit
dem hier vorgestellten Pretty-Printer behandelt werden. Das wird dadurch
ermöglicht, dass eine Metasprache für Dokumente, die schön ausgedruckt werden
sollen, entwickelt wird. Dann erst wird für den Anwendungsfall bestimmt, wie man
Objekte in diese Metasprache übersetzt. Die Vorgehensweise, eine beschreibende
Zwischensprache zu entwerfen &ndash; eine sogenannte *domänenspezifische
Sprache* (domain specific language im Englischen, kurz DSL), ist in der
funktionalen Programmierung gang und gäbe. Im Blogpost
[Systematisch eingebettete DSLs entwickeln in Clojure](https://funktionale-programmierung.de/2013/06/27/dsl-clojure.html)
werden DSLs ausführlich beschrieben.


## Strategie des Prettier Printers

Der Pretty-Printer nimmt eine Beschreibung eines Dokuments entgegen und erzeugt
daraus viele Layouts. Diese unterscheiden sich im Format bzgl. Umbrüchen und
Einrückungen. Dann wählt der Pretty-Printer aus dieser Menge von Layouts das
*beste* aus. Das *beste* hierbei ist das, das jede Zeile möglichst gut ausnutzt,
aber dennoch die vorgegebene Breite nicht überschreitet.

In diesem Blogpost wollen wir unser Augenmerk auf die Verwendung der
Pretty-Printer-DSL setzen. Wie die eigentliche DSL implementiert ist -- besonders
in Hinblick auf Effizienz (es könnten einige Tausend bis Millionen Layouts
entstehen) -- sehen wir in einem folgenden Blogpost.

## Die Pretty-Printer-Dokumentensprache

Unsere Pretty-Printer-DSL besteht aus Dokumenten und Kombinatoren, das heißt
Operatoren, die auf diesen Dokumenten arbeiten. Wir benutzen ab jetzt
Haskell-Syntax, um die Funktionssignaturen und Typen zu beschreiben.


### Sechs Operatoren für das Glück

Folgende fünf Operatoren stehen zur Verfügung, um ein Dokument in unserer
Pretty-Printer-Sprache zu beschreiben:

```haskell
(<>)       :: Doc -> Doc -> Doc
nil        :: Doc
text       :: String -> Doc
line       :: Doc
nest       :: Int -> Doc -> Doc
```

Dabei fügt `<>` zwei Dokumente aneinander (man sagt auch es "konkateniert" sie).
Das Resultat ist wieder ein Dokument. `nil` ist das sogenannte leere Dokument.
Beliebige Dokumente bleiben bei Konkatenation mit `nil` unverändert (etwas
mathematischer ausgedrückt bildet also die Menge aller Dokumente zusammen mit
der assoziativen Verknüpfung `<>` und dem neutralen Element `nil` ein Monoid).
`text` wandelt eine Zeichenkette in ein Dokument um, `line` beschreibt einen
Zeilenumbruch und `nest` rückt ein Dokument ein.

Hinzu kommt ein sechster Operator, `layout`, der ein als String gerendertes
Dokument zurückgibt:

```haskell
layout     :: Doc -> String
```

An den fünf Operatoren und `layout` ist die in der funktionalen Programmierung
häufig vorkommende Trennung von Beschreibung und Ausführung wunderbar
ersichtlich.

Das folgende, mit den obigen Operatoren beschriebene Dokument

```haskell
doc =
  text "<p>" <> 
  nest 2 (
    line <> text "<a>" <> text "This is a link" <> text "</a>" <>
    line <> text "Some text"
  ) <>
  line <> text "</p>"
```

wird mit Hilfe von `layout` so ausgedruckt:

```xml
<p>
  <a>This is a link</a>
  Some text
</p>
```

### Erst die Masse, dann die Klasse

Oben war die Rede von mehreren Layouts, aus denen das beste ausgewählt wird.
Hier ist bisher nichts davon zu sehen. Die Sprache benötigt dafür noch eine
Erweiterung des Begriffs "Dokument" und eine weitere Operation: Wenn ab jetzt
"Dokument" gesagt wird, meint dies eine Sammlung mehrerer möglicher Layouts
(desselben Dokuments). Diese müssen jedoch von derselben Struktur sein und
dürfen sich nur in Zeilenumbrüchen und Einrückungen unterscheiden. Doch wie
kommen wir überhaupt zu verschiedenen Variationen eines Layouts? `group` ist
hier der Schlüssel:

```haskell
group :: Doc -> Doc
```

`group` nimmt ein Dokument (Sammlung mehrerer Layouts) und fügt dieser Sammlung
ein neues Layout hinzu, in welchem jeder Zeilenumbruch durch ein Leerzeichen
ersetzt wird. Wenn wir also `group` auf unser oben definiertes Dokument `doc`
anwenden, käme das folgende Layout hinzu (nach Anwendung von `layout`):

```xml
<p> <a>This is a link</a> Some text </p>
```

Unser Pretty-Printer stellt zusätzlich die Funktion `pretty` bereit:

```haskell
pretty :: Int -> Doc -> Doc
```

Diese wählt aus der übergebenen Sammlung von Layouts das beste für die ebenfalls
übergebene Maximalbreite aus. Hier zur Verdeutlichung obiges Dokument im
Zusammenspiel mit `pretty` und verschiedenen Maximalbreiten:

```haskell
twoLayouts = group doc

layout $ pretty 30 twoLayouts

-- --> 
-- <p>
--   <a>This is a link</a>
--   Some text
-- </p>

layout $ pretty 60 twoLayouts

-- -->
-- <p> <a>This is a link</a> Some text </p>
``` 

Das einzeilige Layout ist für obigen Aufruf zu lang. Im unteren Aufruf
unterschreiten beide Layouts die geforderte Maximalbreite. Somit wird hier das
einzeilige gewählt, da dieses die (erste) Zeile größtmöglich ausnutzt.


## XML aufgehübscht!

Zum Schluss des Blogposts wollen wir uns noch anschauen, wie solch eine
Übersetzung von Domänencode, hier im speziellen XML-Code, in die
Pretty-Printer-DSL aussehen könnte. Wenn wir tatsächlichen XML-Code benutzen
wollten, müssten wir diesen zunächst parsen, um ihn in einer für uns
handhabbaren Form zu haben. Wir nehmen der Einfachheit halber an, dies sei
bereits geschehen und die Repräsentation von XML-Code sähe wie folgt aus:

```haskell
data XML = Element String [Attribute] [XML]
         | Txt String

data Attribute = Attribute String String
```

Ein XML-Element ist ein gemischter Datentyp. Entweder es ist ein tatsächliches
Element, das aus einer Zeichenkette (das ist der Name des Elements), einer Liste
an Attributen und einer Liste an weiteren XML-Elementen besteht. Oder es ist nur
eine einfache Zeichenkette. Ein Attribut besteht aus zwei Zeichenketten: dem
Namen des Attributs und dessen Wert. Die Repräsentation von `<p color="red">
Hallo </p>` ist demnach:

```haskell
Element "p" [Attribute "color" "\"red\""] [Txt "Hallo"]
```

Fangen wir mit den Attributen an. Ein Attributpaar soll immer in eine Zeile
gedruckt werden:

```haskell
attToDOC :: Attribute -> DOC
attToDOC (Attribute key value) = text key <> text "=\"" <> text value <> text "\""
```

Ein Tag wollen wir so drucken lassen, dass es nur dann auf eine Zeile gedruckt
wird, falls alle Attribute auf diese passen. Ansonsten soll nach dem Tag-Namen
umgebrochen und um zwei Zeichen eingerückt werden. Zudem soll dann auch nach
jedem Attributpaar umgebrochen werden:

```haskell
tagToDOC :: String -> [Attribute] -> DOC
tagToDOC name [] = text "<" <> text name <> text ">"
tagToDOC name attributes = group
                           (text "<" <> text name <>
                            (nest 2 (line <> concatDOCs (map attToDOC attributes))) <>
                            line <> text ">")
```

`group` um den gesamten Ausdruck macht die Wahl zwischen "alles auf eine Zeile"
vs "Umbrechen nach jedem Attributpaar" möglich, da `group`, wie oben
beschrieben, den Layouts ein weiteres hinzufügt, bei welchem alle
Zeichenumbrüche entfernt sind. `concatDOCs` nimmt eine Liste von Dokumenten und
fügt diese mit einem Zeilenumbruch dazwischen zu einem Dokument zusammen:

```haskell
concatDOCs :: [DOC] -> DOC
concatDOCs []      = nil
concatDOCs [x]     = x
concatDOCs (x:xs)  = x </> concatDOCs xs
```

`</>` ist eine Hilfsfunktion, die zwei Dokumente mit einem `line`-Dokument
dazwischen konkateniert. Jetzt nehmen wir obige Funktionen zusammen, um ein
ganzes Element auszudrucken:

```haskell
xmlToDOC :: XML -> DOC
xmlToDOC (Txt string)                    = text string
xmlToDOC (Element name attributes xmls)  = group
                                           (tagToDOC name attributes </>
                                            nest 2 (group line <>
                                                    concatDOCs (map xmlToDOC xmls)) </>
                                            text "</" <> text name <> text ">")
```

Im einfachen Textfall erzeugen wir nur ein `TEXT`-Dokument. Wenn wir ein Element
vorliegen haben, lassen wir das Tag drucken, dann ggf. umbrechen und um zwei
Zeichen einrücken, um dann die Kinder-Elemente entsprechend rekursiv drucken zu
lassen.

Das Dokument

```haskell
xml   = Element "p" [
          Attribute "color" "red",
          Attribute "font" "Times",
          Attribute "size" "10"
        ] [
          Txt "Here is some",
          Element "em" [] [Txt "emphasized"],
          Txt "Text. Here is a",
          Element "a" [Attribute "href" "http://example.org"]
            [Txt "link"],
          Txt "elsewhere."
          ]
```

wird mit `layout $ pretty 30 xml` bzw. `layout $ pretty 60 xml` so ausgedruckt:

```xml
<p
  color="red"
  font="Times"
  size="10"
>
 Here is some
  <em>  emphasized </em>
  text. Here is a
  <a
    href="http://example.org"
  >
   link
  </a>
  elsewhere.
</p>


<p color="red" font="Times" size="10" >
 Here is some
  <em>  emphasized </em>
  text. Here is a
  <a href="http://example.org" >  link </a>
  elsewhere.
</p>
```

Wie wir sehen, kommt diese Implementierung noch nicht ganz an die Beispiele von
oben heran: Dort wurden mehrere Attributpaare (wenn möglich) sogar auf eine
Zeile gedruckt. Zudem gibt es unschöne Leerzeichen an gewissen Stellen. Wie man
das elegant verbessern kann, werden wir im nächsten Blogpost sehen, wenn wir uns
dort Hilfs- und Komfortfunktionen für unseren Pretty-Printer schreiben.

## Fazit

Der heutige Blogpost zeigt, dass es nur wenige Operatoren braucht, um eine
mächtige Sprache zu entwickeln. Das besonders Schöne an der Sprache ist, dass,
wie so oft in der funktionalen Programmierung, kleinere Dinge zusammengefügt
werden zu größeren. In einem nächsten Blogpost schauen wir uns an, wie der
Pretty-Printer genau implementiert ist und optimieren unser XML-Beispiel.


<!-- more end -->
