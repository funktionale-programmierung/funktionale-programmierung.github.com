---
layout: post
description: "A prettier printer implemented in Clojure"
title: "Aufgehübscht! -- Pretty-Printing"
author: kaan-sahin
tags: "pretty", "print", "printing", "clojure", "wadler"]
---

Um ein (verschachteltes) Objekt komfortabel untersuchen zu können, wird eine
gute Darstellung selbiges in Form von Text benötigt. Pretty-Printer versuchen
genau das: Dinge so auf den Bildschirm zu drucken, dass die interne Struktur auf
einen Blick ersichtlich ist. Wir schauen uns heute die Pretty-Printing-Strategie
aus [Philip Wadler](http://homepages.inf.ed.ac.uk/wadler/)s Paper "A prettier
printer" (1997) an und sehen daran ein weiteres Mal, dass Datenmodellierung und
Abstraktion der funktionalen Programmierung wunderbar einfachen und konzisen
Quellcode hervorbringen, der komplexe Programme elegant ermöglicht.

<!-- more start -->

*Hinweis: Der komplette Code ist auf
TODO [Github](https://github.com/kaaninho/clojure-macros-example)
zu finden. Wir empfehlen, ihn während des Lesens Stück für Stück auszuführen.*

## The Good, the Bad and the Ugly

Wir werden im Folgenden LISP-Ausdrücke betrachten. Bei ihnen ist es von
besonderer Wichtigkeit, die interne Struktur ersichtlich zu machen und damit den
Sinn von guten Pretty-Printern zu motivieren. Hier ein gutes, ein nicht ganz
gelungenes und ein ganz und gar nicht gelungenes (Pretty-)Printing der
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
besser ersichtlich, da Subausdrücke jeweils um ein Zeichen eingerückt sind.
Zudem erzeugen Zeilenumbrüche noch mehr Struktur. Hier ist jedoch zu bemängeln,
dass zu "aggressiv" umgebrochen wird. Das hat schlechtere Lesbarkeit und mehr
Zeilen im Pretty-Print zur Folge. Das erste Beispiel lässt den Kopf und Rumpf
einer Funktionsdefinition besonders gut erkennen.

Als weiteres Beispiel XML-Code in drei geprinteten Varianten (dieses mal mit 30
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
eine Zeile in den Fließtext eingebettet, kein Umbruch nötig, da die Maximalbreite
nicht überschritten wird. Die Attribute des `p`-Elements werden zunächst
nebeneinander, so lang es Platz hat, und dann untereinander sortiert. Hier wird
also nicht nur stur eine "Vorgehen" befolgt ("breche pro Attribut-Wert-Paar um"),
sondern der vorhandene Platz ausgenutzt.

Obwohl die Beispiele aus zwei unterschiedlichen Domänen kommen (Programmiersprache
Common Lisp und Auszeichnungssprache XML), können beide mit dem hier vorgestellten
Pretty-Printer behandelt werden. Das wird dadurch ermöglicht, dass eine
Metasprache für Dokumente, die schön ausgedruckt werden sollen, entwickelt wird.
Dann erst wird für den Anwendungsfall bestimmt, wie man Objekte in diese
Metasprache übersetzt. Die Vorgehensweise, eine beschreibende Zwischensprache zu
entwerfen, ist in der funktionalen Programmierung gang und gäbe. Man nennt sie
auch *domänenspezifische Sprachen* (domain specific languages im Englischen, kurz
DSL). 
TODO: Verlinke auf Beispiel eines anderen Blogposts zu DSLs


## Strategie des Prettier Printers

Der Pretty-Printer nimmt eine Beschreibung eines Dokuments entgegen und erzeugt
daraus viele Alternativ-Dokumente. Diese unterscheiden sich im Format bzgl.
Umbrüchen und Einrückungen. Dann wählt der Pretty-Printer aus dieser Menge von
Dokumenten das *beste* aus. Das *beste* hierbei ist das, das jede Zeile möglichst
gut ausnutzt, aber dennoch die vorgegebene Breite nicht überschreitet.

In diesem Blogpost wollen wir unser Augenmerk auf die Verwendung der
Pretty-Printer-DSL setzen. Wie die eigentliche DSL implementiert ist -- besonders
in Hinblick auf Effizienz (es könnten einige Tausend bis Millionen
Alternativ-Dokumente entstehen) -- sehen wir in einem folgenden Blogpost.

## Die Pretty-Printer-Dokumentensprache

Unsere Pretty-Printer-DSL besteht aus Dokumenten und Operatoren, die auf diesen
arbeiten. Wir benutzen ab jetzt Haskell-Syntax, um dem Ganzen Form zu geben. 


### Sechs Operatoren für das Glück

Folgende sechs Operatoren stehen zur Verfügung, um ein Dokument in unserer
Prettyprinter-Sprache zu beschreiben:

```haskell
(<>)       :: Doc -> Doc -> Doc
nil        :: Doc
text       :: String -> Doc
line       :: Doc
nest       :: Int -> Doc -> Doc
layout     :: Doc -> String
```

Dabei fügt `<>` zwei Dokumente aneinander. Das Resultat ist wieder ein Dokument.
`nil` ist das sogenannte leere Dokument. Beliebige Dokumente bleiben bei
Konkatenation mit `nil` unverändert (etwas mathematischer ausgedrückt bildet
also die Menge aller Dokumente zusammen mit der assoziativen Verknüpfung `<>`
und dem neutralen Element `nil` ein Monoid). `text` wandelt eine Zeichenkette in
ein Dokument um, `line` beschreibt einen Zeilenumbruch und `nest` rückt ein
Dokument ein. Zu guter Letzt gibt `layout` ein als String gerendertes Dokument
zurück.

Beispielsweise würde folgendes Dokument

```haskell
doc =
  text "<p>" <> 
  nest 2 (
    line <> text "<a>" <> text "This is a link" <> text "</a>" <>
    line <> text "Some text"
  ) <>
  line <> text "</p>"
```

mit Hilfe von `layout` so ausgedruckt werden:

```bash
<p>
  <a>This is a link</a>
  Some text
</p>
```

Oben war die Rede von mehreren Dokumenten, aus denen das beste ausgewählt wird.
Hier ist bisher nichts davon zu sehen. Die Sprache benötigt für diese Erweiterung
eine weitere Operation (`group`) und eine Erweiterung des Begriffs Dokument: 

## Snippets

Ein Dokument ist ein Summentyp aus Text, Zeilenumbruch mit Einrückung und dem
leeren Dokument.

```haskell
Doc = Nil
    | String `Text` Doc 
    | Int `Line` Doc
```





<!-- more end -->
