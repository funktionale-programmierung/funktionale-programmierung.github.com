---
layout: post
description: Buildsysteme mit Haskell
title: "Buildsysteme mit Haskell"
author: stefan-wehr
tags: ["build system", "make", "shake", "Haskell"]
---

Willkommen im Neuen Jahr! Der Blog *Funktionale Programmierung* startet direkt
durch und beschäftigt sich im ersten Artikel im Jahr 2014 mit einem
in Haskell geschriebenen Buildsystem namens `shake`.

Größere Softwareprojekte benutzen (fast) alle ein Buildsystem, um aus Quellcode
automatisch ein fertiges Softwareprodukt zu erstellen. Dazu gehört z.B.
das Kompilieren von Quelldateien, das Linken von Objektdateien, das Generieren
von Dokumentation oder das Zusammenstellen von Distributions-Archiven.

Dieser Blogartikel gibt eine Einführung in das in Haskell geschriebene Buildsystem
[shake](http://community.haskell.org/~ndm/shake/). Dieses System hat den Vorteil, 
dass Abhängigkeiten
zwischen Build-Artefakten dynamisch, d.h. während das Buildsystem läuft, entstehen können.
Bei [make](http://www.gnu.org/software/make/), dem wohl bekanntesten Buildsystem, müssen Abhängigkeiten hingegen
vor Aufruf des Buildsystems bekannt sein, was in der Praxis häufig zu Einschränkungen
und Problemen führt.

Wir benutzen bei uns in der Firma shake, um unser Produkt 
[Checkpad MED](/2013/07/17/medizin-funktional.html) zu kompilieren. Hier spielt
shake seine Stärken voll aus, denn ein wichtiger Bestandteil der Checkpad-Infrastruktur
ist Codegenerierung. Dank dynamischer Abhängigkeiten ist es möglich, mit einem Aufruf des
Buildsystems das Programm zur Codegenerierung zu kompilieren, den Code selbst zu generieren
und den generierten Code zu kompilieren und zu linken. 

Neil Mitchell, der Autor von shake, hat eine Variante des Tools für den Einsatz
bei [Standard Chartered](https://www.sc.com/) entwickelt, um wirklich große Softwareprojekte effizient
kompilieren zu können. Details hierzu sowie detaillierte Infos zur internen
Architektur von shake finden Sie in [diesem Artikel](http://community.haskell.org/~ndm/downloads/paper-shake_before_building-10_sep_2012.pdf).

<!-- more start -->

Wir möchten im folgenden beispielhaft ein Buildsystem für ein in der Sprache C geschriebenes
Projekt entwickeln. Um das Beispiel interessanter zu machen wird eine der beteiligten `.h`-Dateien
generiert, wobei der Quellcode des Generators selbst wiederum Teil des Projekts
ist. (Dies entspricht grob dem Setup, welches wir auch im Checkpad Projekt haben.
Zur Vereinfachung nehmen wir hier aber ein in C geschriebenes Projekt, da
die Abhängkeiten bei der Kompilierung von Haskell-Quellcode deutlich komplizierter
als für C-Quellcode sind.)

Beginnen wir mit einer sehr einfachen Hilfsfunktion zum Extrahieren von `#include` Dateien
aus C-Quelldateien. Die Funktion liest eine `.c`-Datei ein und gibt die Dateiname
aller mit `#include "` beginnenden Zeilen zurück.

{% highlight haskell %}
cIncludes :: FilePath -> Action [FilePath]
cIncludes x =
    do s <- readFile' x
       return $ mapMaybe parseInclude (lines s)
    where
      parseInclude line =
          do rest <- List.stripPrefix "#include \"" line
             return $ takeWhile (/= '"') rest
{% endhighlight %}

Der Typ `Action` ist eine in Shake definierte [Monade](/2013/04/18/haskell-monaden.html),
in der alle Build-Aktion ausgeführt werden. Ein wesentlicher Aspect der `Action`-Monade
ist das Tracken von Abhängigkeiten. So ist z.B. die oben verwendete Funktion `readFile'`
ein Wrapper um die `readFile` Funktion aus der Standardbibliothek, die neben
dem Einlesen der Datei sich auch noch eine Abhängigkeit auf die Datei merkt.

Jetzt kommen wir schon zu den eigentlichen Regeln unseres Buildsystems. Regeln
werden in der `Rules`-Monade definiert. Meistens wird dazu der in Shake definierte Operator
`*>` verwendet, welcher folgende Typsignatur hat:

{% highlight haskell %}
(*>) :: FilePattern -> (FilePath -> Action ()) -> Rules ()
{% endhighlight %}

Wir sehen diesen Operator gleich in Aktion:

{% highlight haskell %}
rules :: Rules ()
rules =
    do "*.o" *> \out ->
           do let c = replaceExtension out "c"
              need (cIncludes c)
              system' "gcc" ["-o", out, "-c", c]
{% endhighlight %}

Oben stehende Regel beschreibt, wie eine `.o`-Datei erstellt wird. Das erste Argument von `*>`
ist dabei das Pattern, das auf die Ausgabedatei matchen muss, dass zweite Argument ist eine
Funktion, die diese Ausgabedatei erstellt. In der obigen Regel sehen wir, dass dazu die zur `.o`
gehörende `.c`-Datei durch `gcc` kompiliert werden. Zuvor wird noch mittels
`need` dynamisch Abhängigkeiten auf die in der `.c`-Datei referenzierten Header-Files 
eingeführt. Wenn Sie mit `make` vertraut sind, haben Sie sicher bemerkt, dass so etwas in
`make` nicht direkt sondern nur mit Tricks funktioniert, und diese Tricks haben oftmals 
ihren Preis.

Abhängigkeit auf Header-Files sind normalerweise trivialerweise erfüllt, denn Header-Files
werden typischerweise nicht durch das Buildsystem generiert. Für unser Beispiel nehmen wir aber
an, dass es ein Header-File `Auto.h` gibt, dass von einem Programm `Codegen` erzeugt wird:

{% highlight haskell %}
       "Auto.h" *> \out ->
           do need ["Codegen"]
              system' "./Codegen" [out]
{% endhighlight %}

Schließlich gibt es noch zwei Regeln für die zwei Binaries `Codegen` und `Main`:

{% highlight haskell %}
       "Main" *> \out -> buildBinary out ["Hello.c", "Main.c"]
       "Codegen" *> \out -> buildBinary out ["Codegen.c"]
{% endhighlight %}

Nachfolgend noch die Funktion `buildBinary` die ein Binary aus gegebenen `.c`-Dateien erstellt.
Dazu wird zunächst eine Abhängigkeit auf die entsprechenden `.o`-Dateien mittels `need` spezifiziert,
was dazu führt dass die `.c`-Dateien mit Hilfe unserer allerersten Regel in `.o`-Dateien kompiliert werden.
Dann werden die `.o`-Dateien durch einen Aufruf von `gcc` gelinkt.

{% highlight haskell %}
buildBinary :: FilePath -> [FilePath] -> Action ()
buildBinary out cs =
    do let os = map (\c -> replaceExtension c "o") cs
       need os
       system' "gcc" (["-o", out] ++ os)
{% endhighlight %}

Zum Abschluss noch die `main`-Funktion, welche im wesentlichen die zu erstellenden Targets
durch Aufruf der Funktion `want` spezifiziert und den soeben Satz an Regeln an `shake` übergibt.

{% highlight haskell %}
main :: IO ()
main =
    do args <- getArgs
       shake shakeOptions $
          do let targets = if null args then ["Main"] else targets
             rules
             want targets
{% endhighlight %}

Wenn Sie unser kleines Buildsystem ausprobieren wollen, finden Sie [hier](/files/build-system-haskell.zip)
ein Beispielprojekt. Viel Spaß beim Experimentieren!

So, das war's für heute. Wir haben, zumindest ansatzweise, gesehen,
dass `shake` das Erstellen von sehr mächtigen
Buildsystem erlaubt. So lange Ihre Projekte keine komplizierten Buildregeln
benötigen, ist der Einsatz von `shake` sicherlich Overkill. Sobald's aber
etwas komplizierter wird, kann sich sein Einsatz lohnen. Im bereits
oben erwähnten [Artikel](http://community.haskell.org/~ndm/downloads/paper-shake_before_building-10_sep_2012.pdf) 
finden sich viele weitere Details zu `shake`, insbesondere auch ein
Vergleich mit anderen Buildsystem.

