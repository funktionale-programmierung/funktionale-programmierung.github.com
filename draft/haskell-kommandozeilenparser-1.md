---
layout: post
description: Beschreibung
title: "Kommandozeilenparser in Haskell - Teil 1"
author: emin-karayel
tags: ["haskell"]
---

In diesem Artikel lernen wir wie Kommandozeilenoptionen in Haskell-Programmen mit der Bibliothek [System.Console.GetOpt](https://hackage.haskell.org/package/base-4.8.0.0/docs/System-Console-GetOpt.html) geparst werden können und wie die geparsten Optionen in einen Record-Type (Tupel mit Datenfeldern) zur typsicheren Weiterverarbeitung im Programm umgerechnet werden können.

Wie der Titel schon andeutet, wird diesem Artikel noch ein zweiter Teil folgen, in dem wir die Haskell Spracherweiterungen [Generics](https://wiki.haskell.org/GHC.Generics) und [Literale auf der Typebene](https://downloads.haskell.org/~ghc/7.8.4/docs/html/users_guide/type-level-literals.html) kennenlernen werden und mit diesen den Code, den wir in diesem Artikel für unser Beispielprogramm entwickelt haben, für andere Programme wiederverwendbar machen.

<!-- more start -->

# Beispielprogramm

Die Bibliotheken `System.Console.GetOpt`, [Control.Monad.Except](https://hackage.haskell.org/package/mtl-2.2.1/docs/Control-Monad-Except.html) und [System.Environment](http://hackage.haskell.org/package/base-4.8.0.0/docs/System-Environment.html#v:getArgs) importieren wir gleich in den ersten Zeilen. Die zweite Bibliothek bietet einem die Möglichkeit, Monaden zu bauen, die fehlschlagen können und Fehlermeldungen zurückliefern. Mit der dritten Bibliothek kann man die Argumente des Programms in der Kommandozeile auslesen. Die Funktion [intercalate](http://hackage.haskell.org/package/base-4.8.0.0/docs/Data-List.html#v:intercalate) aus `Data.List` erlaubt es u.a., Listen von Zeichenketten mit einem Trennzeichen auszugeben.

{% highlight haskell %}
import System.Console.GetOpt
import Control.Monad.Except
import System.Environment
import Data.List(intercalate)
{% endhighlight %}

Wir werden die Monade `ErrorM` benutzen für Berechnungen, die eine Liste von Fehlermeldungen zurückliefern können.
{% highlight haskell %}
type ErrorMessages = [String]
type ErrorM r = Either ErrorMessages r
{% endhighlight %}

Mit der Bibliothek `Control.Monad.Except` ist dieser bereits eine Instanz von der Klasse `MonadError`, die unter anderem die polymorphe Funktion `throwError` zur Verfügung stellt. (siehe auch [Monaden das programmierbare Semikolon](http://funktionale-programmierung.de/2013/04/18/haskell-monaden.html))

Unsere exemplarische Anwendung ist ein Kompressions-Tool, das drei verschiedene Kompressionsstufen unterstützt, die wir mit dem Datentyp `CompressionLevel` beschreiben.
{% highlight haskell %}
data CompressionLevel
    = Low
    | High
    | UltraHigh
    deriving Show
{% endhighlight %}

Außer dem Grad der Kompression übergeben wir dem Programm jeweils den Pfad zu der Datei, die komprimiert werden soll, und den Pfad zu der komprimierten Datei, die erzeugt werden soll. Falls sich bereits eine Datei im Zielpfad befindet, überschreiben wir diese, nur soweit die Option `--force` angeben wurde. Die Benutzerdokumentation für die Aufrufparameter des Programms sehen dabei so aus:

{% highlight bash %}
Usage: compress [option...]
  -l LEVEL  --level=LEVEL      Level of compression (LEVEL can be one of low, high or ultra_high)
  -i FILE   --inputfile=FILE   Path to the input file
  -o FILE   --outputfile=FILE  Path to the output file
  -f        --force            Overwrite exiting files
{% endhighlight %}

## Beschreibung der Kommandozeilenoptionen

Die Optionen werden mit den beiden parametrisierten Datentypen `OptDescr a`, `ArgDescr a` beschrieben, der Typparameter gibt den Ergebnistyp an, in dem die Kommandozeilenoptionen von `getOpt` zurückgeliefert werden.

Dabei stellen wir fest, dass `getOpt` eine Liste mit Elementen vom Typ a zurückliefert (`[a]`), wobei jeweils für jede Option in der Kommandozeile genau ein Element zurückgeliefert wird.

Für unser Beispielprogamm ist deshalb der folgende Datentyp

{% highlight haskell %}
data CompressProgramOption
    = CompressionLevel (ErrorM CompressionLevel)
    | InputFile FilePath
    | OutputFile FilePath
    | Force
{% endhighlight %}

eine passende Instanzierung für `a`. Wie schon in der Einleitung ausgeführt, werden wir diese Liste von Optionen später in einen Record-Typ umrechnen.

## Optionsargumentbeschreibung

Der Datentyp `OptDescr a` beschreibt die Argumente einer Kommandozeilenoption und hat drei Konstruktoren

+ `NoArg` für Kommadozeilenoptionen ohne Argumente
+ `ReqArg` für Kommandozeilenoptionen mit einen notwendigen Argument
+ `OptArg` für Kommandozeilenoptionen mit einem optionalen Argument

Beispielsweise hat die Kommandozeilenoption `--force` kein Argument und wir können es einfach mit

{% highlight haskell %}
forceArgDescr :: ArgDescr CompressProgramOption
forceArgDescr = NoArg Force
{% endhighlight %}

beschreiben. Ein interessanterer Fall ist die Kommondozeilenoption `--level` für den Grad der
Komprimierung mit den drei möglichen Argumenten `low`, `high` und `ultra_high`. Wir benutzen den Konstruktor `ReqArg`, da das Optionsargument obligatorisch ist. Dem Konstruktor `ReqArg` (kurz für required Argument) übergeben wir einen Parser, welcher aus der der Option folgenden Zeichenkette ein Element vom Typ `ErrorM CompressProgrammOption` erzeugt. Dass das Ergebniss in der `ErrorM` Monade ist liegt daran, dass beim Parsen des Optionsarguments ein Fehler auftreten kann. Das letzte Argument gibt den Platzhalter für das Optionsargument in der Nutzerdokumentation wieder.

{% highlight haskell %}
levelArgDescr :: ArgDescr CompressProgramOption
levelArgDescr = ReqArg (CompressionLevel . parseLevelArg) "LEVEL"
    where
      parseLevelArg s =
          case s of
            "low" -> return Low
            "high" -> return High
            "ultra_high" -> return UltraHigh
            _ -> throwError $
                ["Could not parse " ++ s ++ " as argument for the option --level."]
{% endhighlight %}

Auch für die beiden anderen Kommandozeilenoptionen `--inputfile`, `--outputfile` sind die Argumente obligatorisch:

{% highlight haskell %}
inputFileArgDescr :: ArgDescr CompressProgramOption
inputFileArgDescr = ReqArg InputFile "FILE"

outputFileArgDescr :: ArgDescr CompressProgramOption
outputFileArgDescr = ReqArg OutputFile "FILE"
{% endhighlight %}

Hier verwenden wir die Eigenschaft, dass ein Konstruktor in Haskell auch als eine Funktion von seinen Argumenten in den Datentyp verstanden werden kann. Im ghci kann man sich das direkt ausgeben lassen:

{% highlight bash %}
*Main> :t InputFile
InputFile :: FilePath -> CompressProgramOption
{% endhighlight %}

Außerdem ist `FilePath` ein Typsynonym für `String`.

## Optionsbeschreibung

Der Datentyp `OptDescr a` beschreibt eine vollständige Kommandozeilenoption. Dieser hat genau einen Konstruktor `Option` mit vier Argumenten: `data OptDescr a = Option [Char] [String] (ArgDescr a) String`.

Die vier Argumente geben (in Reihenfolge)

+ die möglichen Buchstaben für die Kurzformen der Option,
+ die möglichen Zeichenketten für die Langformen der Option,
+ die Optionsargumentbeschreibung (siehe Abschnitt oben) und
+ eine Klartextbeschreibung für den Benutzer
an.

Für unser Beispielprogramm ergibt sich die folgende Liste von Optionsbeschreibungen:

{% highlight haskell %}
optionDescriptions :: [OptDescr CompressProgramOption]
optionDescriptions =
    [ Option ['l'] ["level"] levelArgDescr
        "Level of compression (LEVEL must be either 'low','high' or 'ultra_high' (default))"
    , Option ['i'] ["inputfile"] inputFileArgDescr "Path to the input file (required)"
    , Option ['o'] ["outputfile"] outputFileArgDescr "Path to the output file (required)"
    , Option ['f'] ["force"] forceArgDescr "Overwrite exiting files"
    ]
{% endhighlight %}

## Umwandlung von Optionslisten in einen Recordtype

Für die Weiterverarbeitung in einem Kommandozeilen-Tool ist es oft besser, die Kommandozeilenoptionen in einem getypten Recordtype darzustellen, das man an die verschiedenen Stellen im Programm zur Verarbeitung weitergeben kann, statt einer Liste von Optionen vom Typ `[CompressProgramOption]`, bei der man in der Liste nach dem jeweiligen Option z.B. der Eingabedatei suchen muss. Hier kann es passieren, dass die Option gar nicht angegeben wurde - und man relativ spät im Ablauf eine Fehlermeldung ausgibt. Wir möchten deshalb die geparsten Optionen in die folgende Datenstruktor umrechnen:

{% highlight haskell %}
data CompressProgramArgs
    = CompressProgramArgs
      { cp_compressionLevel :: CompressionLevel
      , cp_inputFile :: FilePath
      , cp_outputFile :: FilePath
      , cp_force :: Bool
      }
{% endhighlight %}

Die Funktion `fromOptionListToArgs` wandelt die Optionsliste in den obigen Typen um, bzw. liefert eine Fehlermeldung, falls z.B. eine notwendige Option gefehlt hat.

{% highlight haskell %}
fromOptionListToArgs :: [CompressProgramOption] -> ErrorM CompressProgramArgs
fromOptionListToArgs result =
    do inputFile <- getInputFile result
       outputFile <- getOutputFile result
       force <- getForce result
       level <- getLevel result
       return $
           CompressProgramArgs
           { cp_compressionLevel = level
           , cp_inputFile = inputFile
           , cp_outputFile = outputFile
           , cp_force = force
           }
{% endhighlight %}

Die Aktionen in dem obigen `do` Block laufen in der `Either String` Monade. Die Funktionen `getInputFile` und `getOutputFile` gehen die Liste der geparsten Optionen durch, um den Wert der entsprechenden Option zu bestimmen oder liefern eine Fehlermeldung zurück, falls die Option nicht gesetzt wurde.

{% highlight haskell %}
getInputFile :: [CompressProgramOption] -> ErrorM FilePath
getInputFile options =
    case options of
      [] -> throwError ["The option --inputfile was missing."]
      ((InputFile f):_) -> return f
      (_:optionsTail) -> getInputFile optionsTail

getOutputFile :: [CompressProgramOption] -> ErrorM FilePath
getOutputFile options =
    case options of
      [] -> throwError ["The option --outputfile was missing."]
      ((OutputFile f):_) -> return f
      (_:optionsTail) -> getOutputFile optionsTail
{% endhighlight %}

Die Funktionen `getForce` und `getLevel` sind ähnlich definiert, nur, dass diese keine Fehlermeldung zurückliefern, wenn die Option fehlt, sondern den Standardwert.

{% highlight haskell %}
getForce :: [CompressProgramOption] -> ErrorM Bool
getForce options =
    case options of
      [] -> return False
      (Force:_) -> return True
      (_:optionsTail) -> getForce optionsTail

getLevel :: [CompressProgramOption] -> ErrorM CompressionLevel
getLevel options =
    case options of
      [] -> return UltraHigh
      ((CompressionLevel level):_) -> level
      (_:optionsTail) -> getLevel optionsTail
{% endhighlight %}

## Aufruf von getOpt

Die Funktion `getOpt` nimmt, zusätzlich zu den Optionsbeschreibungen und den an das Programm übergebenen Kommadozeilenargumenten, noch einen weiteren Parameter vom Typ `ArgOrder a` an. Dieser gibt an wie Nichtoptionen verarbeitet werden, d.h. Kommandozeilenargumente, die nicht mit einem/oder zwei Bindestrichen beginnen oder direkt einer Option mit Optionsargument folgen.

Die drei möglichen Konstruktoren von `ArgOrder a` sind

+ `RequireOrder`, d.h. nach Nichtoptionen dürfen keine Optionen folgen, oder
+ `Permute`, d.h. Nichtoptionen können beliebig zwischen den Optionen vorkommen, oder
+ `ReturnInOrder (String -> a)`, d.h. Nichtoptionen werden mit der übergebenen Funktion in den selben Datentyp (`a`) geparst, in den auch die Optionen geparst werden.

In unserem Programm erlauben wir gar keine Nichtoptionen. Da `getOpt` diese Möglichkeit nicht direkt unterstützt, übergeben wir `Permute` an `getOpt` und liefern eine Fehlermeldung, falls mindestens eine Nichtoption geparst wurde. Wieder nutzen wir die `ErrorM` Monade, die wir oben definiert haben.

Die `getOpt` Funktion selbst liefert ein Tripel mit den geparsten Kommandozeilenoptionen sowie den Nichtoptionen und Fehlermeldungen zurück.

{% highlight haskell %}
main :: IO ()
main =
    do args <- getArgs
       case getOptResultToArgs (getOpt Permute optionDescriptions args) of
         Left err -> mapM putStrLn err >> putStrLn (usageInfo header optionDescriptions)
         Right result -> compressProgram result
    where
      header = "Usage: compress [option...]"

getOptResultToArgs :: ([CompressProgramOption], [String], [String]) -> ErrorM CompressProgramArgs
getOptResultToArgs (result, nonOptions, errors) =
    do unless (null nonOptions)
           $ throwError ["Could not parse " ++ intercalate ", " nonOptions]
       unless (null errors) $ throwError errors
       fromOptionListToArgs result
{% endhighlight %}

Die Implementation des Hauptprogramms kann dann bequem auf die Datenstruktur `CompressProgramArgs` zugreifen. Da wir in diesem Blogartikel kein echtes Komprimierungsprogramm schreiben wollen, geben wir die Argumente nur aus:
{% highlight haskell %}
compressProgram :: CompressProgramArgs -> IO ()
compressProgram args =
    do putStrLn $ "Input file: " ++ cp_inputFile args
       putStrLn $ "Output file: " ++ cp_outputFile args
       putStrLn $ "Level of compression: " ++ (show $ cp_compressionLevel args)
       putStrLn $ "Overwrite existing files: " ++ (show $ cp_force args)
{% endhighlight %}

# Fortsetzung

Bei dieser Implementation stellen wir fest, dass wir alle Kommandozeilenoptionen in zwei Datenstrukturen (als Summen und Produkttyp) definiert und für jede Option eine Funktion geschrieben haben, die eine Liste vom Summentyp durchläuft, um den Wert in dem Produkttyp zu ermitteln. In der Umwandlungsfunktion `fromOptionListToArgs` haben wir diese Funktionen aufgerufen - und erst wenn alle erfolgreich waren, konnten wir das Ergebnis erzeugen.

Im zweiten Teil dieses Artikels werden wir mit den Spracherweiterungen [Generics](https://wiki.haskell.org/GHC.Generics) und [Literalen auf der Typebene](https://downloads.haskell.org/~ghc/7.8.4/docs/html/users_guide/type-level-literals.html) eine eigene Bibliothek entwickeln, mit der es möglich ist, die Kommandozeilenoptionen eines Programms in einen Record-Type zu Parsen - ohne doppelte Definition der Datenstrukturen und Konvertierungsfunktionen.


