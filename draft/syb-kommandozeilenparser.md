---
layout: post
description: Beschreibung
title: "'Scrap your Boilerplate' am Beispiel eines Kommandozeilenparsers"
author: emin-karayel
tags: ["haskell"]
---

Reflexion und Annotationen werden in objektorientierte Programmiersprachen eingesetzt um Funktionen über eine große Menge von Typen zu abstrahieren.  Bekannte Einsatzfälle sind Annotationen für die Einsprungpunkte von Unit-Tests oder die Serialisierung von Datentypen in verschiedene Formate, bspw. nach XML, JSON oder Protokolbuffers. Ein weiteres interessantes Beispiel ist [args4j](http://args4j.kohsuke.org/) - ein mit Reflexion umgesetzter Kommandozeilenparser.

Für die Abstraktion über die Struktur von Typen in Haskell gibt es unter [anderem](https://wiki.haskell.org/Scrap_your_boilerplate) die Spracherweiterung Generics und Literale auf der Typebene von Haskell. Die beiden Spracherweiterungen ermöglichen im Gegensatz zur Reflexion statische Metaprogrammierung - insbesondere werden Typfehler bereits bei der Kompilierung erkannt.

Wir wollen anhand des Beispiels eines Kommandozeilenparsers diese beiden Spracherweiterungen näher kennenlernen.

<!-- more start -->
In args4j kann man eine Datenstruktur annotieren, so dass der Parser automatisch erzeugt werden kann. Beispielsweise, kann man nach einer Definition der folgenden Form:

{% highlight java %}
public enum CompressionLevel {
    HIGH,
    LOW
}

public class HelloWorldArgs {
    @Option(name="-l",usage="compression level")
    private CompressionLevel compressionLevel;

    @Option(name="-i",usage="input file")
    private File input;

    @Option(name="-o",usage="output file")
    private File output;
}
{% endhighlight %}

Im Hauptprogramm den Parser mit den folgenden Zeilen ausführen:

{% highlight java %}
public static void main(String[] args) throws IOException, CmdLineException {
    CmdLineParser parser = new CmdLineParser(new HelloWorldArgs());
    parser.parseArgument(args);
}
{% endhighlight %}

# Hello World - Beispielprogramm mit Kommandozeilenargumenten #

Wir verwenden die Spracherweiterungen `DataKinds` und `DeriveGeneric` um unsere Datenstruktur für die Kommandozeilenparameter zu beschreiben. Die erste Spracherweiterung erlaubt unter anderem TypeLevel Literals, so kann man Zeichenketten und Zahlen als Typen verwenden. Wir werden diese verwenden um Hilfetexte und den Parameterpräfix an das Feld zu annotieren. Die zweite erlaubt es Code zu schreiben, der über die Struktur von beliebigen Typen (soweit diese bestimmte Eigenschaften erfüllen) definiert ist.

{% highlight haskell %}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}

import GHC.Generics
import System.Environment
import System.Console.GetOpt.Generic
{% endhighlight %}

Das letzte Modul ist unsere erweiterte Version von `System.Console.GetOpt` der Standardbibliothek von Haskell zur Verarbeitung von Kommandozeilen.

Die Definition des Datentypen sieht ganz ähnlich wie in args4j aus:

{% highlight haskell %}
data CompressionLevel
    = Low
    | High
    deriving (Read, Show, Enum, Bounded)

newtype File = File FilePath
    deriving Show

instance ParameterType CompressionLevel where
    argDescr = ReqArg read (show [(minBound::CompressionLevel)..maxBound])

instance ParameterType File where
    argDescr = ReqArg File "FILE"

data HelloWorldArgs
    = HelloWorldArgs
      { ex_compressionLevel
          :: Parameter CompressionLevel (ShortOpts "l") (Description "compression level")
      , ex_inputFile
          :: Parameter File (ShortOpts "i") (Description "input file")
      , ex_outFile
          :: Parameter File (ShortOpts "o") (Description "output file")
      }
      deriving (Show, Generic)
{% endhighlight %}

Das erste Argument des Typkonstruktors `Parameter` ist der Datentyp in dem die Werte des Feldes abgelegt werden sollen. Der Datentyp muss eine Instanz der Klasse `ParameterType` sein. Diese Klasse legt für den Typ fest, ob die Option weitere Argumente hat, ob diese optional oder notwendig sind und wie diese aus den Kommandozeilenargumenten ermittelt werden.

Das nächsten beiden Argumente des Typkonstruktors geben den Optionspräfix und den Hilfetext an. Den Parser kann man nun mit

{% highlight haskell %}
main =
    do args <- getArgs
       let helloWorldArgs :: HelloWorldArgs
           (helloWorldArgs, _, _) = getOptGeneric args
       putStrLn $ show $ helloWorldArgs
{% endhighlight %}

aufrufen.

# Konventionelle Umsetzung #

Eine Umsetzung ohne Generics - wie Sie auch in der [Beschreibung](http://hackage.haskell.org/package/base-4.8.0.0/docs/System-Console-GetOpt.html) von `GetOpt` vorgeschlagen wird, sieht wie folgt aus:

{% highlight haskell %}
empty :: HelloWorldArgs
empty
    = HelloWorldArgs
      { ex_compressionLevel = Parameter Nothing
      , ex_inputFile = Parameter Nothing
      , ex_outputFile = Parameter Nothing
      }

optDescr :: [OptDescr (HelloWorldArgs -> HelloWorldArgs)]
optDescr =
    [ Option ['l'] []
     (fmap (\v opts -> opts { ex_compressionLevel = Parameter $ Just v }) argDescr)
     "compression level"
    , Option ['i'] []
     (fmap (\v opts -> opts { ex_inputFile = Parameter $ Just v }) argDescr)
     "input file"
    , Option ['o'] []
     (fmap (\v opts -> opts { ex_outputFile = Parameter $ Just v }) argDescr)
     "output file"
    ]

main =
    do args <- getArgs
       case (getOpt RequireOrder optDescr args) of
         (optl,n,errs) -> return (foldl (flip id) empty optl, n)
{% endhighlight %}

Abhängig von der Kommandozeile liefert `getOpt` eine Liste vom Typ `HelloWorldArgs -> HelloWorldArgs` zurück, die dann auf den Initialwert `empty` angewendet werden.

Wir wollen natürlich die obigen Definitionen `empty` und `optDescr` "generisch", direkt aus der Datentypdefinition bestimmen.

# Generischer Kommondazeilenparser #

In dem Modul `System.Console.GetOpt.Generic` wird die generische Funktion `getOptGeneric`, sowie die Klasse `ParameterType` und die Typen `ShortOpts` und `Description` definiert:

{% highlight haskell %}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module System.Console.GetOpt.Generic
    ( getOptGeneric
    , ParameterType(..)
    , Parameter(..)
    , ShortOpts(..)
    , Description(..)
    , module System.Console.GetOpt
    ) where

import Data.Proxy
import GHC.Generics
import GHC.TypeLits
import System.Console.GetOpt
import System.Environment
{% endhighlight %}

Die Klasse `ParameterType` gibt an, ob der Kommandozeilenparameter möglicherweise Argumente hat und wie diese ausgelesen werden. Wir verwenden den Typ, der auch in `System.Console.GetOpt` verwendet wird: (`ArgDescr a`)

{% highlight haskell %}
class ParameterType a where
    argDescr :: ArgDescr a
{% endhighlight %}

Allerdings erwartet `getOpt` eine Liste vom Typ `[ArgDescr a]` als Argument: Wir haben für jeden Parameter einen anderen Typ für die Variable `a`, z.B. `File` oder `CompressionLevel`. Wie dieses Problem gelöst wird, werden wir im folgenden Abschnitt sehen.

{% highlight haskell %}
data Parameter a b c
    = Parameter (Maybe a)
    deriving Show

data ShortOpts (l :: Symbol)
data Description (d :: Symbol)
{% endhighlight %}

Die beiden Typen `ShortOpts` und `Description` haben keine Werte - sie werden nur auf Typebene verwendet um die Paramter zu beschreiben, z.B. `Description "compression level"`.

Der Ansatz bei [GHC.Generics](https://wiki.haskell.org/GHC.Generics) ist es eine Repräsentation des Datentyps mittels einer geringen Anzahl von Typkombinatoren zu realisieren. Hier eine - nicht korrekte - vereinfachte Darstellung:
`data Example = Foo String | Bar Bool Int` wird als
{% highlight haskell %}
type ExampleRep = String :+: (Bool :*: Int)
{% endhighlight %}
 dargestellt. In Wirklichkeit gibt es noch den Konstruktor `K1` vor den externen Datentypen und `M1` Konstruktoren, die Kontext Daten z.B. den Namen des Konstruktors (hier `Foo`, `Bar`) oder des Feldes enthalten. Die Funktionen `from` und `to` bilden eine bijektive Abbildung vom Typen in seine Repräsentationsform und zurück, d.h.

{% highlight haskell %}
from :: Example -> ExampleRep
to :: ExampleRep -> Example
{% endhighlight %}

mit `from . to == id` und `to . from == id`.

Der Vorteil der Repräsentation ist, dass man für eine Klasse, die man für beliebige Typen erzeugen will, nur noch Instanzen für eine kleine Menge an Typkonstruktoren (`:*:`, `:+:`, `K1`, `M1`, `U1`, `V1`) schreiben muss. (Es ist nicht immer notwendig alle Konstruktoren zu instanzieren. Beispielsweise wenn man keine Datentypen mit mehreren Konstruktoren unterstützt, dann kann man den Typkonstruktor `:+:` weglassen.)

## Generische Definition der `optDescs` und `empty` Funktionen

Die Klasse `OptDescriptions` verwenden wir intern um über die Struktur der Datentypen abstrahieren können. Die Funktion `empty` liefert den Wert des Datentypes (in Representationsform) für den Fall, dass kein einziger Kommandozeilenparameter angegeben wurde.

Die Funktion `optDescs` liefert eine Liste von Optionsbeschreibungen - diese enthalten, die Namen der Kommandozeilenparameter, so wie die Wirkung eines Kommandozeilenarguments auf den Wert des Datentyps (wiederum in Repräsentationsform).

{% highlight haskell %}

class OptDescriptions f where
    empty :: f p
    optDescs :: [OptDescr (f p -> f p)]

{% endhighlight %}

Wir fangen mit der grundlegenden Instanz an, die für den Typkonstruktor `K1 i c`. Hier definieren wir aber nur eine Instanz für `Parameter a (ShortOpts s) (Description d)`, da jedes Feld nach Spezifikation in dieser Form sein muss.

{% highlight haskell %}

instance (KnownSymbol s, KnownSymbol d, ParameterType a)
    => OptDescriptions (K1 i (Parameter a (ShortOpts s) (Description d))) where
    empty = K1 (Parameter Nothing)
    optDescs = [Option shortOptions [] liftedArgDescr description]
        where
          shortOptions = symbolVal (Proxy :: Proxy s)
          description = symbolVal (Proxy :: Proxy d)
          liftedArgDescr = fmap (const . K1 . Parameter . Just) argDescr

{% endhighlight %}

Hier werden aus den Typlevel Literalen Zeichenketten, dies funktioniert mit der Funktion `symbolVal`, die man sich als Ad-hoc polymorphe Funktionen vorstellen kann, die für jeden Typ vom Kind `Symbol` definiert ist.
{% highlight haskell %}
symbolVal (Proxy :: Proxy "foo") = "foo"
symbolVal (Proxy :: Proxy "bar") = "bar"
{% endhighlight %}

Auch die nächste Instanz ist ein sehr interessanter Fall - wir wollen für das Produkt von zwei Datentypen, die Funktionen `empty` und `optDescs` definieren. Natürlich wollen wir auf die Definition von `empty` und `optDescs` in den Basistypen zurückgreifen.

{% highlight haskell %}
instance (OptDescriptions f, OptDescriptions g) =>  OptDescriptions (f :*: g) where
    empty = empty :*: empty
    optDescs = map (fmap leftLift) optDescs ++ map (fmap rightLift) optDescs
        where
          leftLift f = \(x :*: y) -> (f x :*: y)
          rightLift f = \(x :*: y) -> (x :*: f y)
{% endhighlight %}

Der Typ `M1` wird verwendet um syntaktische Informationen bereitszustellen, z.B. Namen von Konstrukoren, Feldern, über die abstrahiert wird. Wir ignorieren diese Informationen in unserem Anwendungsfall.

{% highlight haskell %}
instance (OptDescriptions f) => OptDescriptions (M1 i t f) where
    empty = M1 $ empty
    optDescs = map (fmap f) optDescs
        where
          f g (M1 x) = M1 (g x)
{% endhighlight %}

Abschliessen noch die Definition von `getOptGeneric` die `getOpt` mit dem Repräsentationstyp von `a` aufruft und die obigen beiden Funktionen `empty` und `optDescs` verwendet.

{% highlight haskell %}
getOptGeneric :: (Generic a, OptDescriptions (Rep a)) => [String] -> (a, [String], [String])
getOptGeneric args = (to (foldl (flip id) empty optList), unparsed, errs)
    where
      (optList, unparsed, errs) = getOpt RequireOrder optDescs args
{% endhighlight %}

# Mögliche Verbesserungen

Aktuell bieten wir keine Möglichkeit, auch Kommandozeilenargumente zu verarbeiten, die mit langen `--foobar` Präfixen versehen sind. Hierfür wäre es noch notwendig Listen auf der Typebene zu verweden `HList` (siehe auch [Promoted lists and tuple types](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/promotion.html) und [HList](https://hackage.haskell.org/package/HList)). Auch ist die Liste der kurzen Präfixe als Zeichenkette dargestellt, statt einer Liste von `Char` auf Typebene.

Zeichenketten auf Typebene können auch auf Gleichheit innerhalb des Typsystems überprüft werden, hierdurch wäre es möglich bereits zum Kompilierungszeitpunkt eine Fehlermeldung zu liefern, falls ein Optionspräfix mehrfach verwendet wurde.

Mit Alternativen wäre es möglich auch komplexere Kommandozeilenparameter Abhängigkeit darzustellen, die Argumente für ein Kompressionsprogramm könnten z.B. wie folgt beschrieben werden.

{% highlight haskell %}
data CmdLineArgs
    = Help (CmdLineAlternative (ShortOpts "-h") (Description "Print usage information") ())
    | Compress (CmdLineAlternative (ShortOpts "-c") (Description "Compress") CompressArgs)
    | Decompress (CmdLineAlternative (ShortOpts "-c") (Description "Compress") DecompressArgs)

data CompressArgs
    = CompressArgs
      { c_inFile :: Parameter File (ShortOpts "-i") (Description "Input file")
        c_outFile :: Paramter File (ShortOpts "-o") (Description "Output file")
        c_compressionLevel :: Parameter CompressionLevel (ShortOpts "-l") (Description "Level")
      }

data DecompressArgs
      { d_inFile :: Parameter File (ShortOpts "-i") (Description "Input file")
        d_outFile :: Paramter File (ShortOpts "-o") (Description "Output file")
      }
{% endhighlight %}

Einige Parameter sind immer notwendig, dort wäre es möglich, das `getOptGeneric` selbst bereits eine Fehlermeldung erzeugt, und dafür der Rückgabetyp nicht mehr in `Maybe` steht (siehe Definition
von Parameter.)

<!-- more end -->

