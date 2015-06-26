---
layout: post
description: Beschreibung
title: "Generics am Beispiel eines Kommandozeilenparsers"
author: emin-karayel
tags: ["haskell"]
---

Reflexion und Annotationen werden in objektorientierte Programmiersprachen von Bibliotheken eingesetzt, die abhängig von der Struktur von Datentypen der Applikation, Funktionalität anbieten.

Ein bekanntes Beispiel ist die Java Bibliothek [Jackson](https://github.com/FasterXML/jackson-docs)
mit der Parser und Serialisierer für anwendungseigene Datentypen erzeugen werden können. Die Annotationen werden dabei eingesetzt, um Besonderheiten der Konvertierung direkt bei der Deklaration anzugeben.

In diesem und auch in vielen anderen Anwendungsfällen von Reflexion und Annotationen geht es u. a. darum, die Menge an [Boilerplate Code](https://en.wikipedia.org/wiki/Boilerplate_code) zu reduzieren. Hierdurch wird die Fehlerquote und der Wartungsaufwand beim Anlegen und Anpassen der Datenstrukturen reduziert. (vgl. auch [DRY Prinzip](https://en.wikipedia.org/wiki/Don%27t_repeat_yourself))

Für die Abstraktion über die Struktur von Typen in Haskell gibt es unter [anderem](https://wiki.haskell.org/Generics) die Spracherweiterungen [Generics](https://wiki.haskell.org/GHC.Generics) und [Literale auf der Typebene](https://downloads.haskell.org/~ghc/7.8.4/docs/html/users_guide/type-level-literals.html) von Haskell. Die beiden Spracherweiterungen ermöglichen im Gegensatz zur Reflexion und Annotation eine typsichere Abstraktion über die Datenstrukur - insbesondere werden Typfehler bereits bei der Kompilierung des Programs erkannt - statt zur Laufzeit.

Ein weiteres interessantes Beispiel ist [args4j](http://args4j.kohsuke.org/) - ein mit Reflexion umgesetzter Kommandozeilenparser für Java. Wir wollen anhand des letzten Beispiels die beiden Spracherweiterungen näher kennenlernen.

Bem.: Für diesen Artikel sind Erfahrungen mit Haskell und GHC von Vorteil.

<!-- more start -->

# Beispielprogramm in Java mit args4j #

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

im Hauptprogramm den Parser mit den folgenden Zeilen ausführen:

{% highlight java %}
public static void main(String[] args) throws IOException, CmdLineException {
    HelloWorldArgs parsedArguments = new HelloWorldArgs();
    CmdLineParser parser = new CmdLineParser(parsedArguments);
    parser.parseArgument(args);
}
{% endhighlight %}

Soweit die Kommandozeilenparameter erfolgreich geparst wurden, sind die entsprechenden Felder des
Objekts `parsedArguments` gesetzt.

# Beispielprogramm mit Kommandozeilenargumenten in Haskell #

Wir verwenden die Spracherweiterungen `DataKinds` und `DeriveGeneric`, um unsere Datenstruktur für die Kommandozeilenparameter zu beschreiben. Die erste Spracherweiterung erlaubt unter anderem Type-level Literals, so kann man Zeichenketten und Zahlen als Typen verwenden. Wir werden diese verwenden, um Hilfetexte und den Parameterpräfix an das Feld zu annotieren. Die zweite erlaubt es Code zu schreiben, der über die Struktur von beliebigen Typen (soweit diese bestimmte Eigenschaften erfüllen) definiert ist.

{% highlight haskell %}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}

import GHC.Generics
import System.Environment
import FP.GenericsExample
{% endhighlight %}

In der letzten Zeile importieren wir das Modul `FP.GenericsExample`, dass wir im Abschnitt "Generischer Kommondazeilenparser" weiter unten vorstellen werden.

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

Der Typkonstruktor `Parameter` kommt aus dem Modul `FP.GenericsExample`, es hat drei Typparameter:
Das erste Argument des Typkonstruktors `Parameter` ist der Datentyp, in dem die Werte des Feldes abgelegt werden sollen. Der Datentyp muss eine Instanz der Klasse `ParameterType` sein. Diese Klasse legt für den Typ fest, ob die Option weitere Argumente hat, ob diese optional oder notwendig sind und wie diese aus den Kommandozeilenargumenten ermittelt werden.

Die nächsten beiden Argumente des Typkonstruktors geben den Optionspräfix und den Hilfetext an. Den Parser kann man nun mit

{% highlight haskell %}
main =
    do args <- getArgs
       let helloWorldArgs :: HelloWorldArgs
           (helloWorldArgs, _unparsedArgs, _error) = getOptGeneric args
       putStrLn $ show $ helloWorldArgs
{% endhighlight %}

aufrufen. Die Funktion `getOptGeneric` liefert ein Tripel mit den verarbeiteten Kommandozeilenoptionen vom Typ `HelloWorldArgs`, sowie unverarbeiten Kommandozeilenargumenten und Fehlermeldungen. Dies ist bis auf die erste Komponente, dieselbe Struktur wie sie auch [getOpt](https://hackage.haskell.org/package/base-4.7.0.1/docs/System-Console-GetOpt.html#g:1) zurückliefert. Bei der ersten Komponente gehen wir in diesem Artikel einen Schritt weiter und erlauben Typen, deren Felder mit den Typkonstruktur `Parameter` definiert sind.

# Konventionelle Umsetzung #

Eine Umsetzung ohne Generics - wie Sie auch in der [Beschreibung](http://hackage.haskell.org/package/base-4.8.0.0/docs/System-Console-GetOpt.html) von dem Modul `System.Console.GetOpt`, dem Standard GHC Modul zur Verarbeitung von Kommandozeilenparametern vorgeschlagen wird - sieht wie folgt aus:

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

Abhängig von der Kommandozeile liefert `getOpt` eine Liste vom Typ `HelloWorldArgs -> HelloWorldArgs` zurück, deren Elemente dann auf den Initialwert `empty` angewendet werden.

Wir wollen natürlich die obigen Definitionen `empty` und `optDescr` "generisch", direkt aus der Datentypdefinition bestimmen, wofür wir [GHC.Generics](https://wiki.haskell.org/GHC.Generics) einsetzen.

# GHC.Generics

Der Ansatz bei [GHC.Generics](https://wiki.haskell.org/GHC.Generics) ist, eine Repräsentation des Datentyps mittels einer geringen Anzahl von Typkombinatoren zu realisieren. Hier eine - nicht korrekte - vereinfachte Darstellung:
{% highlight haskell %}
data Example = Foo String | Bar Bool Int
{% endhighlight %}
wird als
{% highlight haskell %}
type ExampleRep = String :+: (Bool :*: Int)
{% endhighlight %}
 dargestellt. In Wirklichkeit gibt es noch den Konstruktor `K1` vor den externen Datentypen und `M1` Konstruktoren, die Kontextdaten wie den Namen des Konstruktors (hier `Foo`, `Bar`) oder des Feldes enthalten. Die Funktionen `from` und `to` erzeugen eine bijektive Abbildung von Elementen des mittels Generics repräsentierten Typs in seine Repräsentationsform und zurück, d.h.

{% highlight haskell %}
from :: Example -> ExampleRep
to :: ExampleRep -> Example
{% endhighlight %}

wobei `from . to == id` und `to . from == id` gilt.

Der Vorteil der Repräsentation ist, dass man für eine Klasse, die man für beliebige Typen erzeugen will, nur noch Instanzen für eine kleine Menge an Typkonstruktoren (`:*:`, `:+:`, `K1`, `M1`, `U1`, `V1`) schreiben muss. (Es ist nicht immer notwendig, alle Konstruktoren zu instanzieren. Wenn man beispielsweise keine Datentypen mit mehreren Konstruktoren unterstützt, kann man den Typkonstruktor `:+:` weglassen.)

# Generischer Kommondazeilenparser #

In dem Modul `FP.GenericsExample` werden die generische Funktion `getOptGeneric`, sowie die Klasse `ParameterType` und die Typen `Parameter`, `ShortOpts` und `Description` definiert. Wir reexportieren auch die Funktionen und Typen, die in `System.Console.GetOpt` definiert wurden. Die Spracherweiterungen `DataKinds` und `DeriveGeneric` haben wir bereits im Abschnitt "Beispielprogramm mit Kommandozeilenargumenten in Haskell" kennengelernt. Wir benötigen in diesem Modul zusätzlich die Spracherweiterungen: [KindSignatures, ScopedTypeVariables](https://downloads.haskell.org/~ghc/7.8-latest/docs/html/users_guide/other-type-extensions.html), [TypeOperators](https://downloads.haskell.org/~ghc/7.8-latest/docs/html/users_guide/data-type-extensions.html), [FlexibleInstances, FlexibleContexts](https://downloads.haskell.org/~ghc/7.8.4/docs/html/users_guide/type-class-extensions.html).

{% highlight haskell %}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module FP.GenericsExample
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


## Generische Definition der `optDescs` und `empty` Funktionen

Die Klasse `OptDescriptions` verwenden wir intern um über die Struktur der Datentypen abstrahieren zu können. Die Funktion `empty` liefert den Wert des Datentypes (in Repräsentationsform) für den Fall, dass kein einziger Kommandozeilenparameter angegeben wurde, zurück.

Die Funktion `optDescs` liefert eine Liste von Optionsbeschreibungen - diese enthalten, die Namen der Kommandozeilenparameter, sowie die Wirkung eines Kommandozeilenarguments auf den Wert des Datentyps (wiederum in Repräsentationsform).

{% highlight haskell %}

class OptDescriptions f where
    empty :: f p
    optDescs :: [OptDescr (f p -> f p)]

{% endhighlight %}

Wir fangen mit der Instanz für den Typkonstruktor `K1 i c` an. Dieser Konstruktor wird in der Repräsentationsform vor jeden externen Typ `c` gesetzt. Hier definieren wir aber nur eine Instanz für `Parameter a (ShortOpts s) (Description d)`, da jedes Feld nach Spezifikation diese Form haben muss.

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

Hier werden aus den Typlevel-Literalen Zeichenketten. Dies funktioniert mit der Funktion `symbolVal`, die man sich als Ad-hoc polymorphe Funktionen vorstellen kann, die für jeden Typ vom Kind `Symbol` definiert ist.
{% highlight haskell %}
symbolVal (Proxy :: Proxy "foo") = "foo"
symbolVal (Proxy :: Proxy "bar") = "bar"
{% endhighlight %}

Auch die nächste Instanz ist ein sehr interessanter Fall. Wir wollen für das Produkt von zwei Datentypen die Funktionen `empty` und `optDescs` definieren. Natürlich wollen wir auf die Definition von `empty` und `optDescs` in den Basistypen zurückgreifen.

{% highlight haskell %}
instance (OptDescriptions f, OptDescriptions g) =>  OptDescriptions (f :*: g) where
    empty = empty :*: empty
    optDescs = map (fmap leftLift) optDescs ++ map (fmap rightLift) optDescs
        where
          leftLift f = \(x :*: y) -> (f x :*: y)
          rightLift f = \(x :*: y) -> (x :*: f y)
{% endhighlight %}

Der Typ `M1` wird verwendet, um syntaktische Informationen bereitszustellen, z.B. Namen von Konstrukoren und Feldern. Wir ignorieren diese Informationen in unserem Anwendungsfall.

{% highlight haskell %}
instance (OptDescriptions f) => OptDescriptions (M1 i t f) where
    empty = M1 $ empty
    optDescs = map (fmap f) optDescs
        where
          f g (M1 x) = M1 (g x)
{% endhighlight %}

Abschließend noch die Definition von `getOptGeneric`, die `getOpt` mit dem Repräsentationstyp von `a`, aufruft und die obigen beiden Funktionen `empty` und `optDescs` verwendet.

{% highlight haskell %}
getOptGeneric :: (Generic a, OptDescriptions (Rep a)) => [String] -> (a, [String], [String])
getOptGeneric args = (to (foldl (flip id) empty optList), unparsed, errs)
    where
      (optList, unparsed, errs) = getOpt RequireOrder optDescs args
{% endhighlight %}

# Mögliche Verbesserungen

Aktuell bieten wir keine Möglichkeit, auch Kommandozeilenargumente zu verarbeiten, die mit langen `--foobar` Präfixen versehen sind. Hierfür wäre es noch notwendig, Listen auf der Typebene zu verweden `HList` (siehe auch [Promoted lists and tuple types](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/promotion.html) und [HList](https://hackage.haskell.org/package/HList)), falls man so wie bei dem Modul System.Console.GetOpt mehrere Alternativen erlauben will.

Zeichenketten auf Typebene können auch auf Gleichheit innerhalb des Typsystems überprüft werden. Hierdurch wäre es möglich, bereits zum Kompilierungszeitpunkt eine Fehlermeldung zu liefern, falls ein Optionspräfix mehrfach verwendet wurde.

Mit Alternativen wäre es möglich, auch komplexere Kommandozeilenparameterabhängigkeiten darzustellen. Die Argumente für ein Kompressionsprogramm könnten z.B. wie folgt beschrieben werden.

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

Wir haben den Typ `Parameter` in der Form:

{% highlight haskell %}
data Parameter a b c
    = Parameter (Maybe a)
    deriving Show
{% endhighlight %}

definiert. Meistens ist es aber so, dass nicht alle Kommandozeilenoptionen optional sind. Für diesen Fall wäre es möglich, einen weiteren Typ `RequiredParameter` zu definieren, in dem kein `Maybe` mehr vorkommt.

<!-- more end -->

