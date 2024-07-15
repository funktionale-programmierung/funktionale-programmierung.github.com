---
layout: post
description: Beschreibung
title: "Kommandozeilenparser in Haskell - Teil 2"
author: emin-karayel
tags: ["Haskell"]
---

Im [ersten Teil des Artikels](/2015/07/16/haskell-kommandozeilenparser-1.html) haben wir Kommandozeilenoptionen mit der Bibliothek [System.Console.GetOpt](https://hackage.haskell.org/package/base-4.7.0.1/docs/System-Console-GetOpt.html) verarbeitet.

Die geparsten Kommandozeilenoptionen wurden, dabei in Form einer Liste zurückgeliefert, die man durchsuchen muss, um festzustellen, ob eine bestimmte Kommandozeilenoption angegeben wurde. Da es für die Weiterverarbeitung der Kommandozeilenoptionen in der Anwendung von Vorteil ist, die Optionen als Record-Typ darzustellen, haben wir eine Umwandlungsfunktion geschrieben, die die Daten von der Form als Liste des Summentyps in den Produkttyp umwandelt.

Der Ansatz führte jedoch dazu, dass wir jede Kommandozeilenoption einmal als Konstruktor im Summentyp, und einmal als Feld im Recordtyp definiert haben. Zusammen mit dem Eintrag in der Optionsliste (vom Typ `[OptDescr a]`) und dem Code in der Umwandlungsfunktion hatten wir dadurch vier Stellen, die angepasst werden müssten, wenn man z.B. eine Kommandozeilenoption hinzufügt. Allerdings kann es bei späteren Anpassungen leicht passieren, dass man den Summentyp erweitert, aber den Eintrag in der Optionsliste nicht anpasst, ohne dass es hierdurch zu einem Compilerfehler kommt.

Bei der Entwicklung von komplexen Anwendungen sind redundant vorhandene Informationen oft eine Fehlerquelle und führen zu höherem Wartungsaufwand (vgl. auch [DRY Prinzip](https://de.wikipedia.org/wiki/Don%27t_repeat_yourself)).  Um die Redundanz an dieser Stelle zu reduzieren kann man in Haskell die Spracherweiterungen [Generics](https://wiki.haskell.org/GHC.Generics) und [Literale auf der Typebene](https://downloads.haskell.org/~ghc/7.8.4/docs/html/users_guide/type-level-literals.html) einsetzen. Die beiden Spracherweiterungen ermöglichen eine typsichere Abstraktion über die Datenstrukur - insbesondere werden hierdurch Fehler bereits bei der Kompilierung des Programms erkannt, statt erst zur Laufzeit.

Wir wollen in diesem zweiten Teil des Artikels mit Hilfe der obigen Spracherweiterung einen generischen Kommandozeilenparser entwickeln, mit dem das Parsen von Kommandozeilenoptionen in einen Record-Typ ohne redundanten Code möglich ist.

<!-- more start -->

# Beispielprogramm mit einem generischen Kommandozeilenparser
Das [Beispielprogramm](http://funktionale-programmierung.de/2015/07/16/haskell-kommandozeilenparser-1.html) aus unserem letzten Artikel wird mit dem generischen Kommandozeilenparser, den wir in den nächsten Abschnitten entwickeln werden, viel kürzer:

{% highlight haskell %}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}

import GHC.Generics
import System.Environment
import FP.GenericsExample
{% endhighlight %}

In der letzten Zeile importieren wir das Modul `FP.GenericsExample`, das wir im Abschnitt "Generischer Kommondazeilenparser" weiter unten vorstellen werden.

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

data CompressProgramArgs
    = CompressProgramArgs
      { cp_compressionLevel
          :: Option CompressionLevel (ShortOpts "l") (Description "compression level")
      , cp_inputFile
          :: Option File (ShortOpts "i") (Description "input file")
      , cp_outFile
          :: Option File (ShortOpts "o") (Description "output file")
      , cp_force
          :: Option Bool (ShortOpts "f") (Description "force")
      }
      deriving (Show, Generic)
{% endhighlight %}

Der Typkonstruktor `Option` kommt aus dem Modul `FP.GenericsExample`. Es hat drei Typparameter:  Der erste Parameter ist der Datentyp, in dem die Werte des Feldes abgelegt werden sollen. Der Datentyp muss eine Instanz der Klasse `ParameterType` sein. Diese Klasse legt für den Typ fest, ob die Option notwendig ist, ob die Option weitere Argumente hat, ob diese wiederum optional oder notwendig sind und wie diese aus den Kommandozeilenargumenten ermittelt werden.

Die nächsten beiden Parameter des Typkonstruktors `Option` geben den Optionspräfix und den Hilfetext an. Den Parser kann man nun mit

{% highlight haskell %}
main :: IO ()
main =
    do args <- getArgs
       compressProgramArgs <-
           case getOptGeneric args of
             Right r -> return r
             Left err -> fail err
       putStrLn $ show (compressProgramArgs :: CompressProgramArgs)
{% endhighlight %}

aufrufen. Die Funktion `getOptGeneric` liefert entweder eine Fehlermeldung zurück, falls das Parsen der Kommandozeilenargumente fehlgeschlagen ist, oder einen Wert vom dem oben definierten Typ `CompressProgramArgs` züruck.  Wobei wir hier, da die Funktion generisch ist auch einen beliebigen anderen Record-Typ mit Feldern die mit dem Konstruktor `Option` definiert sind, verwenden können.

In den folgenden Abschnitten wollen wir die Spracherweiterungen, die wir für die Implementation der obigen Funktion `getOptGeneric` benötigen werden, kennenlernen.

# Type-Level Literals
Mit Type-Level Literals kann man Zeichenketten und Zahlen auch bei der Definition von Datentypen verwenden.  Wir werden hiervon Gebrauch machen, um die zugehörige Kommandozeilenoption direkt an die Datentypdefinition zu annotieren:

{% highlight haskell %}
data ProgramArgs
    = ProgramArgs
      { ex_compressionLevel
          :: Parameter CompressionLevel (ShortOpts "l") (Description "compression level")
      }
      deriving (Show, Generic)
{% endhighlight %}

Auf die Zeichenkette kann man über die polymorphe Funktion [`symbolVal`](https://hackage.haskell.org/package/base-4.8.1.0/docs/GHC-TypeLits.html#v:symbolVal), die in der Typklasse [`KnownSymbol`](https://hackage.haskell.org/package/base-4.8.1.0/docs/GHC-TypeLits.html#t:KnownSymbol) definiert ist, zugreifen, z.B. gilt:

{% highlight haskell %}
symbolVal (Proxy :: Proxy "foo") == "foo"
{% endhighlight %}

# Generics
Mit Generics kann man über die Struktur von algebraischen Typen abstrahieren. Wir wollen uns die Idee an einem einfachen Beispiel veranschaunlichen, dazu betrachten wir zuerst den Typ `Person` mit einem Konsturktor

{% highlight haskell %}
data Person = Person { firstName :: String, lastName :: String, age :: Int }
{% endhighlight %}

und stellen fest, dass man diesen bijektiv in das Produkt der Typen `String`, `String` und `Int` abbilden kann, d.h. es gibt Funktionen von `Person` nach `((String, String), Int)` und zurück, mit der Definition

{% highlight haskell %}
toPersonRep :: Person -> ((String, String), Int)
toPersonRep (Person { firstName = u, lastName = v, age = w}) = ((u, v), w)

fromPersonRep :: ((String, String), Int) -> Person
fromPersonRep ((u, v), w) = (Person { firstName = u, lastName = v, age = w})
{% endhighlight %}

und den Eigenschaften `fromPersonRep (toPersonRep x) == x` und `toPersonRep (fromPersonRep x) == x`.
Ähnlich kann ein Datentyp mit mehreren Konstruktoren

{% highlight haskell %}
data LegalPerson
    = RealPerson { firstName :: String, lastName :: String, age :: Int }
    | Company { legalName :: String }
{% endhighlight %}

in einem Summentyp abgebildet werden, d.h. es gibt eine bijektive Abbildung von `LegalPerson` nach `Either ((String, String), Int) String`.  Die Funktion setzt den ersten Konstruktor `RealPerson` in `Left` um, und den zweiten Konstruktor `Company` in `Right` um.

{% highlight haskell %}
toLegalPersonRep :: LegalPerson -> Either ((String, String), Int) String
toLegalPersonRep (RealPerson { firstName = u, lastName = v, age = w}) = Left ((u, v), w)
toLegalPersonRep (Company { legalName = x}) = Right x

fromLegalPersonRep :: Either (String, String, Int) String -> LegalPerson
fromLegalPersonRep (Left ((u, v), w)) = RealPerson { firstName = u, lastName = v, age = w}
fromLegalPersonRep (Right x) = Company { legalName = x }
{% endhighlight %}

Ein wichtiger Aspekt ist, dass wir für die isomorphe Darstellung nur zwei Typkonstruktoren (mit zwei Typparametern) gebraucht haben: `(a,b)` und `Either a b`, durch Iteration kann man beliebig viele Konstruktoren oder Felder auf diese beiden abbilden.

## Type families

Polymorphe Funktionen werden in Haskell durch Typklassen definiert, so dass die Implementation sich für jede Instanz, die die Typklasse implementiert unterscheiden kann. Typfamillien erweitern, unter anderem, diese Funktionalität und bieten die Möglichkeit auch für jede Instanz individuelle Typzuordnungen zu definieren. Am obigen Beispiel können wir hierdurch bei der Definition einer Klasse für die beiden Typen `LegalPerson` und `Person` unterschiedliche Repräsentationstypen angeben.

Beispielsweise ist `Show` für `String`, `Int`, `(,)` und `Either` definiert, um den Inhalt von `LegalPerson` auszugeben können wir bis jetzt ohne eine Typklasse nur `show . toLegalPersonRep` hinschreiben, bzw. `show . toPersonRep` die Übersetzung müssen wir noch explizit hinschreiben.  Mit der folgenden Klasse `GenericRepresetable`, in der wir die Typfamillie `Rep` einführen:

{% highlight haskell %}
class GenericRepresetable a where
    type Rep a
    to :: a -> Rep a
    from :: Rep a -> a
{% endhighlight %}

können wir in der Instanz den entsprechenden Repräsentationstyp hinschreiben, z.B. bei `Person`:

{% highlight haskell %}
instance GenericRepresetable Person where
    type Rep = ((String, String), Int)
    to = toPersonRep
    from = fromPersonRep
{% endhighlight %}

und eine `showGeneric` Funktion schreiben, in der wir Ausnutzen, dass `Show` für den Repräsentationstypen definiert ist:

{% highlight haskell %}
showGeneric :: (GenericRepresetable a, Show (Rep a)) => a -> String
showGeneric = show . to
{% endhighlight %}

Wir werden bei der Entwicklung des generischen Kommandozeilenparsers weiter unten Typfamillien an einer zweiten Stelle einsetzen, um dem Record-Typen in den wir die Kommandozeilenargumente übersetzen wollen, den Summentyp in dem die Kommandozeilenoptionen von der Funktion `getOpt` zurückgeliefert werden, mit der Typfamillie `OptListType` zuzuordnen.

## Automatische Erzeugung von Generic-Instanzen
Durch die Anweisung
{% highlight haskell %}
    deriving Generic
{% endhighlight %}
kann man eine solche isomorphe Darstellung `Rep a` für beliebige algebraische Datentypen `a` automatisch erzeugen lassen, dabei werden auch die Funktionen `fromRep :: Rep a f -> a` und `toRep :: a -> Rep a f` erzeugt. Dieser ist mit den Typ-Konstruktoren `:*:`, `:+:`, `K1`, `M1`, `U1`, `V1` konstruiert (statt wie in unserem vereinfachten Beispiel mit `Either` und Paaren.)
Die Typklassen `Rep` und `Generic` sowie die angegebenen Typ-Konstruktoren
sind dabei im Modul
[`GHC.Generics`](https://hackage.haskell.org/package/base-4.8.1.0/docs/GHC-Generics.html) definiert.

Die beiden Typkonstruktoren `:*:`, `:+:` haben die selbe Funktion wie Paare und die `Either` Konstruktion von oben. Die Infixschreibweise erleichtert die Lesbarkeit, vorallem wenn es mehrere Alternativen gibt.  (Statt `Either (Either a b) c` - schreibt man `a :+: b :+: c`.)  Hierfür benötigt man die Spracherweiterung [Type Operators](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/data-type-extensions.html#type-operators).

Der Konstruktor `V1` wird nur für leere Datentypen verwendet (keine Werte), `U1` für Konstruktoren ohne Werte, bspw. Enumerationen. Der Konstruktor `M1` wird - vor jedem Feld, vor jedem Konstruktor, und vor dem ganzen Typen gesetzt und enthält Metadaten zu dem Typ (z.B. Feldnamen, Konstruktornamen und den Namen des Datentyps.)

Die Definition ist ein Kapsellungs-Datentyp (`newtype`), der zwei Phantomparameter enthält, (d.h. Typparameter die nicht in der Definition des Datentyps verwendet werden.)
{% highlight haskell %}
newtype M1 i t f p = M1 { unM1 :: f p }
{% endhighlight %}

Vor allen Feldern wird noch der Konstruktor `K1` eingesetzt, der ähnlich wie der Konstruktor `M1` nur ein Kapsellungs-Datentyp ist.

Bem.: Der Typparameter `f` wird bei `deriving Generic` nicht verwendet - er ist deshalb da, damit es möglich ist, dass einige der Konstruktoren auch bei `deriving Generic1` für Kind `* -> *` Typen wieder verwendet werden können.


# Das Modul FP.GenericsExample

In dem Modul `FP.GenericsExample` werden die generische Funktion `getOptGeneric`, sowie die Klasse `ParameterType` und die Typen `Parameter`, `ShortOpts` und `Description` definiert. Wir reexportieren auch die Funktionen und Typen, die in `System.Console.GetOpt` definiert wurden.

{% highlight haskell %}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
module FP.GenericsExample
    ( getOptGeneric
    , ParameterType(..)
    , Option(..)
    , ShortOpts(..)
    , Description(..)
    , G.ArgDescr(..)
    ) where

import Control.Monad
import Data.Proxy
import Data.Either
import GHC.Generics
import GHC.TypeLits
import qualified System.Console.GetOpt as G
{% endhighlight %}

Die Klasse `ParameterType` gibt an, ob der Kommandozeilenparameter notwendig oder optional ist, falls er optional ist, wird ein Standardwert mit der Funktion `defaultValue` zurückgeliefert, sowie möglicherweise selbst Argumente hat und wie diese ausgelesen werden. Wir verwenden den Typ, der auch in `System.Console.GetOpt` verwendet wird: (`ArgDescr a`)

{% highlight haskell %}
class ParameterType a where
    defaultValue :: Maybe a
    argDescr :: G.ArgDescr a
{% endhighlight %}

Für eine optionale Kommandozeilenoption vom Typ `Bool` ohne Argumente, deren Wert `True` wird falls die Option angeben wurde, können wir die Instanz Beispielsweise wie folgt definieren:

{% highlight haskell %}
instance ParameterType Bool where
    defaultValue = Just False
    argDescr = G.NoArg True
{% endhighlight %}

Hier definieren wir den schon oben beschriebenen Typkonstruktor `Option`:

{% highlight haskell %}
data Option a b c = Option a
    deriving Show

data ShortOpts (l :: Symbol)
data Description (d :: Symbol)
{% endhighlight %}

Die beiden Typen `ShortOpts` und `Description` haben keine Werte - sie werden nur als Phantomtypen verwendet um die Optionen zu beschreiben, z.B. für den Hilfetext (`Description "compression level"`).

## Erzeugung der Optionsbeschreibungen

Die Klasse `OptDescriptions` verwenden wir intern, um über die Struktur der Datentypen abstrahieren zu können. Mit der Typfamillie `OptListType a` entwickeln wir einen Summentyp für die einzelnen Optionen (da die Funktion `getOpt` so aufgebaut ist, dass sie eine Liste der geparsten Optionen zurückliefert). Die Funktion `fromOptionListToArgs` liefert aus einer Liste vom Typ `OptListType a` einen Wert vom Typ `a`, dem Darstellungstypen des Record-Typs, bzw. eine Fehlermeldung zurück, falls eine notwenidge Option in der Liste gefehlt hat.  Die Funktion `optDescriptions` liefert eine Liste von Optionsbeschreibungen für die Kommandozeilenoptionen zurück.

{% highlight haskell %}

class OptDescriptions a where
    type OptListType a
    optDescriptions :: Proxy a -> [OptDescr (OptListType a)]
    fromOptionListToArgs :: [OptListType a] -> ErrorM (a f)

{% endhighlight %}

Wir fangen mit der Instanz für den Typkonstruktor `K1 i c` an. Dieser Konstruktor wird in der Repräsentationsform vor jeden externen Typ `c` gesetzt. Hier definieren wir aber nur eine Instanz für `Option a (ShortOpts s) (Description d)`, da jedes Feld nach Spezifikation diese Form haben muss.

{% highlight haskell %}

instance (KnownSymbol s, KnownSymbol d, ParameterType a)
    => OptDescriptions (K1 i (Option a (ShortOpts s) (Description d))) where
    type OptListType (K1 i (Option a (ShortOpts s) (Description d))) =
        (Option a (ShortOpts s) (Description d))
    optDescriptions Proxy = [G.Option shortOptions [] liftedArgDescr (description)]
        where
          shortOptions = symbolVal (Proxy :: Proxy s)
          description = symbolVal (Proxy :: Proxy d)
          liftedArgDescr = fmap Option argDescr
    fromOptionListToArgs l =
        case l of
          [] -> maybe missingErr (Right . K1 . Option) defaultValue
          (l:_) -> Right (K1 l)
        where
          missingErr = Left $ "Missing required option " ++ (symbolVal (Proxy :: Proxy s))
{% endhighlight %}

Die nächste Instanz ist der interessante Fall. Wir wollen für das Produkt von zwei Datentypen die Funktionen `fromOptionListToArgs` und `optDescriptions` definieren.

{% highlight haskell %}
instance (OptDescriptions a, OptDescriptions b) => OptDescriptions (a :*: b) where
    type OptListType (a :*: b) = Either (OptListType a) (OptListType b)
    optDescriptions Proxy =
        (fmap (fmap Left) (optDescriptions (Proxy :: Proxy a)))
        ++ (fmap (fmap Right) (optDescriptions (Proxy :: Proxy b)))
    fromOptionListToArgs os =
        let (l, r) = partitionEithers os in
        liftM2 (:*:) (fromOptionListToArgs l) (fromOptionListToArgs r)
{% endhighlight %}

Der Typ `M1` wird verwendet, um syntaktische Informationen bereitszustellen, z.B. Namen von Konstrukoren und Feldern. Wir ignorieren diese Informationen in unserem Anwendungsfall.

{% highlight haskell %}
instance (OptDescriptions a) => OptDescriptions (M1 i c a) where
    type OptListType (M1 i c a) = OptListType a
    optDescriptions Proxy = optDescriptions (Proxy :: Proxy a)
    fromOptionListToArgs = liftM M1 . fromOptionListToArgs
{% endhighlight %}

Abschließend noch die Definition von `getOptGeneric`, die `getOpt` mit dem Repräsentationstyp von `a`, aufruft und die beiden polymorphen Funktionen `optDescriptions` und `fromOptionListToArgs` verwendet.

{% highlight haskell %}
getOptGeneric ::
    forall a. (Generic a, OptDescriptions (Rep a))
    => [String]
    -> Either String a
getOptGeneric args =
    case G.getOpt G.RequireOrder (optDescriptions (Proxy :: Proxy (Rep a))) args of
      (optList, [], []) -> liftM to (fromOptionListToArgs optList)
      (_, unparsed, errs) -> Left $ concat [show errs, ", unparsed args: ", (show unparsed)]
{% endhighlight %}

# Mögliche Verbesserungen

Aktuell bieten wir keine Möglichkeit, auch Kommandozeilenargumente zu verarbeiten, die mit langen `--foobar` Präfixen versehen sind. Hierfür wäre es noch notwendig, Listen auf der Typebene zu verweden (siehe auch [Datatype promotion](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/promotion.html)), um so wie bei dem Modul System.Console.GetOpt mehrere Alternativen für eine Option angeben zukönnen.

Noch ist es nicht möglich den gleichen Parametertyp als notwendige und optionale Kommandozeilenoption an unterschiedlichen Stellen zu verwenden. Mit zwei verschiedenen Typkonstruktoren `RequiredOption` und `OptionalOption`

{% highlight haskell %}
data OptionalOption a b c = OptionalOption (Maybe a)
data RequiredOption a b c = RequiredOption a
{% endhighlight %}

wäre es möglich diese Eigenschaft direkt in die Beschreibung des Record-Typen aufzunehmen. Eine weitere Erweiterung wäre der Typkonstruktor `RepeatableOption` für wiederholbare Kommandozeilenoptionen.

{% highlight haskell %}
data RepeatableOption a b c = RepeatableOption [a]
{% endhighlight %}

Zeichenketten auf Typebene können auch auf Gleichheit innerhalb des Typsystems überprüft werden. Hierdurch wäre es möglich, bereits zum Kompilierungszeitpunkt eine Fehlermeldung zu liefern, falls ein Optionspräfix mehrfach verwendet wurde.

Mit Alternativen wäre es möglich, auch komplexere Kommandozeilenparameterabhängigkeiten darzustellen. Die Argumente für ein Kompressionsprogramm könnten z.B. wie folgt beschrieben werden.

{% highlight haskell %}
data CmdLineArgs
    = Help (CmdLineAlternative (ShortOpts "-h") (Description "Print usage information") ())
    | Compress (CmdLineAlternative (ShortOpts "-c") (Description "Compress") CompressArgs)
    | Decompress (CmdLineAlternative (ShortOpts "-d") (Description "Compress") DecompressArgs)

data CompressArgs
    = CompressArgs
      { c_inFile :: RequiredOption File (ShortOpts "-i") (Description "Input file")
      , c_outFile :: RequiredOption File (ShortOpts "-o") (Description "Output file")
      , c_compressionLevel :: OptionalOption CompressionLevel (ShortOpts "-l")
          (Description "Compression level")
      }

data DecompressArgs
    = DecompressArgs
      { d_inFile :: RequiredOption File (ShortOpts "-i") (Description "Input file")
      , d_outFile :: RequiredOption File (ShortOpts "-o") (Description "Output file")
      }
{% endhighlight %}

<!-- more end -->
