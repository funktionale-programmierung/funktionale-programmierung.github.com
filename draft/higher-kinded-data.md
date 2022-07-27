---
layout: post
description: Beschreibung
title: "Higher-Kinded Data für Konfigurationen in Haskell"
author: felix-leitz
tags: ["higher-kinded data","haskell"]
---

In diesem Artikel werden wir sehen, wie wir *Higher-Kinded Data* in Haskell
nutzen können, um Konfigurationen in unseren Programmen abzubilden.

<!-- more start -->

## Erster Versuch ##

Angenommen wir haben ein Programm, das ein *Passwort*, die *URL eines
Services* und dessen *Port* als Konfiguration benötigt.
Diese Konfiguration könnten wir dann durch folgenden Datentyp modellieren.

``` haskell
data Config = Config
  { password :: String
  , serviceUrl :: String
  , servicePort :: Int
  }
  deriving (Show)
```

Wir nehmen an, dass die einzelnen Teile der Konfiguration zum Start des Programms
aus Umgebungsvariablen ausgelesen werden. Wird eine Umgebungsvariable nicht gesetzt,
oder kann der gegebene Wert nicht interpretiert werden, soll ein Standardwert
verwendet werden. Zusätzlich muss das Passwort immer zur Laufzeit angegeben werden
und besitzt deshalb keinen Standardwert.

Die Funktion, die all dies umsetzt, könnte z. B. so aussehen:

``` haskell
getConfig :: IO Config
getConfig = do
  pw <- getPassword
  url <- fromMaybe "localhost" <$> getUrl
  port <- fromMaybe 8080 <$> getPort
  pure $ Config pw url port

getPassword :: IO String
getPassword =
  fromMaybe (error "Environment variable PASSWORD not set")
    <$> lookupEnv "PASSWORD"

getUrl :: IO (Maybe String)
getUrl = lookupEnv "SERVICE_URL"

getPort :: IO (Maybe Int)
getPort = (readMaybe =<<) <$> lookupEnv "SERVICE_PORT"
```

Wir sehen hier drei Hilfsfunktionen, die das Einlesen und Interpretieren der jeweiligen
Teile der Konfiguration übernehmen. In der Funktion `getConfig` wird alles zusammengesetzt.
Hier findet sowohl das Zusammenbauen des `Config`-Datentyps als auch das
Vereinigen der Standardwerte mit den eingelesenen statt.

Dieser Ansatz hat mindestens zwei Probleme: 

- Die Funktion `getConfig` vermischt das Bauen der Konfiguration mit
  dem Lesen der einzelnen Parameter und dem Zusammenbauen mit
  Standardwerten. Dies führt dazu, dass nicht direkt klar ist, ob ein
  Feld einen Standardwert hat.
- Weitere Konfigurations-Typen müssen ihre eigene `getConfig`-Funktion schreiben.

## Zweiter Versuch ##

Eine weitere Möglichkeit wäre, den folgenden Datentyp zu wählen:

``` haskell
data Config' static dynamic = Config
  { password :: dynamic String
  , serviceUrl :: static String
  , servicePort :: static Int
  }
```

Wie wir sehen, wurden im Vergleich zum ersten `Config'`-Datentyp zwei Typ-Parameter eingeführt.
Diese markieren bestimmte Felder als *dynamisch* oder *statisch*, wobei *dynamisch* meinen soll, dass
dieses Feld nur zur Laufzeit einen Wert hat und *statische* Felder hingegen haben schon zur Compile-Zeit
zugewiesene Werte.

<!---
FIXME Der Part oben sollte noch klarer formuliert werden (Johannes/Kaan)
FIXME Der Satz unten auch (Johannes)
-->


Um diese Trennung auch auf Typebene umzusetzen, definieren wir uns die folgenden Typ-Aliase:

``` haskell
type DefaultConfig = Config' Identity Proxy
type PartialConfig = Config' Maybe Identity
type Config = Config' Identity Identity
```

Für die statischen Werte nutzen wir `Identity`. Dies führt dazu, dass in allen Definitionen einer
Konfiguration zur Compile-Zeit diese Werte vorhanden sein müssen. Mit `Proxy` für dynamische Werte erreichen
wir das Gegenteil, diese Werte dürfen zur Compilezeit **nicht** vorhanden sein.

<!---
FIXME "Um Semantik hinter dynamic und static zu implementieren, nutzen wir die zwei 
       eingebauten Datentypen Identity und Proxy."
FIXME Identity und Proxy erläutern
-->

Damit lässt sich nun eine Standard-Konfiguration definieren:

``` haskell
defaultConfig :: DefaultConfig
defaultConfig =
  Config
    { password = Proxy
    , serviceUrl = Identity "localhost"
    , servicePort = Identity 8080
    }
```

Diese Definition enthält zwar nur die Werte, die wir statisch kennen, allerdings sind hier noch
Aufrufe von `Proxy` und `Identity` nötig, die die Lesbarkeit erschweren.

Die Funktion, die nun zur Laufzeit die Umgebungsvariablen einliest, sieht wie folgt aus:

``` haskell
readInPartialConfig :: IO PartialConfig
readInPartialConfig =
  Config
    <$> (Identity <$> getPassword)
    <*> getUrl
    <*> getPort
```

Hier ist zu beachten, dass sowohl die Url als auch der Port eingelesen werden können, aber nicht notwendigerweise
vorhanden sein müssen. Das Passwort dagegen muss dynamisch geladen werden.

Der letzte fehlende Baustein ist die Funktion, die die Standard- und
partielle Konfiguration zur finalen `Config` kombiniert. Dabei werden
jeweils Werte aus der `PartialConfig` denen aus der `DefaultConfig`
vorgezogen.

``` haskell
combineConfig :: DefaultConfig -> PartialConfig -> Config
combineConfig
  (Config _defaultPasswordProxy defaultServiceURL defaultServicePort)
  (Config pw url port) =
    Config
      pw
      (maybe defaultServiceURL Identity url)
      (maybe defaultServicePort Identity port)
```

Die Funktion `getConfig` nutzt nun lediglich die bisher definierten Funktionen:

``` haskell
getConfig :: IO Config
getConfig = combineConfig defaultConfig <$> readInPartialConfig
```

Mit unserer neuen Definition des Konfigurationsdatentyps konnten wir das erste der obigen Probleme beheben:
An der Definition von `Config'` ist klar erkennbar, welche Felder Standardwerte haben und welche nicht und
es gibt einen Wert `defaultConfig`, der alle diese Werte enthält.

Leider ist der Code so nicht wiederverwendbar. Für eine neue Konfiguration müssen wir immer noch
den Datentyp und manche Funktionen neu schreiben.

## Typfamilien zur Hilfe ##

Um den Code wiederverwendbar zu machen, definieren wir uns die
Typfamilie `HKD` (Higher-Kinded Data). So modellieren wir das Anwenden
von Typfunktionen auf Typen:

``` haskell
type HKD :: (Type -> Type) -> Type -> Type
type family HKD f a where
  HKD Identity a = a
  HKD f a = f a
```

Wir definieren, dass die Typfunktion `Identity` angewandt auf einen Typ `a` immer den Typ `a` ergibt. Für alle
anderen Typfunktionen `f` ist das Resultat die Typfunktion angewandt auf `a`, also `f a`.
Durch diese Definition ersparen wir uns später unnötige Aufrufe von `Identity`.

<!---
FIXME Umformulieren? (Johannes)
-->

Unser Konfigurationstyp ist unverändert bis auf den Aufruf von `HKD` in den Signaturen der Felder. Außerdem
müssen wir aus technischen Gründen noch eine Instanz der Typklasse `Generic` generieren lassen.

``` haskell
data Config' static dynamic = Config
  { password :: HKD dynamic String
  , serviceUrl :: HKD static String
  , servicePort :: HKD static Int
  }
  deriving (Generic)
```

Auch die Definition von `defaultConfig` ist fast dieselbe, bis auf die fehlenden Aufrufe des `Identity` Konstruktors:

``` haskell
defaultConfig :: DefaultConfig
defaultConfig =
  Config
    { password = Proxy
    , serviceUrl = "localhost"
    , servicePort = 8080
    }
```

Analog für die partielle Konfiguration:

``` haskell
readInPartialConfig :: IO PartialConfig
readInPartialConfig =
  Config
    <$> getPassword
      <*> getUrl
      <*> getPort
```

Dadurch, dass wir bei der Definition von `Config'` die Typklasse `Generic` abgeleitet haben,
steht uns jetzt die Funktion `genericApply`, mit dem im Folgenden etwas vereinfachten Typ, zur Verfügung:

<!-- genericApply steht nicht zur Verfügung, muss selbst geschrieben werden! 
     Woher kommt genericApply?!
-->

``` haskell
genericApply :: c Identity Proxy -> c Maybe Identity -> c Identity Identity
```

Durch das Ersetzen der Typvariable `c` mit `Config'` erhalten wir denselben Typ wie für `combineConfig`.
Damit haben wir alles, um die Funktion `getConfig` zu bauen:

``` haskell
getConfig :: IO Config
getConfig = genericApply defaultConfig <$> readInPartialConfig
```

Mit dieser Iteration von `Config'` haben wir es endlich geschafft. Für einen neuen Konfigurationstyp müssen wir nur noch
den Typ, den Standardwert und das Einlesen der dynamischen Werte definieren. Das Kombinieren bekommen wir geschenkt.
Insbesondere können die `HKD`-Typfamilie, die Funktion `genericApply` und die nötigen Hilfsfunktionen in eine Bibliothek
ausgelagert werden, da sie unabhängig von der konkreten `Config'`-Definition sind.

Durch drei weitere kleine Typ-Aliase ist es sogar möglich, `Proxy` und `Identity` zu entfernen:

``` haskell
type Default c = c Identity Proxy
type Partial c = c Maybe Identity
type Complete c = c Identity Identity

dynamic :: Proxy a
dynamic = Proxy
```

Final sieht das dann so aus:

<!-- Hier noch Beispielcode aufschreiben -->

Auch diese Definitionen können in die Bibliothek ausgelagert werden.

## Fazit ##

Mit Hilfe von Typfunktionen und Higher-Kinded Data haben wir es geschafft, die Standardwerte unserer Konfiguration vom Einlesen
der dynamischen Werte zu trennen. Wir konnten auf Typebene klar machen, welche Werte dynamisch und welche statisch bekannt sind.
Zu guter Letzt konnten wir Funktionen auslagern, sodass nur relevante Teile neugeschrieben werden müssen.

<!-- more end -->
