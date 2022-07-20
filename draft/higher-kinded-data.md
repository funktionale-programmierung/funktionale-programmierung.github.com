---
layout: post
description: Beschreibung
title: "Higher-Kinded Data für Konfigurationen in Haskell"
author: felix-leitz
tags: ["higher-kinded data","haskell"]
---

In diesem Artikel werden wir sehen, wie wir mit *Higher-Kinded Data* in Haskell
nutzen können, um Konfigurationen in unseren Programmen abzubilden.

<!-- more start -->

## Erster Versuch ##

Angenommen wir haben ein Programm, dass ein *Passwort*, die URL eines
*Service*'s und dessen *Port* als Konfiguration benötigt.
Diese Konfiguration könnten wir dann durch folgenden Datentype modelieren.

``` haskell
data Config = Config
  { password :: String
  , serviceUrl :: String
  , servicePort :: Int
  }
  deriving (Show)
`````````

Wir nehmen an, dass die einzelnen Teile der Konfiguration, zum Start des Programms,
aus Umgebungsvariablen ausgelesen werden. Wird eine Umgebungsvariable nicht gesetzt,
oder kann der gegebene Wert nicht interpretiert werden, soll ein Standard-Wert
verwendet werden. Zusätzlich muss das Passwort immer zur Laufzeit angegeben werden
und besitzt deshalb keinen Standard-Wert.

Die Funktion die all diese umsetzt, könnte z.B. so aussehen:

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
`````````

Wir sehen hier drei Hilfsfunktionen, die das einlesen und interpretieren der jeweiligen
Teile der Konfiguration übernehmen. In der Funktion `getConfig` wird alles zusammengesetzt.
Hier findet einersteits das zusammenbauen des `Config` Datentyps statt, aber auch das
vereinigen der Standard-Werte mit den eingelesenen.

Dieser Ansatz hat einige Probleme. Einerseits vermischt die Funktion `getConfig` das bauen
der Konfiguration, das lesen der einzelnen Parameter und das zusammenbauen mit den
Standart-Werten. Dies führt dazu, dass nicht direkt klar ist ob ein Feld einene Standard-Wert
hat und zusätzlich muss für den nächsten Konfigurations-Typ wieder alles neu geschrieben werden.

## Zweiter Versuch ##

Eine weitere Möglichkeit wäre den folgenden Datentyp zu wählen:

``` haskell
data Config' static dynamic = Config
  { password :: dynamic String
  , serviceUrl :: static String
  , servicePort :: static Int
  }
```

Wie wir sehen können wurden im Vergleich zum ersten `Config'` Datentyp zwei Typ-Parameter eingeführt.
Diese markieren bestimme Felder als *dynamisch* oder *statisch*. Mit *dynamisch* ist gemeint, dass
dies Feld nur zur Laufzeit einen Wert hat, *statische* Felder hingegen haben schon zur Compilezeit
zugewiesene Werte.

Um diese Trennung auch auf Typ Ebene umzusetzten, definieren wir uns drei Typ-Aliase.

``` haskell
type DefaultConfig = Config' Identity Proxy
type PartialConfig = Config' Maybe Identity
type Config = Config' Identity Identity
```

Für die statischen Werte nutzen wir `Identity`, dies führt dazu, dass in allen Definitionen von einer
Konfiguration zur Compilezeit diese Werte vorhanden sein müssen. Mit `Proxy` für dynamische Werte erreichen
wir das Gegenteil, diese Werte dürfen zur Compilezeit **nicht** vorhanden sein.

Damit lässt sich nun eine Standard-Konfiguration definieren.

``` haskell
defaultConfig :: DefaultConfig
defaultConfig =
  Config
    { password = Proxy
    , serviceUrl = Identity "localhost"
    , servicePort = Identity 8080
    }
```

Diese Definition enthält zwar nur die Werte die wir statisch kennen, allerdings sind hier noch störende
aufrufe von `Proxy` und `Identity` nötig.

Die Funktion die nun zur Laufzeit die Umgebungsvariablen einliest sieht wie folt aus:

``` haskell
readinPartialConfig :: IO PartialConfig
readinPartialConfig =
  Config
    <$> (Identity <$> getPassword)
    <*> getUrl
    <*> getPort
```

Hier ist zu beachten, dass sowohl die Url als auch der Port eingelesen werden können, aber nicht notwendigerweise
vorhanden sein müssen. Das Passwort dagegen, muss dynamisch geladen werden.

Der letzte fehlende Baustein ist die Funktion die eine Standard-Konfiguration und eine partielle Konfiguration
miteinander verbindet.

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

Die Funktion `combineConfig` macht genau das, sie setze eine Konfiguration Feld
für Feld zusammen. Dabei wird immer der Wert aus der `PartialConfig` dem aus
der `DefaultConfig` vorgezogen.

Die Funktion `getConfig` nutzt dann nur die bisher definierten Funktionen.

``` haskell
getConfig :: IO Config
getConfig = combineConfig defaultConfig <$> readinPartialConfig
```

Mit unserer neuen Definition, des Konfigurations Datentyps, konnten wir einige Verbesserungen erzielen.
An der Definition von `Config'` ist klar erkennbar, welche Felder Standard-Werte haben und welche nicht und
es gibt einen Wert `defaultConfig` der alle diese Werte enthält.
Leider ist der Code immernoch nicht wiederverwendbar. Für eine neue Konfiguration müssen wir immernoch
den Datentyp und alle Funktionen neu schreiben.

## Typfamilien zur Hilfe ##

Um alle Probleme des vorherigen Ansatzes zu lösen, definieren wir uns die Typfamilie `HKD`. So modelieren
wir das anwenden von Typfunktionen auf Typen.

``` haskell
type HKD :: (Type -> Type) -> Type -> Type
type family HKD f a where
  HKD Identity a = a
  HKD f a = f a
```

Wir definieren das die Typfuntion `Identity` angewant auf einen Typ `a` immer den Typ `a` ergibt. Für alle
anderen Typfuntionen `f` ist das Resultat einfach die Typfuntion angewant auf `a`, also `f a`.
Durch diese Definition ersparen wir uns später unnötige aurufe von `Identity`.

Unser Konfigurationstyp ist unverändert, bis auf den aufruf von `HKD` im Typ der einzelnen Felder. Außerdem
müssen wir aus technischen Gründen noch eine Instanz der Typklasse `Generic` generieren lassen.

``` haskell
data Config' static dynamic = Config
  { password :: HKD dynamic String
  , serviceUrl :: HKD static String
  , servicePort :: HKD static Int
  }
  deriving (Generic)
```

Auch die Definition von `defaultConfig` ist fast die selbe, bis auf die fehlenden aufrufe des `Identity` Konstruktors.

``` haskell
defaultConfig :: DefaultConfig
defaultConfig =
  Config
    { password = Proxy
    , serviceUrl = "localhost"
    , servicePort = 8080
    }
```

Für das einlesen der partiellen Konfiguration gilt das gleiche, auch hier fehlt nur der Aufruf des `Identity` Konstruktors.

``` haskell
readinPartialConfig :: IO PartialConfig
readinPartialConfig =
  Config
    <$> getPassword
      <*> getUrl
      <*> getPort
```

Dadurch das wir bei der Definition von `Config'` die Typklasse `Generic` abgeleitet haben,
steht uns jetzt die Funktion `genericApply`, mit dem im folgendem etwas vereinfachten Typ, zur Verfügung.

``` haskell
genericApply :: c Identity Proxy -> c Maybe Identity -> c Identity Identity
```

Durch das ersetzen von der Typvariable `c` mit `Config'` erhalten wir den selben Typ wie für `combineConfig`.
Damit haben wir alles um die Funktion `getConfig` zu bauen.

``` haskell
getConfig :: IO Config
getConfig = genericApply defaultConfig <$> readinPartialConfig
```

Mit dieser Iteration von `Config'` haben wir es endlich geschafft. Für einen neuen Konfigurationstyp müssen wir nur noch
den Typ, den Standard-Wert und das einlesen der dynamischen Werte definieren. Das kombinieren bekommen wir geschenkt.
Insbesondere kann die `HKD` Typfamilie, die Funktion `genericApply` und die nötigen Hilfsfunktionen in eine Bibliothek
ausgelagert werden, da sie immer gleich sind unabhängig von der `Config'` Definition.

Durch drei weitere kleine Typ-Aliase ist es sogar möglich die verbliebenen Vorkommen von `Proxy` und `Identity` zu entfernen.

``` haskell
type Default c = c Identity Proxy
type Partial c = c Maybe Identity
type Complete c = c Identity Identity

dynamic :: Proxy a
dynamic = Proxy
```

Auch diese Definitionen können in die Bibliothek ausgelagert werden.

## Fazit ##

Mit Hilfe von Typfunktionen und Higher-Kinded Data haben wir es geschafft, die Standard-Werte userer Konfiguration vom einlesen
der dynamischen Werte zu trennen, wir konnten auf Typ Ebene klar machen welche Werte dynamisch und welche statisch bekannt sind und
wir konnten die Funktion zu deren Kombination auslagern, sodass nur der interessante Teil neu geschrieben werden
muss.

<!-- more end -->