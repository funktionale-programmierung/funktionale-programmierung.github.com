---
layout: post
description: Validierung mit applikativen Funktoren
title: "Funktionale Programmierung in der Praxis: Validierung mit applikativen Funktoren"
author: marco-schneider
tags: ["praxis", "haskell", "funktor", "functor", "applikativ", "applicative", "validierung", "validation",]
---

Unser erster Artikel in der Reihe "Funktionale Programmierung in der
Praxis" mit dem Thema *Datenvalidierung mit applikativen Funktoren*.

<!-- more start -->

Willkommen zu unserem ersten Artikel in der Reihe "Funktionale
Programmierung in der Praxis".  In dieser Reihe stellen wir anhand
echter Beispiele Konzepte aus der funktionalen Programmierung vor und
zeigen, wie wir sie *tatsächlich* tagtäglich in der Praxis anwenden.

Heute beschäftigen wir uns mit dem Thema *Datenvalidierung* und
zeigen, wie *applikative Funktoren* dabei helfen, Ungereimtheiten in
Daten in Daten systematisch aufzusammeln und zu verwerten.

# JSON möchte validiert werden

Webservices kommunizieren gerne in Form von JSON-Objekten miteinander,
die von User:innen und anderen Serivces in unserer Applikation landen.
Wie wir alle wissen, ist das JSON-Format durch die schmale Anzahl von
Typen recht eingeschränkt.  Das bedeutet in der Praxis, dass komplexe
Datentypen in der Regel in Strings serialisiert werden.  Außerdem
haben wir oft bestimmte Anforderungen an die Form des konkreten
Datums: Ein `"username"` soll nicht leer sein, eine `"email"` sollte
zumindest ein `"@"`-Symbol enthalten und serialisierte Werte müssen
unserer Deserialisierung entsprechen, um in interne Datentypen
umgewandelt werden zu können.

Für das folgende Beispiel gehen wir von folgender Datendefiniton aus.
Ein:e `User`:in besteht aus:
- einem Namen
- einer Emailadresse
- einer Rolle

Eine Rolle ist eines der folgenden:
- ein:e Entwickler:in
- ein:e Admin
- ein:e Bugreporter:in

Für die Implementierung verwenden wir die Programmiersprache Haskell.
Da könnte das folgendermaßen aussehen:

``` Haskell
data UserRole = ADMIN | DEVELOPER | REPORTER

data User = { userName  :: String
            , userEmail :: String
            , userRole  :: Role }

-- Beispiele
marco = User "Marco" "marco.schneider@active-group.de" DEVELOPER
simon = User "Simon" "simon.haerer.at.active-gropu.de" DEVELOPER
```

Die zwei Beispiele im Code zeigen direkt ein Problem: Wir wissen zwar,
dass `simon`s Emailadresse nicht valide ist, wenn wir sie aber direkt
aus einem JSON-Objekt lesen, hält uns nichts davon ab, eine solche
Invarante (*jede Emailadresse enthält ein "@"-Symbol*) zu ignorieren.
Wir müssen die Daten also irgendwie validieren, bevor sie weiter ins
System eindringen.

Natürlich hält uns nichts davon ab, einen Konstruktor für `User` zu
schreiben, der genau das abfängt:

``` Haskell
type Error = String

makeUser :: String -> String -> Role -> Either Error User
makeUser name email role
  | '@' `elem` email = Right $ User name email role
  | otherwise        = Left  $ "not a valid email"
```

Super, Problem gelöst.  Wenn alles glatt läuft, bekommen wir einen
`Right User`, andernfalls einen `Left Error`.  Obwohl, was ist, wenn
der Username leer ist?  Ein zweiter Versuch:

``` Haskell
type Error = String

makeUser :: String -> String -> Role -> Either Error User
makeUser name email role
  | null name        = Left "not a nonempty string"
  | '@' `elem` email = Right $ User name email role
  | otherwise        = Left "not a valid email"
```

Besser?  Kaum: Was ist, wenn sowohl der Name leer ist als auch die
`email` kein `'@'` enthält?  Wir wollen schließlich alle Fehler
wissen, nicht nur den ersten.  Das heißt aber auch, dass wir eine
andere Signatur brauchen (mit einer Liste von Fehlern für `Left`).

``` Haskell
type Error = String
type Errors = [Error]

makeUser :: String -> String -> UserRole -> Either Errors User
makeUser name email role =
  let nameOk = not $ null name
      emailOk = '@' `elem` email
      rightUser = Right $ User name email role
  in
    if nameOk && emailOk
    then rightUser
    else
      if nameOk && not emailOk
      then Left ["not a valid email"]
      else
        if not nameOk && emailOk
        then Left ["not a nonempty string"]
        else Left ["not a valid email" "not a nonempty string"]
 ```

Naja, so richtig zufrieden macht das nicht.  Außerdem explodiert die
Anzahl an fällen, die wir in immer weiteren Verzweigungen prüfen
müssen.  So wird das nichts.

Allerdings haben wir etwas darüber gelernt, was wir eigentlich wollen:

1. Wir wollen jede Validierungen für sich durchführen können
2. Mehrere Validierungen aneinanderhängen
3. Eine Validierungsfunktion schreiben, die dann ein validiertes
   Ergebnis zurück gibt, wenn alles in Ordnung ist **oder** eine Liste
   **aller** Fehler, falls etwas nicht stimmt.
   
# Validierungsfunktionen

Um dem Ganzen etwas Struktur zu geben, definieren wir zuerst einen
Datentyp, der analog unserer Erkundung des Gebiets von oben unseren
Validierungsergebnissen entspricht.  So ein Ergebnis ist eines der Folgenden:

- Eine erfolgreiche Validierung (`Ok <wert>`) eines Wertes, oder
- eine Menge an Fehlern, die bei der Validierung auftraten (`Fail Errors`).

```Haskell
type Errors = [String]

data Validation c = Ok c 
                  | Fail Errors deriving (Show, Eq)
```

Unsere `Validation` repräsentiert das Ergebnis einer Validierung.  Sie
ist parameterisiert über den Typ einen Kandidatenwerts `c`.  Damit
lassen sich Validierungen für Werte beliebigen Typs angeben.

Als nächstes schreiben wir ein paar Funktionen, die unsere
Validierungen von oben kapseln:

```Haskell
-- | Validate that `candidate` is not empty.
validateNonemptyString :: String -> Validation String
validateNonemptyString candidate
  | null candidate = Fail ["not a nonempty string"]
  | otherwise      = Ok candidate
  
-- | Validate that a string represents an email.
validateEmail :: String -> Validation String
validateEmail candidate
  | '@' `elem` candidate = Ok candidate
  | otherwise            = Fail ["not an email"]

-- | Validate that a `candidate` represents a `UserRole`.
validateUserRole :: String -> Validation UserRole
validateUserRole candidate
  | candidate == "ADMIN"     = Ok ADMIN
  | candidate == "DEVELOPER" = Ok DEVELOPER
  | candidate == "REPORTER"  = Ok REPORTER
  | otherwise                = Fail ["not a role"]
```

Damit können wir schon mal einzelne Validierungen vornehmen:

``` Haskell
validateNonemptyString "ok"
-- Ok "ok" : Validation String
validateNonemptyString ""
-- Fail ["not a nonempty string"] : Validation String

validateEmail "marco.schneider@active-group.de" 
-- Ok "marco.schneider@active-group.de" : Validation String
validateEmail "marco.schneider.at.active-group.de" 
-- Fail ["not a valid email"] : Validation String

validateUserRole "ADMIN"
-- Ok ADMIN : Validation UserRole
validateUserRole "DEVELOPER"
-- Ok DEVELOPER : Validation UserRole
validateUserRole "unknown"
-- Fail ["not a valid role"]
```

Punkt eins (jede Validierung für sich durchführen) ist damit abgehakt.
Jetzt wollen wir allerdings mehrere Ergebnisse aneinanderhängen (oder
kombinieren).

## Kombinieren von Ergebnissen

Was bedeutet es, mehrere Ergebnisse zu kombinieren?  Schauen wir uns
zuerst die Kombination von zwei Fehlerfällen an: Da beide Fehler aus
Listen von Fehlermeldungen bestehen, können wir diese aneinanderhängen
und behalten alle Informationen.  Das klingt doch gut!

``` Haskell
combineValidations :: Validation a -> Validation a -> Validation a
combineValidations (Fail es) (Fail fs) = Fail (es ++ fs)
```

Gut? Nein, überhaupt nicht gut!  Der Haskellcompiler ist zurecht
unzufrieden: Was ist denn, wenn links, rechts oder sogar an beiden
Stellen der Funktion ein `Ok` steht?

Unsere dritte Anforderungen meint: *Entweder* ein valides Ergebnis
oder *alle* Fehler.  Mit nur einem invaliden Ergebnis auf der linken
Seite werden wir also ohnehin nicht glücklikch, auch wenn rechts ein
Erfolg steht.  Also lassen wir die rechte Seite in dem Fall einfach
weg:

``` Haskell
combineValidations :: Validation a -> Validation a -> Validation a
combineValidations (Fail es) (Fail fs) = Fail (es ++ fs)  -- von oben
-- Wenn linkerhand ein Fehler ist und rechts ein Erfolg,
-- interessieren wir uns nicht mehr für den Erfolg:
combineValidations (Fail es) _ = Fail es
```

Okay, wir haben fast alle Fälle abgedeckt -- soweit auch nicht weiter
schwer.  Um den Haskellcomplier glücklich zu machen, brauchen wir noch
zwei Fälle:

1. Links ein Erfolg, rechts ein Fehler
2. Links ein Erfolg, rechts ein Erfolg

Jetzt brauche ich von Ihnen etwas Vertrauensvorschuss: Wir
funktionalen Programmierer:innen können nicht viel anderes als
Funktionen mit Argumenten zu füttern und zu schauen, was rauskommt
(und im Idealfall den Compiler dabei nicht unglücklich machen).  Uns
bleibt an dieser Stelle also nicht viel anderes übrig, also einfach so
zu tun, als stünde Links ein `Ok` dessen Wert eine Funktion ist, die
weiß, was mit dem Wert rechts zu tun ist.  Wenn wir das ein mal kurz
schlucken, dann geht es hier entlang:

```Haskell
applyOkFunctionToValidation :: (a -> b) -> Validation a -> Validation b
-- Nun, ein Misserfolg bleibt ein Misserfolg, 
applyOkFunctionToValidation _ (Fail es) = Fail es
-- Einen Erfolg möchten wir allerdings behalten.  Also nehmen wir, was
-- in den Erfolg eingewickelt ist und wenden `f` darauf an.  Wenn man
-- sich die Typsignatur anschaut, bleibt (außer der trivialen Lösung)
-- auch nicht viel anderes übrig.
applyOkFunctionToValidation f (Ok c) = Ok $ f c
```

Was steht hier?  Unter der Annahme, dass wir aus einem `Ok` auf der
linken Seite eine Funktion bekommen, können wir
`applyOkFunctionToValidation` verwenden, den Wert auf der rechten
Seite darauf anzuwenden.  Ein `Fail` auf der rechten Seite bleibt ein
Fehler (er planzt sich also gewissermaßen die Berechung entlang fort)
und bleibt unverändert.  Ein `Ok` hat ein weiteres `Ok` zur Folge,
indem das in das linke `Ok` gewickelte `f` darauf angewendet wird.
Damit haben wir aus zwei Erfolgen einen gemacht.

Jetzt könnten wir `applyOkFunctionToValidation` fast in
`combineValidations` einsetzen, wäre nicht dessen Typsignatur
`Validation c -> Validation c -> Validation c` im Weg -- wir brauchen ein
`Validation (a -> b) -> Validation a -> Validation b`.
Glücklicherweise haben wir das `a` in der Signatur bisher noch gar
nicht verwendet (bisher hat uns nur der Fehler interessiert und der
hat nichts mit `a` zu tun).  Ein Glück!  Damit ist
`combineValidations` fertig und kann benutzt werden.

```Haskell
applyOkFunctionToValidation :: (a -> b) -> Validation a -> Validation b
applyOkFunctionToValidation _ (Fail es) = Fail es
applyOkFunctionToValidation f (Ok c) = Ok $ f c

combineValidations :: Validation (a -> b) -> Validation a -> Validation b
combineValidations (Fail es) (Fail fs) = Fail (es ++ fs)
combineValidations (Fail es) _ = Fail es
combineValidations (Ok f) v = applyOkFunctionToValidation f v
```

## Fehlt da nicht etwas?

Ja, irgendwie schon.  Dummerweise können wir unsere oben definierten
Validierungsfunktionen nämlich jetzt nicht mehr ohne weiteres als
erstes Argument für `combineValidations` benutzen -- wir wollen ja
eine Funktion im `Ok`.  Um das Problem auch noch aus der Welt zu
schaffen, nehmen wir noch einen letzten Umweg.

Stellen wir uns vor, wir haben eine Validierung links, die, kombiniert
mit einer Validierung rechts, immer das rechte Ergebnis zurück gibt.
Haskell bietet uns schon die Funktion `id : a -> a` an, die immer
exakt ihr Argument zurück gibt.  Wickeln wir das in ein `Ok`, sieht
das Ganze so aus.

```Haskell
(Ok id) `combineValidations` validateNonemptyString ""
-- Fail ["not a nonempty string"]
(Ok id) `combineValidations` validateNonemptyString "Marco"
-- Ok "Marco"
```

Hilft uns das?  Ein bisschen.  Eigentlich ist es nämlich egal, welche
Funktion im `Ok` steckt.  Wir können also ganz allgemein aus jeder
Funktion eine Validierung machen:

```Haskell
makeValidation x = Ok x

makeValidation reverse `combineValidations` validateNonemptyString "Marco"
-- Ok "ocraM"
```

Bringt nicht viel, funktioniert aber.  Obwohl, hilft irgendwie schon
auch, denn ganz unauffällig hat sich hier eine Lösung für Punkte zwei
und drei von oben eingeschichen.

```Haskell
makeValidation User
  `combineValidations` (validateNonemptyString "Marco")
  `combineValidations` (validateEmail "marco.schneider@active-group.de")
  `combineValidations` (validateUserRole "DEVELOPER")
-- Ok (User {userName = "Marco", userEmail = "marco.schneider@active-group.de", userRole = ADMIN})
```

Wer es nicht glaubt, kann gerne die Fehlerfälle überprüfen:

```Haskell
-- Alles falsch
makeValidation User
  `combineValidations` (validateNonemptyString "")
  `combineValidations` (validateEmail "marco.schneider.at.active-group.de")
  `combineValidations` (validateUserRole "DEVELOPE")
-- Fail ["not a nonempty string","not an email","not a role"]

-- Teilweise falsch
makeValidation User
  `combineValidations` (validateNonemptyString "Marco")
  `combineValidations` (validateEmail "marco.schneider.at.active-group.de")
  `combineValidations` (validateUserRole "DEVELOPE")
-- Fail ["not an email","not a role"]
```

Das sieht jetzt vermutlich erst mal magisch aus: Warum wird aus
`makeValidation User` plötzlich eine Validationsfunktion, wenn sie
doch nur sich selbst zurück gibt.  Es führt kein Weg daran vorbei, wir
müssen uns kurz ansehen, wie man sich die einzelnen Substitutionen
vorstellen kann (wichtig: das dient nur der Verantschaulichung und
entspricht nicht wirklich der Ausfühungsmaschinerie).  Dabei trenne
jeweils die Repräsentation der Ausfühungsschritte durch ein `=>`:

```Haskell
makeValidation User
  `combineValidations` (validateNonemptyString "")
  `combineValidations` (validateEmail "marco.schneider.at.active-group.de")
  `combineValidations` (validateUserRole "DEVELOPER")

=> -- Einsetzen der Definition von `User`

(Ok (\name email role -> User name admin role))
  `combineValidations` (validateNonemptyString "Marco")
  `combineValidations` (validateEmail "marco.schneider@active-group.de")
  `combineValidations` (validateUserRole "DEVELOPER")

=> -- Ergebnis der ersten rechten Seite

(Ok (\name email role -> User name admin role))
  `combineValidations` (Ok "Marco")
  `combineValidations` (validateEmail "marco.schneider@active-group.de")
  `combineValidations` (validateUserRole "DEVELOPER")

=> -- Einsetzen des ersten Ergebnisses in die linke Seite

(Ok (\email role -> User "Marco" admin role))
  `combineValidations` (validateEmail "marco.schneider@active-group.de")
  `combineValidations` (validateUserRole "DEVELOPER")

=> -- jetzt wiederholt sich das ganze für die restlichen Argumente

(Ok (\email role -> User "Marco" admin role))
  `combineValidations` "marco.schneider@active-group.de"
  `combineValidations` (validateUserRole "DEVELOPER")

=> 

(Ok (\role -> User "Marco" "marco.schneider@active.group" role))
  `combineValidations` (validateUserRole "DEVELOPER")

=>

(Ok (\role -> User "Marco" "marco.schneider@active.group" role))
  `combineValidations` DEVELOPER

=>

(Ok (User "Marco" "marco.schneider@active.group" DEVELOPER))
```

Tadaa.  Alles in Ordnung.  Analog für den Fall, dass Fehler auftreten
(wir steigen ein wo es spannend wird):

```Haskell
(Ok (\email role -> User "Marco" admin role))
  `combineValidations` (validateEmail "marco.schneider.at.active-group.de")
  `combineValidations` (validateUserRole "DEVELOPER")
=>
(Ok (\email role -> User "Marco" admin role))
  `combineValidations` (Fail ["not an email"])
  `combineValidations` (validateUserRole "DEVELOPER")
=> Wir erinnern uns an oben:  Fehler rechts fressen `Ok`s links
(Fail ["not an email"])
  `combineValidations` (validateUserRole "DEVELOPER")
=> Und haben wir erst mal den Fehler, sammeln wir die restilchen Fehler nur noch zusammen.
(Fail ["not an email"])
  `combineValidations` (Fail ["not a role"])
=>
(Fail ["not an email" "not a role"])
```

Was uns hier natürlich hilft, ist, dass Haskell so nett ist Funktionen
partiell anzuwenden.  Das heißt eine Funktion mit drei Argumenten hat,
wenn wir ein Argument anwenden, eine Funktion mit zwei Argumenten als
Ergebnis.  In Sprachen, in denen das nicht der Fall ist (wie zum
Beispiel Clojure) müssen wir uns ein bisschen mehr anstrengen.

Damit haben wir tatsächlich alles an der Hand, um unsere
Validierungsfunktion zu schreiben:

```Haskell
validateUser :: String -> String -> String -> Validation User
validateUser name email role =
	makeValidation User
	  `combineValidations` validateNonemptyString name
	  `combineValidations` validateEmail email
	  `combineValidations` validateUserRole role
```

Der offizielle Teil ist damit geschafft und unsere drei Kriterien von
ob oben sind erfüllt:

1. Wir wollen jede Validierungen für sich durchführen können
   **:check:** indem wir Funktionen schreiben, die `Validation`s als
   Ergebnis haben
2. Mehrere Validierungen aneinanderhängen: **:check** mit
   `combineValidations`
3. Eine Validierungsfunktion schreiben, die dann ein validiertes
   Ergebnis zurück gibt, wenn alles in Ordnung ist **oder** eine Liste
   **aller** Fehler, falls etwas nicht stimmt. **:check** mit
   `validateUser`

Der Praxisteil ist damit beendet.  Wir haben uns angesehen, wir wir
komponierbare Validationsfunktionen für beliebige Datentypen schreiben
können, wie man sie miteinander verknüpft und dabei sicher sein kann,
wirklich alle Fehler mitzunehmen.

Wer sich allerdings fragt, was daran jetzt so unheimlich applikativ
ist und wo sich der Funktor versteckt, kann sich unten
weitervergnügen.

# Wer oder was ist hier ein applikativer Funktor?

Als funktionale Programmierer:innen sind wir immer daran interessiert,
allgemeinere Eigenschaften und Strukturen in unserem Code zu finden
(oder ihn von Anfang an so zu gestalten).  Ein Beispiel zur
Herangehensweise bei uns im Hause ist das "Finde-den-Funktor-Spiel" ™,
denn: Wo sich ein Funktor versteckt, ist vielleicht auch ein
Applikative Funktor, ist vielleicht auch eine Monade.  Oder allgemein:
Findet man erst ein bisschen Struktur, ist da meistens auch mehr.

Wer schon weiß, was einen Funktor in der Programmierung ausmacht, hat
ihn weiter oben schon entdeckt.  In Haskell schreibt man ihn als
Typklasse so auf:

``` Haskell
class Funktor f where
  fmap :: (a -> b) -> f a -> f b
```

Um jetzt nicht in metaphern darüber zu verfallen, was ein Funktor
*ist*, zeige ich lieber, wo sich oben der Funktor versteckt hat.  Wenn wir die Signatur von `fmap` anschauen, sieht das verdächtig nach `applyOkFunctionToValidation` aus!

``` Haskell
fmap                        :: (a -> b) -> f a -> f b
applyOkFunctionToValidation :: (a -> b) -> Validation a -> Validation b
```

Wir könnten also ganz allgemein sagen, dass unsere `Validation` ein
Funktor ist:

``` Haskell
instance Functor Validation where
  fmap _ (Fail es) = Fail es
  fmap f (Ok c)    = Ok $ f c
  -- oder einfacher
  -- fmap = applyOkFunctionToValidation
```

Mit diesem Wissen bewaffnet, können eine erste kleine Änderungen am
Code von oben durchführen.

``` Haskell
combineValidations :: Validation (a -> b) -> Validation a -> Validation b
combineValidations (Fail es) (Fail fs) = Fail (es ++ fs)
combineValidations (Fail es) _         = Fail es
combineValidations (Ok f)    v         = fmap f v
```

Das sieht nicht nach viel aus, ist aber doch schon einiges.  Unsere
Verwendung von `fmap` an dieser Stelle (und natürlich die
Implementierung von `Functor`) signalisieren allen Lesenden, dass 

- hier bestimmte Gesetze gelten (zu gesetzen bitten die Warnung weiter
  unten nicht übersehen)
- Alles, was für Funktoren sonst noch gilt und aller Code aller
  Bibliotheken, die sonst noch etwas mit Funktoren anzufangen wissen,
  für uns auch verwendbar sind.

## Applikativer Funktor

Irgendwie war ja schon klar, dass hier auch ein Applikativer Funktor
vesteckt ist -- sonst wäre der Titel des Posts eine ganz schöne
Nullnummer.

Schauen wir uns wieder zuerst die Definition der entsprechende
Typklasse in Haskell an, fokussiert auf den Teil, der uns
interessiert:

```Haskell
class Functor f => Applicative f where
  pure  :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b
```

Beide Signaturen könnten uns bereits bekannt vorkommen:

```Haskell
pure           :: a ->          f a
makeValidation :: a -> Validation a

(<*>)              ::          f (a -> b) ->          f a ->          f b
combineValidations :: Validation (a -> b) -> Validation a -> Validation b
```

Dann setzen wir mal ein:
```Haskell
instance Applicative Validation where
  pure  = makeValidation
  (<*>) = combineValidations
```

Un zu guter Letzt:

```Haskell
validateUser :: String -> String -> String -> Validation User
validateUser name email role =
	pure User <*> validateNonemptyString name
	          <*> validateEmail email
	          <*> validateUserRole role
```

Netterweise bekommen wir (wie schon beim `Functor`) alles geschenkt,
was sonst noch für applikative Funktoren zu haben ist.  Nur ein
kleines Beispiel: der `<$>`-Operator macht die Validierung noch ein
kleines bisschen hübscher:

```Haskell
validateUser email name role =
  User <$> validateNonemptyString "Marco"
       <*> validateEmail "marco.schneider@active-group.de"
       <*> validateUserRole "DEVELOPER"
```

Oder, wem die kuriosen `<*>`- und `<$>`-Symbole zu wild sind:

```Haskell
validateUser email name role =
  liftA3 User (validateNonemptyString "Marco")
              (validateEmail "marco.schneider@active-group.de")
              (validateUserRole "DEVELOPER")
```

**Wichtig**: Noch eine Bemerkung zum Thema Typklassen und Signaturen.
Nur, weil die Signaturen von Funktionen so aussehen, als könnten Sie
zu einer Typklasse passen, heißt das noch lange nicht, dass daraus
auch eine *korrekte* Typklasse wird.  Ein solche Klasse besteht in der
Regel aus:

1. Einer Menge von Operationen (`fmap` oder `pure` plus `<*>`).
2. Einer Menge an Gesetzen, die für Werte des Typs unter diesen
   Operationen gelten müssen.

Haskell gibt uns leider nicht die richtigen Mittel an die Hand um
sicherzustellen, dass die jeweiligen Gesetze auch von der Instanz
eingehalten werden.  Zu zeigen, dass unsere Instanzen korrekt sind,
sprengt allerdings den Rahmen dieses ohnehin schon viel zu langen
Posts.
