---
layout: post
description: Validierung mit applikativen Funktoren
title: "Funktionale Programmierung in der Praxis: Validierung mit applikativen Funktoren"
author: marco-schneider
tags: ["Haskell", "Validierung"]
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
Daten systematisch aufzusammeln und zu verwerten.

# Wir wollen JSON validieren

Webservices kommunizieren gerne in Form von JSON-Objekten miteinander,
die von User:innen und anderen Serivces in unserer Applikation landen.
Es ist kein Geheimnis, dass das JSON-Format durch seine schmale Anzahl
von Typen eingeschränkt ist.  Das bedeutet in der Praxis, dass
komplexe Datentypen in der Regel in Strings serialisiert werden.
Außerdem haben wir oft bestimmte Anforderungen an die Form des
konkreten Datums: Ein `"username"` soll nicht leer sein, eine
`"email"` sollte zumindest ein `"@"`-Symbol enthalten und
serialisierte Werte müssen unserer Erwartung entsprechen, um in
interne Datentypen umgewandelt werden zu können.

Für das folgende Beispiel gehen wir von folgender Datenanalyse aus.
Ein:e Nutzer:in besteht aus:
- einem Namen
- einer E-Mail-Adresse
- einer Rolle

Eine Rolle ist eines der folgenden:
- ein:e Entwickler:in
- ein:e Admin
- ein:e Bugreporter:in

Für die Implementierung verwenden wir die Programmiersprache Haskell.
Da könnte das folgendermaßen aussehen:

```haskell
data UserRole = ADMIN | DEVELOPER | REPORTER

data User = { userName  :: String
            , userEmail :: String
            , userRole  :: UserRole }

-- Beispiele
marco = User "Marco" "marco.schneider@active-group.de" DEVELOPER
simon = User "Simon" "simon.haerer.at.active-group.de" DEVELOPER
```

Die zwei Beispiele im Code zeigen direkt ein Problem: Wir wissen zwar,
dass die E-Mail-Adresse von Simon nicht valide ist, wenn wir sie aber
direkt aus einem JSON-Objekt lesen, hält uns nichts davon ab, eine
solche Invariante (*"jede Emailadresse enthält ein "@"-Symbol"*) zu
ignorieren.  Wir müssen die Daten also irgendwie validieren, bevor sie
weiter ins System eindringen.

Wir könnten natürlich einen Konstruktor für `User` schreiben, der
solche Fehler abfängt.  Wir benutzen für das Ergebnis `Either a b =
Left a | Right b`.  Dabei gilt:

- Einen Fehler signalisiert man mit `Left a`
- Einen Erfolg signalisiert man mit `Right b`

```haskell
type Error = String

makeUser :: String -> String -> Role -> Either Error User
makeUser name email role
  | '@' `elem` email = Right (User name email role)
  | otherwise        = Left  "not a valid email"
```

Super, Problem gelöst.  Wenn alles glattläuft, bekommen wir einen
`Right User`, andernfalls einen `Left Error`.  Obwohl, was ist, wenn
der Username leer ist?  Ein zweiter Versuch:

```haskell
type Error = String

makeUser :: String -> String -> Role -> Either Error User
makeUser name email role
  | null name        = Left "not a nonempty string"
  | '@' `elem` email = Right (User name email role)
  | otherwise        = Left "not a valid email"
```

Das ist etwas besser.  Was ist, wenn sowohl der `name` leer ist als
auch die `email` kein `'@'` enthält?  Wir wollen schließlich alle
Fehler wissen, nicht nur den ersten.  Das heißt aber auch, dass wir
eine andere Signatur brauchen (mit einer Liste von Fehlern für
`Left`).

```haskell
type Error = String
type Errors = [Error]

makeUser :: String -> String -> UserRole -> Either Errors User
makeUser name email role =
  let nameOk = not (null name)
      emailOk = '@' `elem` email
  in
    if nameOk && emailOk
    then Right (User name email role)
    else
      if nameOk && not emailOk
      then Left ["not a valid email"]
      else
        if not nameOk && emailOk
        then Left ["not a nonempty string"]
        else Left ["not a valid email" "not a nonempty string"]
 ```

Naja, so richtig zufrieden macht das nicht.  Außerdem explodiert die
Anzahl an Fällen, die wir in immer weiteren Verzweigungen prüfen
müssen.  So wird das nichts.

Allerdings -- denke ich -- haben wir etwas darüber gelernt, was wir
eigentlich wollen:

1. Wir wollen jede Validierung für sich durchführen können
2. Wir wollen mehrere Validierungen kombinieren können
3. Wir wollen eine Validierungsfunktion schreiben, die genau dann ein
   validiertes Ergebnis zurückgibt, wenn alles in Ordnung ist
   **oder** eine Liste **aller** Fehler, falls etwas nicht stimmt.
   
# Validierungsfunktionen

Um dem Ganzen etwas Struktur zu geben, definieren wir zuerst einen
eigenen Datentyp der unseren Validierungsergebnissen entspricht.  Ein
solches Ergebnis ist eines der folgenden:

- Eine erfolgreiche Validierung (`Ok <wert>`) eines Wertes, oder
- eine Liste von Fehlern, die bei der Validierung auftraten (`Fail
  Errors`).  Warum eine Liste?  Stellen wir uns die Validierung eines
  Webformulars mit mehr als einem Formularfeld vor.  Es würde nerven,
  immer nur einen Fehler gesagt zu bekommen, um nach dessen Korrektur
  den nächsten Fehler zu sehen.  Wir möchten darum lieber alle Fehler
  auf ein mal haben.

```haskell
type Errors = [String]

data Validation c = Ok c 
                  | Fail Errors
```

Unsere `Validation` repräsentiert das Ergebnis einer Validierung.  Sie
ist parameterisiert über den Typ eines Kandidatenwerts `c`.  Damit
lassen sich Validierungen für Werte beliebigen Typs angeben.

Als Nächstes schreiben wir ein paar Funktionen, die unsere
Validierungen von oben darstellen:

```haskell
-- | Validate that `candidate` is not the empty string.
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

```haskell
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
Jetzt wollen wir allerdings mehrere Ergebnisse miteinander
kombinieren.

## Kombinieren von Ergebnissen

Schauen wir uns zuerst die Kombination von zwei Fehlerfällen an: Da
beide Fehler aus Listen von Fehlermeldungen bestehen, können wir diese
aneinanderhängen und behalten alle Informationen.  Das klingt doch
gut!

```haskell
combineValidations :: Validation a -> Validation a -> Validation a
combineValidations (Fail es) (Fail fs) = Fail (es ++ fs)
```

Der Haskellcompiler ist natürlich noch unzufrieden: Was ist, wenn
links, rechts oder sogar an beiden Stellen der Funktion ein `Ok`
steht?

Unsere dritte Anforderungen fordert: *Entweder* ein valides Ergebnis
oder *alle* Fehler.  Mit nur einem `Fail` auf der linken Seite wissen
wir schon, dass es nur noch auf ein `Fail` hinauslaufen kann, auch
wenn rechts ein Erfolg steht.  Also lassen wir die rechte Seite in dem
Fall einfach weg:

```haskell
combineValidations :: Validation a -> Validation a -> Validation a
combineValidations (Fail es) (Fail fs) = Fail (es ++ fs)  -- von oben
-- Wenn linkerhand ein Fehler ist und rechts ein Erfolg,
-- interessieren wir uns nicht mehr für den Erfolg:
combineValidations (Fail es) _ = Fail es
```

Okay, wir haben zwei von vier Fällen abgedeckt -- so weit, so einfach.
Um den Haskellcomplier glücklich zu machen, brauchen wir noch zwei
Fälle:

1. Links ein Erfolg, rechts ein Fehler
2. Links ein Erfolg, rechts ein Erfolg

Das klingt jetzt erst mal komisch, aber: Wir funktionalen
Programmierer:innen haben nicht so fürchterlich viele Werkzeuge zur
Verfügung.  Wir können Funktionen mit Argumenten füttern und
schauen, was das Ergebnis ist (oder im Fall von Haskell so lange
probieren, bis der Compiler sein *Okay* gibt).  Uns bleibt an dieser
Stelle also nicht viel anderes übrig, als einfach so zu tun, als
stünde links ein `Ok` dessen Wert eine Funktion ist, die weiß, was mit
dem Wert rechts zu tun ist.  Wenn wir das einmal kurz schlucken,
können wir uns dafür eine kleine Hilfsfunktion schreiben.

```haskell
applyOkFunctionToValidation :: (a -> b) -> Validation a -> Validation b
-- Nun, ein Misserfolg bleibt ein Misserfolg, 
applyOkFunctionToValidation _ (Fail es) = Fail es
-- Einen Erfolg möchten wir allerdings behalten.  Also nehmen wir, was
-- in den Erfolg eingewickelt ist und wenden `f` darauf an.  Wenn man
-- sich die Typsignatur anschaut, bleibt (außer der trivialen Lösung)
-- auch nicht viel anderes übrig.
applyOkFunctionToValidation f (Ok c) = Ok (f c)
```

Was steht hier?  Unter der Annahme, dass wir aus einem `Ok` auf der
linken Seite eine Funktion bekommen, können wir
`applyOkFunctionToValidation` verwenden, um den Wert auf der rechten
Seite darauf anzuwenden.  Ein `Fail` auf der rechten Seite bleibt ein
Fehler (er pflanzt sich also gewissermaßen der Berechung entlang fort)
und bleibt unverändert.  Ein `Ok` hat ein weiteres `Ok` zur Folge,
indem das in das linke `Ok` gewickelte `f` darauf angewendet wird.  So
haben wir aus zwei Erfolgen einen gemacht.

Jetzt könnten wir `applyOkFunctionToValidation` fast in
`combineValidations` einsetzen, wäre nicht dessen Typsignatur
`Validation c -> Validation c -> Validation c` im Weg -- wir brauchen
ein `Validation (a -> b) -> Validation a -> Validation b`.  Wer genau
aufgepasst hat, hat aber gemerkt: Wir haben das `a` in der Signatur
bisher noch gar nicht verwendet (bisher hat uns nur der Fehler
interessiert und der hat nichts mit `a` zu tun).  Wir können also die
Signatur problemlos ändern.  Damit ist `combineValidations` fertig und
kann benutzt werden.

```haskell
applyOkFunctionToValidation :: (a -> b) -> Validation a -> Validation b
applyOkFunctionToValidation _ (Fail es) = Fail es
applyOkFunctionToValidation f (Ok c) = Ok (f c)

combineValidations :: Validation (a -> b) -> Validation a -> Validation b
combineValidations (Fail es) (Fail fs) = Fail (es ++ fs)
combineValidations (Fail es) _ = Fail es
combineValidations (Ok f) v = applyOkFunctionToValidation f v
```

## Fehlt da nicht etwas?

Ja, irgendwie schon.  Dummerweise können wir unsere oben definierten
Validierungsfunktionen nämlich jetzt nicht mehr ohne Weiteres als
erstes Argument für `combineValidations` benutzen -- wir wollen ja
eine Funktion im `Ok`.  Um auch das Problem aus der Welt zu schaffen,
nehmen wir einen letzten Umweg.

Stellen wir uns vor, wir haben eine Validierung links, die, kombiniert
mit einer Validierung rechts, immer das rechte Ergebnis zurückgibt.
Haskell bietet uns schon die Funktion `id : a -> a` an, deren Ergebnis
immer exakt ihr Argument ist.  Wickeln wir das in ein `Ok`, sieht das
Ganze so aus.

```haskell
(Ok id) `combineValidations` validateNonemptyString ""
-- Fail ["not a nonempty string"]
(Ok id) `combineValidations` validateNonemptyString "Marco"
-- Ok "Marco"
```

Hilft uns das?  Ein bisschen.  Eigentlich ist es nämlich egal, welche
Funktion in `Ok` steckt.  Wir können also ganz allgemein aus jeder
Funktion eine Validierung machen:

```haskell
makeValidation x = Ok x

makeValidation reverse `combineValidations` validateNonemptyString "Marco"
-- Ok "ocraM"
```

Bringt nicht viel, funktioniert aber.  Obwohl, hilft irgendwie schon,
denn ganz unauffällig hat sich hier eine Lösung für Punkte zwei und
drei von oben eingeschlichen.

```haskell
makeValidation User
  `combineValidations` (validateNonemptyString "Marco")
  `combineValidations` (validateEmail "marco.schneider@active-group.de")
  `combineValidations` (validateUserRole "DEVELOPER")
-- Ok (User {userName = "Marco", userEmail = "marco.schneider@active-group.de", userRole = ADMIN})
```

Wer es nicht glaubt, kann gerne die Fehlerfälle überprüfen:

```haskell
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
`makeValidation User` plötzlich eine Validierungsfunktion, wenn sie
doch nur den ursprünglichen Konstruktor einwickelt?  Es führt kein Weg
daran vorbei, wir müssen uns kurz ansehen, wie man sich die einzelnen
Substitutionen vorstellen kann (wichtig: das dient nur der
Veranschaulichung und entspricht nicht wirklich der
Ausfühungsmaschinerie).  Dabei trenne ich jeweils die Repräsentation
der Ausführungsschritte durch ein `=>`:

```haskell
makeValidation User
  `combineValidations` (validateNonemptyString "")
  `combineValidations` (validateEmail "marco.schneider@active-group.de")
  `combineValidations` (validateUserRole "DEVELOPER")

=> Einsetzen der Definition von `User`

(Ok (\name email role -> User name admin role))
  `combineValidations` (validateNonemptyString "Marco")
  `combineValidations` (validateEmail "marco.schneider@active-group.de")
  `combineValidations` (validateUserRole "DEVELOPER")

=> Ergebnis der ersten rechten Seite

(Ok (\name email role -> User name admin role))
  `combineValidations` (Ok "Marco")
  `combineValidations` (validateEmail "marco.schneider@active-group.de")
  `combineValidations` (validateUserRole "DEVELOPER")

=> Einsetzen des ersten Ergebnisses in die linke Seite

(Ok (\email role -> User "Marco" admin role))
  `combineValidations` (validateEmail "marco.schneider@active-group.de")
  `combineValidations` (validateUserRole "DEVELOPER")

=> Jetzt wiederholt sich das ganze für die restlichen Argumente

(Ok (\email role -> User "Marco" admin role))
  `combineValidations` (Ok "marco.schneider@active-group.de")
  `combineValidations` (validateUserRole "DEVELOPER")

=> 

(Ok (\role -> User "Marco" "marco.schneider@active.group" role))
  `combineValidations` (validateUserRole "DEVELOPER")

=>

(Ok (\role -> User "Marco" "marco.schneider@active.group" role))
  `combineValidations` (Ok DEVELOPER)

=>

(Ok (User "Marco" "marco.schneider@active.group" DEVELOPER))
```

Presto!  Alles in Ordnung.  Analog für den Fall, dass Fehler auftreten
(wir steigen ein wo es spannend wird):

```haskell
(Ok (\email role -> User "Marco" admin role))
  `combineValidations` (validateEmail "marco.schneider.at.active-group.de")
  `combineValidations` (validateUserRole "DEVELOPER")

=>

(Ok (\email role -> User "Marco" admin role))
  `combineValidations` (Fail ["not an email"])
  `combineValidations` (validateUserRole "DEVELOPER")

=> Wir erinnern uns an oben:  Fehler rechts fressen `Ok`s links

(Fail ["not an email"])
  `combineValidations` (validateUserRole "dummy")

=> Und haben wir erst mal den Fehler, sammeln wir die restlichen Fehler nur noch zusammen.

(Fail ["not an email"])
  `combineValidations` (Fail ["not a role"])

=>

(Fail ["not an email" "not a role"])
```

Was uns hier im Hintegrund hilft, ist, dass in Haskell Funktionen
partiell angewendet werden können.  Das heißt eine Funktion mit drei
Argumenten hat, wenn wir ein Argument anwenden, eine Funktion mit zwei
Argumenten als Ergebnis.  In Sprachen, in denen das nicht der Fall ist
(wie zum Beispiel Clojure) müssen wir uns ein bisschen mehr
anstrengen.

Damit haben wir tatsächlich alles an der Hand, um unsere
Validierungsfunktion zu schreiben:

```haskell
validateUser :: String -> String -> String -> Validation User
validateUser name email role =
	makeValidation User
	  `combineValidations` validateNonemptyString name
	  `combineValidations` validateEmail email
	  `combineValidations` validateUserRole role
```

Der offizielle Teil ist geschafft, denn unsere drei Kriterien von oben
sind erfüllt.  Wir wollen

1. jede Validierungen für sich durchführen können -> wir
   schreiben Funktionen die `Validation`s als Ergebnis haben
2. Mehrere Validierungen aneinanderhängen -> mit `combineValidations`
3. eine Validierungsfunktion schreiben, die dann ein validiertes
   Ergebnis zurück gibt, wenn alles in Ordnung ist **oder** eine Liste
   **aller** Fehler, falls etwas nicht stimmt -> mit `validateUser`

Der Praxisteil ist damit beendet.  Wir haben uns angesehen, wie wir
komponierbare Validierungsfunktionen für beliebige Datentypen
schreiben können, wie man sie miteinander verknüpft und dabei sicher
sein kann, wirklich alle Fehler mitzunehmen.

Wer sich allerdings fragt, was daran jetzt so unheimlich applikativ
ist und wo sich der Funktor versteckt, kann sich unten
weitervergnügen.

# Wer oder was ist hier ein applikativer Funktor?

Als funktionale Programmierer:innen sind wir immer daran interessiert,
allgemeinere Eigenschaften und Strukturen in unserem Code zu finden
(oder ihn von Anfang an so zu gestalten).  Ein Beispiel zur
Herangehensweise bei uns im Hause ist das "Finde-den-Funktor-Spiel"™,
denn: Wo sich ein Funktor versteckt, ist vielleicht auch ein
applikativer Funktor, ist vielleicht auch eine Monade.  Oder
allgemein: Findet man erst ein bisschen Struktur, ist da meistens noch
mehr.

Wer schon weiß, was einen Funktor in der Programmierung ausmacht, hat
ihn weiter oben schon entdeckt.  In Haskell schreibt man ihn als
Typklasse so auf:

```haskell
class Functor f where
  fmap :: (a -> b) -> f a -> f b
```

Um jetzt nicht in Metaphern darüber zu verfallen, was ein Funktor
*ist*, zeige ich lieber, wo sich oben der Funktor versteckt hat.  Wenn
wir die Signatur von `fmap` anschauen, sieht das verdächtig nach
`applyOkFunctionToValidation` aus!

```haskell
fmap                        :: (a -> b) ->          f a ->          f b
applyOkFunctionToValidation :: (a -> b) -> Validation a -> Validation b
```

Wir könnten also ganz allgemein sagen, dass unsere `Validation` ein
Funktor ist:

```haskell
instance Functor Validation where
  fmap _ (Fail es) = Fail es
  fmap f (Ok c)    = Ok (f c)
  -- oder einfacher
  -- fmap = applyOkFunctionToValidation
```

Mit diesem Wissen ausgerüstet, können wir eine erste kleine Änderungen
am Code von oben durchführen.

```haskell
combineValidations :: Validation (a -> b) -> Validation a -> Validation b
combineValidations (Fail es) (Fail fs) = Fail (es ++ fs)
combineValidations (Fail es) _         = Fail es
combineValidations (Ok f)    v         = fmap f v
```

Das sieht nicht nach viel aus, ist aber doch schon einiges.  Unsere
Verwendung von `fmap` an dieser Stelle (und natürlich die
Implementierung von `Functor`) signalisiert allen Lesenden, dass

- hier bestimmte Gesetze gelten (zu Gesetzen bitte die Warnung weiter
  unten nicht übersehen)
- aller Code aller Bibliotheken, die sonst noch etwas mit Funktoren
  anzufangen wissen, für uns auch verwendbar ist.

## Applikativer Funktor

Irgendwie war ja schon klar, dass hier auch ein applikativer Funktor
versteckt ist -- sonst wäre der Titel des Posts eine ganz schöne
Nullnummer.

Schauen wir uns wieder zuerst die Definition der entsprechende
Typklasse in Haskell an:

```haskell
class Functor f => Applicative f where
  pure  :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b
```

Beide Signaturen könnten uns bereits bekannt vorkommen:

```haskell
pure           :: a ->          f a
makeValidation :: a -> Validation a

(<*>)              ::          f (a -> b) ->          f a ->          f b
combineValidations :: Validation (a -> b) -> Validation a -> Validation b
```

Dann setzen wir mal ein:
```haskell
instance Applicative Validation where
  pure  = makeValidation
  (<*>) = combineValidations
```

Und zu guter Letzt:

```haskell
validateUser :: String -> String -> String -> Validation User
validateUser name email role =
	pure User <*> validateNonemptyString name
	          <*> validateEmail email
	          <*> validateUserRole role
```

Netterweise bekommen wir (wie schon beim `Functor`) alles geschenkt,
was sonst noch für applikative Funktoren zu haben ist.  Nur ein
kleines Beispiel: der `<$>`-Operator (`(<$>) :: Functor f => (a -> b)
-> f a -> f b,` wobei `(<$>) = fmap`) macht die Validierung noch ein
kleines bisschen hübscher:

```haskell
validateUser email name role =
  User <$> validateNonemptyString "Marco"
       <*> validateEmail "marco.schneider@active-group.de"
       <*> validateUserRole "DEVELOPER"
```

# Was bringt mir das Ganze?

Völlig zurecht könnten Sie sich jetzt fragen, warum wir den ganzen
Aufwand betreiben.  Warum den Funktor suchen?  Und gar applikative
Funktoren?  Da wir uns hier mit der *praktischen* Anwendung von
funktionaler Programmierung beschäftigen, möchte ich nur ein Detail
herauspicken.

Wenn man sich die Typsignatur von `(<*>) :: f (a -> b) -> f a -> f b`
(oder auch die Definition) anschaut, fällt auf: Die einzelnen
Argumente sind voneinander unabhängig.  Das heißt, ich bin theoretisch
frei im Ausdruck

```haskell
make <$> foo <*> bar <*> baz
```

`foo`, `bar` und `baz` in beliebiger Reihenfolge auszurechen -- oder
in beliebigem Kontext (so lange die Typen "passen")!  Das heißt, dass
ich beispielsweise entscheiden könnte, meine Implementierung von `<*>`
so zu schreiben, dass jedes Argument auf einem eigenen Thread
ausgewertet wird und das Ergebnis aufgerufen wird, wenn alle
parallelen Berechnungen *fertig* sind.  Das gilt dann natürlich für
jede Instanz von `Applicative`, die sich an die Gesetze hält -- in dem
Fall an das Assoziativgesetz.

```haskell
pure (.) <*> u <*> v <*> w = u <*> (v <*> w)
```

Es soll also gelten: Die Anwendung der Komposition `pure (.) <*> u <*>
v <*> w` ist äquivalent dazu, `u` auf das Ergebnis von `v <*> w`
anzuwenden (oder einfacher gesagt: Komposition applikativer Werte mit
`pure (.)` verhält sich wie Komposition von Funktionen mit `.`).  Es
soll also *egal sein, in welcher Reihenfolge das Ergebnis berechnet
wird*.

# Fazit

Wir haben Ihnen eine Variante der Datenvalidierung in der funktionalen
Programmierpraxis vorgestellt.  Dazu haben wir einen applikativen
Funktor verwendet und gezeigt, dass wir damit sehr einfach
Teilergebnisse miteinander kombinieren können und so immer genau
Bescheid wissen, was alles falsch lief.

Der begleitende Quellcode für diesen Artikel [ist auf
Github](https://github.com/neshtea/applikative-validierung) zu finden.

# Wichtiger Nachklapp

Noch eine Bemerkung zum Thema Typklassen und Signaturen.  Nur weil die
Signaturen von Funktionen so aussehen, als könnten sie zu einer
Typklasse passen, heißt das noch lange nicht, dass daraus auch eine
*korrekte* Typklasse wird.  Eine solche Klasse besteht in der Regel
aus:

1. Einer Menge von Operationen (`fmap` oder `pure` plus `<*>`)
2. Einer Menge an Gesetzen, die für Werte des Typs unter diesen
   Operationen gelten müssen

Haskell gibt uns leider nicht die richtigen Mittel an die Hand, um
sicherzustellen, dass die jeweiligen Gesetze auch von der Instanz
eingehalten werden.  Zu zeigen, dass unsere Instanzen korrekt sind,
sprengt allerdings den Rahmen dieses ohnehin schon langen Posts.

*Update 9. Juli 2022* Selbstverständlich hat die applikative
Validierung nichts mit Haskell oder gar statischer Typisierung zu tun.
Zu sehen ist das zum Beispiel in unserer Clojure-Bibliothek
[`active-clojure`](https://github.com/active-group/active-clojure#applicative-validation).
Hier finden Sie eine Implementierung applikativer Funktoren in der
dynamisch typisierten Sprache [Clojure](https://clojure.org/).
