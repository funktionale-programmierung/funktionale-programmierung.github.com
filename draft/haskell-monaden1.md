---
layout: post
description: "Monaden: Das programmierbare Semikolon"
title: "Monaden: Das programmierbare Semikolon"
author: uwe-schmidt
tags: ["Haskell", "Monaden"]
---

# Monaden: Das programmierbare Semikolon #

Unter den Software-Entwicklern gibt es einige, die über die
Haskell-Anhänger witzeln: *Wenn du mit einem Haskell-Fan sprichst,
achte mal darauf, wie viele Minuten es dauert, bis das Wort Monade
fällt*. Manche Entwickler schließen daraus voreilig, dass man mit
Haskell nur arbeiten kann, wenn man weiß, was eine Monade ist.

Viele Haskell-Entwickler nutzen Monaden, ohne viel darüber
nachzudenken. Die intuitive `do`-Notation, die das Arbeiten mit
Monaden sehr einfach macht, wird sowohl für die Ein- und Ausgabe als
auch z.B. in der häufig verwendeten Parser-Bibliothek [parsec][parsec] und in
vielen anderen Bibliotheken genutzt. Monaden nutzen ist eine sehr
einfache und bequeme Sache.

Man kann in Haskell also sehr wohl Programme entwickeln, ohne Monaden
bis in alle Einzelheiten zu verstehen, aber insbesondere für
[Real-World-Haskell][rwh]-Projekte bilden Monaden ein wichtiges
Software-technisches Konzept, mit dem wiederverwendbare und modular
erweiterbare Software konstruiert werden kann.

Einer der prominentesten Entwicker der Sprache Haskell,
[Philip Wadler][wadler], hat in einer Vortragsdiskussion auf die Frage
*Wie würden Sie einem Nicht-Haskeller erklären, was eine Monade ist?*
geantwortet: *Ein programmierbares Semikolon*.

Diese Antwort bedarf ein wenig Erklärung.

<!-- more start -->

Alle Programmiersprachen benötigen die Konzepte _Sequenz_, _Verzweigung_
und _Wiederholung_. In der funktionalen Welt werden Verzweigungen durch
bedingte Ausdrücke und Wiederholungen durch Rekursion realisiert. Die
Sequenz, das Hintereinander-Ausführen von Berechnungen, geschieht durch
die Funktionskomposition dargestellt, durch den 2-stelligen Operator
`.`

{% highlight haskell %}
(.) :: (b -> c) -> (a -> b) -> (a -> c)
g . f = \ x -> g (f x)
{% endhighlight %}

Definiert man den Operator `>=>` als

{% highlight haskell %}
(>=>) :: (a -> b) -> (b -> c) -> (a -> c)
f >=> g = g . f
{% endhighlight %}

so hat man die gewohnte Schreibweise von links nach rechts und man
kann eine Folge von geschachtelten Funktionsaufrufen wie eine Pipe in
UNIX schreiben

{% highlight haskell %}
f1 >=> f2 >=> ... >=> fn
{% endhighlight %}

Der Operator `>=>` ist [assoziativ][assoziativ] und besitzt die Identitätsfunktion `id` als
[neutrales Element][neutralesElement]. Mathematiker kennen solche Strukturen unter dem
Namen [Monoid][monoid].

Bei vielen imperativen Programmiersprachen, einschließlich der
objektorientierten, wird eine Folge von Berechnungen mit Hilfe des `;`
als Operator konstruiert. Allerdings verbirgt sich in diesen Sprachen
üblicherweise etwas mehr hinter dem `;`-Operator als nur die reine
Komposition von Funktionen im mathematischen Sinn.

## Partielle Funktionen ##

Betrachten wir als Beispiel einen Methodenaufruf in Java
`x.f()`. Dieser funktioniert nur dann, wenn die Variable `x` nicht die
`null`-Referenz enthält. Bei der Ausführung würde in diesem Fall kein
Wert berechnet werden, sondern eine Ausnahme ausgelöst werden. Nach
Haskell übertragen, hätte `f` also den Typ

{% highlight haskell %}
f :: a -> Maybe b
{% endhighlight %}

wobei `a` der Typ von x ist und `b` der Resultattyp von `f`. Für das
Ergebnis von `f` gibt es zwei Alternativen: `Just y` falls der Aufruf 
erfolgreich war und `y` das Ergebnis ist, oder `Nothing`
für den Fehlerfall `NullPointerException`.

Wenn wir in Haskell solche partiellen Funktionen - also Funktionen, die
nicht immer funktionieren - hintereinander ausführen möchten, so
müssen wir die Funktionskomposition `>=>` definieren als

{% highlight haskell %}
(>=>) :: (a -> Maybe b) -> (b -> Maybe c) -> (a -> Maybe c)
f >=> g = \ x -> case f x of
                   Nothing -> Nothing
                   Just x2 -> g x2
{% endhighlight %}

Sowie eine Funktion `Nothing` als Resultat liefert, wird die
Berechnung mit dem Resultat `Nothing` beendet.

Wir können die Funktionen `f1` bis `fn` also wieder, wie bei der
einfachen Komposition, zu einer Pipe zusammen setzen

{% highlight haskell %}
f1 :: a   -> Maybe b
f2 :: b   -> Maybe ...
...
fn :: ... -> Maybe c

f1 >=> f2 >=> ... >=> fn :: a -> Maybe c
{% endhighlight %}

Wir haben den `>=>` Operator, der dem `;` aus der imperativen Welt
entspricht, hier neu programmiert, und zwar so, dass das Scheitern
einer Teilberechnung zum sofortigen Beenden der gesamten Berechnung
führt.

## Berechnungen mit Ausnahmen ##

Die Modellierung mit dem `Maybe`-Datentypen ist die
einfachste Möglichkeit mit Fehlern umzugehen. Sie ist Software-technisch aber nur dann
sinnvoll, wenn es in einem Kontext nur eine Fehlerart gibt, oder wenn
die Fehlerart unwesentlich ist.

Etwas freundlicher und flexibler ist es, wenn die Fehlerart mindestens
durch einen Text als Fehlermeldung beschrieben werden kann.

{% highlight haskell %}
type Exc a = Either Err a -- (Left Err) für den Fehlerfall, (Right a) bei Erfolg
type Err   = String       -- oder etwas Allgemeineres

f1 :: a   -> Exc b
f2 :: b   -> Exc ...
...
fn :: ... -> Exc c
{% endhighlight %}

Der `>=>`-Operator muss wie folgt redefiniert (reprogrammiert) werden,
damit die Funktionen `f1` bis `fn` wie bisher kombiniert werden können:

{% highlight haskell %}
(>=>) :: (a -> Exc b) -> (b -> Exc c) -> (a -> Exc c)
f (>=>) g = \ x -> case f x of
                     Left err -> Left err
                     Right x  -> g x
{% endhighlight %}

Damit können wir `f1` bis `fn` auf gewohnte Weise kombinieren:

{% highlight haskell %}
f1 >=> f2 >=> ... >=> fn :: a -> Exc c
{% endhighlight %}

Sowie eine der Funktionen ein `Left msg` liefert, wird die Berechnung
beendet und dieser Wert bildet das Endresultat.

## Zustands-behaftete Berechnungen ##

Zurück zur imperativen Welt. Die häufigste Operation dort ist die
Zuweisung, `:=` in Pascal, `=` in C und Java. Diese Zuweisung arbeitet
auf Programmvariablen. Programmvariablen können, im Gegensatz zu
Variablen im mathematischen Sinn, während der Ausführung eines
Programms ihren Wert ändern.

Möchte man so einen Zustand in der funktionalen Welt verwenden, so
muss man alle Funktionen, die direkt oder indirekt diesen Zustand
nutzen, mit einem zusätzlichen Parameter hierfür versehen. Das
Funktionsresultat wird zu einem Paar aus Resultat und verändertem
Zustand erweitert.

Nehmen wir an, wir möchten einen Interpretierer für eine einfache
imperative Sprache, z.B. für eine eigene Domänen-spezifische Sprache,
implementieren, so könnten wir dieses mit folgendem Datenmodell
umsetzen:

{% highlight haskell %}
type State    = Map Variable Value
type Variable = String
type Value    = Int      -- oder etwas Komplexeres

f1 :: a   -> State -> (b,   State)
f2 :: b   -> State -> (..., State)
...
fn :: ... -> State -> (c,   State)
{% endhighlight %}

Die Funktionen `f1` bis `fn` müssen alle so umgeschrieben werden, dass
der Zustand durch die Berechnung hindurch gefädelt wird.  Können wir
für diese Funktionen den Operator `>=>` sinnvoll definieren, so dass
wir das Durchfädeln des Zustand in `>=>` kapseln können?

Der erste Schritt ist eine Vereinheitlichung der Signaturen. Hierfür
führen wir einen Typ `ST` (Abkürzung für State Transformer) ein.

{% highlight haskell %}
type ST a = State -> (a, State)

f1 :: a   -> ST b
f2 :: b   -> ST ...
...
fn :: ... -> ST c
{% endhighlight %}

Damit können wir `>=>` wie folgt realisieren

{% highlight haskell %}
(>=>) :: (a -> ST b) -> (b -> ST c) -> (a -> ST c)
f >=> g = \ x s0 ->
            let (y, s1) = f x s0 in
            let (r, s2) = g y s1 in
            (r, s2)
{% endhighlight %}

Mit `>=>` wird einen neue Funktion mit den Parametern `x` und `s0`
konstruiert. In dieser wird mit `f x s0` das Zwischenergebnis `y` und
der neue Zustand `s1` berechnet, anschließend wird mit `g y s1` das
Resultat `r1` und der Endzustand `s2` berechnet.

{% highlight haskell %}
f1 >=> f2 >=> ... >=> fn :: a -> ST c
{% endhighlight %}

Damit haben wir das `;` also zum dritten Mal reprogrammiert.

## Ein- und Ausgabe ##

Bisher ist die Ein- und Ausgabe vollständig außer Acht gelassen
worden. Für die Modellierung von Ein- und Ausgabe können die
Überlegungen für die Zustands-behaftete Berechnung von oben direkt
übertragen werden. Wir betrachten alle Größen außerhalb des eigenen
Programms als Weltzustand.

{% highlight haskell %}
type World = ...    -- abstrakt
{% endhighlight %}

Die interne Struktur dieses Typs wird durch das Haskell-Laufzeitsystem
versteckt. Die Schnittstelle zum Betriebssystem enthält die
Funktionen, mit denen auf den Weltzustand lesend und schreibend
zugegriffen werden kann.

Funktionen, die den Weltzustand referenzieren, arbeitet mit einem Typ
`IO`

{% highlight haskell %}
type IO a = World -> (a, World)

(>=>) :: (a -> IO b) -> (b -> IO c) -> (a -> IO c)
f >=> g = ...
{% endhighlight %}

Man erkennt leicht, dass `World` dem Typ `State` aus dem oberen
Beispiel entspricht, `IO` korrespondiert zu `ST`. Den `>=>`-Operator
können wir hier aber nicht explizit entwickeln, die Implementierung
bleibt, genauso wie der Typ `World`, im Laufzeitsystem von Haskell
verborgen.

## Die Monad-Klasse ##

Betrachten wir noch einmal die Signaturen von `>=>` aus den oben entwickelten Beispielen:

{% highlight haskell %}
(>=>) :: (a ->       b) -> (b ->       c) -> (a ->       c)
(>=>) :: (a -> Maybe b) -> (b -> Maybe c) -> (a -> Maybe c)
(>=>) :: (a -> Exc   b) -> (b -> Exc   c) -> (a -> Exc   c)
(>=>) :: (a -> ST    b) -> (b -> ST    c) -> (a -> ST    c)
(>=>) :: (a -> IO    b) -> (b -> IO    c) -> (a -> IO    c)
{% endhighlight %}

Spätestens hier sollten wir versuchen, `>=>` zu einem polymorphen
Operator zu machen, und zwar polymorph in dem Typkonstruktor (`Maybe`,
`Exc`, ...).

{% highlight haskell %}
(>=>) :: (a ->     m b) -> (b ->     m c) -> (a ->     m c)
{% endhighlight %}

Es wird also zum Überladen von `>=>` eine Typklasse benötigt. Diese
erwartet bei Instanziierung einen Typkonstruktor, und nicht wie `Eq`,
`Ord` und andere einfache Klassen einen Typ.  Diese Klasse wird die
berühmt-berüchtigte Monad-Klasse sein.

Wir haben bei der einfachen Funktionskomposition gesehen, dass diese
assoziativ ist und das `id` das neutrale Element bezüglich `.` und so
auch bezüglich `>=>` ist. Man kann einfach nachrechnen, dass die
anderen vier Implementierungen von `>=>` ebenfalls assoziativ sind,
und für alle gibt es auch ein neutrales Element (Übung).

Um von einer korrekten Instanz der `Monad`-Klasse zu sprechen, sollten
die Operationen so implementiert sein, dass die Assoziativität für
`>=>` gilt und und dass ein neutrales Element für `>=>` exisitiert.

Die vordefinierte Klasse `Monad` aus dem Haskell-`Prelude` definiert
`>=>` nicht direkt, sondern über einen sogenannten `bind`-Operator,
geschrieben als `>>=`. Sie hat folgende Gestalt:

{% highlight haskell %}
class Monad m where
  return :: a -> m a
  (>>=)  :: m b -> (b -> m c) -> m c
{% endhighlight %}

`return` ermöglicht es, einfache Werte
zu monadischen Werten anzuheben, zu *liften*.

Warum `>>=` und nicht `>=>`?  Man erkennt, das die Signatur von `>>=`
einfacher als die von `>=>` ist.  Bei `>>=` sind der erste Parameter
und das Resultat einfache monadische Werte, bei `>=>` sind dieses
Funktionen, die einen monadischen Wert berechnen. Dieses läßt den
Schluss zu, dass `>>=` manchmal einfacher zu implementieren ist als
`>=>`.

{% highlight haskell %}
(>=>) :: (a -> m b) -> (b -> m c) -> (a -> m c)
(>>=) ::       m b  -> (b -> m c) ->       m c
{% endhighlight %}

Wir definieren `>=>` mit Hilfe von `>>=` wie folgt

{% highlight haskell %}
(>=>) :: Monad m => (a -> m b) -> (b -> m c) -> (a -> c)
f >=> g = \ x -> f x >>= g
{% endhighlight %}

In Haskell98 enthält die Monad-Klasse noch zwei weitere
Funktionen `>>` und `fail`

{% highlight haskell %}
(>>) :: m a -> m b -> m b
x >> y = x >>= \ _ y

fail :: String -> m a
{% endhighlight %}

`>>` ist eine *Bequemlichkeits*-Funktion, bei der der zweite
monadische Wert nicht vom ersten abhängt. `fail` wird gemeinhin in der
Haskell-Community als Designfehler angesehen. Die Funktion wird bei
*pattern-match*-Fehlern in Zusammenhang mit der `do`-Notation
benötigt. Wir werden diese Operationen in den folgenden Beispielen
nicht weiter verwenden.

In den Monaden-Tutorien, die es im Haskell-Wiki und an
anderen Stellen in reichlicher Anzahl gibt, werden die
Monaden-Gesetzte meistens für `>>=` und `return` formuliert. Aber
dadurch, dass `>>=` im Gegensatz zu `>=>` eine unsymmetrische Signatur
besitzt, sehen das *Assoziativ*- und das *Neutrale-Element*-Gesetzte für `>>=` auch
nicht so klar und einfach aus, wie für `>=>`.

## Die Maybe-Monade ##

Wie sieht eine Instanz von Monad für Maybe aus? Die wichtigen
Überlegungen haben wir schon bei der Entwickung von `>=>` gemacht, wir
müssen diese nur auf `>>=` übertragen.

{% highlight haskell %}
instance Monad Maybe where
  return x       = Just x

  Nothing  >>= f = Nothing
  (Just x) >>= f = f x
{% endhighlight %}

Es ist einfach nachzurechnen, dass wir mit dieser Definition von `>>=`
die Definition von `>=>` aus dem ersten Beispiel erhalten. Damit ist
auch die Assoziativität gesichert. Es bleibt die Frage nach dem
neutralen Element. Für `Maybe` bekommen wir

{% highlight haskell %}
idMaybe :: a -> Maybe a
idMaybe x = Just x
{% endhighlight %}

und es gilt

       idMaybe >=> f
     =
       f
     =
       f >=> idMaybe

`idMaybe` kann man aber formulieren, ohne Eigenschaften von Maybe direkt zu nutzen

       idMaybe x = Just x
     =
       idMaybe x = return x
     =
       idMaybe = return . id

Wir sehen an der dritten Form, dass `idMaybe` nicht mehr von Maybe
abhängt, `return . id` ist also nicht nur für Maybe, sondern für alle
Monaden das gesuchte neutrale Element bezüglich `>=>`.

Die Monaden-Instanz für `Maybe` ist im [Haskell Prelude][Maybe]
vordefiniert. Wir können diese also direkt verwenden.


## Die Exception-Monade ##

Der `Exc`-Datentyp war oben als `type`-Alias definiert. Hierfür können
wir, ohne Erweiterungen des Haskell-Standards zu nutzen, keine
Instanzen bilden. Daher werden wir für den Datentyp nicht den vordefinierten
`Either`-Typ, sondern einen gleichwertigen eigenen Summendatentyp verwenden.

{% highlight haskell %}
data Exc a = Val a
           | Exc String
{% endhighlight %}

und die Instanz analog zur früheren `>=>`-Implementierung entwickeln.

{% highlight haskell %}
instance Monad Exc where
  return x = Val x
  (Exc e) >>= f = Exc e
  (Val x) >>= f = f x
{% endhighlight %}

Um Fehler auszulösen, nutzen wir eine Funktion `throwError`. Mit
dieser Funktion können wir den Konstruktor `Exc` *verstecken*.

{% highlight haskell %}
throwError :: String -> Exc a
throwError msg = Exc msg

-- oder

throwError = Exc
{% endhighlight %}

Damit die Funktion `throwError` überladen und so auch von
anderen Fehlermonaden genutzt werden kann, gibt es im Paket
[mtl][mtl] im Modul [Control.Monad.Error][ControlMonadError] die
Klasse [MonadError][MonadError], in der `throwError` und eine
Funktion zum Behandeln von Ausnahmen, `catchError` deklariert sind.

Für `Either` gibt es inzwischen eine vordefinierte
[Monadeninstanz][DataEither]. Unter Verwendung dieser hätten wir
unseren `Exc`-Datentyp nicht neu definieren müssen, sondern weiter mit
`Either` arbeiten können.

## Die Zustandsmonade ##

Um für die Zustandsmonade Instanzen von Monad zu bilden, müssen wir
den `type`-Alias

{% highlight haskell %}
type ST a = State -> (a, State)
{% endhighlight %}

zu einem `newtype` machen

{% highlight haskell %}
newtype ST a = ST (State -> (a, State))
{% endhighlight %}

Hier werden wir dazu gezwungen, da wir für genau diese Form von
Funktionen Instanzen bauen möchten. Die Entwickung von `>>=`
orientiert sich wieder an der zugehörigen Definition von `>=>`.
Allerdings macht der zusätzliche Konstruktor den Code etwas
unübersichtlicher.

{% highlight haskell %}
instance Monad ST where
  return x = ST $ \ s -> (x, s)

  (ST f1) >>= g = ST $ \ s0 ->
                       let (r1, s1) = f1 s0 in
                       let (ST f2)  = g  r1 in
                       f2 s1
{% endhighlight %}

Möchten wir einen einfachen Wert in eine Zustandstransformation liften
(`return`), so wird in dieser Funktion der Zustand `s` nur durchgereicht.

Bei `>>=` konstruieren wir eine Zustandstransformation, in der `f1`
auf den Anfangszustand `s0` angewendet wird, die zweite
Zustandstransformation `f2` wird mit `g r1` berechnet. Diese
angewendet auf den Zwischenzustand `s1` liefert und das Resultat.

## Ein- und Ausgabe ##

Auf die gleiche Art wie die Zustandsmonade wird, jedenfalls konzeptionell, die `IO`-Monade im
[Laufzeitsystem von Haskell][IO] implementiert. Für Routinen, die Ein- und
Ausgabe machen, arbeiten wir also immer in einer Monade. Bei
Anwendungen, die im Wesentlichen I/O machen, somit eigentlich ständig.

Hier kann das Schreiben von Funktionen nur mit `>=>`, `>>=` und
`return` unübersichtlich und unbequem werden. Aus diesem Grunde ist die
`do`-Notation entwickelt worden. Monadischer Code kann damit lesbarer
strukturiert werden. Im folgenden Beispiel werden die Funktionen `f1`
bis `f3` mit dem `>>=` kombiniert

{% highlight haskell %}
f x0 = f1 x0 >>= \ x1 ->
       f2 x1 >>= \ x2 ->
       f3 x2 >>= \ x3 ->
       return $ x1 + x2 + x3
{% endhighlight %}

dieses sieht in der `do`-Notation übersichtlicher aus.  Vorsicht: Der `<-`
sollte aber nicht mit einer Zuweisungen aus den imperativen Sprachen
gleich gesetzt werden.

{% highlight haskell %}
f x0 = do x1 <- f1 x1
          x2 <- f2 x1
          x3 <- f3 x3
          return $ x1 + x2 + x3
{% endhighlight %}

In den folgenden Beispielen werden wir bei monadischem Code
überwiegend die `do`-Notatation nutzen. Man sollte dabei aber immer
beachten, dass dieses *nur* eine angenehmere Syntax als `>>=` ist, die
Ausdruckskraft der Sprache aber nicht verändert wird.


## Ausblick ##

Wir haben in diesem Artikel die Grundlagen über Monaden diskutiert und
gesehen, dass es neben der IO-Monade noch eine Reihe weiterer
wichtiger und häufig genutzter Monaden gibt. Unsere Beispielliste ist
aber nur der Anfang. In [Real-Word-Haskell][rwh]-Programmen stehen wir
fast immer vor der Aufgabe, Fehlerbehandlung, Zustands-behaftete
Berechnungen, Ein- und Ausgabe und weitere Anwendungs-spezifische
Aspekte zu kombinieren. Wir werden also vor der Aufgabe stehen, die
oben diskutierten einfachen Monaden zu kombinieren, um mehrere Aspekte
gleichzeitig zu berücksichtigen.

Im zweiten Teil dieser Übersicht über Monaden werden wir eine kleine
Domänen-spezifische Sprache entwickeln. Wir werden mit einer sehr
einfachen Sprache beginnen und diese schrittweise um neue Aspekte,
Fehlerbehandlung, Variablen, Nichtdeterminismus und I/O erweitern.
Wir werden dabei sehen, dass, wenn wir unseren Code in monadischer
Form schreiben, bei den Erweiterungen nichts Bestehendes verändert
oder weg geworfen werden muss. Die Erweiterungen werden alle von
lokaler Natur sein und bestehende Programmteile können ohne Änderung
weiter verwendet werden.

[ControlMonadError]: <http://hackage.haskell.org/packages/archive/mtl/latest/doc/html/Control-Monad-Error.html> "Control.Monad.Error"

[DataEither]: <http://hackage.haskell.org/packages/archive/base/latest/doc/html/Data-Either.html#t:Either> "Data.Either"

[IO]: <http://hackage.haskell.org/packages/archive/base/latest/doc/html/Prelude.html#t:IO> "IO"

[MonadError]: <http://hackage.haskell.org/packages/archive/mtl/latest/doc/html/Control-Monad-Error.html#t:MonadError> "MonadError"

[Maybe]: <http://hackage.haskell.org/packages/archive/base/latest/doc/html/Prelude.html#t:Maybe> "Maybe"

[mtl]: <http://hackage.haskell.org/package/mtl> "mtl"

[rwh]: <http://www.realworldhaskell.org/> "Real World Haskell"

[wadler]: <http://homepages.inf.ed.ac.uk/wadler/> "Philip Wadler"

[parsec]: <http://hackage.haskell.org/package/parsec> "parsec"

[neutralesElement]: <http://de.wikipedia.org/wiki/Neutrales_Element> "neutrales Element"

[assoziativ]: <http://de.wikipedia.org/wiki/Assoziativgesetz> "assoziativ"

[monoid]: <http://de.wikipedia.org/wiki/Monoid> "Monoid"
