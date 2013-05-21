---
layout: post
description: Monaden in Aktion
title: "Monaden in Aktion: Teil 1"
author: uwe-schmidt
tags: ["Haskell", "Monaden"]
---

# Monaden in Aktion: Teil 1#

Im [letzten Artikel][fp1] über Monaden haben wir die Grundlagen
diskutiert. Hier soll es darum gehen, eigene Monaden zur Lösung
Software-technischer Aufgaben selbst zu entwickeln.  Wir werden sehen,
wie ein Stück Software modular und durch lokale Erweiterungen um neue
Funktionalität ergänzt werden kann, ohne bestehende Teile zu verändern
oder zu refaktorisieren.

Als laufendes Beispiel werden wir die klassische Aufgabe der
Auswertung von Ausdrücken behandeln. Wir werden mit einfachen
arithmetischen Ausdrücken und Konstanten beginnen.  Hierfür werden wir
einen *[rein funktional][rf]* geschriebenen Interpretierer
angeben. Dieser wird in einem ersten Schritt in eine *monadische* Form
transformiert, ohne dass die Funktionalität verändert wird.

Anschließend werden wir Erweiterungen vornehmen, die in einem
herkömmlichen Interpretierer nur schwer und mit hohem Aufwand möglich
sind.  Wir werden eine sinnvolle Fehlerbehandlung hinzufügen,
nichtdeterministische Berechnungen ermöglichen, Variablen in den
Ausdrücken zulassen und zum Schluss die Sprache um Zuweisungen,
Schleifen und Ein- und Ausgabe erweitern.

In diesem Teil über Monaden in Aktion werden wir nur die ersten
Schritte entwickeln.  Die Erweiterungen um Variablen, Zuweisungen und
E/A werden im Teil 2 diskutiert werden.

## Rein funktionaler Code versus I/O-behafteter Code ##

In einem guten Design für ein etwas komlexeres Software-System ist es
nicht nur in Haskell-Projekten wichtig, das System so zu
modularisieren, dass die Teile, in denen Ein- und Ausgaben gemacht
werden, sauber getrennt werden von den Teilen, in denen reine
Verarbeitung von Daten gemacht werden.  Insbesondere für die
Sicherheit eines Systems ist es von Bedeutung, die Ein- und
Ausgabe-Teile zu isolieren, möglichst kein und übersichtlich zu
halten, und die Menge der zulässigen I/O-Operationen und deren
Argumente genau zu kontrollieren. Haskell bietet gegenüber anderen
Sprachen den Vorteil, vom Typsystem überprüfen zu lassen, in welchen
Teilen eines Systems Ein- und Ausgabe gemacht werden.

Dieses ist ein großes Software-technisches Plus, aber es erfordert
auch doppelte Sorgfalt beim Entwurf. Funktionen mit I/O können nicht
innerhalb von *puren* Funktionen verwendet werden. Dieses wird vom
Typsystem verhindert.

Wenn im Laufe eines Projekt festgestellt wird, dass in einer
elementaren Funktion `f` Ein- und/oder Ausgabe notwendig ist, so
müssen alle Funktionen die dieses `f` direkt oder indirekt nutzen, so
umgeschrieben werden, dass sie in der `IO`-Monade laufen.  Dieses kann
in heißen Projektphasen manchmal schlicht nicht machbar sein. Als
Notlösung wird dann leicht in eine Kiste mit schmutzigen Tricks
gegriffen und Funktionen, die `unsafe???` heißen,
genutzt. Üblicherweise ist das der erste Schritt zu einem
unzuverlässigen und unwartbaren System.

Mit dem hier vorgestellten monadischem Programmierstil werden wir
nicht in diese hässliche Falle laufen, wir werden aber trotzdem
die Kontrolle über die IO-behafteten Operationen behalten.

## Auswertung arithmetischer Ausdrücke ##

Wir beginnen in unserem Beispiel mit der Auswertung arithmetischer
Ausdrücke mit den 5 Grundrechenarten ``+, -, *, div, mod`` und
betrachten der Einfachheit halber nur ganzzahlige Arithmetik. Das
Ausgangsbeispiel [`Expr0.hs`][Expr0]:

{% highlight haskell %}
module Expr0 where

data Expr  = Const  Int
           | Binary BinOp Expr Expr
             deriving (Show)

data BinOp = Add | Sub | Mul | Div | Mod
             deriving (Eq, Show)

type Result = Int

eval :: Expr -> Result
eval (Const i)
    = i

eval (Binary op l r)
    = let f = lookupFtab op
      in
        f (eval l) (eval r)

type BinFct = Result -> Result -> Result

lookupFtab :: BinOp -> BinFct
lookupFtab op
    = case lookup op ftab of
        Nothing -> error
                   "operation not implemented"
        Just f -> f

ftab :: [(BinOp, BinFct)]
ftab = [ (Add, (+))
       , (Sub, (-))
       , (Mul, (*))
       , (Div, div)
       ]
{% endhighlight %}

Ein Ausdruck `Expr` besitzt zwei Ausprägungen, ganzzahlige Konstante
`Const` und 2-stellige Operationen `Binary`. Binäre Ausdrücke
enthalten einen Operator `BinOp` und 2 Teilausdrücke.  Der
Interpretierer `eval` berechnet aus einem Ausdruck einen Wert vom Typ
`Result`.  Die Auswertung der Konstanten ist trivial, binäre Ausdrücke
werden ausgewertet, indem die Teilausdrücke durch rekursive Aufrufe
von `eval` ausgewertet werden, die Bedeutungsfunktion der Operatoren
wird aus einer Tabelle `ftab` mit `lookupFtab` ausgelesen und auf auf
die Teilergebnisse angewendet. Der `Mod`-Operator feht mit Absicht
noch in der Tabelle, damit wir den Fehlerfall in `lookupFtab` testen
können.

Einige Testausdrücke:

{% highlight haskell %}
-- (2 + 4) * 7
e1 = Binary Mul (Binary Add (Const 2)
                            (Const 4)
                )
                (Const 7)
-- 1 / 0
e2 = Binary Div (Const 1) (Const 0)
-- 1 % 0
e3 = Binary Mod (Const 1) (Const 0)
{% endhighlight %}

und eine *ghci-Session*

    zwiebel> ghci Expr0.hs
    ...
    *Expr0> e1
    Binary Mul (Binary Add (Const 2) (Const 4)) (Const 7)
    
    *Expr0> eval e1
    42
    
    *Expr0> e2
    Binary Div (Const 1) (Const 0)
    
    *Expr0> eval e2
    *** Exception: divide by zero
    
    *Expr0> e3
    Binary Mod (Const 1) (Const 0)
    
    *Expr0> eval e3
    *** Exception: operation not implemented

Der Interpretierer liefert für `e1` das erwartete Ergebnis. Er besitzt
aber den groben Software-technischen Mangel, dass in Fehlerfällen, wie
`eval e2` und `eval e3`, die Berechnung vom *ghci*-Laufzeitsystem
einfach abgebrochen wird und nicht, wie es sein sollte, dieser Fehler
von der `eval`-Funktion behandelt wird.

## Auswertung arithmetischer Ausdücke mit Fehlererkennung ##

Die fehlende Fehlererkennung werden wir, um den Aufwand für die
Implementierung dieses zusätzlichen Aspekts zu veranschaulichen, in
dieses erste Beispiel integrieren. Dazu muss das Resultat des
Interpretierers zu einem Summendatentyp `Result` erweitert werden, um
sowohl die *richtigen* Werte als auch die Fehlermeldungen
darzustellen.

Das Ausgangsbeispiel erweitert um Fehlererkennung [`Expr0a.hs`][Expr0a]:

{% highlight haskell %}
module Expr0a where

data Expr  = Const  Int
           | Binary BinOp Expr Expr
             deriving (Show)

data BinOp = Add | Sub | Mul | Div | Mod
             deriving (Eq, Show)


data Result a
           = Val { val :: a }
           | Exc { exc :: String }
             deriving (Show)

eval :: Expr -> Result Int
eval (Const i)
    = Val i

eval (Binary op l r)
    = case lookupFtab op of
        Exc s -> Exc s
        Val f ->
            case eval l of
              Exc s -> Exc s
              Val x ->
                  case eval r of
                    Exc s -> Exc s
                    Val y -> f x y

type BinFct = Int -> Int -> Result Int

lookupFtab :: BinOp -> Result BinFct
lookupFtab op
    = case lookup op ftab of
        Nothing -> Exc
                   "operation not implemented"
        Just f  -> Val f

ftab :: [(BinOp, BinFct)]
ftab = [ (Add, lift2 (+))
       , (Sub, lift2 (-))
       , (Mul, lift2 (*))
       , (Div, div')
       ]
    where
      lift2 f = \ x y -> Val $ f x y

div' :: Int -> Int -> Result Int
div' x y
    | y == 0    = Exc "division by zero"
    | otherwise = Val $ x `div` y
{% endhighlight %}

Wir sehen, dass wir in `eval` sowohl den `Const`- als auch den
`Binary`-Fall modifizieren müssen. Bei der Auswertung einer
2-stelligen Operation werden drei Fallunterscheidungen
notwendig. Diese immer gleich strukturierten Fehlertests werden für
`lookupFtab` und die beiden `eval`-Aufrufe benötigt.

Die Funktionstabelle `ftab` enthält jetzt Funktionen, die als Resultat
auch eine Fehlermeldung liefern können, zum Beispiel für eine Division
durch 0.

Eine *ghci-Session* mit den Ausdrücken aus dem ersten Beispiel:

    zwiebel> ghci Expr0a.hs
    ...
    *Expr0a> e1
    Binary Mul (Binary Add (Const 2) (Const 4)) (Const 7)
    
    *Expr0a> eval e1
    Val {val = 42}
    
    *Expr0a> e2
    Binary Div (Const 1) (Const 0)
    
    *Expr0a> eval e2
    Exc {exc = "division by zero"}
    
    *Expr0a> e3
    Binary Mod (Const 1) (Const 0)
    
    *Expr0a> eval e3
    Exc {exc = "operation not implemented"}

Die Session zeigt, dass die Fehlererkennung funktioniert. Unser Ziel
wurde also erreicht. Aber an allen Stellen im Interpretierer mussten
die Ergebnisse mit dem Konstruktor `Val` *eingewickelt* werden oder es
musste mit dem `Exc`-Konstruktor eine Fehlermeldung generiert werden.

Der Aspekt der Fehlererkennung konnte also nicht lokal, sondern nur
durch Änderungen in fast allen Funktionen umgesetzt werden. Für
Interpretierer in praktisch relevanten Systemen kann der Aufwand im
Vergleich zu diesem Beispiel noch um Größenordnungen höher werden.

## Auswertung arithmetischer Ausdücke in monadischem Stil ##

Zurück zu den Monaden.  Wir werden die mangelhafte Fehlererkennung
noch einen Moment ignorieren und das erste Beispiel [`Expr0.hs`][Expr0] in eine
gleichwertige monadische Form bringen.  Dazu werden wir die
allereinfachste Monade, die Identitäts-Monade, nutzen. Wir müssen dazu
allerdings den `Result`-Datentyp, für den wir die Monade instanziieren
möchten, verallgemeinern, da Monaden für Typkonstruktoren, und nicht
für einfache Typen definiert werden können. Diese Verallgemeinerung
haben wir auch schon im zweiten Beispiel [`Expr0a.hs`][Expr0a] nutzbringend
eingesetzt.

Das Ausgangsbeispiel [`Expr0.hs`][Expr0] in monadischer Form [`Expr1.hs`][Expr1]:

{% highlight haskell %}
module Expr1 where

data Expr  = Const  Int
           | Binary BinOp Expr Expr
             deriving (Show)

data BinOp = Add | Sub | Mul | Div | Mod
             deriving (Eq, Show)

newtype Result a
    = Val { val :: a }
      deriving (Show)

instance Monad Result where
    return        = Val
    (Val v) >>= g = g v

eval :: Expr -> Result Int
eval (Const i)
    = return i

eval (Binary op l r)
    = do f <- lookupFtab op
         f (eval l) (eval r)

type BinFct = Result Int -> Result Int -> Result Int

lookupFtab :: BinOp -> Result BinFct
lookupFtab op
    = case lookup op ftab of
        Nothing -> error
                   "operation not implemented"
        Just f -> return f

ftab :: [(BinOp, BinFct)]
ftab  = [ (Add, liftM2 (+))
        , (Sub, liftM2 (-))
        , (Mul, liftM2 (*))
        , (Div, liftM2 div)
        ]

liftM2  :: (Monad m) =>
           (a1 -> a2 -> r) -> (m a1 -> m a2 -> m r)
liftM2 f m1 m2
    = do x1 <- m1
         x2 <- m2
         return (f x1 x2)
{% endhighlight %}


Hier ist zu beachten, dass wir nicht nur `eval` in eine monadische
Form gebracht haben, sondern auch `lookupFtab` und die
Bedeutungsfunktionen in `ftab.` Dieses ist notwendig, weil in diesen
Funktionen, wie wir bei der Auswertung von `e2` und `e3` gesehen
haben, Fehler auftreten können.

`liftM2` ist eine Funktion mit der aus 2-stelligen *puren* Funktionen
2-stellige monadische Funktionen generiert werden können. Da solche
*lift*-Funktionen immer wieder benötigt werden, gibt es diese auch
vordefiniert in [Control.Monad][ControlMonad].

Ein Testlauf

    zwiebel> ghci Expr1.hs
    ...
    *Expr1> eval e1
    Val {val = 42}
    
    *Expr1> eval e2
    Val {val = *** Exception: divide by zero
    
    *Expr1> eval e3
    *** Exception: operation not implemented

Die Funktionalität ist identisch zu der aus [`Expr0.hs`][Expr0]. In diesem ersten
Schritt haben wir das Beispiel nur komplizierter gemacht, wir haben
nichts gewonnen, sondern *nur* in die Erweiterbarkeit investiert.

## Erweiterung der monadischen Form um Fehlererkennung ##

Wie Fehler in dem Interpretierer repräsentiert werden können, haben
wir im Beispiel [`Expr0a.hs`][Expr0a] gesehen. Wir werden den gleichen
Summen-Datentyp `Result` nutzen. Aber die wiederholten Test auf
`Exc`/`Val` werden in die `Monad`-Instanz gezogen und tauchen so nur
ein einziges Mal im Code auf.  Der Code-Ausschnitt zeigt die
Erweiterungen zu [`Expr1.hs`][Expr1], der Rest bleibt unverändert. Die
vollständige Quelle steht unter [`Expr2.hs`][Expr2].

{% highlight haskell %}
module Expr2 where

...

data Result a
           = Val { val :: a }
           | Exc { exc :: String }  -- neu
             deriving (Show)

instance Monad Result where
    return        = Val
    (Exc e) >>= _ = (Exc e)         -- neu
    (Val v) >>= g = g v

throwError :: String -> Result a    -- neu
throwError = Exc

...

lookupFtab :: BinOp -> Result BinFct
lookupFtab op
    = ...
        Nothing -> throwError       -- modifiziert
                   "operation not implemented"
        Just f -> ...

ftab :: [(BinOp, BinFct)]
ftab = [ ...
       , (Div, div')                 -- modifiziert
       ]
                                    -- neu
div'   :: Result Int -> Result Int -> Result Int
div' x y
    = do x1 <- x
         y1 <- y
         if y1 == 0
           then throwError
                "division by zero"
           else return (x1 `div` y1)
{% endhighlight %}

`Result` arbeitet mit der Fehler-Monade. Berechnungen werden im
Fehlerfall sofort abgebrochen. `lookupFtab` war eine der partiellen
Funktionen, hier wird nur `error` durch `throwError` ausgewechselt.
`div` war die andere Schwachstelle. In `div'` wird die Vorbedingung `y
/= 0` geprüft, bevor die eigentliche Division ausgeführt wird.

Der Testlauf von oben ergibt jetzt folgende Resultate

    zwiebel> ghci Expr2.hs
    ...
    *Expr2> eval e1
    Val {val = 42}
    
    *Expr2> eval e2
    Exc {exc = "division by zero"}
    
    *Expr2> eval e3
    Exc {exc = "operation not implemented"}

Wir sehen, dass nur durch Hinzufügen weniger Zeilen der Aspekt der
Fehlererkennung in den bestehenden monadischen Interpretierer integriert werden
konnte. Und nur dort, wo Fehler auftreten konnten, `lookupFtab` und
`div`, sind lokale Veränderungen vorgenommen worden.  Die
`eval`-Funktion wurde an keiner Stelle angefasst.

In dem nicht monadischen Stil mussten wir an jeder Stelle, an der ein
`Result`-Wert berechnet wird, die Verzweigung `Val`/`Exc` einfügen.
Alleine beim Auswerten binärer Ausdrücke trat diese Fallunterscheidung
drei mal auf.  Die Investition des Umschreibens in monadische Form in
[`Expr1.hs`][Expr1] hat sich also schon ausgezahlt.

`throwError` möchte man natürlich in allen Monaden mit Fehlererkennung
nutzen.  Es ist also sinnvoll, `throwError` in einer Typklasse zu
deklarieren. Die Klasse `MonadError` im Modul
[Control.Monad.Error][ControlMonadError] übernimmt diese Aufgabe.
Außerdem enthält sie auch eine Funktion `catchError` zum Abfangen und
Behandeln von Fehlern, die in unserem Beispiel aber nicht benötigt
wird.  Da `MonadError` eine Multi-Parameter-Typklasse ist, werden bei
ihrer Verwendung einige Haskell-Spracherweiterungen benötigt. Der
vollständige Code hierfür findet sich im Beispiel [`Expr2a.hs`][Expr2a].

## Erweiterung um Nichtdeterminismus mit der Listen-Monade ##

In dem folgenden Beispiel werden wir die Menge der Operatoren
beispielhaft um einen etwas ungewöhnlichen Operator `PlusMinus`
erweitern. Dieser soll es uns ermöglichen, eine Art
Intervall-Arithmetik zu machen. ``2 +/- 1`` soll als Resultat die
beiden Werte `1` und `3` ergeben.

Wir haben also einen Operator, der mehrere Werte als Resultat
liefert. Das Arbeiten mit Funktionen mit mehreren Resultaten wird
manchmal etwas unpräzise als Nichtdeterminismus bezeichnet. Man
arbeitet anstatt mit Funktionen mit Relationen. Funktionen vom Typ `a
-> b` werden zu Funktionen vom Typ `a -> [b]` verallgemeinert.

Der `Result`-Typ wird so erweitert, dass darin eine Liste gespeichert
wird.

{% highlight haskell %}
newtype Result a
           = Val { val :: [a] }
{% endhighlight %}

Der Einfachheit halber lassen wir die Fehlerbehandlung im ersten
Versuch außer Acht und nehmen [`Expr1.hs`][Expr1] als Ausgangspunkt. Die
Erweiterung besteht aus dem neuen Operator, der Veränderung der
Monaden-Definition und der Implemetierung der `+/-`-Operation. Der
Rest bleibt unverändert.  [`Expr3.hs`][Expr3] enthält das vollständige
Programm.

{% highlight haskell %}
module Expr3 where

...

data BinOp = ...
           | PlusMinus          -- neu

newtype Result a
           = Val { val :: [a] } -- erweitert
             deriving (Show)

instance Monad Result where     -- erweitert
    return x       = Val [x]
    (Val xs) >>= g = Val $
                     concat [val (g x) | x <- xs]

...

ftab :: [(BinOp, BinFct)]
ftab = [ ...
       , (PlusMinus, plusMinus)  -- neu
       ]

...

plusMinus :: BinFct                 -- neu
plusMinus (Val xs1) (Val xs2)
    = Val $ concat [ [x1 + x2, x1 - x2]
                   | x1 <- xs1
                   , x2 <- xs2
                   ]

-- smart constructor
interval :: Int -> Int -> Expr
interval mid rad
    = Binary PlusMinus (Const mid) (Const rad)
{% endhighlight %}

Die Monade für `Result` besteht im Wesentlichen aus der Listen-Monade,
wie sie in Haskell vordefiniert ist.

{% highlight haskell %}
instance Monad [] where
  return x = [x]
  xs >>= g = concat [g x | x <- xs]
{% endhighlight %}

Nur kommt noch das Aus- und Einwickeln mit `val` und `Val` dazu. Es
wird auf alle Elemente aus `xs` die Funktion `g` angewendet,
anschließend wird die resultierende Liste von Listen mit `concat` zu
einer einfachen Liste zusammen gefasst.

Zum Ausprobieren benötigen wir einige neue Testausdrücke:

{% highlight haskell %}
i1 = interval 1 1
i2 = interval 6 2

e4 = Binary Mul i1 (Const 7)
e5 = Binary Add i1 i2
e6 = Binary PlusMinus e4 e5
e7 = Binary Div i2 i1
{% endhighlight %}

Eine *ghci-Session*

    zwiebel> ghci Expr3.hs
    ...
    *Expr3> eval e1
    Val {val = [42]}
    
    *Expr3> eval i1
    Val {val = [2,0]}
    
    *Expr3> eval i2
    Val {val = [8,4]}
    
    *Expr3> eval e4
    Val {val = [14,0]}
    
    *Expr3> eval e5
    Val {val = [10,6,8,4]}
    
    *Expr3> eval e6
    Val {val = [24,4,20,8,22,6,18,10,10,-10,6,-6,8,-8,4,-4]}
    
    *Expr3> eval e7
    Val {val = [4*** Exception: division by zero

<!-- ] hack für emacs markdown mode, Klammer zu fehlt -->

Wir sehen, dass die *alten* Ausdrücke, z.B. `e1`, wie bisher
ausgewertet werden, sie sind nur in eine einelementige Liste
eingewickelt.

Die Intervalle bestehen aus Listen mit 2 Werten. Wird eine 2-stellige
Operation ausgeführt, so bekommt man eine Liste der Länge `n1 * n2`
wobei `n1` und `n2` die Anzahl Resultate der Teilausdrücke ist.

`eval e7` zeigt, das die Fehlererkennung noch fehlt.  Hierzu müssen
wir die beiden Versionen von `Result` zu einer neuen kombinieren, mit
der wir beide Aspekte, Fehlerbehandlung und Nichtdeterminismus,
realisieren können:

{% highlight haskell %}
data Result a
           = Val { val :: [a] }
           | Exc { exc :: String }
{% endhighlight %}

Spannend hierbei wird die Erweiterung der Monade für `Result`, die wir
folgendermaßen realisieren:

{% highlight haskell %}
instance Monad Result where
    return x       = Val [x]
    (Exc e) >>= _  = (Exc e)
    (Val xs) >>= g = if null es
                     then Val (concat (map val vs))
                     else head es
        where
          (vs, es) = partition isVal (map g xs)
              where
                isVal (Val _) = True
                isVal (Exc _) = False
{% endhighlight %}

`return` bleibt unverändert. In einem Fehlerfall wird die Berechnung
abgebrochen. Sonst wird auf jedes Element aus `xs` `g`
angewendet. Dieses Resultat wird mit `partition` in Fehler- und
Werte-Liste partitioniert. Wenn in keinem Fall ein Fehler aufgetreten
ist, werden die Resultate wie bisher konkateniert und in `Val`
eingewickelt, sonst bildet der erste Fehler das Resultat. Die
Berechnung wird also beim ersten Fehler abgebrochen. Das vollständige
Programm steht unter [`Expr3a.hs`][Expr3a]

Ein Testlauf zeigt, dass die Fehlerbehandlung jetzt wie gewünscht funktioniert:

    zwiebel> ghci Expr3a.hs
    ...
    *Expr3a> eval e1
    Val {val = [42]}
    
    *Expr3a> eval e2
    Exc {exc = "division by zero"}
    
    *Expr3a> eval e6
    Val {val = [24,4,20,8,22,6,18,10,10,-10,6,-6,8,-8,4,-4]}
    
    *Expr3a> eval i1
    Val {val = [2,0]}
    
    *Expr3a> eval i2
    Val {val = [8,4]}
    
    *Expr3a> eval $ Binary Div i2 i1
    Exc {exc = "division by zero"}

Die gewählte Definition von `>>=` muss nicht die einzige sinnvolle
Definition sein. Denkbar wäre auch eine etwas liberalere Handhabung
der Fehler, bei der man die Fehler ignoriert, solange man wenigstens
ein richtiges Resultat hat. Das Software-technisch Entscheidende ist,
dass für die Art der Interpretation nur lokale Änderungen in der
Monade notwendig sind.

Der modifizierte Code ([`Expr3b.hs`][Expr3b]):

{% highlight haskell %}
instance Monad Result where
    return x       = Val [x]
    (Exc e) >>= _  = (Exc e)
    (Val xs) >>= g = if not (null vs)
                     then Val (concat (map val vs))
                     else head es
        where
          (vs, es) = partition isVal (map g xs)
              where
                isVal (Val _) = True
                isVal (Exc _) = False
{% endhighlight %}

Durch die Änderung `null es` in `not (null vs)` in `>>=` ergibt sich
folgendes Fehlerverhalten:

    zwiebel> ghci Expr3b.hs
    ...
    *Expr3a> eval e2
    Exc {exc = "division by zero"}
    
    *Expr3a> eval i1
    Val {val = [2,0]}
    
    *Expr3a> eval i2
    Val {val = [8,4]}
    
    *Expr3a> eval $ Binary Div i2 i1
    Val {val = [4,2]}

Die Fehler bei ``8 `div` 0`` und ``4 `div` 0`` werden ignoriert, da
``8 `div` 2`` und ``4 `div` 2`` zu `[4, 2]` ausgewertet werden.

Was haben wir bisher erreicht? Durch das Umschreiben des
ursprünglichen Programms [`Expr0.hs`][Expr0] in eine monadische Form
[`Expr1.hs`][Expr1] konnten wir den Aspekt der Fehlererkennung auf
einfache Weise und mit ausschließlich lokalen Änderungen und
Erweiterungen integrieren ([`Expr2.hs`][Expr2]). Das gleiche war möglich
bei der Erweiterung durch *nichtdeterministische* Funktionen
([`Expr3b.hs`][Expr3b]).  Und schließlich haben wir in
[`Expr3a.hs`][Expr3a] und [`Expr3b.hs`][Expr3b] gesehen, wie die beiden
Aspekte Fehlererkennung und Nichtdeterminismus nur durch lokale
Erweiterung der Monade kombiniert werden konnten.


## Zusammenfassung und Ausblick: Teil 1 ##

Wir haben in den Beispielen [`Expr1.hs`][Expr1] bis [`Expr3b.hs`][Expr3b]
gesehen, dass wir die einfache Ausdrucksauswertung in unterschiedliche
Richtungen erweitern konnten, ohne existierende Funktionalität zu
überarbeiten. Neue Aspekte konnten alleine durch die Erweiterung des
`Result`-Datentyps und der monadischen Operationen `return` und `>>=`
und deren Verwandten integriert werden.

In einem nichtmonadischen Stil hätten wir, wie am Beispiel zur
Fehlererkennung ([`Expr0a.hs`][Expr0a]) demonstriert, für jeden neuen Aspekt die
Schnittstellen und die Implementierungen vieler über das gesamte
Programm verstreuter Funktionen erweitern müssen.

Die hier entwickelten Erweiterungen sind trotzdem nur die ersten
Schritte gewesen. Im zweiten Teil wird das Arbeiten mit Variablen, mit
imperativen Sprachelementen wie Zuweisungen und Schleifen und mit Ein-
und Ausgabe dazu kommen.  Damit sind wir dann nicht mehr weit entfernt
von einem Interpretierer für eine kleine aber vollwertige eingebettete
(Domänen-spezifische) Programmiersprache.

Für's Erste viel Spaß beim Puzzlen mit den Monaden und Ausprobieren der [Beispiele].

[Expr0]: </code/Monaden2/Expr0.hs>  "Ausdrucksauswertung im rein funktionalen Stil"

[Expr0a]: </code/Monaden2/Expr0a.hs>  "Ausdrucksauswertung im rein funktionalen Stil mit Fehlererkennung"

[Expr1]: </code/Monaden2/Expr1.hs>  "Ausdrucksauswertung im monadischen Stil"

[Expr2]: </code/Monaden2/Expr2.hs>  "Ausdrucksauswertung mit Fehlererkennung"

[Expr2a]: </code/Monaden2/Expr2a.hs> "Ausdrucksauswertung mit Fehlererkennung"

[Expr3]: </code/Monaden2/Expr3.hs>  "Ausdrucksauswertung mit Nichtdeterminismus"

[Expr3a]: </code/Monaden2/Expr3a.hs> "Ausdrucksauswertung mit Nichtdeterminismus"

[Expr3b]: </code/Monaden2/Expr3b.hs> "Ausdrucksauswertung mit Nichtdeterminismus"

[Expr4]: </code/Monaden2/Expr4.hs>  "Ausdrucksauswertung mit freien und gebundenen Variablen"

[Expr5]: </code/Monaden2/Expr5.hs>  "Ausdrucksauswertung mit Programm-Variablen"

[Expr6]: </code/Monaden2/Expr6.hs>  "Ausdrucksauswertung mit Programm-Variablen und IO"

[Beispiele]: </files/Monaden2/Monaden2.zip> ".zip Archiv für die Beispiele"

[ControlMonadError]: <http://hackage.haskell.org/packages/archive/mtl/latest/doc/html/Control-Monad-Error.html> "Control.Monad.Error"

[Reader]: <http://hackage.haskell.org/packages/archive/mtl/latest/doc/html/Control-Monad-Reader.htm> "Control.Monad.Reader"

[MonadIO]: <http://hackage.haskell.org/packages/archive/basic-prelude/latest/doc/html/CorePrelude.html#t:MonadIO> "MonadIO"
			   
[hackage]: <http://hackage.haskell.org/>

[mtl]: <http://hackage.haskell.org/package/mtl>

[transformers]: <http://hackage.haskell.org/package/transformers>

[rwh]: <http://www.realworldhaskell.org/> "Real World Haskell"

[DataMap]: <http://www.haskell.org/ghc/docs/latest/html/libraries/containers/Data-Map.html> "Data.Map"

[ControlMonad]: <http://hackage.haskell.org/packages/archive/base/latest/doc/html/Control-Monad.html> "Control.Monad"

[fp1]: <http://funktionale-programmierung.de/2013/04/18/haskell-monaden.html>
[rf]: <http://funktionale-programmierung.de/2013/03/12/rein-funktional.html>
