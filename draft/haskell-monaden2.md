---
layout: post
description: Monaden in Aktion
title: "Monaden in Aktion"
author: uwe-schmidt
tags: ["Haskell", "Monaden"]
---

# Monaden in Aktion #

Im letzten Artikel über Monaden haben wir die Grundlagen
diskutiert. Hier soll es darum gehen, eigene Monaden zur Lösung
Software-technischer Aufgaben selbst zu entwickeln.  Wir werden sehen,
wie ein Stück Software modular und durch lokale Erweiterungen um neue
Funktionalität ergänzt werden kann, ohne bestehende Teile zu verändern
oder zu refaktorisiern.

Als laufendes Beispiel werden wir die klassische Aufgabe der
Auswertung von Ausdrücken behandeln. Wir werden mit einfachen
arithmetischen Ausdrücken und Konstanten beginnen.  Hierfür werden wir
einen *rein funktional* geschriebenen Interpretierer angeben. Dieser
wird in einem ersten Schritt in eine *monadische* Form transformiert,
ohne dass die Funktionalität verändert wird.

Den ersten Aspekt, den wir hinzu fügen möchten, ist eine sinnvolle
Fehlerbehandlung.  Der Interpretierer wird also nicht mehr
*abstürzen*, sondern immer ein Resultat liefern, entweder ein Ergebnis
oder eine aussagekräftige Fehlermeldung.  Diesen Aspekt werden wir mit
der Fehler-Monade in den Griff bekommen.

Anschließend wird die Ausdruckssprache in unterschiedliche Richtungen
erweitert.  Eine erste Erweiterung ist die Hinzunahme von Operatoren,
die bei der Auswertung mehrere Resultate liefern können. In der
Mathematik ist das Wurzelziehen ein Beispiel für eine solche
Operation. In der Haskell-Gemeinde wird für diese mehrfachen Resultate
häufig der Begriff der nichtdeterministischen Berechnung
verwendet. Für diese Erweiterung werden wir die Listen-Monade nutzen.

Die zweite Erweiterung werden Ausdrücke mit Variablen sein. Zur
Auswertung solcher Ausdrücke benötigt man eine Belegung der Variablen
mit Werten, man hat also während der Auswertung ein *Environment*,
eine Umgebung, über die die Werte zugreifbar werden. Hier kommt die
sogenannte [Reader]-Monade zum Einsatz.

In der folgenden Erweiterung werden wir die Variable zu
Programm-Variablen machen.  Die Menge der Ausdrücke wird um
Zuweisungen, Verzweigungen und Schleifen erweitert. Es wird hierzu die
[Zustands][State]-Monade verwendet.

Als letzten Schritt werden wir Operationen für die Ein- und Ausgabe
hinzu nehmen.  Die `IO`-Monade muss also irgendwie in unsere eigene
Monade integriert werden.

## Rein funktionaler Code versus I/O-behafteter Code ##

Bei der Entwicklung von Haskell-Programmen gibt es ein Problem, dass
bei den heutigen Mainstream-Sprachen nicht auftritt. Funktionen mit
I/O können nicht innerhalb von *puren* Funktionen verwendet
werden. Dieses wird vom Typsystem verhindert.

Wenn im Laufe eines Projekt festgestellt wird, dass in einer
elementaren Funktion `f` Ein- und/oder Ausgabe notwendig ist, so
müssen alle Funktionen die dieses `f` direkt oder indirekt nutzen, so
umgeschrieben werden, dass sie in der `IO`-Monade laufen.  Dieses kann
in heißen Projektphasen manchmal schlicht nicht machbar sein. Als
Notlösung wird dann leicht in eine Kiste mit schmutzigen Tricks
gegriffen und Funktionen, die `unsave???` heißen, genutzt. Diese ist
üblicherweise der erste Schritt zu einem unzuverlässigen und
unwartbaren System.

Mit dem hier vorgestellten monadischem Programmierstil werden wir
nicht in diese hässliche Falle laufen.

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
    = let mf = lookupMft op
      in
        mf (eval l) (eval r)


type MF = Result -> Result -> Result

lookupMft :: BinOp -> MF
lookupMft op
    = case lookup op mft of
        Nothing -> error
                   "operation not implemented"
        Just mf -> mf

mft :: [(BinOp, MF)]
mft = [ (Add, (+))
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
wird aus einer Tabelle `mft` mit `lookupMft` ausgelesen und auf auf
die Teilergebnisse angewendet. Der `Mod`-Operator feht mit Absicht
noch in der Tabelle, damit wir den Fehlerfall in `lookupMft` testen
können.

Einige Testausdrücke:

{% highlight haskell %}
e1 = Binary Mul (Binary Add (Const 2)
                            (Const 4)
                )
                (Const 7)
e2 = Binary Div (Const 1) (Const 0)
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
aber den groben Software-technischen Mangel, dass in Fehlerfällen,
`eval e2` und `eval e3`, die Berechnung vom *ghci*-Laufzeitsystem
einfach abgebrochen wird und nicht, wie es sein sollte, dieser Fehler
von der `eval`-Funktion behandelt wird.

## Auswertung arithmetischer Ausdücke in monadischem Stil ##

Wir werden die mangelhafte Fehlerbehandlung noch einen Moment
ignorieren und die `eval`-Funktion in eine gleichwertige *monadische*
Form bringen. Dazu werden wir die allereinfachste Monade, die
Identitäts-Monade, nutzen. Wir müssen dazu allerdings den
`Result`-Datentyp, für den wir die Monade instanziieren möchten,
verallgemeinern, da Monaden für Typkonstruktoren, und nicht für
einfache Typen, definiert werden können.

Das Ausgangsbeispiel in monadischer Form [`Expr1.hs`][Expr1]:

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
    = do mf <- lookupMft op
         mf (eval l) (eval r)


type MF = Result Int -> Result Int ->
          Result Int

lookupMft :: BinOp -> Result MF
lookupMft op
    = case lookup op mft of
        Nothing -> error
                   "operation not implemented"
        Just mf -> return mf

mft :: [(BinOp, MF)]
mft  = [ (Add, liftM2 (+))
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
Form gebracht haben, sondern auch `lookupMft` und die
Bedeutungsfunktionen in `mft.` Dieses ist notwendig, weil in diesen
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

Die Funktionalität ist identisch zu der aus *Expr0*. In diesem ersten
Schritt haben wir das Beispiel nur komplizierter gemacht, wir haben
nichts gewonnen, sondern *NUR* in die Erweiterbarkeit investiert.

## Erweiterung um Fehlerbehandlung ##

Sollen Fehler in der Anwendung selbst behandelt werden, so müssen die
Fehler im `Result`-Datentyp repräsentiert werden. Nur so kann im
Interpretierer unterschieden werden, ob eine Auswertung einen Fehler
oder einen *normalen* Wert liefert. `Result` wird also zu einem
Summen-Datentyp erweitert. Der Code-Ausschnitt zeigt die
Erweiterungen, der Rest bleibt unverändert. Die vollständige Quelle
steht unter [Expr2.hs][Expr2].

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

lookupMft :: BinOp -> Result MF
lookupMft op
    = ...
        Nothing -> throwError       -- modifiziert
                   "operation not implemented"
        Just mf -> ...

mft :: [(BinOp, MF)]
mft = [ ...
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
Fehlerfall sofort abgebrochen. `lookupMft` war eine der partiellen
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
Fehlererkennung in den bestehenden Interpretierer integriert werden
konnte. Und nur dort, wo Fehler auftreten konnten, `lookupMft` und
`div`, sind lokale Veränderungen vorgenommen worden.  Die
`eval`-Funktion wurde an keiner Stelle angefasst.

In einem nicht monadischen Stil hätten wir an jeder Stelle, an der ein
`Result`-Wert berechnet wird, die Verzweigung `Val`/`Exc` einfügen
müssen, alleine beim Auswerten binärer Ausdrücke hätten wir diesen
Test drei mal einzufügen.  Die Investition in [Expr1.hs][Expr1] hat
sich also schon ausgezahlt.

`throwError` möchte man natürlich in allen Ausnahme-Monaden nutzen.
Es ist also sinnvoll, `throwError` in einer Typklasse zu
deklarieren. Die Klasse `MonadError` im Modul
[Control.Monad.Error][ControlMonadError] übernimmt diese Aufgabe.
Außerdem enthält sie auch eine Funktion `catchError` zum Abfangen und
Behandeln von Fehlern, die in unserem Beispiel aber nicht benötigt
wird.  Da `MonadError` eine Multi-Parameter-Typklasse ist, werden bei
ihrer Verwendung einige Haskell-Spracherweiterungen benötigt. Der
vollständige Code hierfür findet sich im Beispiel [Expr2a.hs][Expr2a].

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
Versuch außer Acht und nehmen [Expr1.hs][Expr1] als Ausgangspunkt. Die
Erweiterung besteht aus dem neuen Operator, der Veränderung der
Monaden-Definition und der Implemetierung der `+/-`-Operation. Der
Rest bleibt unverändert.  [Expr3.hs][Expr3] enthält das vollständige
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

mft :: [(BinOp, MF)]
mft = [ ...
      , (PlusMinus, plusMinus)  -- neu
      ]

...

plusMinus :: MF                 -- neu
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
                     then Val $ concat . map val $ vs
                     else head es
        where
          (vs, es) = partition isVal . map g $ xs
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
Programm steht unter [Expr3a.hs][Expr3a]

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
ein richtiges Resultat hat. Das Software-technisch entscheidende ist,
dass für die Art der Interpretation nur lokale Änderungen in der
Monade notwendig sind.

Der modifizierte Code ([Expr3b.hs][Expr3b]):

{% highlight haskell %}
instance Monad Result where
    return x       = Val [x]
    (Exc e) >>= _  = (Exc e)
    (Val xs) >>= g = if not (null vs)
                     then Val $ concat . map val $ vs
                     else head es
        where
          (vs, es) = partition isVal . map g $ xs
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
ursprünglichen Programms [Expr0.hs][Expr0] in eine monadische Form
[Expr1.hs][Expr1] konnten wir den Aspekt der Fehlererkennung auf
einfache Weise und mit ausschließlich lokalen Änderungen und
Erweiterungen integrieren ([Expr2.hs][Expr2]). Das gleiche war möglich
bei der Erweiterung durch *nichtdeterministische* Funktionen
([Expr3b.hs][Expr3b]).  Und schließlich haben wir in
[Expr3a.hs][Expr3a] und [Expr3b.hs][Expr3b] gesehen, wie die beiden
Aspekte Fehlererkennung und Nichtdeterminismus nur durch lokale
Erweiterung der Monade kombiniert werden konnten.

## Ausdrücke mit Variablen ##

Im folgenden Schritt werden wir die Ausdruckssprache um Variablen
erweitern.  Dazu wird die abstrakte Syntax, der Datentyp `Expr` auf
zwei Arten erweitert.  Als einfache Ausdrücke werden Variablen
zugelassen. Außerdem sollen, wie in Haskell auch, lokale Variablen
durch `let`-Ausdrücke eingeführt werden können.

Es stellt sich dann natürlich die Frage, ob, und wenn ja, wie diese
Erweiterung zu der existierenden Lösung hinzugefügt werden kann. Wir
werden, um die Beispiele übersichtlich zu halten, den
Nichtdeterminismus nicht weiter betrachten, sondern von wieder von
[Expr2.hs][Expr2] ausgehen.

Der Datentyp `Expr` erweitert um die beiden neuen Varianten:

{% highlight haskell %}
data Expr  = ...
           | Var Id
           | Let Id Expr Expr
		   
type Id    = String
{% endhighlight %}

Was ist die Bedeutung eines Ausdrucks mit freien Variablen?  Zur
Auswertung solcher Ausdrücke brauchen wir eine Zuordnung von Werten zu
den freien Variablen, eine Belegung. Diese Belegung werden wir in
einer sogenannten Umgebung, engl. `Env`-ironment gehalten. Die
Bedeutung eines Ausdrucks kann als eine Funktion aufgefasst werden,
die eine Umgebung auf einen Resultatwert abbildet.  `eval` wird also
einen Ausdruck (`Expr`) nehmen, und daraus eine Funktion
berechnen. Erst wenn diese Funktion auf eine Umgebung angewendet wird,
bekommen wir einen konkreten Wert, eine Zahl oder eine Fehlermeldung.

{% highlight haskell %}
newtype Result a
           = Res { unRes :: Env -> ResVal a }

type Env   = [(Id, Int)]

data ResVal a
           = Val { val :: a }
           | Exc { exc :: String }
             deriving (Show)
{% endhighlight %}

Der `Result`-Typ wird zu einem Funktionstyp `Env -> ResultVal a`, der
wegen der `Monad`-Instanz in einen `newtype` verpackt werden
muss. Damit berechnet `eval` aus einem Ausdruck eine Funktion, die aus
einem `Env` ein `ResVal Int`. Dieser entspricht dem alten `Result` aus
[Expr2.hs][Expr2]. Für die Umgebung wird hier eine einfache
Schlüssel-Wert-Liste genommen.  Bei praktischen Anwendungen werden
üblicherweise effizientere Container-Implementierungen, z.B. aus
`Data.Map` genutzt.

Um eine Auswertung von Ausdrücken mit Variablen zu starten
(`runEval`), benötigen wir also neben dem Ausdruck auch noch eine
Belegung der Variablen.

{% highlight haskell %}
runEval :: Expr -> Env -> ResVal Int
runEval e env
    = let (Res mf) = eval e in
      mf env
{% endhighlight %}

Der Kern dieser Erweiterung ist die Monaden-Instanz für `Result`:

{% highlight haskell %}
instance Monad Result where
    return x      = Res $ \ env -> Val x
    (Res f) >>= g = Res $ \ env ->
                    case f env of
                      (Exc e) -> (Exc e)
                      (Val v) -> unRes (g v) env
{% endhighlight %}

`return x` konstruiert eine Funktion, deren Resultat immer `Val x`
ist, also nicht von der Umgebung abhängt.

Bei `>>=` müssen wir wieder eine Funktion konstruieren. In dieser wird
als erstes `f` auf die Umgebung angewendet, was entweder einen Fehler
`e` liefert oder eine Wert `v`. Dieser kann auf `g` angewendet werden,
was uns eine weitere Funktion liefert, die ebenfalls auf die Umgebung
`env` angewendet wird. Sowohl `f` als auch `g v` nutzen also die
gleiche Umgebung.

Wir haben aber noch keine direkten Funktionen, die das `env` nutzen,
was aber bei der Auswertung von `Var ident` notwendig ist. Hierfür
definieren wir eine Funktion `ask` zum Auslesen der gesamten Umgebung.

{% highlight haskell %}
ask :: Result Env
ask = Res $ \ env -> Val env
{% endhighlight %}

Mit `ask` kann jetzt die `eval`-Funktion für Variablenzugriff erstellt
werden.

{% highlight haskell %}
eval (Var ident)
    = do env <- ask
         case lookup ident env of
           Nothing -> throwError
                      $ "free variable '"
                         ++ ident
                         ++ "' found"
           Just v  -> return v
{% endhighlight %}

Es wird das Environment ausgelesen und mit `lookup ident env` der Wert
der Variablen in der Umgebung gesucht. Dieser bildet das Resultat. Im
Fehlerfall wird eine entsprechende Fehlermeldung generiert.

Mit der Redefinition der Monade und dieser Erweiterung von `eval` ist
die Erweiterung der Ausdrücke um *freie* Variablen abgeschlossen.

Was noch fehlt, sind die lokalen, mit `Let` eingeführten Variablen für
Zwischenergebnisse.  Hierfür muss das Environment lokal erweitert und
für die lokale Auswertung der Ausdrücke genutzt werden.

Diese lokale Veränderung kann man, wie bei `ask`, allgemeingültig
implementieren mit einer `local` genannten Funktion

{% highlight haskell %}
local :: (Env -> Env) -> Result a -> Result a
local f (Res g) = Res $ \ env -> g (f env)
{% endhighlight %}

Mit Hilfe von `local` können lokal gebundene Variablen auf folgende
Art implementiert werden:

{% highlight haskell %}
eval (Let ident e1 e2)
    = do v1 <- eval e1
         local (addEnv ident v1)
                   (eval e2)
    where
      addEnv i v env = (i, v) : env
{% endhighlight %}

Der Ausdruck `e1` bestimmt den Wert der lokalen Variablen `ident`.  Er
wird hier in dem bisherigen `env` ausgewertet. Das Paar `(ident, v1)`
wird vorne vor die `env`-Liste eingefügt, so dass es in einer Suche
als erstes gefunden wird.  Mit diesem neuen Environment wird der
Ausdruck `e2` ausgewertet.

Damit ist die Erweiterung der Ausdruckssprache um freie und um lokal
gebundene Variablen vollständig. Wieder waren nur Veränderungen an der
Monade und Erweiterungen um die neuen Sprachelemente notwendig. Nichts
Bestehendes musste refaktorisiert werden.

Es fehlen noch einige einfache Ausdrücke für einen Testlauf:

{% highlight haskell %}
x  = Var "x"
y  = Var "y"

l1 = Binary Mul x y
l2 = Let "x" (Const 6) l1
l3 = Let "y" (Const 7) l2

v0 = runEval l1 []
v1 = runEval l1 [("x",2),("y",3)]
v2 = runEval l1 [("x",4),("y",5)]
v3 = runEval l2 [("y",5)]
v4 = runEval l3 []
{% endhighlight %}

und eine *ghci-Session*

    zwiebel> ghci Expr4.hs
    ...
    *Expr4> l1
    Binary Mul (Var "x") (Var "y")
    
    *Expr4> runEval l1 []
    Exc {exc = "free variable 'x' found"}
    
    *Expr4> runEval l1 [("x",2),("y",3)]
    Val {val = 6}
    
    *Expr4> [("x",4),("y",5)]
    Val {val = 20}
    
    *Expr4> l2
    Let "x" (Const 6) (Binary Mul (Var "x") (Var "y"))
    
    *Expr4> runEval l2 [("y",5)]
    Val {val = 30}
    
    *Expr4> l3
    Let "y" (Const 7) (Let "x" (Const 6) (Binary Mul (Var "x") (Var "y")))
    
    *Expr4> runEval l3 []
    Val {val = 42}

Die Monade mit dem Environment und den Funktionen `ask` und `local`
ist die sogenannte *Reader*-Monade. `ask` und `local` sind deklariert
in [Control.Monad.Reader][Reader]. In
[Real-World-Haskell][rwh]-Programmen wird diese Monade häufig genutzt,
um Konfigurations-Parameter an den unterschiedlichtsten Stellen
zugreifbar zu machen, ohne diese explizit durch Funktionen durch zu
reichen.

## Ausdrücke mit Programm-Variablen und Zuweisungen ##

In der folgenden Erweiterung der Ausdruckssprache werden wir
Programm-Variablen, Zuweisungen, Sequenzen, Schleifen und
Verzweigungen hinzu nehmen. Um das Beispiel übersichtlich zu halten,
werden wir nicht, wie es in realen Anwendungen sinnvoll wäre,
syntaktisch zwischen Ausdrücken und Anweisungen unterscheiden. Auch
werden wir, wie es in vielen Skriptsprachen (leider) üblich ist, auf
die Deklaration von Variable verzichten.  Variablen entstehen bei der
ersten Zuweisung.  Die Zuweisungen werden, wie in C, als Ausdrücke mit
Seiteneffekten behandelt.

Die abstrakte Syntax hat folgende Gestalt:

{% highlight haskell %}
data Expr  = Const  Int
           | Binary BinOp Expr Expr
           | Var    Id
           | Assign Id    Expr             -- neu
           | If     Expr  Expr Expr        -- neu
           | While  Expr  Expr             -- neu

data BinOp = Add | Sub | Mul | Div | Mod
           | Seq                           -- neu
{% endhighlight %}

Die Berechnungen finden also wie in dem letzten Beispiel in einer
Umgebung statt. Diese speichert die Belegung der Variablen. Der
entscheidende Unterschied ist aber, dass die Belegung sich während der
Berechnung ändern kann.  Diese sich ständig ändernde Umgebung
bedeutet, dass neben dem eigentlichen Wert immer noch der
möglicherweise veränderte Zustand als zusätzliches Resultat zurück
geliefert wird. Wir werden hierfür die Zustands-Monade nutzen.


{% highlight haskell %}
type VarState = [(Id, Int)]

data ResVal a
           = Val { val :: a }
           | Exc { exc :: String }
             deriving (Show)

newtype Result a
           = Res { unRes :: VarState -> (ResVal a, VarState) }
{% endhighlight %}

Der Zustand `VarState` ist wie oben als einfache Liste von
Schlüssel-Wert-Paaren realisiert. Die entscheidende Veränderung
gegenüber [Expr4.hs][Expr4] ist das Tupel `(ResVal a, VarState)` als
Resultattyp.

Die Monade für `Result` wird als Kombination der Fehler- und der
Zustands-Monade realisiert:

{% highlight haskell %}
instance Monad Result where
  return x       = Res $ \ st0 -> (Val x, st0)
  (Res f1) >>= g = Res $ \ st0 ->
                   let (r1, st1) = f1 st0 in
                   case r1 of
                     (Exc e) -> (Exc e, st1)
                     (Val v) -> let Res f2 = g v in
                                f2 st1
{% endhighlight %}

`return` konstruiert aus einem Wert eine Funktion, die den Zustand
unverändert durch reicht.  In `>>=` wird in der Funktion `st0` über
`st1` bis zum Endergebnis *durchgefädelt*.

Wir benötigen aber noch, analog zu `ask` Funktionen zum lesenden und
schreibenden Zugriff (`get` und `put`).

{% highlight haskell %}
get :: Result VState
get =  Res $ \ st -> (Val st, st)

put :: VState -> Result ()
put new = Res $ \ old  -> (Val (), new)
{% endhighlight %}

`get` arbeitet analog zu `ask`. `put` nimmt einen neuen Zustand und
wechselt den alten gegen diesen aus. Das eigentliche Resultat dieser
Aktion `()` ist hier unwesentlich.

Wie werden die neuen Sprachelemente implementiert? Beginnen wir mit
der Zuweisung:

{% highlight haskell %}
eval (Assign ident e1)
    = do v1 <- eval e1
         st <- get
         put   (setVar ident v1 st)
         return v1
    where
      setVar i v s = (i, v) : filter ((/= i) . fst) s
{% endhighlight %}

Die rechte Seite der Zuweisung wird ausgewertet, der Zustand
ausgelesen (`get`), der modifizierte Zustand berechnet (`setVar`) und
geschrieben. Wie in der Sprache C ist hier das Resultat der Zuweisung
der Wert der rechten Seite.

Für die häufig auftretende Codefolge `get >>= \ x -> put (f x)` gibt
es eine Bequemlichkeitsfunktion

{% highlight haskell %}
modify :: (s -> s) -> Result s
modify f
    = do s <- get
	     put (f s)
{% endhighlight %}

Die Verzweigung und die Schleife werden nach dem üblichen Muster
interpretiert, wobei in den Bedingungen `0` als `False` und alle
anderen Werte als `True` aufgefasst werden.

{% highlight haskell %}
eval (If c t e)
    = do v <- eval c
         if v /= 0
           then eval t
           else eval e

eval st@(While c b)
    = do v <- eval c
         if v /= 0
           then do eval b
                   eval st
           else return v
{% endhighlight %}

Es bleibt die Sequenz. Diese ist nichts anderes, als ein 2-stelliger
Operator (in der Sprache C das `,`). Bei der Auswertung wird der erste
Teilausdruck ausgewertet und das Ergebnis vergessen. Nur der Effekt
auf den Zustand ist von Interesse. Die Funktionstabelle für die
Operatoren muss also nur um eine Zeile erweitert werden.

{% highlight haskell %}
mft :: [(BinOp, MF)]
mft = [ ...
      , (Seq, liftM2 (\ x y -> y))
      ]
{% endhighlight %}

Es bleibt noch der Start einer Berechnung mittels `runEval`.  Diese
Funktion liefert jetzt zwei Werte, ein Resultat und einen Endzustand.

{% highlight haskell %}
runEval :: Expr -> VarState -> (ResVal Int, VarState)
runEval e st0
    = let (Res mf) = eval e in
      mf st0
{% endhighlight %}

Mit diesen wenigen Erweiterungen haben wir aus der Auswertung
arithmetischer Ausdrücke eine kleine, aber vollständige imperative
Programmiersprache entwickelt.  Und, wie immer unter Verwendung des
monadischen Stils, ausschließlich durch Modifikation der
Monaden-Definition und einiger lokaler Erweiterungen.

Es fehlt noch eine Demo mit zwei kleinen Programmen:

{% highlight haskell %}
p1 = foldr1 (Binary Seq)
     [ Assign "t" (Var "x")
     , Assign "x" (Var "y")
     , Assign "y" (Var "t")
     ]

p2 = While
       (Binary Sub x (Const 100))
       (Assign "x" (Binary Add x (Const 1)))
    where
      x = Var "x"
{% endhighlight %}

Im ersten Programm `p1` werden zwei Variablen `x` und `y` vertauscht,
in `p2` wird bis `100` gezählt.

    zwiebel> ghci Expr5.hs
    ...
    *Expr5> runEval p1 [("x", 42), ("y",23)]
    (Val {val = 42},[("y",42),("x",23),("t",42)])
    
    *Expr5> runEval p2 [("x", 1)]
    (Val {val = 0},[("x",100)])

## Programme mit Ein- und Ausgabe ##

Einer der größten Entwurfsfehler beim Entwickeln in Haskell ist eine
fehlende Überlegung, in welchen Funktionen Ein- und/oder Ausgabe
gemacht werden muss. Solche Fehler sind bei späteren Erweiterungen
häufig nicht mehr zu auszubessern, ohne große Teile eines Systems neu
zu schreiben.

In den bisher entwickelten Beispielen haben wir genau diesen Fehler
gemacht.  Was passiert, wenn wir für [Expr5.hs][Expr5] im zweiten
Release die Anforderung bekommen, bei der Auswertung von Ausdrücken
Werte einlesen oder Zwischenergebnisse ausgeben zu müssen?

Dieses bedeutet, das während der Auswertung nicht nur der interne
Zustand mit den Programm-Variablen verändert wird sondern auch der
Weltzustand in der `IO`-Monade.  Wir müssen also die `Result`-Monade
mit der `IO`-Monade kombinieren.  Nur dadurch, dass wir bisher in
einem monadischen Stil entwickelt haben, werden wir wieder auf
einfache Weise den Aspekt der Ein- und Ausgabe in das bisherige System
integrieren können.

Der `Result`-Datentyp wird so verändert, dass die Funktionen
in der `IO`-Monade laufen:

{% highlight haskell %}
newtype Result a
           = Res { unRes :: VarState -> IO (ResVal a, VarState) }
           --                          ^^^^
{% endhighlight %}

Entsprechend müssen `return` und `>>=` sowie `throwError`, `get` und
`put` so verändert werden, dass sie in der `IO`-Monade laufen:

{% highlight haskell %}
instance Monad Result where
  return x       = Res $ \ st0 -> return (Val x, st0)
  (Res f1) >>= g = Res $ \ st0 ->
                   do (r1, st1) <- f1 st0
                      case r1 of
                        (Exc e) -> return (Exc e, st1)
                        (Val v) -> let Res f2 = g v in
                                   f2 st1
instance MonadError String Result where
  throwError s   = Res $ \ st -> return (Exc s, st)

instance MonadState VarState Result where
  get            = Res $ \ st -> return (Val st, st)
  put new        = Res $ \ _  -> return (Val (), new)
{% endhighlight %}

Um auf den Weltzustand mit den vordefinierten `IO`-behafteten
Laufzeitsystem-Funktionen in der `Result`-Monade zu arbeiten,
benötigen wir noch eine *lift*-Funktion `liftIO`

{% highlight haskell %}
liftIO :: IO a -> Result a
liftIO a  = Res $ \ st ->
            do v <- a
	           return (Val v, st)
{% endhighlight %}

Für `liftIO` gibt es wieder eine vordefinierte Klasse [MonadIO].

Mit dieser Erweiterung der `Result`-Monade können wir beginnen, die
Ein- und Ausgabeoperationen zu implementieren.  Wir benötigen zwei
neue syntaktische Konstrukte für das Lesen und Schreiben von Werten.

{% highlight haskell %}
data Expr  = ...
           | Read
           | Write String Expr
{% endhighlight %}

`Read` wird zum Einlesen einer Zahl genutzt, `Write` zur Ausgabe eines
Strings und dem Resultat eines Ausdrucks. Die eigentliche
Schnittstelle zum I/O-System bilden hier die Hilfsfunktionen `readInt`
und `writeInt`.

{% highlight haskell %}
eval Read
    = readInt

eval (Write msg e)
    = do v <- eval e
         writeInt msg v
         return v

readInt :: Result Int
readInt = liftIO $ ...

writeInt :: Show a => String -> a -> Result ()
writeInt m v = liftIO $ ...
{% endhighlight %}

Der vollständige Code ist in [Expr6.hs][Expr6] zu finden. Wir werden
diesen mit zwei keinen Beispielen ausprobieren:

{% highlight haskell %}
-- read 2 numbers and display product

p1 = Write "Result is: "
     (Binary Mul Read Read)

-- count to 100 with trace

p2 = While
       (Binary Sub x (Const 100))
       (Write "x = "
          (Assign "x" (Binary Add x (Const 1))))
    where
      x = Var "x"
{% endhighlight %}

Eine *ghci-Session* mit zwei Testläufen zeigt die erwarteten
Ergebnisse:

    zwiebel> ghci Expr6.hs
    ...
    *Expr6> runEval p1 []
    give a number: 6
    give a number: 7
    Result is: 42
    (Val {val = 42},[])
    
    *Expr6> runEval p2 [("x", 1)]
    x = 2
    x = 3
    ...
    x = 99
    x = 100
    (Val {val = 0},[("x",100)])

Die Integration von Ein- und Ausgabe konnte, dank des monadischen
Stils, wieder duch lokale Änderung der Monade und lokale Erweiterungen
realisiert werden.  Alle anderen existierenden Codeteile konnten
unverändert weiter verwendet werden.

## Zusammenfassung und Ausblick ##

Wir haben in den Beispielen [Expr1.hs][Expr1] bis [Expr6.hs][Expr6]
gesehen, dass wir die einfache Ausdrucksauswertung in unterschiedliche
Richtungen erweitern konnten, ohne existierende Funktionalität zu
überarbeiten. Neue Aspekte konnten alleine durch die Erweiterung des
`Result`-Datentyps und der monadischen Operationen `return` und `>>=`
und deren Verwandten integriert werden.

In einem nichtmonadischen Stil hätten wir für jeden neuen Aspekt die
Schnittstellen und die Implementierungen vieler über das gesamte
Programm verstreuter Funktionen erweitern müssen.

Wir haben in den Beispielen alle Monaden *per Hand* gebaut und auch
*per Hand* kombiniert. Für das Verständnis von Monaden ist dieses ein
sehr sinnvolles Vorgehen.  Aber die verwendeten Monaden und deren
Kombinationen wiederholen sich.  In vielen etwas anspruchsvolleren
Projekten muss man IO, Fehlerbehandlung und einen internen Zustand
verwalten, meist kommt noch die Notwendigkeit einer Konfigurierbarkeit
dazu, d.h. man muss wiederholt eine *IO-Exception-Reader-State*-Monade
entwickeln, was spätestens nach dem zweiten Mal langweilig wird.

Dieses ruft nach einem Werkzeugkasten, mit dem man Standard-Monaden
auf flexible Art zu einem Monaden-Stapel ( *monad stack* ) zusammen
setzten kann.  Hierzu gibt es die sogenannten Monaden-Transformatoren
( *monad transformer* ), mit denen man aus einer gegebenen Basis-Monade
eine neue Monade erzeugen kann, die dann um einen neuen Aspekt
angereichert worden ist.

Die meisten, der hier diskutierten Monaden, könnte man mit diesen
Monaden-Transformatoren zusammen setzten, so auch unsere letzte
*IO-Exception-State*-Monade.  Auf [hackage] findet man zum Beispiel
die [mtl]-und [transformers]-Bibliotheken, die im Moment überwiegend
für diesen Zweck eingesetzt werden.  Unter anderem werden im
[Real-World-Haskell][rwh]-Buch einige in der Praxis bedeutsame
Beispiele für den Einsatz von Monaden-Transformatoren behandelt.

Monaden sind inzwischen nicht mehr nur eine Domäne von Haskell.  Auch
in anderen Sprachen, wie Scala, Javascript, Ruby, ...  wird versucht,
diesen Ansatz des programmierbaren Semikolons einzusetzen.
Insbesondere bei eingebetteten Domänen-spezifischen Sprachen (DSLs)
bietet dieser Stil Software-technisch viele Vorteile.

Die hier vorgestellten Beispiele sind also nicht das Ende der
Geschichte, sondern erst der Anfang.

Viel Spaß beim Ausprobieren der [Beispiele].

[Expr0]:  </code/Monaden2/Expr0.hs>  "Ausdrucksauswertung im rein funktionalen Stil"
[Expr1]:  </code/Monaden2/Expr1.hs>  "Ausdrucksauswertung im monadischen Stil"
[Expr2]:  </code/Monaden2/Expr2.hs>  "Ausdrucksauswertung mit Fehlererkennung"
[Expr2a]: </code/Monaden2/Expr2a.hs> "Ausdrucksauswertung mit Fehlererkennung"
[Expr3]:  </code/Monaden2/Expr3.hs>  "Ausdrucksauswertung mit Nichtdeterminismus"
[Expr3a]: </code/Monaden2/Expr3a.hs> "Ausdrucksauswertung mit Nichtdeterminismus"
[Expr3b]: </code/Monaden2/Expr3b.hs> "Ausdrucksauswertung mit Nichtdeterminismus"
[Expr4]:  </code/Monaden2/Expr4.hs>  "Ausdrucksauswertung mit freien und gebundenen Variablen"
[Expr5]:  </code/Monaden2/Expr5.hs>  "Ausdrucksauswertung mit Programm-Variablen"
[Expr6]:  </code/Monaden2/Expr6.hs>  "Ausdrucksauswertung mit Programm-Variablen und IO"
[Beispiele]: </files/Monaden2/Monaden2.zip> ".zip Archiv für die Beispiele"

[ControlMonad]:	<http://hackage.haskell.org/packages/archive/base/latest/doc/html/Control-Monad.html>
                "Control.Monad"

[ControlMonadError]:	<http://hackage.haskell.org/packages/archive/mtl/latest/doc/html/Control-Monad-Error.html>
               "Control.Monad.Error"

[Reader]:		<http://hackage.haskell.org/packages/archive/mtl/latest/doc/html/Control-Monad-Reader.htm>
               "Control.Monad.Reader"
[MonadIO]:     <http://hackage.haskell.org/packages/archive/basic-prelude/latest/doc/html/CorePrelude.html#t:MonadIO>
               "MonadIO"
			   
[hackage]:     <http://hackage.haskell.org/>
[mtl]:         <http://hackage.haskell.org/package/mtl>
[transformers]: <http://hackage.haskell.org/package/transformers>
[rwh]:         <http://www.realworldhaskell.org/> "Real World Haskell"

