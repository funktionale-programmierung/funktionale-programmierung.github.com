---
layout: post
description: Monaden in Aktion
title: "Monaden in Aktion"
author: uwe-schmidt
tags: ["Haskell", "Monaden"]
---

# Monaden in Aktion #

Im letzten Artiken über Monaden haben wir die Grundlagen
diskutiert. Hier soll es darum gehen, eigene Monaden zur Lösung
Software-technischer Aufgaben zu entwickeln.  Wir werden sehen, wie
ein Stück Software modular und durch lokale Erweiterungen um neue
Funktionalität ergänzt werden kann, ohne bestehende Teile zu verändern
oder zu refaktorisiern.

Als laufendes Beispiel werden wir die klassische Aufgabe der
Auswertung von Ausdrücken behandeln. Wir werden mit einfachen
arithmetischen Ausdrücken und Konstanten beginnen.  Hierfür werden wir
einen *rein funktional* geschriebenen Interpretierer angeben. Diesen
werden wir in eine *monadische* Form transformieren, ohne die
Funktionalität zu verändern.

Den ersten Aspekt, den wir hinzu fügen möchten, ist eine sinnvolle
Fehlerbehandlung.  Der Interpretierer wird also nicht mehr
*abstürzen*, sondern immer ein Resultat liefern, entweder ein Ergebnis
oder eine aussagekräftige Fehlermeldung.  Diesen Aspekt werden wir mit
der Fehler-Monade in den Griff bekommen.

Anschließend werden wir die Ausdruckssprache in unterschiedliche
Richtungen erweitern.  Eine erste Erweiterung wird die Hinznahme von
Operatoren sein, die bei der Auswertung mehrere Resultate liefern
können. In der Mathematik ist das Wurzel ziehen ein Beispiel für eine
solche Operation. In der Haskell-Gemeinde wird für diese mehrfachen
Resultate häufig der Begrif der nicht deterministischen Berechnung
verwendet.  Hierfür werden wir die Listen-Monade nutzen.

Die zweite Erweiterung werden Ausdrücke mit Variablen sein. Zur
Auswertung solcher Ausdrücke benötigt man eine Belegung der Variablen
mit Werten, man hat also wärend der Auswertung ein *Environment*,
einen Kontext, über den die Werte zugreifbar werden. Hier kommt die
sogenannte [Reader]-Monade zum Einsatz.

In der folgenden Erweiterung werden wir die Variable zu
Programmvariablen machen.  Die Menge der Ausdrücke wir um Zuweisungen,
Verzweigungen und Schleifen erweitert.  Die [Zustands][State]-Monade
kommt hier zum Einsatz.

Als letzten Schritt werden wir Operatione für die Ein- und Ausgabe
hinzu nehmen.  Die IO-Monade muss also irgendwie in unsere eigene
Monade integriert werden.

## Rein funktionaler Code versus I/O-behafteter Code ##

Bei der Entwicklung von Haskell-Programmen gibt es ein Problem, dass
bei den heutigen Mainstream-Sprachen nicht auftritt. Funktionen mit
I/O können nicht innerhalb von *puren* Funktionen verwendet
werden. Dieses wird vom Typsystem verhindert.

Wenn im Laufe eines Projekt vestgestellt wird, dass in einer
elementaren Funktion `f` Ein- und/oder Ausgabe notwendig ist, so
müssen alle Funktionen die dieses `f` direkt oder indirekt nutzen, so
umgeschrieben werden, dass sie in der IO-Monade laufen.  Dieses kann
in heißen Projektphasen manchmal schlicht nicht machbar sein. Als
Notlösung wird dann leicht in eine Kiste mit schmutzigen Tricks
gegriffen und Funktionen, die `unsave???` heißen genutzt. Diese ist
üblicherweise der erste Schritt zu einem *verwarzten* System.

Mit dem hier vorgestellten monadischem Programmierstil werden wir
nicht in dieses hässliche Problem hinein laufen.

## Auswertung arithmetischer Ausdücke ##

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
`Const` und 2-stellige Operationen `Binary`. Binäre Ausdrücke enhalten
einen Operator `BinOp` und 2 Teilausdrücke.  Der Interpretierer `eval`
berechnet aus einem Ausdruck einen Wert vom Typ `Result`.  Die
Auswertung der Konstanten ist trivial, binäre Ausdrücke werden
ausgewertet, indem die Teilausdrücke durch rekursive Aufrufe von eval
ausgewertet werden, die Bedeutungsfunktion der Operatoren wird aus
einer Tabelle `mft` mit `lookupMft` ausgelesen und auf auf die
Teilergebnisse angewendet. Der `Mod`-Operator feht mit Absicht noch in
der Tabelle, damit wir den Fehlerfall in `lookupMft` testen können.

Einige Testausdrücke:

{% highlight haskell %}
e1 = Binary Mul (Binary Add (Const 2)
                            (Const 4)
                )
                (Const 7)
e2 = Binary Div (Const 1) (Const 0)
e3 = Binary Mod (Const 1) (Const 0)
{% endhighlight %}

und eine *ghci*-Session

> zwiebel> ghci Expr0.hs
> ...
> *Expr0> e1
> Binary Mul (Binary Add (Const 2) (Const 4)) (Const 7)
>
> *Expr0> eval e1
> 42
>
> *Expr0> e2
> Binary Div (Const 1) (Const 0)
>
> *Expr0> eval e2
> *** Exception: divide by zero
>
> *Expr0> e3
> Binary Mod (Const 1) (Const 0)
>
> *Expr0> eval e3
> *** Exception: operation not implemented

Der Interpretierer liefert für `e1` das erwartete Ergebnis. Er besitzt
aber den groben Software-technischen Mangel, dass in Fehlerfällen,
`eval e2` und `eval e3`, die Berechnung vom *ghci*-Laufzeitsystem
einfach abgebrochen wird und nicht, wie es sein sollte, dieser Fehler
von der `eval`-Funktion behandelt wird.

## Auswertung arithmetischer Ausdücke in monadischem Stil ##

Wir werden die mangelhafte Fehlerbehandlung noch einen Moment
ignorieren und die `eval`-Funktion in eine gleichwertige *monadische*
Form bringen. Dazu werden wir die allereinfachste Monade, die
Identitätsmonade, nutzen. Wir müssen dazu allerdings den
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

> zwiebel> ghci Expr1.hs
> ...
> *Expr1> eval e1
> Val {val = 42}
>
> *Expr1> eval e2
> Val {val = *** Exception: divide by zero
>
> *Expr1> eval e3
> *** Exception: operation not implemented

Die Funktionalität ist identisch zu der aus *Expr0*. In diesem ersten
Schritt haben wir das Beispiel nur kompilizerter gemacht, wir haben
nichts gewonnen, *NUR* in die Erweiterbarkeit investiert.

## Erweiterung um Fehlerbehandlung ##

Sollen Fehler in der Anwendung selbst behandelt werden, so müssen die
Fehler im `Result`-Datentyp repräsentiert werden. Nur so kann im
Interpretierer unterschieden werden, ob eine Auswertung einen Fehler
oder einen *normalen* Wert liefert. `Result` wird also zu einem
Summen-Datentyp erweitert. Der Code-Auschnitt zeigt die Erweiterungen,
der Rest bleibt unverändert. Die vollständige Quelle steht unter
[Expr2.hs][Expr2].

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
`div` war die andere Schwachstelle. In `div''` wird die Vorbedingung
`y /= 0` geprüft, bevor die eigentliche Division ausgeführt wird.

Der Testlauf von oben ergibt jetzt folgende Resultate

> zwiebel> ghci Expr2.hs
> ...
> *Expr2> eval e1
> Val {val = 42}
> 
> *Expr2> eval e2
> Exc {exc = "division by zero"}
> 
> *Expr2> eval e3
> Exc {exc = "operation not implemented"}

Wir sehen, dass nur durch Hinzufügen weniger Zeilen der Aspekt der Fehlererkennung
in den bestehenden Interpretierer integriert werden konnte. Und nur dort, wo Fehler
auftreten konnten, `lookupMft` und `div`, sind lokale Veränderungen vorgenommen worden.
Die `eval`-Funktion wurde an keiner Stelle angefasst.

In einem nicht monadischen Stil hätten wir an jeder Stelle, an der ein
`Result`-Wert berechnet wird, die Verzweigung `Val`/`Exc` einfügen
müssen, alleine beim Auswerten binärer Ausdrücke hätten wir diesen
Test 3 mal einzufügen.  Die Investition in [Expr1.hs][Expr1] hat sich
also schon ausgezahlt.

Randnotiz: `throwError` möchte man natürlich in allen Ausnahme-Monaden
nutzen.  Es ist also sinnvoll, `throwError` in einer Typklasse zu
deklarieren. Die Klasse `MonadError` ist im Modul
[Control.Monad.Error][ControlMonadError] übernimmt diese Aufgabe.
Außerdem enthält sie auch eine Funktion `catchError` zum Abfangen und
Behandeln von Fehlern, was in unserem Beispiel nicht benötigt wird.
Da `MonadError` eine Multi-Parameter-Klasse ist, werden bei ihrer
Verwendung einige Haskell-Spracherweiterungen benötigt. Der
vollständige Code hierfür findet sich im Beispiel [Expr2a.hs][Expr2a].

## Erweiterung um Nichtdeterminismus mit der Listen-Monade ##

In dem folgenden Beispiel werden wir die Menge der Operatoren um einen
etwas ungewöhnlichen Operator `PlusMinus` erweitern. Dieser soll es
uns ermöglichen, eine Art Intervall-Arithmetik zu machen. ``2
`plusMinus` 1`` soll als Resultat die beiden Werte `1` und `3`
ergeben.

Wir haben also einen Operator, der mehrere Werte als Resultat
liefert. Dieses Arbeiten mit Funktionen mit mehreren Resultaten wird
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

Nur kommt noch das Aus- und Einwickelen mit `val` und `Val` dazu. Es wird
auf alle Elemente aus `xs` die Funktion `g` angewendet, anschließend
wird die resultierende Liste von Listen mit `concat` zu einer
einfachen Liste zusammen gefasst.

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

> zwiebel> ghci Expr3.hs
> ...
> *Expr3> eval e1
> Val {val = [42]}
> 
> *Expr3> eval i1
> Val {val = [2,0]}
> 
> *Expr3> eval i2
> Val {val = [8,4]}
> 
> *Expr3> eval e4
> Val {val = [14,0]}
> 
> *Expr3> eval e5
> Val {val = [10,6,8,4]}
> 
> *Expr3> eval e6
> Val {val = [24,4,20,8,22,6,18,10,10,-10,6,-6,8,-8,4,-4]}
> 
> *Expr3> eval e7
> Val {val = [4*** Exception: division by zero

<!-- ] hack für emacs markdown mode, Klammer zu fehlt -->

Wir sehen, dass die *alten* Ausdrücke, `e1`, wie bisher ausgewertet
werden, sie sind nur in eine einelementigen Liste eingewickelt.

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
hier folgendermaßen realisieren:

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
abgebrochen. Sonst wird auf jedes Element aus `g` angewendet. Dieses
Resultat wird mit `partition` in Fehler- und Werte-Liste
partitioniert. Wenn in keinem Fall ein Fehler aufgetreten ist, werden
die Resultate wie bisher konkateniert und in `Val` eingewickelt, sonst
bildet der erste Fehler das Resultat. Die Berechnung wird also beim
ersten Fehler abgebrochen. Das vollständige Programm steht unter
[Expr3a.hs][Expr3a]

Ein Testlauf zeigt, dass die Fehlerbehandlung wie gewünscht funktioniert:

> zwiebel> ghci Expr3a.hs
> ...
> *Expr3a> eval e1
> Val {val = [42]}
> 
> *Expr3a> eval e2
> Exc {exc = "division by zero"}
> 
> *Expr3a> eval e6
> Val {val = [24,4,20,8,22,6,18,10,10,-10,6,-6,8,-8,4,-4]}
> 
> *Expr3a> eval i1
> Val {val = [2,0]}
> 
> *Expr3a> eval i2
> Val {val = [8,4]}
> 
> *Expr3a> eval $ Binary Div i2 i1
> Exc {exc = "division by zero"}

Dieses muss nicht die einzige sinnvolle Definition von `>>=`
sein. Denkbar wäre auch eine etwas liberalere Handhabung der Fehler,
bei der man die Fehler ignoriert, solange man wenigstens ein richtiges
Resulat hat. Das Software-technisch entscheidende ist, dass für die
Art der Interpretation nur lokale Änderungen in der Monade notwendig
sind.

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

> zwiebel> ghci Expr3b.hs
> ...
> *Expr3a> eval e2
> Exc {exc = "division by zero"}
> 
> *Expr3a> eval i1
> Val {val = [2,0]}
> 
> *Expr3a> eval i2
> Val {val = [8,4]}
> 
> *Expr3a> eval $ Binary Div i2 i1
> Val {val = [4,2]}

Die Fehler bei ``8 `div` 0`` und ``4 `div` 0`` werden ignoriert,
da ``8 `div` 2`` und ``4 `div` 2`` zu `[4, 2]` ausgewertet werden.

Was haben wir bisher erreicht? Durch das Umschreiben des
ursprünglichen Programms [Expr0.hs][Expr0] in eine monadische Form
[Expr1.hs][Expr1] konnten wir den Aspekt der Fehlererkennung auf
einfache Weise und mit ausschließlich lokalen Änderungen und
Erweiterungen integrieren ([Expr2.hs][Expr2]). Das gleiche war möglich bei der Erweiterung
durch *nichtdeterminischtische* Funktionen ([Expr3b.hs][Expr3b]).  Und schließlich haben wir
in [Expr3a.hs][Expr3a] und [Expr3b.hs][Expr3b] gesehen, wie die beiden
Aspekte Fehlererkennung und Nichtdeterminismus nur durch
lokale Erweiterung der Monade kombiniert werden konnten.

## Ausdrücke mit Variablen ##

Im folgenden Schritt werden wir die Ausdruckssprache um Variablen erweitern.
Dazu wird die abstrakte Syntax, der Datentyp `Expr` auf zwei Arten erweitert.
Als einfache Ausdrücke werden Variablen zugelassen. Außerdem sollen, wie in Haskell auch,
lokale Variablen durch `let`-Ausdrücke eingeführt werden können.

Es stellt sich dann natürlich die Frage, ob, und wenn ja, wie diese Erweiterung
zu der existierenden Lösung hinzugefügt werden kann. Wir werden, um die Beispiele
übersichtlich zu halten, den Nichtdeterminismus nicht weiter betrachten, sondern von
[Expr2.hs][Expr2] ausgehen.

Der Datentyp `Expr` erweitert um die beiden neuen Varianten:

{% highlight haskell %}
data Expr  = Const  Int
           | Binary BinOp Expr Expr
           | Var    Id
           | Let    Id    Expr Expr
		   
type Id    = String
{% endhighlight %}

Was ist die Bedeutung eines Audruck mit freien Variablen?  Zur
Auswertung solcher Ausdrücke brauchen wir eine Zuordnug von Werten zu
den freien Variablen, eine sogennate Umgebung, ein `Env`-ironment. Die
Bedeutung eines Ausdrucks kann als eine Funktion aufgefasst
werden, die eine Umgebung auf einen Resultatwert abbildet.
`eval` wird also einen Ausdruck (`Expr`) nehmen, und daraus eine Funktion
berechnen. Erst wenn diese Funktion auf eine Umbegung angewendet wird,
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

Der `Result`-Typ wird ein Funktionstyp `Env -> ResultVal a`, der wegen
der `Monad`-Instanz in einen `newtype` verpackt werden muss. Damit
berechnet `eval` aus einem Ausdruck eine Funktion, die aus einem `Env`
ein `ResVal Int`. Dieser entspricht dem alten `Result` aus
[Expr2.hs][Expr2]. Für die Umgebung wird hier eine einfache
Schlüssel-Wert-Liste genommen.  Bei praktischen Anwendungen werden
üblicherweise effizentere Container-Implementierungen, z.B. `Data.Map` genutzt.

Um eine Auswertung von Ausdrücken mit Variablen zu starten (`runEval`),
benötigen wir also neben dem Ausdruck auch noch eine Belegung der Variablen.

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
`env` angewendet wird. Sowohl `f` als auch `g v` nutzen die gleiche
Umgebung.

Wir haben aber noch keine direkten Funktionen, die das `env` nutzen,
was aber bei der Auswertung von `Var ident` notwendig ist. Hierfür definieren
wir eine Funktion `ask` zum Auslesen der gesamten Umgebung.

{% highlight haskell %}
ask :: Result Env
ask = Res $ \ env -> Val env
{% endhighlight %}

Mit `ask` kann jetzt die `eval`-Funktion für Variablenzugriff erstellt werden.

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

Es wird das Environment ausgelesen und mit `lookup ident env` der Wert der Variablen
in der Umgebung gesucht. Dieser bildet das Resultat. Im Fehlerfall wird eine
entsprechende Fehlermeldung generiert.

Mit der Erweiterung der Monade und dieser Erweiterung von `eval`
ist die Erweiterung der Ausdrücke um freie Variablen abgeschlossen.

Was noch fehlt, sind die lokalen, mit `Let` eingeführten Variablen für Zwischenergebnisse.
Hierfür muss das Environment lokal erweitert und für die lokale Auswertung der Ausdrücke
genutzt werden.

Diese lokale Veränderung kann man, wie bei `ask`, allgemeingültig implementieren
mit `local` genannten Funktion

{% highlight haskell %}
local :: (Env -> Env) -> Result a -> Result a
local f (Res g) = Res $ \ env -> g (f env)
{% endhighlight %}

Mit Hilfe von `local` können lokal gebundene Variablen auf folgende Art
implementiert werden:

{% highlight haskell %}
eval (Let ident e1 e2)
    = do v1 <- eval e1
         local (addEnv ident v1)
                   (eval e2)
    where
      addEnv i v env = (i, v) : env
{% endhighlight %}

Der Ausdruck `e1` bestimmt den Wert, der lokalen Variablen `ident`. Dieses Paar wird
vorne vor die `env`-Liste eingefügt, so dass es in einer Suche als erstes gefunden wird.
Mit diesem neuen Environment wird der Ausdruck `e2` ausgewertet.

Damit ist die Erweiterung der Ausdruckssprache um freie und um lokal gebundene
Variablen abgeschlossen. Wieder waren nur Veränderungen an der Monade und Erweiterungen
um die neuen Sprachelemente notwendig. Nichts Bestehendes musste refaktorisiert werden.

Es fehlen noch einige einfache Ausdrücke

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

und ein Testlauf

> zwiebel> ghci Expr4.hs
> ...
> *Expr4> l1
> Binary Mul (Var "x") (Var "y")
> 
> *Expr4> runEval l1 []
> Exc {exc = "free variable 'x' found"}
> 
> *Expr4> runEval l1 [("x",2),("y",3)]
> Val {val = 6}
> 
> *Expr4> [("x",4),("y",5)]
> Val {val = 20}
> 
> *Expr4> l2
> Let "x" (Const 6) (Binary Mul (Var "x") (Var "y"))
> 
> *Expr4> runEval l2 [("y",5)]
> Val {val = 30}
> 
> *Expr4> l3
> Let "y" (Const 7) (Let "x" (Const 6) (Binary Mul (Var "x") (Var "y")))
> 
> *Expr4> runEval l3 []
> Val {val = 42}

Die Monade mit dem Environment und den Funktionen `ask` und `local`
ist die sogenannte *Reader*-Monade. `ask` und `local` sind deklariert
in [Control.Monad.Reader][Reader]. In
[Real-World-Haskell][rwh]-Programmen wird diese Monade häufig genutzt, um
Konfigurations-Parameter an den unterschiedlichtsten Stellen zugreifbar zu machen,
ohne diese explizit durch Funktionen durch zu reichen.




{% highlight haskell %}
{% endhighlight %}



[Expr0]:  </code/monaden2/Expr0.hs>
[Expr1]:  </code/monaden2/Expr1.hs>
[Expr2]:  </code/monaden2/Expr2.hs>
[Expr2a]: </code/monaden2/Expr2a.hs>
[Expr3]:  </code/monaden2/Expr2.hs>
[Expr3a]: </code/monaden2/Expr2a.hs>
[Expr3a]: </code/monaden2/Expr2a.hs>

[ControlMonad]:	<http://hackage.haskell.org/packages/archive/base/latest/doc/html/Control-Monad.html>                "Control.Monad"
[ControlMonadError]:	<http://hackage.haskell.org/packages/archive/mtl/latest/doc/html/Control-Monad-Error.html>
               "Control.Monad.Error"

[Reader]:		<http://hackage.haskell.org/packages/archive/mtl/latest/doc/html/Control-Monad-Reader.htm>
               "Control.Monad.Reader"
