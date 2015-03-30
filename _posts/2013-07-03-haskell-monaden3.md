---
layout: post
description: Monaden in Aktion
title: "Mehr Monaden in Aktion"
author: uwe-schmidt
tags: ["Haskell", "Monaden"]
---

Dieser Artikel ist der dritte einer Serie von Artikeln über
Monaden in Haskell.

* Im [ersten Artikel][fp1] der Serie haben wir die Grundlagen
  diskutiert.
* Im [zweiten Teil der Serie][fp2] haben wir begonnen, eigene 
  Monaden zur Lösung Software-technischer Aufgaben zu entwickeln
  und haben dabei die
  Fehler-Monade und die Listen-Monade kennen gelernt.
  
Im heutigen Teil möchten wir diesen Aspekt vertiefen und erneut
demonstrieren, wie durch Monaden
ein Stück Software modular und durch lokale Erweiterungen um neue
Funktionalität ergänzt werden kann, ohne bestehende Teile zu verändern
oder zu refaktorisieren. 

<!-- more start -->

Als laufendes Beispiel haben wir die klassische Aufgabe der Auswertung von 
Ausdrücken behandelt.
Die Mächtigkeit der Ausdruckssprache war bisher aber noch sehr beschränkt.
Wir werden nun die Sprache als erstes um freie und gebundene Variablen
erweitern.  Zur Verarbeitung hierfür wird die sogenannte Reader-Monade
eingesetzt werden.

Im folgenden Schritt werden Zuweisungen, Sequenzen, Verzweigungen und
Schleifen hinzu kommen. Damit bekommen wir schon eine
Turing-vollständige Programmiersprache. Die Reader-Monade wird
durch die Zustands-Monade ersetzt werden.

Die letzte Erweiterung wird die Ein- und Ausgabe betreffen. Der
Interpretierer muss dann also in der IO-Monade laufen. Dieses
oftmals schwierig Vorgehen, nämlich erst am
Schluss eines Entwicklungsprozesses an Ein- und Ausgabe zu denken,
wird uns in diesem Fall durch den monadischen Programmierstil keinerlei
Schwierigkeiten bereiten.

## Ausdrücke mit Variablen ##

Im diesem ersten Schritt werden wir die Ausdruckssprache um Variablen
erweitern.  Dazu wird die abstrakte Syntax, der Datentyp `Expr` auf
zwei Arten erweitert.  Als einfache Ausdrücke werden Variablen
zugelassen. Außerdem sollen, wie in Haskell auch, lokale Variablen
durch `let`-Ausdrücke eingeführt werden können.

Es stellt sich dann natürlich die Frage, ob, und wenn ja, wie diese
Erweiterung zu der existierenden Lösung hinzugefügt werden kann. Wir
werden, um die Beispiele übersichtlich zu halten, den
Nichtdeterminismus nicht weiter betrachten, sondern wieder von
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
den freien Variablen. Diese Zuordnung werden wir in
einer sogenannten Umgebung, engl. `Env`-ironment speichern. Die
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
muss. Damit berechnet `eval` aus einem Ausdruck eine Funktion vom Typ
`Env -> ResVal Int`. Dieser entspricht dem alten `Result` aus
[Expr2.hs][Expr2]. Für die Umgebung wird hier eine einfache
Schlüssel-Wert-Liste genommen.  Bei praktischen Anwendungen werden
üblicherweise effizientere Container-Implementierungen, z.B. aus
[`Data.Map`][DataMap] genutzt.

Um eine Auswertung von Ausdrücken mit Variablen zu starten
(`runEval`), benötigen wir also neben dem Ausdruck auch noch eine
Belegung der Variablen.

{% highlight haskell %}
runEval :: Expr -> Env -> ResVal Int
runEval e env
    = let (Res f) = eval e in
      f env
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
unverändert durchreicht.  In `>>=` wird in der Funktion `st0` über
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

Es bleibt die Sequenz. Diese ist nichts anderes als ein 2-stelliger
Operator (in der Sprache C das `,`). Bei der Auswertung wird der erste
Teilausdruck ausgewertet und das Ergebnis vergessen. Nur der Effekt
auf den Zustand ist von Interesse. Die Funktionstabelle für die
Operatoren muss also nur um eine Zeile erweitert werden.

{% highlight haskell %}
ftab :: [(BinOp, BinFct)]
ftab = [ ...
       , (Seq, liftM2 (\ x y -> y))
       ]
{% endhighlight %}

Es bleibt noch der Start einer Berechnung mittels `runEval`.  Diese
Funktion liefert jetzt zwei Werte, ein Resultat und einen Endzustand.

{% highlight haskell %}
runEval :: Expr -> VarState -> (ResVal Int, VarState)
runEval e st0
    = let (Res f) = eval e in
      f st0
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
`put` so verändert werden, dass sie in der `IO`-Monade laufen.

{% highlight haskell %}
instance Monad Result where
  -- Das return auf der linken Seite des = ist das zu definierende der
  -- Result-Monade. Das return auf der rechten Seite kommt aus der
  -- IO-Monade.
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

Die meisten der hier diskutierten Monaden, könnte man mit diesen
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
[fp2]: <http://funktionale-programmierung.de/2013/05/22/haskell-monaden2.html>
[rf]:  <http://funktionale-programmierung.de/2013/03/12/rein-funktional.html>
