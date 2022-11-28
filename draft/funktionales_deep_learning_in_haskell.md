---
layout: post
description: Beschreibung
title: "Funktionales Deep Learning in Haskell - Teaser"
author: raoul-schlotterbeck
tags: ["Funktionale Programmierung", "Deep Learning", "Haskell", "ConCat"]
---

Bei der Active Group entwickeln wir für Siemens eine App, die mithilfe von 
Neuronalen Netzen Anomalien in Produktionsprozessen erkennen soll. Während 
Python unbestritten die alles dominierende Sprache in Sachen Deep Learning ist, 
haben wir natürlich nach einer funktionaleren Lösung gesucht und sind dabei in Conal 
Elliots GHC-Plugin 
<a href="https://github.com/compiling-to-categories/concat" target="_blank">ConCat</a>
fündig geworden. Im ersten Teil dieser Reihe schauen wir uns an, warum es ein wahrer 
Hochgenuss ist, gängige Bibliotheken wie TensorFlow oder PyTorch zu Gunsten von 
ConCat über Bord zu werfen, und geben einen ersten groben Einblick in die
funktionsweise von Deep Learning mit ConCat. 

<!-- more start -->

## Was macht eigentlich so eine Deep Learning Bibliothek? ##

Im Rahmen dieses Blogposts reicht es, sich ein Neuronales Netz als eine Funktion mit
vielen Parametern vorzustellen. Während des sogennanten *Trainings* sollen die 
Parameter so eingestellt werden, dass das Netz auf einem gegebenen Datensatz 
möglichst "gute" Ausgaben produziert. Wie gut eine Ausgabe ist, wird dabei anhand 
einer *Fehlerfunktion* beurteilt, die in den meisten Fällen die Ausgabe des Netzes 
mit einer vorgegebenen, gewünschten Ausgabe vergleicht und daraus eine Zahl 
berechnet. Wir haben es hier also mit einem Optimierungsproblem zu tun, bei dem die 
eben erwähnte Zahl minimiert werden soll.

Für Neuronale Netze funtioniert das erfahrungsgemäß am Besten mit dem 
<a href="https://de.wikipedia.org/wiki/Gradientenverfahren" target="_blank"> Gradientenverfahren</a>.
Dabei wird zunächst eine zu optimierende Funktion nach ihren 
Parametern abgeleitet. Die Ableitung zeigt in Richtung des steilsten Anstiegs. 
Werden die Parameter nun entlang (entgegen) ihrer Ableitung angepasst, stehen die 
Chancen gut, dass die Funktion mit den angepassten Parametern höhere (niedrigere) 
Werte produziert als vorher (sofern man dabei keinen zu großen Schritt gemacht hat).

Es wundert daher nicht, dass die Kernkompetenz von Deep Learning Bibliotheken das 
<a href="https://de.wikipedia.org/wiki/Automatisches_Differenzieren" target="_blank"> 
automatische Berechnen von Ableitungen</a> ist. Als Komposition vieler 
Teilfunktionen ist die Ableitung eines Neuronalen Netzes nach der Kettenregel eine 
Komposition vieler Teilableitungen. Da Funktionskomposition assoziativ ist, steht 
uns frei, in welcher Reihenfolge wir die Komponenten auswerten. Wie sich 
herausstellt, ist dabei der Rechenaufwand bei rechtsassoziativer Auswertung 
(vorwärts) abhängig von der Eingangsdimension - das ist in diesem Fall die Anzahl 
der Parameter - und für den linksassoziativen Fall (rückwärts) von der 
Ausgangsdimension - in diesem Fall 1, da die Fehlerfunktion ja eine Zahl ausgibt. Es 
wundert daher auch nicht, dass Deep Learning Bibliotheken linksassoziativ arbeiten. 

Wie folgendes Beispiel zeigt 

```
(g o f)'(x) = g'(f(x)) o f'(x)
```

gibt es dabei ein ziemlich kniffliges Problem: Um `g'(f(x))` auszuwerten, müssen wir 
`f(x)` kennen. Wir brauchen daher ein Programm, welches das gesamte Neuronale Netz ein 
mal vorwärts auswertet, sich dabei alle Zwischenergebnisse und die Operationen, die 
diese erzeugt haben, merkt und diese Informationen in der rückwärtsläufigen 
Berechnung der Ableitung wieder abgreift. All diese Informationen werden in einem 
sogennanten *Wengert-* oder *Gradient-Tape* gespeichert und die Generierung und 
Verwaltung eines solchen Tapes ist das, was Deep Learning Bibliotheken eigentlich so 
machen.

## Deep Learning mit TensorFlow ##

In TensorFlow kann man sich dieses Tape (etwas lose aufgefasst) sogar anschauen. Das 
sieht z.B. so aus:

<img src="/files/funktionales-deep-learning-in-haskell/gradients.overview.png" width="400">

Dieser Tape- oder Graphenbasierte Ansatz bringt leider mit sich, dass alles, was wir 
in TensorFlow machen, im Hintergrund in einen Teil einer ziemlich komplizierten 
Maschinerie übersetzt werden muss. Interessierte LeserInnen mögen versuchen, zu 
verstehen, was TensorFlow z.B aus 
einem harmlosen `+` <a href="https://github.com/tensorflow/tensorflow/blob/v2.11.0/tensorflow/python/ops/math_ops.py#L3926-L4004" target="_blank"> so alles macht </a>.

### Beispiel: Simpler Autoencoder in TensorFlow ###

In etwa folgendermaßen könnte man in TensorFlow ein simples Neuronales Netz 
implementieren, das aus vier *Schichten* (d.i. im Wesentlichen eine 
Matrixmultiplikation) besteht, jeweils gefolgt von einer *Aktivierungsfunktion*.

```python
class Autoencoder:

    def __init__(self, dim):
        self.dims = [dim, dim, dim // 2, dim // 2, dim]
        self.weights = [] 
        self.biases = []
        for i in range(4):
            self.weights.append(
                tf.Variable(tf.random.normal(shape=(self.dims[i+1],self.dims[i])))
                )
            self.biases.append(
                tf.Variable(tf.random.normal(shape=(self.dims[i+1],1)))
                )
    
    def __call__(self, x):
        tf.convert_to_tensor([x], dtype=tf.float32)
        out = tf.matmul(self.weights[0], 
                        inputs, transpose_b=True) + self.biases[0]
        out = tf.tanh(out)
        out = tf.matmul(self.weights[1], out) + self.biases[1]
        out = tf.nn.relu(out)
        out = tf.matmul(self.weights[2], out) + self.biases[2]
        out = tf.tanh(out)
        
        return tf.matmul(self.weights[3], out) + self.biases[3]
```

Ohne weiter in Details zu gehen, lassen sich hieran schon einige Probleme 
verdeutlichen:

- Der Code ist sehr unübersichtlich und kompliziert.
- Große Teile des Codes haben gar nichts mit dem eigentlichen Netz zu tun, sondern 
  mit dem TensorFlow-Graphen.
- Der Code ist sehr spezialisiert auf TensorFlow interne Typen. Generalisierung und 
  Abstraktion ist in diesem Kontext kaum noch möglich.
- Die einzelnen Teile des Graphen lassen sich überhaupt nicht mehr separat testen; 
  um zu überprüfen, ob das Netz korrekte Werte ausgibt, muss man den ganzen Graphen
  laufen lassen.
- Manche Teile von Python sind kompatibel mit dem TensorFlow-Graph, andere nicht.
- Das ganze ist sehr anfällig für Fehler, die zur Laufzeit teils kryptische 
  Meldungen produzieren
- ...

<!-- 
- Was dabei im Hintergrund passiert, wissen wir nicht wirklich und können wir auch nicht kontrollieren.
- Manche Teile von Python sind verträglich damit, andere nicht.
- Dabei werden seher spezialisierte, TensorFlow-interne Typen benutzt
- Das alles wirkt sich äußerst ungünstig auf Konzepte wie Abstraktion, Generalisierung, Kompositionalität und Testbarkeit aus
- ... und führt sehr leicht zu teils kryptischen Fehlermeldungen zur Laufzeit.
 Wir haben hier offenbar mit genau der Art von Code zu tun, die wir als funktionale Programmierer aus gutem Grund möglichst weit an den Rand unserer Anwendung drücken wollen. Hier geht es aber um Funktionalität, die eigentlich im Kern unserer Anwendung liegt.
 -->

Man sieht z.B. folgender Fehlermeldung...

```python
>>> import autoencoder as ae
>>> encoder = ae.Autoencoder(4)
>>> encoder([1,2,3,4])
Traceback (most recent call last):
  File "<stdin>", line 1, in <module>
  File "/nix/store/2l3cr4ksxsz96ixz1dl2gdxg50byl45l-python3-3.10.4-env/lib/python3.10/site-packages/tensorflow/python/util/traceback_utils.py", line 153, in error_handler
    raise e.with_traceback(filtered_tb) from None
  File "/nix/store/2l3cr4ksxsz96ixz1dl2gdxg50byl45l-python3-3.10.4-env/lib/python3.10/site-packages/tensorflow/python/framework/func_graph.py", line 1147, in autograph_handler
    raise e.ag_error_metadata.to_exception(e)
ValueError: in user code:

    File "/Users/raoulschlotterbeck/Downloads/tensorfun/autoencoder.py", line 19, in __call__  *
        out = tf.matmul(self.weights[0], tf.convert_to_tensor([x], dtype=tf.float32)) + self.biases[0]

    ValueError: Dimensions must be equal, but are 4 and 1 for '{{node MatMul}} = MatMul[T=DT_FLOAT, transpose_a=false, transpose_b=false](MatMul/ReadVariableOp, Const)' with input shapes: [4,4], [1,4].
```
nicht direkt an, dass die Ursache darin lag, dass in obiger Definition der call-
Methode...

```python
def __call__(self, xs):
    ...
    out = tf.matmul(self.weights[0], 
                        inputs, transpose_b=True) + self.biases[0]
    ...
```

der wichtige Hinweis `transpose_b=True` fehlte. 

## Deep Learning mit ConCat ##

In einem Satz beschrieben nutzt ConCat die Isomorphie zwischen kartesisch 
abgeschlossenen Kategorien und Lambdakalkülen und greift damit in GHCs 
Kompilierprozess ein, um die Lambda-Ausdrücke der Kompilierersprache in andere
Kategorien zu transformieren.

![ConCat Pipeline](/files/funktionales-deep-learning-in-haskell/concat-pipeline.png)

Eine solche Kategorie ist die Kategorie der *Reverse Mode Automatic Differentation* 
(`RAD`). Die rückwärtsläufige Ableitung einer Funktion `f :: a -> b` wird durch 
einen Aufruf der Form `toCcc f :: RAD a b` erzeugt, der dem Plugin sagt, dass `f`
in die Kategorie `RAD` transformiert werden soll. Das Ergebnis ist ein gewöhnlicher 
Haskell-Datentyp hinter dem sich eine gewöhnliche Haskell-Funktion verbirgt.

Mit ConCat könnte man daher obigen Autoencoder so implementieren:

```haskell
type AutoencoderPars (f :: Nat -> * -> *) n m =
  ( (f m --+ f n)
      :*: (f m --+ f m)
      :*: (f n --+ f m)
      :*: (f n --+ f n)
  )

autoencoder ::
  (KnownNat n, KnownNat m, Additive numType, Floating numType, Ord numType) =>
  AutoencoderPars f n m numType -> f n numType -> f n numType
autoencoder = affine @. affTanh @. affRelu @. affTanh
```

Ohne weiter in Details zu gehen, lassen sich hieran schon einige Vorteile 
verdeutlichen:

- Der Code ist auf die Wesentlichen Konzepte reduziert.
- Der Autoencoder ist eine pure, ganz normale Haskell-Funktion, die das, und nur das 
  macht, was ein Autoencoder so macht.
- Die Typen sind generisch gehalten.
- Der Autoencoder lässt sich leicht testen.
- Durch Verwendung von Datentypen, die Informationen über ihre Dimension enthalten,
  weist GHC schon beim Kompilieren darauf hin, wenn irgendwo Dimensionen nicht 
  zusammenpassen.
- ...

Jetzt wissen wir schon mal, dass wir für Deep Learning lieber ConCat benutzen. Aber 
wie macht man denn Deep Learning mit ConCat eigentlich? Das schauen wir uns in einem 
späteren Blogpost an.

<!-- more end -->

<!-- 
Mike:
  - mit Autoencoder anfangen
  - das ist natürlich nur der Autoencoder, wir brauchen noch Gradienten
  - ConCat ersetzt normalen FUnktionspfeil durch einen Pfeil, der statt der eigentlichen Funktion die Ableitung ausrechnet (mit Hilfe von Kategorientheorie)
  - vielleicht erwähnen, dass es nicht so ein komplizierter Tape-Algorithmus ist
  - wie das genau aussieht, schreiben wir in einem zukünftigen Post
 -->