---
layout: post
description: Nebenläufigkeit mit Software Transactional Memory
title: "Nebenläufigkeit mit Software Transactional Memory"
author: stefan-wehr
tags: ["Nebenläufigkeit", "Haskell", "STM"]
---

Nebenläufigkeit ist aus modernen Softwaresystemen nicht mehr wegzudenken.
Sei es, um Performancesteigerungen durch paralleles Ausführen auf mehreren
Prozessorkernen zu erzielen, oder weil das zugrundeliegende Problem
inherent nebenläufig ist. In einem
[anderen Blogartikel](/2013/03/06/parallel-haskell.html) haben wir uns
bereits mit der Parallelisierung von Programmen bzw. Algorithmen
beschäftig.

Heute soll es nun um echte Nebenläufigkeit gehen. Mit "echter Nebenläufigkeit"
meine ich, dass die Nebenläufigkeit nicht Mittel zum Zweck ist, sondern
im zu lösenden Problem schon drinsteckt. Ein gutes Beispiel ist z.B.
ein Webserver, der quasi gleichzeitig Anfragen von verschiedenen Clients
beantwortet. Um einen solchen Webserver zu programmieren, ist
Nebenläufigkeit ein gutes Modell: jeder Client wird als separater Thread
programmiert und kann damit isoliert betrachtet werden.

Jeder der schon einmal ein nebenläufiges Programm geschrieben hat, weiß
allerdings wie
schwer es sein kann, Daten zwischen verschiedenen Threads auszutauschen:
Race Conditions und Deadlocks sind oft die Folge. In diesem Artikel geht
es um "Software Transactional Memory", kurz STM, einer alternativen
Technik zur Kommunikation zwischen Threads. Wir setzen in der Serverkomponente
unseres Produkts [CheckpadMED](/2013/07/17/medizin-funktional.html)
ausschließlich auf STM zur Kommunikation zwischen Threads und sind damit
sehr zufrieden. STM ist sprachunabhängig und steht für eine Vielzahl
von funktionalen Sprachen (z.B. [Clojure](http://clojure.org/refs),
[Haskell](http://hackage.haskell.org/package/stm),
[OCaml](http://cothreads.sourceforge.net/),
[Scala](http://github.com/djspiewak/scala-stm/tree/master))
und anderen Sprache
(z.B. [C/C++](http://gcc.gnu.org/wiki/TransactionalMemory),
[C#](http://github.com/jbakic/Shielded),
[Java](http://multiverse.codehaus.org/overview.html)) zur Verfügung.
Funktionale Sprachen scheinen aber für den Einsatz von STM deutlich
besser gerüstet zu sein, da dort unveränderbare Datenstrukturen und der
Verzicht auf global-veränderbare Variablen Standard sind. In diesem
Artikel zeigen wir, wie STM in Haskell funktioniert, die Konzepte
sind aber in den anderen Sprachen sehr ähnlich und übertragbar.

<!-- more start -->

Wie in der Einleitung motiviert, ist Nebenläufigkeit eine schöne
Abstraktion um z.B. einen Webserver zu programmieren. Kompliziert
wird es allerdings, sobald Zustand in's Spiel
kommt (wie sooft). Wenn nämlich mehrere Threads auf einem geteilten Zustand
operieren, kann es durch das gleichzeitige Modifizieren dieses Zustands
zu *Race Conditions* und damit zu korrupten Daten kommen. Der übliche Weg, Race Conditions zu
vermeiden und die Kommunikation zwischen verschiedenen Threads zu
synchronisieren, sind *Locks*. Wenn man beim Einsatz von Locks allerdings
nicht sehr vorsichtig ist, kommt es leicht zu *Deadlocks*, d.h. das
Programm hängt. Jeder, der schon mal mit Locks gearbeitet hat, weiß wie
schwierig es ist, Deadlocks und Race Conditions zu vermeiden bzw. zu
debuggen.

Starten wir mit einem praktischen Beispiel.
Wir wählen das Standardbeispiel, nämlich die Modellierung von
Überweisungen zwischen Bankkonten. Dabei soll die wichtige
Eigenschaft gelten, dass kein Geld verloren gehen kann: wenn wir einen
Betrag `N` von Konto `k1` auf Konto `k2` übertragen, soll entweder der
Betrag von `k1` abgebucht und auf `k2` aufgebucht werden, oder beide
Konten sollen unverändert bleiben.

Diese Eigenschaft können wir direkt als Haskell-Code hinschreiben. Wir
nehmen dazu zunächst an, dass ein Konto durch den Typen `Account`
modelliert wird und dass es zwei Funktion `deposit` und `withdraw` zum
Einzahlen bzw. Abbuchen gibt. Der Einfachheit halber zahlen wir nur ganze
Beträge (Typ `Int`) auf Konten ein.

Die Funktion zum Überweisen von `k1` auf `k2` sieht dann so aus:

{% highlight haskell %}
transfer :: Account -> Account -> Int -> IO ()
transfer k1 k2 amount =
    atomically (do deposit k2 amount
                   withdraw k1 amount)
{% endhighlight %}

Durch das `atomically` wird der übergebene Codeblock als atomar
gekennzeichnet und das STM-Laufzeitsystem sorgt dafür, dass der Block
entweder ganz oder gar nicht ausgeführt wird.

In der Datenbank-Welt kennt man ein solches Konzept unter dem Begriff
*Transaktion*. Im obigen Beispielcode ist also der Block

{% highlight haskell %}
               (do deposit k2 amount
                   withdraw k1 amount)
{% endhighlight %}

eine Transaktion.

Nun möchten wir auch noch den Typen `Account` sowie die Funktionen
`deposit` und `withdraw` mit Leben füllen. In STM kommunizieren Threads
über sogenannte Transaktionsvariablen. D.h. alle Daten, die von mehrere
Threads verändert werden, müssen in eine Transaktionsvariable
gepackt werden. Eine Transaktionsvariable hat den Typ `TVar a`, wobei
`a` der Typ der Inhalts ist. Für unser Konto definieren wir also

{% highlight haskell %}
type Account = TVar Int
{% endhighlight %}

Auf einer solche `TVar` stehen drei wichtige Operationen zur Verfügung:

{% highlight haskell %}
newTVar   :: a -> STM (TVar a)      -- neue Transaktionsvariable anlegen
readTVar  :: TVar a -> STM a        -- Inhalt lesen
writeTVar :: TVar a -> a -> STM ()  -- Inhalt schreiben
{% endhighlight %}

Alle drei Operationen laufen also in der
`STM`-[Monade](/2013/04/18/haskell-monaden.html) und diese drei
Operationen sind (im wesentlichen) auch die einzigen Operationen, die in
der `STM`-Monade zur Verfügung stehen. Damit wird bereits im Typsystem
unterschieden zwischen normalen seiteneffektbehafteten Operationen
(`IO`-Monade) und den transaktionellen Operation der STM-Welt. Die
`atomically`-Funktion hat folgenden Typ:

{% highlight haskell %}
atomically :: STM a -> IO a
{% endhighlight %}

Daraus sehen wir, dass Operationen auf Transkationsvariablen
immer innerhalb eines `atomically`-Blocks ausgeführt werden müssen, das
stellt das Typsystem sicher. Den einzigen Fehler, den ein Programmierer hier noch machen
kann, ist eine falsche Einteilung des Programms in atomare Blöcke. Dies
aber ist ein logischer Fehler, und solche Fehler kann ein Compiler
oder ein Laufzeitsystem nur schwerlich verhindern.

So, mit diesem Wissen ausgestattet können wir nun die Operationen
`withdraw` und `deposit` implementieren.

{% highlight haskell %}
deposit :: Account -> Int -> STM ()
deposit k amount =
    do bal <- readTVar k
       writeTVar k (bal + amount)

withdraw :: Account -> Int -> STM ()
withdraw k amount = deposit k (- amount)
{% endhighlight %}

Damit haben wir nun mit STM ein ganz einfaches Bankkonto
implementiert. Wenn nun die `transfer`-Funktion aus mehreren Threads
aufgerufen wird, stellt das STM-Laufzeitsystem sicher, dass sich
die Threads nicht in's Gehege kommen und dass somit alle Kontostände
immer konsistent sind.

Wie aber sorgt das STM-Laufzeitsystem für diese schöne Eigenschaft?
Um diese Frage und um weiterführende STM-Operationen zum Blockieren und
zur Kombination von Transaktionen möchten wir uns im nächsten Artikel
kümmern.

Zusammenfassend lässt sich sagen: STM ist eine Alternative zu Locks. Mit STM werden
Codeblöcke als *atomar* gekennzeichnet und das Laufzeitsystem
kümmert sich darum, dass so gekennzeichnete Blöcke auch wirklich atomar
ausgeführt werden. Damit lassen sich Race Conditions und Deadlocks viel
einfacher verhindern als dies beim Einsatz von Locks der Fall ist.

Ein weiterer Vorteil von STM gegenüber von Locks, der bisher unerwähnt
geblieben ist: mit STM geschriebene
Komponenten lassen sich sehr gut kombinieren, da sich atomare Blöcke aus
verschiedenen Komponenten einfach zu größeren Blöcken zusammensetzen lassen.
Wenn man also einmal darüber nachgedacht hat, wie die atomaren Blöcke
innerhalb der Komponenten sein sollen, muss man mit STM nur noch darüber
nachdenken, ob und wie es atomare Operationen über Komponentengrenzen
hinaus geben muss.

Mit Locks ist das nicht so einfach. Hier muss man oft bei der Kombination
zweier Komponenten das Locking-Protokoll aufbohren und neu durchdenken, um
die Komponenten zusammenbringen zu können. Eine genauere Erklärung hierzu
findet sich, wie auch das vorhergehende Beispiel, in Simon Peyton-Jones
wunderschönem Artikel
[Beautiful Concurrency](http://research.microsoft.com/en-us/um/people/simonpj/papers/stm/beautiful.pdf)
aus dem Buch *Beautiful Code* (O'Reilly, 2007).

Ich freue mich über Rückmeldungen!
