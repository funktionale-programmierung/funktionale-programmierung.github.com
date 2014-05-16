---
layout: post
description: Noch mehr über Software Transactional Memory
title: "Noch mehr über Software Transactional Memory"
author: stefan-wehr
tags: ["Haskell", "STM"]
---

Heute geht's nochmal um Software Transactional Memory (STM), eine schöne
und saubere Möglichkeit, um in Programmen mit Nebenläufigkeit umzugehen.
Hier im Blog gab's ja schon eine
[Einleitung](http://funktionale-programmierung.de/2014/03/28/stm-haskell.html)
zu STM sowie einen
[weiterführenden Artikel](http://funktionale-programmierung.de/2014/04/24/stm-haskell2.html).
Bisher haben wir gesehen, dass STM traditionellen Techniken zum Umgang mit
Nebenläufigkeit (wie z.B. Locks) überlegen ist:

* Atomare Blöcke werden einfach als solche deklariert werden und der
Programmier muss Atomizität nicht explizit durch Locks sicherstellen.
* Mit STM entwickelte Komponenten können einfach zu neuen Komponenten
zusammengebaut werden, was mit Locks oftmals die Überarbeitung des
Locking-Modells nach sich zieht.

Heute soll es nun abschließend um ein mögliches Ausführungsmodell für STM
gehen. Das vorgestellte Modell ist konzeptionell sehr einfach und vom
Prinzip auch so
im [GHC](http://haskell.org/ghc) Compiler für Haskell umgesetzt. Natürlich
können anderen STM-Implementierungen auch anderes Ausführungsmodelle
verwenden, vorausgesetzt die Atomizitätsgarantien werden nicht
verletzt.

<!-- more start -->

Als Beispiel betrachten wir den Code aus dem
[ersten Artikel](http://funktionale-programmierung.de/2014/03/28/stm-haskell.html)
dieser Serie:

{% highlight haskell %}
type Account = TVar Int

transfer :: Account -> Account -> Int -> IO ()
transfer k1 k2 amount =
    atomically (do deposit k2 amount
                   withdraw k1 amount)
deposit :: Account -> Int -> STM ()
deposit k amount =
    do bal <- readTVar k
       writeTVar k (bal + amount)

withdraw :: Account -> Int -> STM ()
withdraw k amount = deposit k (- amount)
{% endhighlight %}

Dieser Code ist eine Lösung für das *Bankkonto-Probleme:* es soll Geld
zwischen zwei Konten übertragen werden, so dass entweder das Geld von einen
Konto weg und auf dem anderen Konto drauf ist, oder dass die Überweisung
als Ganzes fehlschlägt. Diese Eigenschaft soll auch mit nebenläufigen
Threads gelten und andere Threads sollen die Auswirkung einer Überweisung
erst sehen, wenn diese vollständig abgeschlossen ist.

Um das Ausführungsmodell von STM zu erklären, nehmen wir an, dass für
zwei Konten `acc1` und `acc2` zwei Threads eine Überweisung von jeweils 50
EUR von `acc1` auf `acc2` durchführen.

Was passiert jetzt zur Laufzeit? Die Hauptidee ist, dass STM-Transkationen
optimistisch, also auf gut Glück ausgeführt werden. Das
STM-Laufzeitsystem führt also ohne jedes Locking die beiden STM-Transaktionen

    atomically (transfer acc1 acc2 50) -- Thread A

und

    atomically (transfer acc1 acc2 50) -- Thread B

aus. Damit das funktioniert, dürfen Schreiboperationen auf
Transaktionsvariablen natürlich nicht direkt im Hautspeicher
stattfinden. Sonst würden andere Threads ja möglicherweise ungültige
Zwischenzustände sehen.

Stattdessen werden Schreiboperationen in einem *Log* gesammelt. Ganz am
Ende einer STM-Transaktion werden diese Schreiboperationen dann im
Anschluss an eine *Validierungsphase* in den
Hauptspeicher überführt *(Commit)*. In der Validierungsphase wird überprüft, ob die Leseoperationen,
die die STM-Transaktion während ihres Ablaufs durchgeführt hat, noch mit
dem aktuellen Hauptspeicherinhalt konsistent sind. Zu dieser Überprüfung
werden im Log auch die Leseoperationen protokolliert. Falls Inkonsistenzen
festgestellt sind wird die STM-Transaktion abgebrochen *(Rollback)* und zu einem
späteren Zeitpunkt erneut ausgeführt.

Schauen wir uns das mal am Beispiel an.

![bild](/files/stm-haskell/stm-000.png)

Die beiden grünen Boxen symbolisieren den Hauptspeicherinhalt der beiden
Konten. Wir nehmen an, dass zunächst Thread A mit der Transaktion
beginnt. Im folgenden Bildchen ist dargestellt, was in den
ersten vier Schritten der Transaktion passiert.

![bild](/files/stm-haskell/stm-004.png)

Im Log werden also zwei Dinge vermerkt:

* Welcher Wert wurde in welche Variable geschrieben?
* Welcher Wert wurde aus welcher Variable gelesen.

Sie sehen auch, dass der Wert von `acc1` im Hauptspeicher nicht verändert
wird.

Im nächsten Schritt tritt nun Thread B in Aktion und führt seine
Transaktion aus. Die vermerkten Aktion im Log sind analog zu Thread A.
Danach ist wieder Thread A am Zug. Da die
Transaktion von Thread A am Ende angekommen ist, beginnt nun die
Validierungsphase.

![bild](/files/stm-haskell/stm-009.png)


In der Validierungsphase wird überprüft, ob der Inhalt des Hauptspeichers
konsistent mit den im Log protokollierten Leseoperationen ist. Für Thread A
ist dies der Fall, daher folgt jetzt das Commit der Transaktion, die
Schreiboperationen aus dem Log werden also in den Hauptspeicher geschrieben.

![bild](/files/stm-haskell/stm-010.png)

So, jetzt ist wieder Thread B am Zug. Hier schlägt die Validierung fehl,
denn im Log steht, dass aus `acc2` der Wert 50 gelesen wurde, der aktuelle
Inhalt im Speicher ist aber 100.

![bild](/files/stm-haskell/stm-011.png)

Also kommt es jetzt zum Rollback.

![bild](/files/stm-haskell/stm-012.png)

Dieses Mal gibt es keine weiteren Threads mehr, also läuft die Transaktion
durch und wird schließlich in den Hauptspeicher übernommen.

![bild](/files/stm-haskell/stm-017.png)

So, das war's nicht nur für heute, sondern auch für die Artikel-Serie über
STM. Ich hoffe, es war für Sie viel Interessantes dabei. Ich freue mich
über Fragen und Feedback.
