---
layout: post
description: Mehr über Software Transactional Memory
title: "Mehr über Software Transactional Memory"
author: stefan-wehr
tags: ["Nebenläufigkeit", "Haskell"]
---

Vor kurzem war
[Nebenläufigkeit mit Software Transactional Memory](/2014/03/28/stm-haskell.html)
das Thema hier im Blog. Da Nebenläufigkeit auch in modernen Softwaresystemen
immer noch häufig ein schwieriges Problem und eine Quelle vieler Fehler
ist, schauen wir uns heute Software Transactional Memory (kurz: STM)
nochmal genauer an. Funktionale Sprache wie
z.B. [Haskell](http://haskell.org), [Scala](http://www.scala-lang.org/)
oder [clojure](http://clojure.org/) bieten gute Unterstützung für STM,
denn der deklarative Programmierstil und der weitgehende Verzicht auf
Seiteneffekte passen gut zum STM-Paradigma.

Wir haben in dem ersten Artikel gesehen, dass STM eine Alternative zu
Locks darstellt, die einfacher zu benutzen ist und bei der Probleme wie
Deadlocks oder Race Conditions typischerweise nicht oder nur sehr selten
auftreten. Meine tägliche Erfahrung mit STM in unserem Softwareprodukt
[CheckpadMED](/2013/07/17/medizin-funktional.html) bestätigt mich immer
wieder darin, dass Nebenläufigkeit mit STM eigentlich (relativ) einfach
ist: man muss lediglich spezifizieren, welche Codeblöcke atomar laufen
sollen, und das Laufzeitsystem stellt dann diese Atomizitätseigentschaft
sicher.

Im heutigen Artikel untersuchen wir STM etwas genauer. Wir gehen
dabei der Behauptung nach, die wir im ersten Artikel aufgestellt haben,
nämlich dass auch Gründe der Softwarearchitektur für STM sprechen:
während zwei mit Locks implementierte Programmkomponenten oftmals nicht
ohne Änderung des Locking-Protokolls zu einer neuen Komponenten
zusammengebaut werden können, klappt ein solcher Zusammenbau mit STM meist
problemlos,
ohne dass man sich über Implementierungsdetails der Komponenten Gedanken
machen muss.

<!-- more start -->

Wenn sich zwei
Komponente problemlos zu einer größeren Komponente zusammenbauen lassen,
spricht man häufig auch von guter *Komponierbarkeit* der Komponenten. Mit
STM entwickelte Komponenten sind also gut komponierbar, während das für
Komponenten, die Nebenläufigkeit mit Locks kontrollieren, nur bedingt
gilt.

Schauen wir uns dazu ein Beispiel an. Im
[ersten Artikel über STM](/2014/03/28/stm-haskell.html)
haben wir ein Standarbeispiel für nebenläufiges Programmieren
implementiert: es soll Geld von einem Konto `k1` auf ein anderes Konto
`k2` überwiesen
werden und zwar so, dass kein Geld verloren geht:  wenn wir einen
Betrag `N` von Konto `k1` auf Konto `k2` übertragen, soll entweder der
Betrag von `k1` abgebucht und auf `k2` aufgebucht werden, oder beide
Konten sollen unverändert bleiben.

Dazu haben wir zwei Hilfsfunktionen `deposit` und `withdraw` geschrieben:

{% highlight haskell %}
deposit :: Account -> Int -> STM ()
deposit k amount =
    do bal <- readTVar k
       writeTVar k (bal + amount)

withdraw :: Account -> Int -> STM ()
withdraw k amount = deposit k (- amount)
{% endhighlight %}

Beide Funktionen haben als Ergebnistyp `STM ()`. Das bedeutet, dass beide
Funktionen in einer `STM`-Transaktion laufen müssen. Die Funktion
`transfer`, die die eigentliche Überweisung durchführt, fasst dann
`deposit` und `withdraw` zu einer Transaktion zusammen. Dies geschieht
durch die Benutzung von `atomically`. Die Funktionen `deposit` und `withdraw` werden also
*atomar* ausgeführt:

{% highlight haskell %}
transfer :: Account -> Account -> Int -> IO ()
transfer k1 k2 amount =
    atomically (do deposit k2 amount
                   withdraw k1 amount)
{% endhighlight %}

Durch das `atomically` wird der übergebene Codeblock als atomar
gekennzeichnet und das STM-Laufzeitsystem sorgt dafür, dass der Block
entweder ganz oder gar nicht ausgeführt wird. Außerdem sind Änderungen
an Transaktionsvariablen erst nach Abschluss des `atomically`-Blocks
für andere Threads sichtbar.

Wir möchten jetzt die beiden "Komponenten" `deposit` und `withdraw`
benutzen, um eine neue Funktionalität umzusetzen: der Betrag `2 * N`
soll von einem Konto abgebucht und zu gleichen Teilen auf zwei andere
Konten übertragen werden.

Mit `STM` geht das sehr einfach, wir müssen lediglich den Aufruf von
`withdraw` und die beiden Aufrufe von `deposit` zu einem atomaren
Block zusammenfassen:

{% highlight haskell %}
splitTransfer :: Account -> Account -> Account -> Int -> IO ()
splitTransfer k1 k2 k3 amount =
    atomically (do withdraw k1 (2 * amount)
                   deposit k2 amount
                   deposit k3 amount)
{% endhighlight %}

Um zu sehen, dass ein solches Komponieren von vorhandenen Komponenten mit
Locks häufig schwieriger ist, implementieren wir jetzt das Kontobeispiel
mittels Locks in Java.

Zunächst einmal die Modellierung eines Kontos:

{% highlight java %}
class Account {
    private int m_amount;
    private OrderedLock m_lock; // wird später benötigt
    public Account() {
        m_lock = new OrderedLock();
    }
    public int getAmount() {
        return m_amount;
    }
    public synchronized void deposit(int i) {
        m_amount += i;
    }
    public void withdraw(int i) {
        deposit(-i);
    }
    public OrderedLock getLock() {
        return m_lock;
    }
}
{% endhighlight %}


Nun zur `transfer`-Funktion. Folgende Implementierung tut's nicht, denn
der Geldbetrag ist zwischenzeitlich von einem Konto abgebucht und vom
anderen nicht, und dieser Zwischenzustand ist für andere Threads sichtbar.

{% highlight java %}
    void transferWrong(Account k1, Account k2, int amount) {
        k1.withdraw(amount);
        k2.deposit(amount);
    }
{% endhighlight %}

Wir brauchen also zusätzliche Locks auf den Konten. Dazu machen wir von
der Methode `getLock()` Gebrauch, die wir schon vorher in der
`Account`-Klasse definiert haben. Diese Methode liefert eine Objekt
vom Typ `OrderedLock` zurück, welches eine `acquire()` und eine `release`
Methode besitzt. (Den vollständigen Java-Code finden Sie [hier](/files/stm-haskell/AccountMain.java).)

{% highlight java %}
    void transferWrong2(Account k1, Account k2, int amount) {
        k1.getLock().acquire();
        k2.getLock().acquire();
        k1.withdraw(amount);
        k2.deposit(amount);
        k2.getLock().release();
        k1.getLock().release();
    }
{% endhighlight %}

Halt! Dieser Code kann furchtbar Deadlocks verursachen: wenn z.B. ein Thread
`transferWrong2(x, y, 50)` und ein anderer Thread `transferWrong2(y, x, 100)` aufruft, 
kann es passieren dass der erste Thread das Lock von `x`
hält und auf das Lock von `y` wartet und der zweite Thread das Lock von
`y` hält und auf das von `x` wartet: ein klassisches Deadlock.

Um solche Deadlocks zu vermeiden, unterwirft man die Locks in einem
Programm typischerweise einer willkürlichen Ordnung und hält diese Ordnung
immer ein, wenn man auf mehreren Locks `acquire` aufrufen möchte.
Dazu haben wir der Klasse `OrderedLock` eine Instanzvariable `m_order` spendiert, die beim
Erstellen einer neuen Instanz inkrementiert wird. Damit kann man dann die Methoden
`acquireLocks` und `releaseLocks` implementieren, welche zwei Locks in der richtigen
Reihenfolge besetzen bzw. freigeben. Hier der Code von `OrderedLock`:

{% highlight java %}
class OrderedLock {
    private static int s_order;
    private static synchronized int getAndIncOrder() {
        int x = s_order;
        s_order++;
        return x;
    }
    private Lock m_lock;
    private int m_order;
    public OrderedLock() {
        m_lock = new ReentrantLock();
        m_order = getAndIncOrder();
    }
    public int getOrder() {
        return m_order;
    }
    public void acquire() {
        m_lock.lock();
    }
    public void release() {
        m_lock.unlock();
    }
    public static void acquireLocks(OrderedLock l1, OrderedLock l2) {
        if (l1.getOrder() < l2.getOrder()) {
            l1.acquire();
            l2.acquire();
        } else if (l1.getOrder() > l2.getOrder()) {
            l2.acquire();
            l1.acquire();
        }
    }
    public static void releaseLocks(OrderedLock l1, OrderedLock l2) {
        if (l1.getOrder() < l2.getOrder()) {
            l2.release();
            l1.release();
        } else if (l1.getOrder() > l2.getOrder()) {
            l1.release();
            l2.release();
        }
    }
}
{% endhighlight %}

Jetzt können wir `transfer` korrekt implementieren:

{% highlight java %}
    public static void transfer(Account k1, Account k2, int amount) {
        try {
            OrderedLock.acquireLocks(k1.getLock(), k2.getLock());
            k1.withdraw(amount);
            k2.deposit(amount);
        } finally {
            OrderedLock.releaseLocks(k1.getLock(), k2.getLock());
        }
    }
{% endhighlight %}

So, nun aber zur Implementierung von `splitTransfer`, was ja ein Geldbetrag
zwischen drei Konten verteilen soll. Um hier
korrektes Verhalten sicherzustellen, nützt uns das Locking-Protokoll
von `transfer` nichts, denn hier sind nur zwei Konten
beteiligt. Stattdessen müssen wir ein neues Locking-Protokoll
entwerfen. Dazu spendieren wir `OrderedLock` zwei neue statische Methoden
`acquireLocks3` und `releaseLocks3`. Wir verzichten hier auf das Abdrucken
dieser Methode. Stattdessen gleich der Code von `splitTransfer`.

{% highlight java %}
    public static void splitTransfer(Account k1, Account k2, Account k3, int amount) {
        try {
            OrderedLock.acquireLocks(k1.getLock(), k2.getLock(), k3.getLock());
            k1.withdraw(2 * amount);
            k2.deposit(amount);
            k3.deposit(amount);
        } finally {
            OrderedLock.releaseLocks(k1.getLock(), k2.getLock(), k3.getLock());
        }
    }
{% endhighlight %}

Man sieht schon an diesem kleinen Beispiel, dass man sich mit Locks
beim Komponieren von Komponenten immer wieder auf's Neue Gedanken
über das Locking-Protokoll machen muss. In unserem Beispiel hier
konnten wir zum Glück `withdraw` und `deposit` unverändert
weiterverwenden,
aber bei größeren Komponenten
ist es auch häufig nötig, dass man eigentliche interne Details
einer Komponenten öffentlich machen muss, um ein
zusammengesetztes Locking-Protokoll implementieren zu können.

Mit `STM` muss man lediglich darauf achten, dass die öffentlichen
Funktionen einer Komponente Ergebnistypen der Form `STM ...` haben.
Dann lassen sich solche Funktionen aus verschiedenen Komponenten
sehr einfach zu neuen Transaktionen zusammenbauen. Klar, man muss immer
noch überlegen, welche Funktionalitäten Teil einer Transaktion sein müssen
bzw. sollen. Man muss sich aber, anders als mit Locks, keine Gedanken
machen wie man die durch Transaktionen erreichte Atomizität sicherstellt,
das erledigt das STM-Laufzeitsystem.

So, das war's für heute. Wir werden das nächste Mal die kleine Serie über STM mit
der Vorstellung eines möglichen Ausführungsmodells beenden.

Ich freue mich über Rückmeldungen!
