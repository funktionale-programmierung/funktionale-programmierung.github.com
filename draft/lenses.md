---
layout: post
description: Funktionale Linsen
title: "Funktionale Linsen"
author: david-frese
tags: ["Clojure", "ClojureScript", "Linsen"]
---

Linsen sind eine funktionale Abstraktion, die sich für uns schon in
mehreren Projekten als sehr nützlich erwiesen haben. Mit ihnen kann man
sehr gut komplexe Eigenschaften größerer Datenstrukturen definieren,
solche Eigenschaften abfragen und insbesondere ändern. Dieser Artikel
soll zeigen was Linsen sind, und wie man sie dafür verwenden kann. Die
verwendete Programmiersprache ist [Clojure](http://clojure.org/), in
der wir zur Zeit sehr viel und gerne programmieren.

<!-- more start -->

## Motivation

Als motivierendes Beispiel stellen wir uns vor, wir hätten ein
einfaches Telefonbuch als eine Datenstruktur folgender Art vorliegen:

{% highlight clojure %}
(def book-1
  {"Mike" #{[:work "071170709468"] [:home "07071xxx"]}
   "David" #{[:home "07121xxx"]}})
{% endhighlight %}

Das Telefonbuch ist also eine Map mit Namen als Schlüssel, und einem
Set von Einträgen als Wert. Jeder Eintrag besteht aus einem Tupel aus
der Art des Eintrags und einem String mit der Telefonnummer selbst.

Als Aufgabe stellen wir uns zwei Funktionen: Eine schaut nach, ob zu
einem Namen ein bestimmter Eintrag vorhanden ist, die Andere fügt
einen Eintrag hinzu. Dabei sollte es keine Rolle spielen ob ein Name
überhaupt schon im Telefonbuch vorhanden ist oder nicht:

{% highlight clojure %}
(defn has-entry? [book name kind number] ...)
(defn add-entry [book name kind number] ...)
{% endhighlight %}

## Was sind Linsen

Zunächst einmal kommt das Wort vom englischen *Lens*, es sind also
nicht die Linsen zum Essen gemeint, sondern die zum Durchgucken. Und
diese Analogie ist recht treffend: man hält eine Linse vor etwas
Großes, und sieht einen kleineren Teil davon. Vom Programmieren her
geht es also erst einmal darum, dass man mithilfe einer Linse einen
Wert aus einer Datenstruktur *herausziehen* kann. In Clojure könnte
man das so definieren:

{% highlight clojure %}
(defprotocol Lens
  (yank [this data]))
{% endhighlight %}

Dies definiert Linsen als ein Protokoll, das von verschiedenen Typen
implementiert werden kann, indem man eine Funktion `yank` mit einem
weiteren Parameter `data` über Werte diesen Typs definiert.

Das allein wäre aber natürlich noch nicht der Rede wert. Entscheind
ist, dass eine Linse ausserdem die Möglichkeit bietet, den Wert, den
sie *fokussiert*, zu modifizieren! Modifizieren heißt in der
funktionalen Programmierung natürlich, eine neue Datenstruktur zu
erstellen, die an der fokussierten Stelle einen neuen Wert enthält. Es
kommt also noch eine Funktion `shove`, zum *Einschieben* eines neuen
Wert dazu:

{% highlight clojure %}
(defprotocol Lens
  (yank [this data])
  (shove [this data v]))
{% endhighlight %}

Eine Möglichkeit konkrete Linsen zu erzeugen ist nun, die beiden
Funktionen `yank` und `shove` explizit zu definieren:

{% highlight clojure %}
(defrecord ExplicitLens
  [yanker shover]
  Lens
  (yank [this data] (yanker data))
  (shove [this data v] (shover data v)))

(defn lens
  [yanker shover]
  (ExplicitLens. yanker shover))
{% endhighlight %}

## Anwendung

Die richtigen Eigenschaften als Linsen zu definieren, ist manchmal gar
nicht so einfach. Die erste die wir hier brauchen werden, ist der Wert
der in einer Map zu einem bestimmten Schlüssel hinterlegt ist. Dazu
schreiben wir eine Funktion `member`, die einen Schlüssel und einen
Default-Wert nimmt, und eine Linse erzeugt, die, über eine konkrete
Map gehalten, den zugehörigen Wert *fokussiert*:

{% highlight clojure %}
(defn member
  [key & [default]]
  (lens #(get % key default)
        #(if (= %2 default)
           (dissoc %1 key)
           (assoc %1 key %2))))
{% endhighlight %}

Die Funktion `yank` der `member`-Linse gibt den zum Schlüssel
passenden Wert zurück (oder den Default-Wert, falls der Schlüssel
nicht in der Map ist); die Funktion `shove` ändert den Wert zu einem
Schlüssel oder entfernt Schlüssel und Wert aus der Map, wenn wir den
Default-Wert übergeben.

Damit können wir die erste interessante Eigenschaft eines
Telefonbuchs als Linse definieren, nämlich das Set der Einträge zu
einem Namen:

{% highlight clojure %}
(defn book-entries
  [name]
  (member name #{}))
{% endhighlight %}

Wie gesagt, können wir mit einer Linse diese Eigenschaft lesen und
setzen. Ein Beispiel:

{% highlight clojure %}
(def my-entries (book-entries "David"))

(yank my-entries book-1)
;; => #{[:home "07121xxx"]}

(shove my-entries book-1 #{})
;; => {"Mike" #{[:work "071170709468"] [:home "07071xxx"]}}
{% endhighlight %}

Dadurch, dass `member` einen Map-Eintrag komplett entfernt, der dem
Default-Wert entspricht, enthält das neue Telefonbuch, das der letzte 
Ausdruck erzeugt, keinen Schlüssel `"David"` mehr.

Wir müssen jetzt ausserdem noch in das Set der Einträge *einsteigen*.
Dazu sind Linsen folgender Art hilfreich:

{% highlight clojure %}
(defn contains
  [v]
  (lens #(contains? % v)
        #(if %2
           (conj %1 v)
           (disj %1 v))))
{% endhighlight %}

Die Funktion `contains` nimmt einen Wert und gibt eine Linse zurück,
die über der boolschen Eigenschaft *fokussiert*.  Die `yank`-Funktion
dieser Liste prüft, ob dieser Wert in einem Set enthalten ist oder
nicht; die `shove`-Funktion ergänzt oder löscht einen Wert, abhängig
vom zweiten Argument.

Für unsere Telefonbuch-Einträge könnten wir also zunächst definieren:

{% highlight clojure %}
(defn entries-contains [kind number]
  (contains [kind number]))
{% endhighlight %}

Und so können wir das direkt auf den Sets verwenden:

{% highlight clojure %}
(def contains-my-work-number
  (entries-contains :work "071170709475"))

(yank contains-my-work-number #{[:home "07121xxx"]})
;; => false

(shove contains-my-work-number #{[:home "07121xxx"]} true)
;; => #{[:home "07121xxx"] [:work "071170709475"]}
{% endhighlight %}

Jetzt wollen wir noch die Linsen für die
Map-Einträge und die für die Sets kombinieren. In diesem
Fall wollen wir sie *aneinander hängen*, oder *übereinander legen*, um
im Bild zu bleiben. Die kombinierte Linse sollte beim Lesen erst die
`yank`-Funktion der Linse für einen Map-Eintrag anwenden, und dann auf
dem resultieren Set die `yank`-Funktion einer Linse für den
Set-Eintrag anwenden. Beim Schreiben, der `shove`-Funktion sollte es
entsprechend andersherum passieren. Wenn wir einmal Wunschdenken
anwenden brauchen wir einen Kombinator, nennen wir ihn `>>`, der
folgendes kann:

{% highlight clojure %}
(def contains-my-work-number-for-me
  (>> my-entries contains-my-work-number))

(yank contains-my-work-number-for-me book-1)
;; => false

(shove contains-my-work-number-for-me book-1 true)
;; => {"Mike" #{[:work "071170709468"] [:home "07071xxx"]}
;;     "David" #{[:home "07121xxx"] [:work "071170709475"]}
{% endhighlight %}

Und tatsächlich ist es auch gar nicht so schwer, diesen überaus
nützlichen Kombinator zu definieren:

{% highlight clojure %}
(defn >>
  [l1 l2]
  (lens (fn [data] (yank l2 (yank l1 data)))
        (fn [data v] (shove l1 data (shove l2 (yank l1 data) v)))))
{% endhighlight %}

(Die Erweiterung auf mehr als zwei Linsen ist auch nicht schwer.)

## Lösung

Kommen wir zum Schluss nun zu den beiden Funktionen auf
Telefonbüchern, die wir uns zu Beginn als Aufgabe gestellt haben:

{% highlight clojure %}
(defn has-entry? [book name kind number] ...)
(defn add-entry [book name kind number] ...)
{% endhighlight %}

Wenn wir eine Linse für das Vorhandensein eines Eintrags definieren:
{% highlight clojure %}
(defn book-contains [name kind number]
  (>> (book-entries name)
      (entries-contains kind number)))
{% endhighlight %}

Dann sind die beiden Funktionen einfach die Anwendung dieser Linse:
{% highlight clojure %}
(defn has-entry? [book name kind number]
  (yank (book-contains name kind number) book))

(defn add-entry [book name kind number]
  (shove (book-contains name kind number) book true))
{% endhighlight %}

Wie man jetzt noch ein `remove-entry` definieren könnte ist sicherlich naheliegend.

## Regeln

Nicht alles, was das dem obigen `Lens`-Protokoll entspricht, sollte man
als Linse betrachten. Folgende Regeln, oder *Gesetze*, machen Linsen
erst sinnvoll:

1. Man zieht immer das raus was man rein gesteckt hat:

    `(yank l (shove l d v))` == `v`

2. Reinstecken, was man raus gezogen hat, ändert nichts:

    `(shove l d (yank l d))` == `d`

3. Zweimal reinstecken ist das gleiche wie einmal:

    `(shove l (shove l d v) v)` == `(shove l d v)`

Die Linsen aus diesem Eintrag erfüllen alle Gesetze.

## Zusammenfassung

Mit Linsen lassen sich modifizierbare Eigenschaften von
Datenstrukturen sehr präzise und mit minimaler Redundanz definieren.

Sehr praktisch ist es zum Beispiel zusammen mit unserer
Webclient-Bibliothek [reacl](https://github.com/active-group/reacl),
wie bereits in einem [früheren Artikel]({% post_url 2014-07-07-reacl %})
erwähnt. Aber wir haben Linsen auch erfolgreich in
[Xtend](http://www.eclipse.org/xtend/) auf einer mutierbaren
Java-Datenstruktur eingesetzt.
