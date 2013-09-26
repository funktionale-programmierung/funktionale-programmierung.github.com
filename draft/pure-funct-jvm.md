---
layout: post
description: Rein funktionale JVM Sprache Frege
title: "Titel"
author: ingo-wechsung
tags: ["Java", "Haskell", "Frege", "JVM"]
---

Die Auswahl an Programmiersprachen für die JVM ist riesig. 
Auch für funktionale Programmierer wird einiges geboten, von Scala über verschiedene ML-Dialekte bis hin zu Clojure.
Eine Nische war jedoch bislang nicht besetzt: 
die der Haskell-artigen, also rein funktionalen Sprachen mit Bedarfsauswertung und Typinferenz.
Dabei erfreut sich Haskell außerhalb der JVM-Welt wachsender Beliebtheit.

<!-- more start -->

<!-- Das ist auch die Syntax für Kommentare, die im HTML nachher
auftauchen. -->

## JVM ohne Haskell ##

Eine häufig gestellte Frage im Haskell-Wiki ist folgende: 
[_Warum ist GHC nicht für JVM oder .NET verfügbar?_](http://www.haskell.org/haskellwiki/GHC/FAQ#Why_isn.27t_GHC_available_for_.NET_or_on_the_JVM.3F)

Die Antwort ist recht detailliert, und lautet zusammengefaßt so: 
Es sei ein lohnendes, aber nicht zu unterschätzendes Unterfangen, 
die größten Schwierigkeiten ergäben sich daraus, daß man letztlich die GHC Laufzeitumgebung 
samt aller primitiven Operationen für die jeweiligen Umgebungen neu implementieren und dann auch pflegen müßte.
Diese Herausforderung sei jedoch "mostly broad rather than deep" 
und mit genügend Ressourcen könne man sie durchaus angehen.

Letzteres ist meiner Auffassung nach eine Fehleinschätzung.

Zum einen lauern durchaus schwierige, meines Wissens bisher ungeklärte, technische Probleme,
beispielsweise im Bereich der Nebenläufigkeit: Das Thread-Modell der JVM ist ein ganz anderes
als das der GHC-Umgebung. Man denke an GHC-Features wie _green threads_ und _software transactional memory_. 
Ob so etwas 1:1 in der JVM implementiert werden kann, steht in Frage.





 
Code: `f(a,b)`. Als Block:

    <-- vier Leerzeichen eingerückt

Codebeispiele einer Programmiersprache:

{% highlight scheme %}
(repl)
{% endhighlight %}

Links mit [Text](http://URL).

Hervorhebungen *mit Stern* oder _Unterstrich_.  **Doppelt** für mehr
__Druck__.  Geht auch mitt*endr*in in einem Wort.

<!-- more end -->
