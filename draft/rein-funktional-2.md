---
layout: post
description: Rein funktionale Programmierung - Teil 2
title: "Eine kleine Einführung in die rein funktionale Programmierung - Teil 2"
author: michael-sperber
tags: ["rein funktional", "Racket", "Schneckenwelt"]
---

Im [einem vorigen Beitrag](2013/02/08/rein-funktional.html) haben wir das Vorhaben begonnen,
eine Schneckenwelt zu implementieren.  Zur Erinnerung:

> \[...\] nehmen wir uns ein konkretes Beispiel vor: Es geht um
> die Simulation und Visualisierung einer Welt (vielleicht für ein
> Videospiel), in der sich *Schnecken* bewegen \[...\].
> Die Schneckenwelt ist zweidimensional, und wir fangen mit sehr sturen
> Schnecken an, die sich stets in die gleiche Richtung bewegen und sich
> nicht davon abhalten lassen.  

> In diesem Posting kümmern wir uns erst einmal um die individuellen
> Schnecken, die wir in einem späteren Posting dann in der Schneckenwelt
> anordnen.  In einem dritten Posting werden wir das Programm so
> erweitern, daß die Schnecken *Schleimspuren* hinterlassen und den
> Schleimspuren anderer Schnecken ("die stinken") ausweichen.  Das ganze
> visualisieren wir dann dergestallt, daß es so aussieht:

<div id="center">
<img src="/files/2013-02-08-rein-funktional/snailworld.gif">
</img>
</div>

<br/>

Wir erweitern [den Code vom letzten
Mal](/files/2013-02-08-rein-funktional/snail.rkt) und ordnen Schnecken
in einer *Schneckenwelt* an:

<!-- more start -->

{% highlight scheme %}
; Eine Schneckenwelt besteht aus:
; - Schnecken
(struct snail-world (snails))

(define sw1 (snail-world (list s1 s2 s3)))
{% endhighlight %}

Das `snails`-Feld der Schneckenwelt besetzen wir mit einer *Liste*
aller Schnecken.  Die eingebaute `list`-Funktion macht aus den
Schnecken `s1`, `s2` und `s3` eine Liste - `sw1` ist dann die
Schneckenwelt daraus.

Was können wir mit so einer Schneckenwelt anfangen?  Zwei Sachen wären
nett:

- die Schneckenwelt grafisch anzeigen
- die Schneckenwelt animieren, so daß die Schnecken sich bewegen

Im [letzten Beitrag](2013/02/08/rein-funktional.html) hatten wir bereits eine einzelne Schnecke
in einer Szene plaziert. Um die ganze Schnecke anzuzeigen,
müssen wir mit einer leeren Szene
anfangen und sukzessive mit `draw-snail` eine Schnecke nach der
nächsten in die Szene plazieren.  Bei jeder Schnecke kommt dabei
wieder eine neue Szene heraus, bis schließlich alle Schnecken in der
Szene untergekommen sind.  Hier ist die Funktion, die das macht:

{% highlight scheme %}
; Schneckenwelt malen
; draw-snail-world: snail-world -> scene
(define draw-snail-world
  (lambda (sw)
    (foldl draw-snail
           (empty-scene width height)
           (snail-world-snails sw))))
{% endhighlight %}

Diese Funktion benutzt die eingebaute [`foldl`-Funktion](http://docs.racket-lang.org/reference/pairs.html#%28def._%28%28lib._racket/private/list..rkt%29._foldl%29%29), eine der
[Eckpfeiler der funktionalen
Programmierung](https://twitter.com/PLT_Borat/status/173024002376339456).
Die
[`foldl`-Funktion](http://docs.racket-lang.org/reference/pairs.html#%28def._%28%28lib._racket/private/list..rkt%29._foldl%29%29)
iteriert über einer Liste und transformiert bei jedem Schritt einen
Zustand unter Zuhilfenahme des jeweils nächsten Listenelement in einen
neuen Zustand.

In diesem Fall beginnt `foldl` mit der leeren Szene, die von
`(empty-scene width height)` erzeugt wird, schnappt sich das erste
Listenelement der Schneckenliste `(snail-world-snails sw)` und wendet
auf beides `draw-snail` an.  Dabei kommt wiederum eine Szene heraus,
in der jetzt die erste Schnecke plaziert ist.  Die `foldl`-Funktion
schnappt sich jetzt die nächste Schnecke, und ruft wieder `draw-snail`
auf, und wieder kommt eine Szene heraus, und so weiter bis die Liste
zu Ende ist.  Am Ende kommt dann eine Szene heraus, in der alle
Schnecken plaziert sind.

Der obige Code benötigt noch Definitionen für die Breite `width` und
die Höhe `height` der Szene:

{% highlight scheme %}
(define width 180)
(define height 150)
{% endhighlight %}

Damit ist die grafische Darstellung fertig; es fehlt noch die
Animation.  Dazu bewegen wir einfach alle Schnecken einer
Schneckenwelt mit Hilfe der schon geschriebenen Funktion `move-snail`:

{% highlight scheme %}
; Schneckenwelt bewegen
; next-snail-world: snail-world -> snail-world
(define next-snail-world
  (lambda (sw)
    (snail-world
     (map move-snail
          (snail-world-snails sw)))))
{% endhighlight %}

Diese Funktion füttert die Schneckenliste der Schneckenwelt in die
eingebaute Funktion
[`map`](http://docs.racket-lang.org/reference/pairs.html?q=map#%28def._%28%28lib._racket%2Fprivate%2Fmap..rkt%29._map%29%29).
Diese wendet eine gegebene Funktion auf jedes Element einer Liste an
und macht aus den Ergebnissen wieder eine Liste.  In diesem Fall
wendet sie die Funktion `move-snail` auf jede Schnecke der
Schneckenwelt an und macht aus den resultierenden bewegten Schnecken
wieder eine Liste; `next-snail-world` macht daraus dann eine neue
Schneckenwelt.

Zu guter letzt können wir das ganze noch animieren.  Dazu benutzen
wir eine weitere Library, die bei Racket dabei ist, nämlich
[`2htdp/universe`](http://docs.racket-lang.org/teachpack/2htdpuniverse.html).
Um sie einzubinden, müssen wir am Anfang noch eine `require`-Form
einfügen:

{% highlight scheme %}
(require 2htdp/universe)
{% endhighlight %}

Für unsere Animation benutzen wir die
[`big-bang`-Form](http://docs.racket-lang.org/teachpack/2htdpuniverse.html?q=big-bang&q=big-bang%23&q=struct&q=place-image&q=circle#%28form._world._%28%28lib._2htdp%2Funiverse..rkt%29._big-bang%29%29):

{% highlight scheme %}
(big-bang sw1
          (on-tick next-snail-world 0.2)
          (to-draw draw-snail-world width height))
{% endhighlight %}

Das Argument `sw1` ist die Anfangs-Schneckenwelt.  Die Animation läßt
nun eine getaktete Uhr laufen, und bei jedem Taktschlag (alle 0.2
Sekunden) wendet sie die Funktion `next-snail-world` auf die
Schneckenwelt an - das besagt die `on-tick`-Klausel.  Nach jedem
Taktschlag wird die aktuelle Schneckenwelt angezeigt durch die
Funktion `draw-snail-world`, was die `to-draw`-Klausel besagt.

Zum Schluß lohnt es sich, die rein funktionale Programmierung noch
einmal in Perspektive zu setzen: Die Funktion `next-snail-world`
bewegt *alle Schnecken gleichzeitig*.  Angenommen,
`move-snail` würde imperativ funktionieren, also die Schnecke in situ
modifizieren.  Beim aktuellen `next-snail-world` würde das keinen
Unterschied machen, da die Schnecken alle unabhängig voneinander
wären.  Was aber, wenn die Schnecken aufeinander achten müßten, also
z.B. anderen Schnecken oder deren Schleimspuren ausweichen müßten?
Dazu mehr in einem zukünftigen Beitrag.

Den Code zu diesem Beitrag können Sie übrigens
[hier](/files/2013-02-15-rein-funktional-2/snailworld.rkt) herunterladen.

<!-- more end -->