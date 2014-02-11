---
layout: post
description: "Monoid-Comprehensions und Anfragesprachen für Datenbanksysteme"
title: "Comprehending Queries"
author: torsten-grust
tags: ["Datenbankanfragen", "SQL", "comprehensions", "Monoid", "Monaden", "Haskell"]
---

Mit dem *Haskell 98 Report* kam im Dezember 2002 ein langwieriger
Standardisierungsprozess für die funktionale Programmiersprache Haskell endlich
zu einem positiven Ende.  Das waren eigentlich *good news for everyone*, aber
ich erinnere mich, dass ich damals grummelte: eines der aus meiner Sicht
zentralen Features der Sprache, die **Monad Comprehensions**, war im Laufe des
Prozesses aus dem Haskell-Sprachstandard entfernt worden.  Vor allem dem
Datenbanker in mir tat das weh: ausdrucksstarke Comprehensions sind nämlich eine
ganz hervorragende Repräsentation für Datenbankanfragen.  Vor dem *Haskell 98
Report* war Haskell mein Prototyping-Werkzeug für die Formulierung,
Transformation und Optimierung von Queries gewesen — und jetzt?

<!-- more start -->

## Bring Back Monad Comprehensions ##

Aber: niemals aufgeben!  Etwa ab 2009 oder 2010 begann ich, während unserer
Arbeit hier am Lehrstuhl für Datenbanksysteme der Universität Tübingen, mein
T-Shirt mit dem Aufdruck "**Bring Back Monad Comprehensions**" zu tragen. Das
zeigte bei einigen der wissenschaftlichen Mitarbeiter offenbar Wirkung: wir
beschlossen an einer Extension für den Haskell-Compiler GHC zu arbeiten, um die
Monad Comprehensions wenigstens als Spracherweiterung wieder in Haskell
zu etablieren.  Seit GHC Version 7.2 ist diese Erweiterung (GHC-Pragma
`MonadComprehensions`) allgemein verfügbar.[^tshirt] 

## Comprehending Queries ##

Wenn man die Augen etwas zukneift, ist zwischen Comprehensions und einem 
grundlegenden Formalismus — dem Tupel-Relationen-Kalkül (kurz TRC: *tuple 
relational calculus*) — dessen sich die Datenbanker zur Darstellung von Anfragen 
bedienen, eigentlich kein Unterschied zu erkennen.  Etwa formuliert der TRC die
Query "*Finde alle dringlichen Bestellungen, deren Status noch offen ist*"
in gewohnter Mengenschreibweise:

    { o | o ∈ orders ∧ o.orderpriority = '1-URGENT' ∧ o.orderstatus = 'O' }

Die "äquivalente" List-Comprehension in Haskell:

{% highlight haskell %}
[ o | o <- orders, orderpriority o == "1-URGENT", orderstatus o == 'O' ]
{% endhighlight %}

Syntaktisch kein nennenswerter Unterschied.  Mit semantischer Äquivalenz müssen
wir etwas vorsichtiger sein: TRC operiert über *Mengen* und kennt daher weder
Duplikate noch eine Ordnung auf den resultierenden Tupeln.  Duplikate und
Ordnung sind aber durchaus relevant für *List*-Comprehensions.  Offenbar ist der
Typ `[a]` und die assozierte Listen-Monade nicht die beste Wahl, um 
Query-Comprehensions dieser Art zu interpretieren.

TRC verarbeitet Tupelmengen — das können Haskell und seine Monad Comprehensions ebenfalls.
Den entsprechenden Typ `Set a` und die zugehörige Mengen-Monade importieren wir aus Modul
`Data.Set.Monad`.  Alles andere bleibt wie zuvor:

{% highlight haskell %}
[ o | o <- orders :: Set Order, orderpriority o == "1-URGENT", orderstatus o == 'O' ] :: Set Order
{% endhighlight %}

(die Typ-Annotationen `:: Set Order`, wobei `Order` einen geeigneten Tupel- oder Recordtyp
für Bestellungen bezeichnen soll, sind optional und hier nur zur Verdeutlichung angebracht).
Diese Set-Comprehension ist jetzt tatsächlich eine getreue Haskell-Repräsentation der TRC-Query.

TRC ist kein akademisches Spielzeug, sondern *die* Grundlage für das Design der *Intergalactic
Data Speak*, nämlich SQL.  Alle syntaktischen Elemente — die Bindung von Variable `o` an die
Tupel aus `orders`, die nachfolgenden Prädikate und die `SELECT`-Klausel (quasi der *head* der
Comprehension) — finden sich direkt wieder:

{% highlight sql %}
SELECT o
FROM   orders o
WHERE  o.orderpriority = '1-URGENT'
AND    o.orderstatus = 'O'
{% endhighlight %}

(Ganz genau genommen ist hier aufgrund SQLs Multimengen-Semantik bereits ein dritter Monad 
im Spiel, `Bag a`, aber darauf kommen wir unten gleich zurück.)

## Monoid Comprehensions ##

## SQL-Like Comprehensions (Comprehensive Comprehensions) ##

## TPC-H ##

TPC-H ist die "Drosophila der Datenbanker"

{% highlight sql %}
SELECT o_orderpriority, COUNT(*) as order_count
FROM   orders
WHERE  o_orderdate >= '1995-01-01'
AND    o_orderdate <  '1995-07-01'
AND    EXISTS (SELECT *
                FROM   lineitem
                WHERE  l_orderkey = o_orderkey
                AND    l_commitdate < l_receiptdate)
GROUP BY o_orderpriority
ORDER BY o_orderpriority
{% endhighlight %}

<!-- write ~2000 words -->

Links mit [Text](http://URL).

[^tshirt]: Da sag' noch einmal jemand, Informatikern wäre es egal, welche 
Klamotten sie tragen. Don Stewart meinte dazu auf Twitter: <blockquote class="twitter-tweet" align="center" lang="en"><p>TIL that the &quot;bring back monad comprehensions&quot; tshirt project from mid 00s <a href="http://t.co/0jFbPnqQaJ">http://t.co/0jFbPnqQaJ</a> inspired <a href="http://t.co/ZIAGqW3ycH">http://t.co/ZIAGqW3ycH</a></p>&mdash; Don Stewart (@donsbot) <a href="https://twitter.com/donsbot/statuses/377546421312188416">September 10, 2013</a></blockquote><blockquote class="twitter-tweet" align="center" lang="en"><p>/2 first case of tshirt driven development ??</p>&mdash; Don Stewart (@donsbot) <a href="https://twitter.com/donsbot/statuses/377546537385332736">September 10, 2013</a></blockquote>

<!-- more end -->
