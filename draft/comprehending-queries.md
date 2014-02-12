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
Query "*Finde alle dringlichen Bestellungen, deren Status noch offen ist*" in
gewohnter Mengenschreibweise (die Beispiele orientieren sich am
[TPC-H-Benchmark](http://FIXME), den man durchaus als "die Drosophila der
Datenbankforschung" bezeichnen könnte):

    { o | o ∈ orders ∧ o.o_orderpriority = '1-URGENT' ∧ o.o_orderstatus = 'O' }

Die "äquivalente" List-Comprehension in Haskell:

{% highlight haskell %}
[ o | o <- orders, o_orderpriority o == "1-URGENT", o_orderstatus o == 'O' ]
{% endhighlight %}

Syntaktisch kein nennenswerter Unterschied.  Mit semantischer Äquivalenz müssen
wir jedoch etwas vorsichtiger sein: TRC operiert über *Mengen* und kennt daher
weder Duplikate noch eine Ordnung auf den resultierenden Tupeln.  Duplikate und
Ordnung sind aber durchaus relevant für *List*-Comprehensions.  Offenbar ist der
Typ `[a]` und die assozierte Listen-Monade nicht die beste Wahl, um Query-
Comprehensions dieser Art zu interpretieren.

TRC verarbeitet Tupelmengen — das können Haskell und seine Monad Comprehensions ebenfalls.
Den entsprechenden Typ `Set a` und die zugehörige Mengen-Monade importieren wir aus Modul
`Data.Set.Monad`.  Alles andere bleibt wie zuvor:

{% highlight haskell %}
[ o | o <- orders :: Set Order, o_orderpriority o == "1-URGENT", o_orderstatus o == 'O' ] :: Set Order
{% endhighlight %}

(die Typ-Annotationen `:: Set Order`, wobei `Order` einen geeigneten Tupel- oder
Recordtyp für Bestellungen bezeichnen soll, sind optional und hier nur zur
Verdeutlichung angebracht). Diese Set-Comprehension ist jetzt tatsächlich eine
getreue Haskell-Repräsentation des TRC-Ausdrucks.

TRC ist kein akademisches Spielzeug, sondern *die* Grundlage für das Design der
*Intergalactic Data Speak*, nämlich SQL.  Alle Elemente — die Bindung von
Variable `o` an die Tupel aus `orders`, die nachfolgenden Filter und der `head`
der Comprehension (hier: die `SELECT`-Klausel) — finden sich direkt wieder:

{% highlight sql %}
SELECT o
FROM   orders o
WHERE  o.o_orderpriority = '1-URGENT'
AND    o.o_orderstatus = 'O'
{% endhighlight %}

(Ganz genau genommen ist hier aufgrund SQLs Multimengen-Semantik bereits ein
dritter Monad im Spiel, `Bag a`, aber darauf kommen wir unten gleich zurück.)

## Monoid Comprehensions ##

Iteration über Tupelmengen (Tabellen) und die Anwendung von Filtern ist sicher
zentral aber doch nur die halbe Miete, wenn man eine Anfragesprache wie SQL als
Meßlatte anlegt.  Wie steht's mit Unteranfragen \[einfach darstellbar durch geschachtelte
Comprehensions, siehe unten\], Aggregation und Quantifikation wie z.B. SQLs `EXISTS(...)`?

Monaden stellen klasse Abstraktionen für Container (Listen, Mengen) dar, aber **Monoide**
liefern die ideale Repräsentation für Aggregationen, also Operationen, die einen Container
von Werten auf einen einzelnen Wert reduzieren (man denke etwa an `SUM(...)`, `COUNT(*)`
oder `MAX(...)` in SQL).  Ein Monoid *T* = (*z*,⊗) ist eine der einfachsten algebraischen Strukturen,
die lediglich eine zweistellige assoziative Operation ⊗ mit neutralem Element *z* definiert.
In Haskell findet man diese Strukturen in der Typklasse `Monoid` wieder:

{% highlight haskell %}
class Monoid a where
    mempty  :: a             -- z
    mappend :: a -> a -> a   -- ⊗
    mconcat :: [a] -> a      -- reduziert Elemente eines Containers (Liste) auf einen Wert
    mconcat = foldr mappend mempty
{% endhighlight %}

Im Modul `Data.Monoid` finden sich bereits eine Reihe von Monoiden, die im
Kontext von Datenbankanfragen sehr nützlich sind, bspw. die Monoide `Sum` =
(`0`,`+`) und `Any` = (`False`,`||`) mit denen sich die SQL-Aggregatfunktionen
`SUM(...)` und `EXISTS(...)` darstellen lassen (Quantifikation ist eigentlich
nichts anderes als Aggregation über Booleschen Werten).

Wenn wir die beiden Zutaten Monaden und Monoide in der folgenden Form
zusammenbringen, erhalten wir die sogenannten **Monoid Comprehensions**. Als
Notation hat sich <tt>{⋯|⋯}</tt><sup>*T*</sup> durchgesetzt:

1. Zunächst wird eine Monad Comprehension formuliert, die einen Container von Werten liefert, bevor

2. die Werte dieses Containers mittels (der `mconcat`-Funktion des) Monoid *T* reduziert werden.

Einfache Beispiele für Monoid Comprehensions wären etwa

1. <tt>{ o.o_totalprice | o ∈ orders, o.o_orderstatus = 'O' }</tt><sup>`Sum`</sup>, 
   um den Gesamtpreis aller noch offenen Bestellungen zu errechnen, oder 

2. <tt>{ o.o_orderstatus = 'O' | o ∈ orders }</tt><sup>`Any`</sup>, 
   um zu überprüfen, ob überhaupt noch offene Bestellungen vorliegen (diese Query hat den Ergebnistyp `Bool`).

Die Idee der Reduktion von monadischen Containern `m a` in der oben genannten
zweiten Phase lässt sich in Haskell natürlich ebenfalls ganz generell
ausdrücken.  Zentrum des Ganzen ist die Funktion `reduce`, die die
`mconcat`-Funktion des Monoids `t` nutzt, um die Elemente in `m a` zu
aggregieren. (Die Signatur von `reduce` verrät, dass es wir hier tatsächlich
mit einer sog. Monad-Algebra zu tun haben.[^monad-algebra])

{% highlight haskell %}
class Monad m => MonadAlgebra m a where
  reduce :: Monoid t => (a -> t) -> m a ->  t

-- Reduktion von monadischen Containern (hier: Listen und Mengen)
instance MonadAlgebra [] a where
  reduce f = mconcat . map f

instance Ord a => MonadAlgebra Set a where
  reduce f = reduce f . Set.toList

-- Nützliche Monad-Algebren für SQL-artige Queries
distinct :: (Ord a, MonadAlgebra m a) => m a -> Set a
any, all :: MonadAlgebra m Bool => m Bool -> Bool
sum :: (Num a, MonadAlgebra m a) => m a -> a
count :: MonadAlgebra m a => m a -> Int
exists :: MonadAlgebra m a => m a -> Bool

distinct = reduce Set.singleton
any      = getAny . reduce Any
all      = getAll . reduce All
sum      = getSum . reduce Sum
count    = getSum . reduce (Sum . const 1)
exists   = getAny . reduce (Any . const True)
{% endhighlight %}

Wenn wir jetzt noch `(^)` als Postfix-Applikation für Funktionen definieren,

{% highlight haskell %}
(^) :: a -> (a -> b) -> b
(^) = flip ($)
{% endhighlight %}

erhalten wir eine Haskell-Notation, die den Monoid Comprehensions <tt>{⋯|⋯}</tt><sup>*T*</sup>
sehr nahe kommt.  Unsere beiden Beispiele von eben lesen sich jetzt wie folgt:

{% highlight haskell %}
[ o_totalprice o | o <- orders, o_orderstatus o == 'O' ]^sum 

[ o_orderstatus o == 'O' | o <- orders ]^any
{% endhighlight %}

Das sieht schon recht elegant und — für einen Datenbanker — sehr vertraut und vor
allem idiomatisch aus.  Aber Haskell setzt noch einen drauf:

## SQL-Like Comprehensions (Comprehensive Comprehensions) ##

## TPC-H ##

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

[^monad-algebra]: `reduce` definiert eine Monad-Algebra in Kleisli-Darstellung.  Details dazu finden
sich zum Beispiel auf [Jeremy Gibbons Blog](http://patternsinfp.wordpress.com/2012/01/19/comprehensions/).

<!-- more end -->
