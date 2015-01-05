---
layout: post
description: Haskell für Einsteiger, Teil 4
title: "Haskell für Einsteiger, Teil 4"
author: stefan-wehr
tags: ["Haskell"]
---

Mit dem heutigen Blogartikel geht nach etwas längerer Pause die Serie
"Haskell für Einsteiger" weiter.
Die Serie richtet sich an Leser mit Programmiererfahrung, die
Lust auf Haskell haben, bisher aber den Einstieg in die Sprache nicht richtig
geschafft haben. Im [ersten Teil](/2014/07/25/haskell-einstieg.html) ging
es um eine abgespeckte Variante des Unix-Tools `tail` und im
[zweiten Teil](/2014/09/18/haskell-einstieg-2.html) haben wir ein
Programm zur Analyse von Textdateien mit verschiedenen Encodings
geschrieben. Dann haben wir im
[dritten Teil](/2014/10/23/haskell-einstieg-3.html)
einen Pretty-Printer für [JSON](http://json.org) entwickelt.
Der heutige vierte Teil ist relativ kurz, behandelt aber ein wichtiges
Thema, nämlich den Umgang mit Strings in Haskell.

Übrigens: auf der [BOB 2015](http://bobkonf.de/2015/) gibt es auch
ein
[Haskell-Tutorial für Einstieger](http://bobkonf.de/2015/fischmann.html).

<!-- more start -->

String-Verarbeitung in Haskell ist eigentlich elegant und einfach, den der
Datentyp `String` in Haskell ist lediglich eine Abkürzung für den
Typ `[Char]`. D.h. ein String ist eine Liste von Unicodezeichen, und damit
stehen uns alle
[Listenfunktionen](http://hackage.haskell.org/package/base-4.7.0.1/docs/Data-List.html)
aus Haskells Standardbibliothek zur Verfügung um Strings zu bearbeiten.

Ich habe oben "eigentlich" geschrieben, denn in der Praxis wird der
`String` Typ immer seltener verwendet. Die Benutzung von Listen als
Strings ist zwar elegant, aber auch ziemlich inneffizient. Schließlich ist
eine Liste in Haskell als eine einfach verkettete Liste implementiert,
was zur Folge hat, dass ein String der Länge `N` ungefähr `5 * N` Wörter
im Speicher benötigt. Daher gibt es eine Reihe von Alternativen im
Haskell-Ökosystem, die beiden wichtigsten sind dabei:

* [Data.Text](https://hackage.haskell.org/package/text-1.2.0.3) eine
  effiziente Kodierung von Strings, basierend auf einer internen UTF-16
  Kodierung.
* [Data.ByteString](https://hackage.haskell.org/package/bytestring-0.10.4.1). Streng
  genommen ist das keine wirkliche Alternative zu Strings, denn
  mit `Data.ByteString` werden rohe Bytearrays repräsentiert, also ohne
  Encoding. In manchen Situation ist dies aber besser als die Bytes zu
  dekodieren.

Damit Sie ein bißchen mit Strings vertraut werden, möchte ich als
Beispiel eine Funktion zum Parsen eines sehr einfachen
Konfigurationsformat verwenden.
Um loszulegen benötigen Sie
lediglich eine Installation der
[Haskell Platform](https://www.haskell.org/platform), sowohl die neue
Version 2014.2.0.0 als auch die vorige Version 2013.2.0.0
wird unterstützt. Außerdem brauchen Sie ein Checkout
des
[git-Repositories](https://github.com/funktionale-programmierung/haskell-for-beginners.git)
zu dieser Artikelserie.

In diesem Posting versuche ich, die Funktionsweise des Codes möglichst
verständlich zu erläutern. Allerdings
würde es den Rahmen dieses Blogs sprengen, auf jedes Detail
einzugehen. Hierzu sei das Studium des einen oder
anderen [Haskell-Tutorials](http://learnyouahaskell.com/chapters) oder
[-Buchs](http://www.realworldhaskell.org/) empfohlen. Natürlich können Sie
Rückfragen auch als Kommentar zu diesem Artikel stellen.

Jetzt aber zum Parser für das einfache Konfigurationsformat. Wir möchten
eine Konfigurationsdatei parsen, in der Schlüssel-Wert-Paare stehen. In
jeder Zeile steht ein solches Paar, der Wert ist vom Schlüssel durch `:`
separiert. Zeilen, die mit `#` sind Kommentarzeilen. Hier ein Beispiel:

~~~
# Dies ist ein Kommentare
foo: bar
empty:

invalid
spam: egg
~~~

Das Ergebnis des Parsen dieser Konfigurationsdatei soll dann die Liste
`[("foo","bar"),("empty",""),("spam","egg")]` sein.

# String

Wir starten erstmal mit einer Version für `String`. Zunächst ein paar
Imports aus der Standardlibrary:

<div class="highlight"><pre><code class="language-haskell" data-lang="haskell"><span class="kr">import</span> <span class="nn"><a href="http://hackage.haskell.org/package/base-4.7.0.1/docs/Data-Char.html">Data.Char</a></span>
<span class="kr">import</span> <span class="nn"><a href="http://hackage.haskell.org/package/base-4.7.0.1/docs/Data-Maybe.html">Data.Maybe</a></span>
<span class="kr">import</span> <span class="k">qualified</span> <span class="nn"><a href="http://hackage.haskell.org/package/base-4.7.0.1/docs/Data-List.html">Data.List</a></span> <span class="k">as</span> <span class="n">List</span></code></pre></div>

Wenn Sie selbst ein Haskell Programm schreiben, aber nicht wissen welche
Funktionen in welchen Modulen zu finden sind, gibt es mindestens drei
Möglichkeiten, dies herauszufinden:

* Sie benutzen [hoogle](http://www.haskell.org/hoogle/) oder
  [hayoo](http://holumbus.fh-wedel.de/hayoo/hayoo.html),
  zwei Suchmaschine für Haskell-API-Dokumentation.
* Sie studieren die
  [Übersicht](http://www.haskell.org/ghc/docs/latest/html/libraries/) der
  gängigen Haskell Module.
* Sie schauen sich auf [hackage](http://hackage.haskell.org/) um.

Jetzt machen wir gleich mit der eigentlichen Parse-Funktion weiter.

{% highlight haskell %}
parseKeyValues :: String -> [(String, String)]
parseKeyValues str =
    mapMaybe parseLine (lines str)
    where
      parseLine :: String -> Maybe (String, String)
      parseLine str =
          case List.span (/= ':') (stripStart str) of
            ('#':_, _) -> Nothing -- Kommentare ignorieren
            (key, ':':rest) -> Just (key, strip rest)
            _ -> Nothing          -- Fehler ignorieren
      stripStart :: String -> String
      stripStart = dropWhile isSpace
      strip :: String -> String
      strip = reverse . stripStart . reverse . stripStart
{% endhighlight %}

Die `parseKeyValues`-Funktion nimmt einen `String` und liefert eine Liste
aus Schlüssel-Wert-Paaren zurück. Zuerst zerlegen wir den Eingabestring
mittels `lines` in einzelne Zeilen. Dann wenden wir die lokal definierte
`parseLine`-Funktion auf jede Zeile an. Dazu verwenden wir
[`mapMaybe`](http://hackage.haskell.org/package/base-4.7.0.1/docs/Data-Maybe.html#v:mapMaybe),
welches die [`Nothing`](http://hackage.haskell.org/package/base-4.7.0.1/docs/Data-Maybe.html#t:Maybe)-Werte in den Ergebnissen von `parseLine` ignoriert.

Die `parseLine`-Funktion und die beiden Hilfsfunktionen `stripStart` und
`strip` benutzen typische Listenfunktionen wie [`span`](http://hackage.haskell.org/package/base-4.7.0.1/docs/Data-List.html#v:span), [`dropWhile`](http://hackage.haskell.org/package/base-4.7.0.1/docs/Data-List.html#v:dropWhile)
und [`reverse`](http://hackage.haskell.org/package/base-4.7.0.1/docs/Data-List.html#v:reverse), ebenso wie Pattern-Matching über Listen. An der
Tatsache, dass wir `strip` und `stripStart` selbst definieren müssen,
sehen wir aber auch, dass typische Stringfunktionen in Haskells
Standardbibliothek oft fehlen.

# Data.Text

Die Bibliothek
[Data.Text](https://hackage.haskell.org/package/text-1.2.0.3) bringt solche
Funktionen direkt mit. Wir implementieren nun eine Variante von
`parseKeyValues` mittels `Data.Text`. Wir starten dazu mit einer etwas
komisch anmutenden Zeile:

{% highlight haskell %}
{-# LANGUAGE OverloadedStrings #-}
{% endhighlight %}

Der Teil zwischen `{-#` und `#-}` ist ein
[Language-Pragma](https://downloads.haskell.org/~ghc/7.2.1/docs/html/users_guide/pragmas.html). Damit
weisen wir den [GHC-Compiler](http://haskell.org/ghc) an, Stringliteralen
wie `"Hallo"` nicht den fixen Typ `String` zu geben, sondern den Typen
abhängig vom Kontext zu machen. `"Hallo"` kann als auch den Typ `Text` aus
`Data.Text` haben (wenn es der Kontext verlangt).

Jetzt geht's weiter mit Imports:

<div class="highlight"><pre><code class="language-haskell" data-lang="haskell"><span class="kr">import</span> <span class="nn"><a href="http://hackage.haskell.org/package/base-4.7.0.1/docs/Data-Char.html">Data.Char</a></span>
<span class="kr">import</span> <span class="nn"><a href="http://hackage.haskell.org/package/base-4.7.0.1/docs/Data-Maybe.html">Data.Maybe</a></span>
<span class="kr">import</span> <span class="k">qualified</span> <span class="nn"><a href="http://hackage.haskell.org/package/text-1.2.0.3/docs/Data-Text.html">Data.Text</a></span> <span class="k">as</span> <span class="n">T</span></code></pre></div>

Da `Data.Text` die Namen vieler Funktion aus Haskells
[`Prelude`](https://hackage.haskell.org/package/base-4.3.0.0/docs/Prelude.html)
verwendet, importiert man `Data.Text`
typischwerweise qualifizert, als Alias hat sich der Buchstabe `T`
eingebürgert.

Die `parseKeyValues`-Funktion für `Data.Text` sieht sehr ähnlich aus wie bei
der String-Variante:

{% highlight haskell %}
parseKeyValues :: T.Text -> [(T.Text, T.Text)]
parseKeyValues txt =
    mapMaybe parseLine (T.lines txt)
    where
      parseLine :: T.Text -> Maybe (T.Text, T.Text)
      parseLine txt =
          case T.span (/= ':') (T.stripStart txt) of
            (key, value) ->
                if "#" `T.isPrefixOf` key
                then Nothing               -- Kommentare ignorieren
                else case T.uncons value of
                       Just (_, rest) -> Just (key, T.strip rest)
                       Nothing -> Nothing  -- Fehler ignorieren
{% endhighlight %}

Im wesentlichen präfixen wir alle Listenfunktionen mit `T.`. Das
funktioniert, weil diese Funktionen in `Data.Text` mit demselben Namen
implementiert sind. Da der Typ `T.Text` nun aber keine Liste mehr ist,
sondern den String intern als ein Array von UTF-16 kodierten Characters
repräsentiert, können wir, anders als bei der String-Variante, kein
Pattern-Matching auf solchen Werte machen. Stattdessen benutzen wir
[`T.isPrefixOf`](https://hackage.haskell.org/package/text-1.2.0.3/docs/Data-Text.html#v:isPrefixOf) um zu prüfen, ob eine Zeile ein Kommentar ist, und
extrahieren das erste Zeichen aus dem `T.Text`-Wert mittels
[`T.uncons`](https://hackage.haskell.org/package/text-1.2.0.3/docs/Data-Text.html#v:uncons). Sie sehen aber auch, dass solche typischen
Stringfunktionen wie `stripStart` und `strip` direkt bei `Data.Text`
mitgeliefert werden.

Bevor ich zum Fazit komme, möchte ich nur kurz zwei Punkte ansprechen:

* Der Typ `String` ist lazy, `T.Text` hingegen strikt. Wir werden in einem
  späteren Artikel noch genauer auf die Unterschiede zwischen lazy und
  strikt eingehen. Hier möchte ich nur bemerken, dass es auch ein Modul
  [`Data.Text.Lazy`](https://hackage.haskell.org/package/text-1.2.0.3/docs/Data-Text-Lazy.html) gibt, welches eine lazy-Variante von `Text`
  definiert.
* Um zwischen `String` und `T.Text` zu konvertieren, gibt es die Funktionen
  `T.pack :: String -> T.Text` sowie `T.unpack :: T.Text -> String`.

# Fazit

Für praxisrelevante Programme verwenden ich und viele andere
Haskell-Programmierer fast ausschließlich den Typen `Text` aus
[`Data.Text`](https://hackage.haskell.org/package/text-1.2.0.3). Falls doch mal irgendwo ein `String` auftaucht,
konvertiere ich diesen sofort mittels `T.pack`.

So, das war's für heute. Ich hoffe, das Lesen hat Ihnen Spaß gemacht. Ich
freue mich über Feedback jeglicher Art!
