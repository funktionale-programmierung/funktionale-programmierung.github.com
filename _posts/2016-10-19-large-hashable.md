---
layout: post
description: Eine Haskell-Bibliothek für große Hashes
title: "Eine Haskell-Bibliothek für große Hashes"
author: stefan-wehr
tags: ["Haskell"]
---

Im heutigen Artikel möchten wir die Haskell-Bibliothek
[large-hashable](https://hackage.haskell.org/package/large-hashable) vorstellen, die es
ermöglicht, von beliebigen Haskell-Datentypen große Hashes z. B. mittels
MD5 oder SHA512 zu berechnen. Wir haben diese Bibliothek im Rahmen
unseres Produkts [Checkpad MED](/2013/07/17/medizin-funktional.html) entwickelt, um effizient feststellen zu können,
ob sich das Ergebnis einer Berechnung möglicherweise geändert hat. In diesem Artikel
zeigen wir zum einen wie man die Bibliothek benutzt, zum anderen
beleuchten wir exemplarisch einen wichtigen Implementierungsaspekt, nämlich die
Integration von C-Code in Haskell.

<!-- more start -->

## Motivation ##

Bevor wir uns die [large-hashable](https://hackage.haskell.org/package/large-hashable) Bibliothek genauer anschauen, möchten
wir zuerst noch kurz auf die Motiviation eingehen, die zur Entwicklung
der Bibliothek geführt hat. Eine wichtige Funktionalität unseres Produkts
[Checkpad MED](http://cpmed.de/) ist die Aufbereitung von Krankenhausdaten für Smartphones
und Tablets. Daten im Krankenhaus (wie z. B. Laborbefunde) ändern sich aber
häufig. Daher benutzen wir in Checkpad ein selbstentwickeltes Framework,
welches das Aufbereiten der Daten von der Reaktion auf Änderungen trennt.
Mit dem Framework schreibt man dann den Code zur Aufbereitung der Daten
so, als ob die Daten statisch wären und das Framework kümmert sich
darum, dass die Aufbereitung erneut getriggert wird, sobald sich relevante
Daten geändert haben. Wer bei dieser Beschreibung an ein kontinuierlich
laufendes Buildsystem denkt, liegt genau richtig: Auch bei Buildsystem
sind typischerweise die Regeln zur Erstellung von Buildartefakten getrennt
von der Logik zur Neuausführung der Regeln bei Änderungen an den
Quelldateien.

Bei der Entwicklung des Frameworks liegt eine wichtige Fragestellung im
effizienten Erkennen von Änderungen an den Eingabedaten. Buildsysteme
benutzen hierfür neben dem Änderungszeitpunkt der Quelldateien auch häufig
Hashes, die z. B. mittels des MD5-Algorithmus aus den Quelldateien
berechnet werden. Da unser Framework nicht nur Dateien sondern beliebige
Haskell-Werte als Eingaben und Zwischenergebnisse
unterstützt, können wir nicht immer auf den Zeitpunkt
der letzten Änderung zurückgreifen. Daher benutzt das Framework
große Hashwerte, um zu prüfen, ob sich Eingabedaten oder
Zwischenergebnisse geändert haben. Dieses Vorgehen ist in Ordnung, so lange
die Wahrscheinlichkeit von Kollisionen beim Hashing sehr sehr klein ist.

Am Anfang haben wir solche Hashwerte naiv berechnet; d. h. um die
MD5-Prüfsumme von einem Haskell-Wert zu berechnen haben wir
zuerst den Wert serialisiert, um danach aus dem resultierende Byte-Array
die MD5-Prüfsumme zu berechnen. Dieses Vorgehen ist natürlich alles andere
als effizient. Wie kann man dieses Problem also effizienter lösen?

Für Haskell gibt es bereits die Bibliothek [hashable](https://hackage.haskell.org/package/hashable), die aus einem
Haskell-Wert einen Hash berechnet. Der Hash ist dabei einfach ein Integer. Für
unser Problem ist die hashable-Bibliothek nicht geeignet, da die
Wahrscheinlichkeit von Kollisionen zu groß ist: Zum einen liegt das an der
Größe eines Integers (typischerweise 64 Bit), zum anderen aber auch daran,
dass die hashable-Bibliothek im Kontext von Datenstrukturen wie
Hash-Tabellen verwendet werden und für solche Datenstrukturen ist es
absolut normal, dass ab und an Hash-Kollisionen auftreten.

Wir mussten also eine eigene Bibliothek zur Berechnen von großen Hashes
entwicklen. Die Bibliothek heißt large-hashable und ist unter der BSD3 Lizenz auf
[hackage](https://hackage.haskell.org/package/large-hashable) verfügbar. Der Quellcode
wird auf [github](https://github.com/factisresearch/large-hashable) verwaltet. Durch die Verwendung von large-hashable konnte
wir die Performanz von einer unserer Komponenten um bis 50% steigern!

## Benutzung der Bibliothek ##

Herzstück der large-hashable Bibliothek ist die Typklasse
[LargeHashable](https://hackage.haskell.org/package/large-hashable-0.1.0.3/docs/Data-LargeHashable-Class.html#t:LargeHashable).
Jeder Typ, von dem wir einen großen
Hash berechnen möchten, muss eine Instanz dieser Typklasse sein. Man kann
solche Instanzen von Hand schreiben oder auch generieren lassen. Schreiben
wir zunächst für einen beispielhaften Typ `Name` die Instanz erstmal von Hand:

{% highlight haskell %}
import Data.LargeHashable

data Name = Name { firstName :: String, lastName :: String }

instance LargeHashable Name where
    updateHash name =
        do updateHash (firstName name)
           updateHash (lastName name)
{% endhighlight %}

Der Typ der [`updateHash`](https://hackage.haskell.org/package/large-hashable-0.1.0.3/docs/Data-LargeHashable-Class.html#v:updateHash) Funktion ist in diesem Fall
`Name -> LH ()`. Dabei ist [`LH`](https://hackage.haskell.org/package/large-hashable-0.1.0.3/docs/Data-LargeHashable-Intern.html#t:LH)
eine [Monade](/2013/04/18/haskell-monaden.html), die sich um das Verwalten
des bisher akkumulierten Hash-Werts kümmert. Mittels der `do`-Notation
hashen wir also den Vor- und den Nachnamen.

Wenn wir diese Definition nun in den [GHCi](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/ghci.html)
laden, können wir interaktiv die MD5-Prüfsumme eines Namens berechnen. Der Beispielcode ist auch
[hier](/code/large-hashable/Example.hs) verfügbar.

{% highlight bash %}
$ ghci Example.hs
*Main> runMD5 (updateHash (Name "Stefan" "Wehr"))
4d2f940ee9d0d540ff847cd3c74b36ce
{% endhighlight %}

Die zweite Möglichkeit, an eine Instanz von `LargeHashable` zu kommen, ist
durch [generisches Programmieren](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#generic-programming).
Hierbei wird über die Struktur der
Datenkonstruktoren der Hashwert zur Laufzeit berechnet. Um diese Funktionalität zu nutzen
muss man lediglich die Sprachoption `DeriveGeneric` einschalten
(z. B. durch das Pragma `{-# LANGUAGE DeriveGeneric #-}` ganz am Anfang der
Datei) und das Modul `GHC.Generics` importieren. Dann können wir uns
für jede Instanz der Typklasse `Generic` automatisch eine Instanz von
`LargeHashable` erzeugen lassen. Zum Beispiel so:

{% highlight haskell %}
{-# LANGUAGE DeriveGeneric #-}

import Data.LargeHashable
import GHC.Generics

data Person = Person { name :: Name, age :: Int }
    deriving (Generic)

instance LargeHashable Person
{% endhighlight %}

Die dritte Möglichkeit ist Metaprogrammierung mit Hilfe von
[TemplateHaskell](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#template-haskell).
Dabei wird beim Kompilieren Code ausgeführt, der für
einen gegebenen Typen eine Instanz von `LargeHashable` erzeugt. Dazu muss
man die Sprachoption `TemplateHaskell` aktivieren (am besten wieder durch
ein Pragma zu Beginn der Datei: `{-# LANGUAGE TemplateHaskell #-}`). Dann
kann man Folgendes schreiben:

{% highlight haskell %}
{-# LANGUAGE TemplateHaskell #-}

import Data.LargeHashable

data Car = Car { company :: String, model :: String, year :: Int }

$(deriveLargeHashable ''Car)
{% endhighlight %}

Der Code innerhalb von `$(...)` wird während des Kompilierens
ausgeführt. An dieser Stelle wird dann die `LargeHashable`-Instanz von
`Car` erzeugt.

Hier nochmal eine GHCi-Sitzung:

{% highlight bash %}
$ ghci Example.hs
*Main> let stefan = Name "Stefan" "Wehr"
*Main> runMD5 (updateHash (Person stefan 37))
c03533a58e15759f8f3aea6ccaec09d7
*Main> runMD5 (updateHash (Car "Ford" "Fiesta" 2009))
432c598b0b6810b4191cda88ed38348c
{% endhighlight %}

Welche dieser drei Arten soll ein Programmierer nun wählen, um eine
Instanz von `LargeHashable` zu schreiben? Die manuelle Art gibt dem
Programmierer volle Kontrolle über die Instanz, allerdings muss er oder
sie dabei viel Boilerplate-Code schreiben. Die andere beiden Arten
unterscheiden sich unter anderem durch Performanz-Eigenschaften. Die Variante mit
TemplateHaskell führt zu einer deutlichen längeren Kompilierzeit des
Programms, allerdings ist die Performanz zur Laufzeit identisch zu einer
vergleichbaren, von Hand geschriebenen Instanz. Die Variante über das
generische Programmien bringt wenig Einbußen bei der Kompilierzeit,
allerdings ist die Performanz zur Laufzeit schlechter. Der Hash wird nämlich
nicht direkt aus dem Datentyp sondern aus dessen generischer
Repräsentation berechnet.

## Implementierung ##

Wir möchten nun noch auf die Implementierung
eingehen. Dabei ist der komplette Sourcecode der large-hashable Bilbiothek
auf [github](https://github.com/factisresearch/large-hashable)
verfügbar.
Um den Rahmen des Artikels bezüglicher der detaillierten Implementierung
nicht zu sprengen, greifen wir uns einen interessanten Aspekt heraus:
die Integration mit C-Code. Aus
Performanzgründen benutzt large-hashable in C geschriebene
Routinen, um den Hashwert zu berechnen.
Dabei wird am Anfang ein Kontext für die Hash-Berechnung
initialisiert. Danach wird der Kontext fortlaufend
aktualisiert. Schlussendlich wird aus dem Kontext dann der eigentliche
Hash extrahiert.
Für MD5 benutzt
large-hashable dabei C Funktionen wie z. B.

{% highlight C %}
void md5_init(struct md5_ctx *ctx);
void md5_update(struct md5_ctx *ctx, uint8_t *data, uint32_t len);
// Komplettes Headerfile unter: https://github.com/factisresearch/large-hashable/blob/master/cbits/md5.h
void md5_finalize(struct md5_ctx *ctx, uint8_t *out);
{% endhighlight %}

Um nun diese Funktionen in Haskell aufrufen zu können,
bedarf es folgender Importe und Deklaration:

{% highlight haskell %}
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Alloc
import Data.Word

data RawCtx

foreign import ccall unsafe "md5.h md5_init"
    c_md5_init :: Ptr RawCtx -> IO ()

foreign import ccall unsafe "md5.h md5_update"
    c_md5_update :: Ptr RawCtx -> Ptr Word8 -> Int -> IO ()

foreign import ccall unsafe "md5.h md5_finalize"
    c_md5_finalize :: Ptr RawCtx -> Ptr Word8 -> IO ()
{% endhighlight %}

Damit machen wir die im Header `md5.h` deklarierten C-Funktion
`md5_init`, `md5_update` und `md5_finalize` unter den Namen
`c_md5_init`, `c_md5_update` bzw/ `c_md5_finalize` in Haskell bekannt.
Der Typ `RawCtx` ist ein sogannter "Phantomtyp": Es gibt keine Werte des
Typs, er dient lediglich dazu den Pointer auf den Kontext der MD5-Berechnung als
einen Typ zu repräsentieren, nämlich als `Ptr RawCtx`. Der Typ `Ptr Word8`
ist analog die Haskell-Repräsentation eines Pointers auf ein Word8,
entspricht also dem Typen `uint8_t *` in C. Wichtig hierbei ist allerdings, dass diese
Typen reine Konvention sind, d. h. der Compiler kann uns nicht daran hindern einen
Pointer mit dem falschen Haskell-Typ zu verwenden.
Z. B. könnten wir fälschlicherweise auch den C-Typ `uint8_t *` als
`Ptr RawCtx` in Haskell repräsentieren.

Um zu demonstrieren wie man mit solchen C-Funktionen arbeitet, zeigen
wir zum Abschluss die Funktion aus large-hashable, die einen Kontext zur MD5-Berechnung
alloziert, damit eine Berechnung durchführt und am Schluss den Hash
extrahiert.

{% highlight haskell %}
withCtx :: (RawCtx -> IO ()) -> IO (Word64, Word64)
withCtx action =
    allocaBytes 96 $ \(ptr :: Ptr RawCtx) ->
    do c_md5_init ptr
       action ptr
       allocaBytes 16 $ \(resPtr :: Ptr Word8) ->
           do c_md5_finalize ptr resPtr
              let first = castPtr resPtr :: Ptr Word64
              w1 <- peek first
              let second = castPtr (plusPtr resPtr (sizeOf w1)) :: Ptr Word64
              w2 <- peek second
              return (w1, w2)
{% endhighlight %}

Die Funktion [`allocaBytes`](http://hackage.haskell.org/package/base-4.9.0.0/docs/Foreign-Marshal-Alloc.html#v:allocaBytes)
alloziert einen Speicher der angegebenen Größe (hier: 96 Bytes, was der Größe eines Kontexts zur MD5-Berechnung entspricht),
führt damit eine Aktion aus und gibt den Speicher dann wieder frei. In unserem Fall passiert in der Aktion Folgendes:
wir initialisieren zunächst mittels `c_md5_init` den Speicher als MD5-Kontext. Dann lassen wir die IO-Aktion `action` laufen,
was typischerweise den Kontext zur Hash-Berechnung aktualisiert.
Nach Abschluss von `action` benutzen wir nochmals `allocaBytes`, um 16 Bytes für den
finalen Hashwert zu allozieren. Mittels `c_md5_finalize` schreiben wir den Hashwert in den Pointer `resPtr`. Dann extrahieren
wir mittels Pointerarithmetik und der Funktion [`peek`](http://hackage.haskell.org/package/base-4.9.0.0/docs/Foreign-Storable.html#v:peek)
das erste und das zweite 64-Bit Wort aus dem `resPtr` und geben dieses Paar zurück.

Sie sehen also: C-Funktion in Haskell einzubinden ist gar nicht so
schwer. Natürlich kann es aber in einer Funktion wie `withCtx`
zu Segmentation Faults und ähnlichen schlimmen Fehlern kommen; Dinge die in einer Sprache wie Haskell normalerweise nicht vorkommen.
Daher sollte man auch immer zweimal darüber Nachdenken und gute Gründe haben, wenn man C-Funktionen in Haskell benutzen will.

Das soll es für heute gewesen sein. Ich hoffe, es war etwas Interessantes dabei! Ich freue mich auf jeden Fall über
Ihre Fragen und Anregungen.
