---
layout: post
description: Monaden fürs Reverse Engineering
title: "Monaden fürs Reverse Engineering"
author: joachim-breitner
tags: ["Haskell", "Monaden", "Praxis"]
---

Der Spieleverlag Ravensburger hat in seinem Programm den „Tiptoi-Stift“, der mit einer Kamera in der Spitze und einem Lautsprecher ausgestattet Kinderbücher zum Sprechen bringt. Die Logik für den eingebauten Prozessor steckt dabei in einer Datei im GME-Format, die man sich auf der Webseite von Ravensburger herunterlädt und auf den Stift kopiert.

Ein paar interessierte Bastler haben sich dieses proprietäre, binäre Dateiformat vorgenommen und weitgehend entschlüsselt, so dass man jetzt seine eigenen Bücher und Spiele für den Tiptoi-Stift erstellen kann. Das dabei entstandene Programm [tttool](http://tttool.entropia.de/) zum analysieren und erzeugen von GME-Dateien ist in Haskell implementiert, und mal wieder waren Monaden dabei eine große Hilfe.

Dieser Artikel geht auf zwei Anwendungen von Monaden in diesem Projekt ein:

 * Dass sich Parser gut als mit Monaden spezifizieren lassen, ist nichts neues. Hier wurde der Parser zusätzlich so instrumentiert, dass er sich merkt, welche Dateibereiche welche Bedeutung hatten, was beim Verstehen eines unbekannten Dateiformates eine große Hilfe ist. Dank der Abstraktion durch Monaden musste der Parser selbst kaum angepasst werden.
 * Diese GME-Dateien lassen sich nicht ohne weiteres in einem Rutsch rausschreiben, da man dazu vorher wissen müsste, an welchen Stellen in der Datei später was landet. Da wäre es geschickt, wenn man beim Programmieren „in die Zukunft blicken könnte“. Das geht tatsächlich: Mit Monaden und der rekursiven Do-Notation.

<!-- more start -->

Im folgenden gehen wir nicht direkt auf „echte“ [GME-Format](https://github.com/entropia/tip-toi-reveng/blob/master/GME-Format.md) ein, sondern überlegen uns einfache Beispielformate.

# Ein protokollierender Parser #

Um den Appetit auf die Monaden zu erhöhen, gönnen wir uns Apperitiv erst einmal einen Parser, der von Hand geschrieben ist:

## Die ersten Bytes  ##

Die Datei, die wir zerlegen wollen, ist binär, also werden wir sie in einen [`ByteString`](http://hackage.haskell.org/packages/archive/bytestring/latest/doc/html/Data-ByteString.html#t:ByteString) einlesen. Diese Datenstruktur speichert eine Folge von Bytes effizienter als Listen. Wir importieren die nötigen Module mit

{% highlight haskell %}
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Word
{% endhighlight %}

Im ersten Schritt gehen wir einfach mal davon aus, dass die Datei immer vier Bytes groß ist: Erst kommt eine 8-Bit-Zahl und dann zwei 16-Bit-Zahlen. Es ist nicht schwer, eine Funktion zu schreiben, die dieses einfache Dateiformat zerlegt:

{% highlight haskell %}
parser1 :: ByteString -> (Word8, Word16, Word16)
parser1 bs = (byte0, word1, word2)
  where
    byte0 = BS.index bs 0
    word1 = fromIntegral (BS.index bs 1) + 265 * fromIntegral (BS.index bs 2)
    word2 = fromIntegral (BS.index bs 3) + 265 * fromIntegral (BS.index bs 4)
{% endhighlight %}

Die Funktion `fromIntegral` konvertiert hierbei vom Typ `Word8`, den ein Element eines ByteStrings hat, zu dem Typ `Int`, den wir hinterher haben wollen.

## Hilfsfunktionen ##

Offensichtlich ist dieser Code nicht schön, denn er ist voller Redundanzen und Indexberechnungen. Mit ein paar Hilfsfunktionen geht das deutlich schöner:

{% highlight haskell %}
type Index = Int

getWord8At :: ByteString -> Index -> Word8
getWord8At bs i = BS.index bs i

getWord16At :: ByteString -> Index -> Word16
getWord16At bs i = fromIntegral b1 + 265 * fromIntegral b2
  where b1 = getWord8At bs i
        b2 = getWord8At bs (i+1)

parser2 :: ByteString -> (Word8, Word16, Word16)
parser2 bs = (byte0, word1, word2)
  where
    byte0 = getWord8At  bs 0
    word1 = getWord16At bs 1
    word2 = getWord16At bs 3
{% endhighlight %}

Damit haben wir den ersten Schritt in Richtung eines Kombinator-Parsers gemacht: Kleine Bausteine werden zu komplexen Parsern zusammengebaut.

## Index-Berechnungen vermeiden ##

Das nächste Problem mit dieser Definition ist dass die Funktion `parser2` die Startpositionen der Bausteine – im Beispiel die `0`, `1` und `3` – wissen muss. Das geht vielleicht noch in diesem einfachen Beispiel gut, aber spätestens wenn die Teile des Datei verschiedene Längen haben, werden wir ein Problem haben.

Es wäre also sinnvoll, wenn jeder Baustein nicht nur das Ergebnis des zurückliefert, sondern seinem Aufrufer auch verrät, an welcher Stelle es weitergeht. So sieht zum Beispiel der Baustein für 8-Bit-Zahlen jetzt so aus:

{% highlight haskell %}
getWord8AtI :: ByteString -> Index -> (Word8, Index)
getWord8AtI bs i = (BS.index bs i, i+1)
{% endhighlight %}

Wenn wir diesen Baustein nun verwenden wollen, müssen wir den neuen Index entsprechend weiterreichen:
{% highlight haskell %}
getWord16AtI :: ByteString -> Index -> (Word16, Index)
getWord16AtI bs i0 = (fromIntegral b1 + 265 * fromIntegral b2, i2)
  where (b1, i1) = getWord8AtI bs i0
        (b2, i2) = getWord8AtI bs i1

parser3 :: ByteString -> (Word8, Word16, Word16)
parser3 bs = (byte0, word1, word2)
  where
    i0 = 0
    (byte0, i1) = getWord8AtI  bs i0
    (word1, i2) = getWord16AtI bs i1
    (word2, i3) = getWord16AtI bs i2
{% endhighlight %}

## Der Parser-Typ ##

Logisch gesehen ist das schon ziemlich schön so: Wir können diese Parser beliebig aneinanderhängen und die ganze Index-Rumrechnerei wird uns abgenommen. Aber schön zu schreiben ist das nicht, mit diesen vielen Index-Variablen, die da rumfahren. Auch diese können wir doch sicherlich weg-abstrahieren!

Zuerst stellen wir fest, dass die Typen von `getWord8AtI` und `getWord16AtI` recht groß geworden sind und dabei die spannende Information vor lauter Indizes verschwindet. Verstecken wir diese also in einem neuen Typ:
{% highlight haskell %}
newtype Parser a = Parser (ByteString -> Index -> (a, Index))
{% endhighlight %}

Diese Zeile kann man wie folgt verstehen: „Ein Parser-Baustein, der etwas von Typ `a` parsen soll, ist eine Funktion, die den Inhalt der zu parsenden Datei und den Index, wo das Etwas zu finden ist, erwartet und neben dem Etwas auch noch verrät, wo es danach weitergeht.“

Die Definition des Parser, der eine 8-Bit-Zahl einliest, hat sich dadurch kaum verädert:
{% highlight haskell %}
getWord8P :: Parser Word8
getWord8P = Parser (\bs i -> (BS.index bs i, i+1))
{% endhighlight %}

Die anderen Parser werden gleich deutlich schöner. Doch zuerst definieren wir noch zwei Hilfsfunktionen, um einen Wert vom Typ Parser einfacher verwenden zu können:
{% highlight haskell %}
runParser :: Parser a -> ByteString -> Index -> (a, Index)
runParser (Parser p) = p

evalParser :: Parser a -> ByteString -> a
evalParser p bs = fst $ runParser p bs 0
{% endhighlight %}


## Die erste Monade ##

Nun machen wir einen kleinen Sprung und definieren nun in welchem Sinne unser `Parser`-Typ eine Monade ist – schließlich wurden Monaden bereits in der Artikelreihe (Teil 1, Teil 2, Teil 3) von Uwe Schmidt hier ausführlich motiviert. Wir werden direkt danach sehen, was uns das gebracht hat:

{% highlight haskell %}
instance Monad Parser where
    return x = Parser (\bs i -> (x, i))
    p1 >>= p2 = Parser (\bs i0 ->
        let (x, i1) = runParser p1 bs i0
            (y, i2) = runParser (p2 x) bs i1
        in (y, i2))
{% endhighlight %}

Diese Instanz definiert zwei Sachen:

 * Wie ein Parser aussieht, der nichts macht, genannt `return`. Ein solcher
   ignoriert die Datei (`bs`) und gibt die aktuelle Position zurück (`i`).
 * Ein Operator `(>>=)`, genannt *bind*, der zwei Parser aneinanderhängt, um einen neuen zu
   bauen. Dabei darf der zweite Parser (`p2`) das Ergebnis des ersten Parsers
   (`x`) verwenden, also geben wir ihm das mit.

Das schöne an so einer Monaden-Definition ist, dass man nun in Haskell die elegante `do`-Notation verwenden kann, und schon sieht unser Code richtig lesbar aus:

{% highlight haskell %}
getWord16P :: Parser Word16
getWord16P = do
    b1 <- getWord8P
    b2 <- getWord8P
    return (fromIntegral b1 + 265 * fromIntegral b2)

parser4 :: ByteString ->  (Word8, Word16, Word16)
parser4 = evalParser $ do
    byte0 <- getWord8P
    word1 <- getWord16P
    word2 <- getWord16P
    return (byte0, word1, word2)
{% endhighlight %}

Man muss sich nun weder mit den Indizes herumschlagen, noch muss man den `ByteString` immer herumreichen: Um all das kümmert sich `(>>=)` unter der Haube der Do-Notation.

## Library-Code ##

Die Verwendung der `Monad`-Typklasse hat noch mehr Vorteile. So gibt es eine Reihe von Library-Funktionen, die mit jeder Monade funktionieren, also auch mit unseren. Als Beispielt dient die Funktion `replicateM :: Monad m => Int -> m a -> m [a]`, mit der die monadische Aktion mehrfach ausgeführt wird.

So können wir Beispielsweise elegant eine Liste von 16-Bit-Zahlen parsen, die der ihre Länge (als 8-Bit-Zahl) vorangestellt wird:

{% highlight haskell %}
import Control.Monad

getWord16ListP :: Parser [Word16]
getWord16ListP = do
    n <- getWord8P
    entries <- replicateM (fromIntegral n) getWord16P
    return entries
{% endhighlight %}

## Verweise ##

Das GME-Dateiformat hat nicht nur Bytes und solche Arrays, sondern auch Verweise: Da ist z.b. das erste Wort eine Position in der Datei, an der dann das eigentliche Objekt, z.B. eine Liste von Zahlen, steht.

Mit dem „externen“ Interface unseres `Parser`-Typs können wir das nicht parsen. Man könnte zwar mit vielen Aufrufen von `getWord8P` vorspulen, kommt dann aber nicht mehr zurück. Das heißt wir müssen auf die Implementierung von `Parser` zugreifen, und bauen uns folgenden Kombinator:

{% highlight haskell %}
lookAt :: Int -> Parser a -> Parser a
lookAt offset p = Parser $ \bs i ->
    let (x,_) = runParser p bs offset
    in  (x,i)

getInd :: Parser a -> Parser a
getInd p = do
    offset <- getWord16P
    lookAt (fromIntegral offset) p
{% endhighlight %}

Damit können wir schön das folgende, recht komplizierte Format parsen: Die Datei beginnt mit zwei 16-Bit-Zahlen, die jeweils die Offsets von Listen von 16-Bit-Zahlen (mit vorangestellter Länge als 8-Bit-Zahl) enthalten:

{% highlight haskell %}
parser5 :: ByteString -> ([Word16], [Word16])
parser5 = evalParser $ do
	list1 <- getInd getWord16ListP
	list2 <- getInd getWord16ListP
	return (list1, list2)
{% endhighlight %}

## Segmente ##

Soweit so gut, damit könnten wir das GME-Dateiformat parsen. Das Problem ist allerdings, dass das Format nicht dokumentiert ist und durch „intensives draufschauen“ entschlüsselt wird. Daher wäre es gut zu wissen, welche Teile der Datei man jetzt eignetlich verstanden hat, welche Bytes eigentlich wo gelandet sind, wo noch Lücken sind und wo etwas eventuell zweimal gelesen wurde (was auf einen Fehler im Formatverständnis hinweisen würde).

Wir wollen also dass der Parser nebenher noch eine Liste von _Segmenten_ sammelt, die einen Namen und ein Byte-Bereich enthalten. Damit ändert sich die Definition von `Parser`:

{% highlight haskell %}
type Seg = (String, Index, Index)
newtype Parser a = Parser (ByteString -> Index -> (a, [Seg], Index))
{% endhighlight %}

Die Funktionen, die nur das „externe“ Interface von `Parser` verwenden, müssen nicht angepasst werden; lediglich jene die in den `Parser`-Typ reinschauen müssen geändert werden. Diese schreiben sich – gesteuert durch die Typen – praktisch von selber:

{% highlight haskell %}
runParser :: Parser a -> ByteString -> Index -> (a, [Seg], Index)
runParser (Parser p) = p

evalParser :: Parser a -> ByteString -> (a, [Seg])
evalParser p bs =
    let (x,s,_) = runParser p bs 0
    in (x,s)

instance Monad Parser where
    return x = Parser (\bs i -> (x, [], i))
    p1 >>= p2 = Parser (\bs i0 ->
        let (x, s1, i1) = runParser p1 bs i0
            (y, s2, i2) = runParser (p2 x) bs i1
        in (y, s1++s2, i2))

getWord8P :: Parser Word8
getWord8P = Parser (\bs i -> (BS.index bs i, [], i+1))

lookAt :: Int -> Parser a -> Parser a
lookAt offset p = Parser $ \bs i ->
    let (x,s,_) = runParser p bs offset
    in  (x,s,i)
{% endhighlight %}

Noch haben wir keine Segmente benannt und reichen nur die leere Liste umher. Also brauchen wir einen Kombinator, der das, was ein Parser eingelesen hat, benennt. Gleichzeitig werden alle Segmente, die der übergebene Parser benennt, noch qualifiziert: So bekommen wir einen schönen Parse-Baum, der die Datei erklärt.

{% highlight haskell %}
named :: String -> Parser a -> Parser a
named n p = Parser $ \bs i0 ->
    let (x, segments, i1) = runParser p bs i0
    in  (x, (n,i0,i1):[(n ++ n', i0', i1') | (n',i0',i1') <- segments], i1)
{% endhighlight %}

Diese Funktion verwenden wir jetzt großzügig in unserer Hauptfunktion, um die Teile zu benennen:

{% highlight haskell %}
parser6 :: ByteString -> (([Word16], [Word16]), [Seg])
parser6 = evalParser $ named "Header" $ do
	list1 <- getInd $ named "Liste1" getWord16ListP
	list2 <- getInd $ named "Liste2" getWord16ListP
	return (list1, list2)
{% endhighlight %}

Nun wird es Zeit, mal den Code in Aktion zu sehen:

{% highlight haskell %}
> parser6 (BS.pack [4,0,7,0,1,23,0,1,42,0])
(([23],[42]),[("Header",0,4),("Header/Liste1",4,7),("Header/Liste2",7,10)])
{% endhighlight %}

Nun kann man sich noch schöne Funktionen basteln, die aus einer Liste von Segmenten und der Originaldatei einen übersichtlichen annotierten Hex-Dump erstellt und dabei bei Überlappungen warnt, und das Reverse Engineering kann weitergehen...


# Eine Monade mit Blick in die Zukunft #



Links mit [Text](http://URL).

Hervorhebungen *mit Stern* oder _Unterstrich_.  **Doppelt** für mehr
__Druck__.  Geht auch mitt*endr*in in einem Wort.

<!-- more end -->

