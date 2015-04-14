---
layout: post
description: Monaden fürs Reverse Engineering
title: "Monaden fürs Reverse Engineering"
author: joachim-breitner
tags: ["Haskell", "Monaden", "Praxis"]
---

Der Spieleverlag Ravensburger hat in seinem Programm den „Tiptoi-Stift“, der mit einer Kamera in der Spitze und einem Lautsprecher ausgestattet Kinderbücher zum Sprechen bringt. Die Logik für den eingebauten Prozessor steckt dabei in einer Datei im GME-Format, die man sich auf der Webseite von Ravensburger herunterlädt und auf den Stift kopiert.

Ein paar interessierte Bastler haben sich dieses proprietäre, binäre Dateiformat vorgenommen und weitgehend entschlüsselt, so dass man jetzt seine eigenen Bücher und Spiele für den Tiptoi-Stift erstellen kann. Das dabei entstandene Programm [tttool](http://tttool.entropia.de/) zum Analysieren und Erzeugen von GME-Dateien ist in Haskell implementiert, und mal wieder waren Monaden dabei eine große Hilfe.

Dieser Artikel geht auf zwei Anwendungen von Monaden in diesem Projekt ein:

 * Dass sich Parser gut mit Monaden spezifizieren lassen, ist nichts neues. Hier wurde der Parser zusätzlich so instrumentiert, dass er sich merkt, welche Dateibereiche welche Bedeutung hatten, was beim Verstehen eines unbekannten Dateiformates eine große Hilfe ist. Dank der Abstraktion durch Monaden musste der Parser selbst kaum angepasst werden.
 * Diese GME-Dateien lassen sich nicht ohne weiteres in einem Rutsch rausschreiben, da man dazu vorher wissen müsste, an welchen Stellen in der Datei später was landet. Da wäre es geschickt, wenn man beim Programmieren „in die Zukunft blicken könnte“. Das geht tatsächlich: Mit Monaden und der rekursiven Do-Notation.

<!-- more start -->

Im folgenden gehen wir nicht direkt auf das „echte“ [GME-Format](https://github.com/entropia/tip-toi-reveng/blob/master/GME-Format.md) ein, sondern überlegen uns einfache Beispielformate.

# Ein protokollierender Parser #

Um den Appetit auf die Monaden zu erhöhen, gönnen wir uns als Aperitif erst einmal einen Parser, der von Hand geschrieben ist:

## Die ersten Bytes  ##

Die Datei, die wir zerlegen wollen, ist binär, also werden wir sie in einen [`ByteString`](http://hackage.haskell.org/packages/archive/bytestring/latest/doc/html/Data-ByteString.html#t:ByteString) einlesen. Diese Datenstruktur speichert eine Folge von Bytes effizienter als Listen. Wir importieren die nötigen Module mit

{% highlight haskell %}
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Word
{% endhighlight %}

Im ersten Schritt gehen wir einfach mal davon aus, dass die Datei immer fünf Bytes groß ist: Erst kommt eine 8-Bit-Zahl und dann zwei 16-Bit-Zahlen. Es ist nicht schwer, eine Funktion zu schreiben, die dieses einfache Dateiformat zerlegt:

{% highlight haskell %}
parser1 :: ByteString -> (Word8, Word16, Word16)
parser1 bs = (byte0, word1, word2)
  where
    byte0 = BS.index bs 0
    word1 = fromIntegral (BS.index bs 1) + 2^8 * fromIntegral (BS.index bs 2)
    word2 = fromIntegral (BS.index bs 3) + 2^8 * fromIntegral (BS.index bs 4)
{% endhighlight %}

Die Funktion `fromIntegral` konvertiert hierbei vom Typ `Word8`, den ein Element eines ByteStrings hat, zu dem Typ `Int`, den wir hinterher haben wollen.

## Hilfsfunktionen ##

Offensichtlich ist dieser Code nicht schön, denn er ist voller Redundanzen und Indexberechnungen. Mit ein paar Hilfsfunktionen geht das deutlich schöner:

{% highlight haskell %}
type Index = Int

getWord8At :: ByteString -> Index -> Word8
getWord8At bs i = BS.index bs i

getWord16At :: ByteString -> Index -> Word16
getWord16At bs i = fromIntegral b1 + 2^8 * fromIntegral b2
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

Das nächste Problem mit dieser Definition ist, dass die Funktion `parser2` die Startpositionen der Bausteine – im Beispiel die `0`, `1` und `3` – wissen muss. Das geht vielleicht noch in diesem einfachen Beispiel gut, aber spätestens wenn die Teile des Datei verschiedene Längen haben, werden wir ein Problem haben.

Es wäre also sinnvoll, wenn jeder Baustein nicht nur das Ergebnis des zurückliefert, sondern seinem Aufrufer auch verrät, an welcher Stelle es weitergeht. So sieht zum Beispiel der Baustein für 8-Bit-Zahlen jetzt so aus:

{% highlight haskell %}
getWord8AtI :: ByteString -> Index -> (Word8, Index)
getWord8AtI bs i = (BS.index bs i, i+1)
{% endhighlight %}

Wenn wir diesen Baustein nun verwenden wollen, müssen wir den neuen Index entsprechend weiterreichen:
{% highlight haskell %}
getWord16AtI :: ByteString -> Index -> (Word16, Index)
getWord16AtI bs i0 = (fromIntegral b1 + 2^8 * fromIntegral b2, i2)
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

Die Definition des Parser, der eine 8-Bit-Zahl einliest, hat sich dadurch kaum verändert:
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

Zur Erinnerung: Der Operator `$` dient nur darzu, Klammern zu sparen; `f $ g $ h x` ist das Gleiche wie `f (g (h x))`.


## Die erste Monade ##

Nun machen wir einen kleinen Sprung und definieren, in welchem Sinne unser `Parser`-Typ eine Monade ist – wem das jetzt zu schnell geht, dem sei die Artikelreihe zu Monaden ([Teil 1], [Teil 2], [Teil 3]) von Uwe Schmidt ans Herz gelegt. Wir werden direkt danach sehen, was uns das gebracht hat:

[Teil1]: http://funktionale-programmierung.de/2013/04/18/haskell-monaden.html
[Teil2]: http://funktionale-programmierung.de/2013/05/22/haskell-monaden2.html
[Teil3]: http://funktionale-programmierung.de/2013/07/03/haskell-monaden3.html

{% highlight haskell %}
instance Monad Parser where
    return x = Parser (\bs i -> (x, i))
    p1 >>= p2 = Parser (\bs i0 ->
        let (x, i1) = runParser p1 bs i0
            (y, i2) = runParser (p2 x) bs i1
        in (y, i2))
{% endhighlight %}

Diese Instanz legt zweierlei fest:

 * Wie ein Parser aussieht, der nichts macht, genannt `return`. Ein solcher
   ignoriert den Inhalt der Datei (`bs`), belässt die aktuelle Position `i`,
   wie sie ist, und gibt als Ergebnis jenes `x` zurück, das es bekommen hat.
 * Ein Operator `(>>=)`, genannt *bind*, der zwei Parser aneinanderhängt, um einen neuen zu
   bauen. Dabei darf der zweite Parser (`p2`) das Ergebnis des ersten Parsers
   (`x`) verwenden, also geben wir ihm das mit.

Das schöne an so einer Monaden-Definition ist, dass man nun in Haskell die elegante `do`-Notation verwenden kann, und schon sieht unser Code richtig lesbar aus:

{% highlight haskell %}
getWord16P :: Parser Word16
getWord16P = do
    b1 <- getWord8P
    b2 <- getWord8P
    return (fromIntegral b1 + 2^8 * fromIntegral b2)

parser4 :: ByteString -> (Word8, Word16, Word16)
parser4 = evalParser $ do
    byte0 <- getWord8P
    word1 <- getWord16P
    word2 <- getWord16P
    return (byte0, word1, word2)
{% endhighlight %}

Man muss sich nun weder mit den Indizes herumschlagen, noch muss man den `ByteString` immer herumreichen: Um all das kümmert sich `(>>=)` unter der Haube der `do`-Notation.

## Library-Code ##

Die Verwendung der `Monad`-Typklasse bringt noch mehr Vorteile. So gibt es eine Reihe von Library-Funktionen, die mit jeder Monade funktionieren, also auch mit unserer. Als Beispiel dient hier die Funktion `replicateM :: Monad m => Int -> m a -> m [a]`, mit der die monadische Aktion mehrfach ausgeführt wird.

So können wir beispielsweise elegant eine Liste von 16-Bit-Zahlen parsen, der ihre Länge (als 8-Bit-Zahl) vorangestellt wird:

{% highlight haskell %}
import Control.Monad

getWord16ListP :: Parser [Word16]
getWord16ListP = do
    n <- getWord8P
    entries <- replicateM (fromIntegral n) getWord16P
    return entries
{% endhighlight %}

## Verweise ##

Das GME-Dateiformat hat nicht nur Bytes und solche Arrays, sondern auch Verweise: Da ist z.B. das erste Wort eine Position in der Datei, an der dann das eigentliche Objekt, z.B. eine Liste von Zahlen, steht.

Mit dem „externen“ Interface unseres `Parser`-Typs können wir das nicht parsen. Man könnte zwar mit vielen Aufrufen von `getWord8P` vorspulen, kommt dann aber nicht mehr zurück. Das heißt wir müssen auf die Implementierung von `Parser` zugreifen, und bauen uns folgende Kombinatoren:

{% highlight haskell %}
lookAt :: Index -> Parser a -> Parser a
lookAt offset p = Parser $ \bs i ->
    let (x,_) = runParser p bs offset
    in  (x,i)

indirection :: Parser a -> Parser a
indirection p = do
    offset <- getWord16P
    lookAt (fromIntegral offset) p
{% endhighlight %}

Wichtig bei `lookAt` ist, dass die aktuelle Position des Parsers gespeichert und nachher zurückgesetzt wird.

Damit können wir schön das folgende, recht komplizierte Format parsen: Die Datei beginnt mit zwei 16-Bit-Zahlen, die jeweils die Offsets von Listen von 16-Bit-Zahlen (mit vorangestellter Länge als 8-Bit-Zahl) enthalten:

{% highlight haskell %}
parser5 :: ByteString -> ([Word16], [Word16])
parser5 = evalParser $ do
	list1 <- indirection getWord16ListP
	list2 <- indirection getWord16ListP
	return (list1, list2)
{% endhighlight %}

## Segmente ##

Soweit so gut: Damit könnten wir das GME-Dateiformat parsen. Das Problem ist allerdings, dass das Format nicht dokumentiert ist und durch „intensives draufschauen“ entschlüsselt wird. Daher wäre es gut zu wissen, welche Teile der Datei man jetzt eigentlich verstanden hat, welche Bytes welche Bedeutung haben, wo noch Lücken sind und wo etwas eventuell zweimal gelesen wurde (was auf einen Fehler im Formatverständnis hinweisen würde).

Wir wollen also, dass der Parser nebenher noch eine Liste von _Segmenten_ sammelt, die einen Namen und einen Byte-Bereich enthalten. Damit ändert sich die Definition von `Parser`:

{% highlight haskell %}
type Seg = (String, Index, Index)
newtype Parser a = Parser (ByteString -> Index -> (a, [Seg], Index))
{% endhighlight %}

Die Funktionen, die nur das „externe“ Interface von `Parser` verwenden, müssen nicht angepasst werden; lediglich jene, die in den `Parser`-Typ reinschauen. Diese schreiben sich – gesteuert durch die Typen – praktisch von selber:

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

Noch haben wir keine Segmente benannt und reichen nur die leere Liste umher. Also brauchen wir einen Kombinator, der das, was ein Parser eingelesen hat, benennt. Gleichzeitig werden alle Segmente, die der übergebene Parser identifiziert, noch qualifiziert: Wie wir gleich sehen werden bekommen wir so eine schöne hierarchische Übersicht, die die Datei erklärt.

{% highlight haskell %}
named :: String -> Parser a -> Parser a
named n p = Parser $ \bs i0 ->
    let (x, segments, i1) = runParser p bs i0
        segments' = (n,i0,i1) : map qualify segments
    in  (x, segments', i1)
 where qualify (n', i0, i1) = (n ++ "/" ++ n', i0, i1)
{% endhighlight %}

Diese Funktion verwenden wir jetzt großzügig in unserer Hauptfunktion, um die Teile zu benennen:

{% highlight haskell %}
parser6 :: ByteString -> (([Word16], [Word16]), [Seg])
parser6 = evalParser $ named "Header" $ do
	list1 <- indirection $ named "Liste1" getWord16ListP
	list2 <- indirection $ named "Liste2" getWord16ListP
	return (list1, list2)
{% endhighlight %}

Lässt man diese Funktion auf eine kleine Testeingabe los, so sieht man nicht nur das korrekte Ergebnis, sondern auch der Aufbau der Funktion wird gezeigt.

{% highlight haskell %}
> parser6 (BS.pack [4,0,7,0,1,23,0,1,42,0])
(([23],[42]),[("Header",0,4),("Header/Liste1",4,7),("Header/Liste2",7,10)])
{% endhighlight %}

Nun kann man sich noch schöne Funktionen basteln, die aus einer Liste von Segmenten und der Originaldatei einen übersichtlichen annotierten Hex-Dump erstellt. Bei einer echten GME-Datei kann das dann z.B. so aussehen:


    At 0x00000200 Size    20960: Header/Scripts
       0x00000200: B5 33 00 00 40 1F 00 00 A0 F5 05 00 C2 F6 05 00
       (skipping 1308 lines)
       0x000053D0: AC F0 05 00 E9 F1 05 00 26 F3 05 00 63 F4 05 00
    
    At 0x000053E0 Size       66: Header/Scripts/12150
       0x000053E0: 10 00 22 54 00 00 42 54 00 00 62 54 00 00 82 54
       0x000053F0: 00 00 90 54 00 00 B0 54 00 00 BE 54 00 00 CC 54
       0x00005400: 00 00 DA 54 00 00 E8 54 00 00 F6 54 00 00 04 55
       0x00005410: 00 00 12 55 00 00 20 55 00 00 2E 55 00 00 3C 55
       0x00005420: 00 00
    
    At 0x00005422 Size       32: Header/Scripts/12150/Line 0
       0x00005420:       01 00 00 00 00 F9 FF 01 01 00 02 00 00 00
       0x00005430: E8 FF 01 00 00 00 00 E8 FF 01 01 00 02 00 00 00
       0x00005440: 01 00
    
    At 0x00005442 Size       32: Header/Scripts/12150/Line 1
       0x00005440:       01 00 00 00 00 F9 FF 01 02 00 02 00 00 00
       0x00005450: E8 FF 01 00 00 00 00 E8 FF 01 01 00 02 00 00 00
       0x00005460: 01 00
    
    At 0x00005462 Size       32: Header/Scripts/12150/Line 2
       0x00005460:       01 00 00 00 00 F9 FF 01 03 00 02 00 00 00
       0x00005470: E8 FF 01 00 00 00 00 E8 FF 01 01 00 02 00 00 00
       0x00005480: 01 00


Zusätzlich kann das Programm nun unverstandene Bereiche der Datei aufzeigen und vor Überlappungen warnen, so dass das Reverse Engineering ungebremst weitergehen kann...


# Eine Monade mit Blick in die Zukunft #

Nun soll das `tttool` diese GME-Dateien nicht nur einlesen, sondern auch wieder ausgeben. Auch hier bieten sich Monaden als komfortable Abstraktionsschicht an, schließlich hat die “Gib dies aus, dann jenes und dann folgendes“ einen stark sequentiellen Touch. Und die `do`-Notation ist attraktiv.

## Noch eine Monade ##

Die Monade dazu ist recht einfach: Neben einem Rückgabewert (der meist nicht interessiert) wird eben auch eine Liste von Bytes zurückgegeben. In produktiv eingesetztem Code sollte man hier einen besseren Datentypen nehmen, aber für das Beispiel genügen Listen:

{% highlight haskell %}
newtype Write a = Write ([Word8], a)

runWrite :: Write a -> ([Word8], a)
runWrite (Write result) = result

execWrite :: Write a -> [Word8]
execWrite (Write result) = fst result

instance Monad Write where
    return x = Write ([], x)
    w1 >>= w2 = let (bytes1, x1) = runWrite w1
                    (bytes2, x2) = runWrite (w2 x1)
                in  Write (bytes1 ++ bytes2, x2)
{% endhighlight %}

Analog zum Parser wird das Schreiben eines Bytes noch mit dem Blick „unter die Haube“ des `Write`-Konstruktors implementiert, während uns danach die Abstraktionsschicht Monade genügt:

{% highlight haskell %}
writeWord8 :: Word8 -> Write ()
writeWord8 b = Write ([b], ())

writeWord16 :: Word16 -> Write ()
writeWord16 w = do
    writeWord8 (fromIntegral w1)
    writeWord8 (fromIntegral w2)
  where (w2, w1) = w `divMod` (2^8)

writeWord16List :: [Word16] -> Write ()
writeWord16List ws = do
    writeWord8 (fromIntegral (length ws))
    mapM_ writeWord16 ws
{% endhighlight %}

## Positionsinformation ##

Wie würden wir jetzt das Dateiformat von oben (Zwei 16-Bit-Zahlen mit den Offsets von zwei Listen) erzeugen? Man kann natürlich die Positionen „von Hand“ vorhersehen und dann rausschreiben:

{% highlight haskell %}
writeAll1 :: ([Word16], [Word16]) -> Write ()
writeAll1 (ws1, ws2) = do
    writeWord16 4
    writeWord16 (4 + 1 + 2 * fromIntegral (length ws1))
    writeWord16List ws1
    writeWord16List ws2
{% endhighlight %}

Aber es ist klar dass das nicht zielführend ist. Die `Write`-Monade sollte uns diese Arbeit abnehmen können!

Lösen wir erst mal ein einfacherers Problem und ändern das Format: Es sollen erst die Listen, und dann deren Position geschrieben werden. Unser Wunsch wäre, folgendes schreiben zu können:

{% highlight haskell %}
writeAll2 :: ([Word16], [Word16]) -> Write ()
writeAll2 (ws1, ws2) = do
    pos1 <- getPosition
    writeWord16List ws1
    pos2 <- getPosition
    writeWord16List ws2
    writeWord16 pos1
    writeWord16 pos2
{% endhighlight %}

Wie können wir ein solches `getPosition` implementieren? Dazu müssen wir den Typ unserer Monade anpassen, denn er muss nun seine Position wissen, und die muss ihm irgendwer mitteilen. Das sieht also so aus:

{% highlight haskell %}
newtype Write a = Write (Word16 -> ([Word8], a))

runWrite :: Write a -> Word16 -> ([Word8], a)
runWrite (Write result) = result

execWrite :: Write a -> [Word8]
execWrite (Write result) = fst $ result 0

instance Monad Write where
    return x = Write $ \_ -> ([], x)
    w1 >>= w2 = Write $ \pos1 ->
                let (bytes1, x1) = runWrite w1 pos1
                    pos2 = pos1 + fromIntegral (length bytes1)
                    (bytes2, x2) = runWrite (w2 x1) pos2
                in  (bytes1 ++ bytes2, x2)

writeWord8 :: Word8 -> Write ()
writeWord8 b = Write $ \_ -> ([b], ())
{% endhighlight %}

Der restliche Code muss wieder nicht verändert werden. Die Funktion `getPosition` gibt einfach den Parameter zurück, natürlich ohne neue Bytes zu produzieren:

{% highlight haskell %}
getPosition :: Write Word16
getPosition = Write $ \pos -> ([], pos)
{% endhighlight %}

## Der Blick in die Zukunft ##

Nun ist das Format nun mal leider so, dass wir die Positionen der Listen brauchen, _bevor_ wir die Listen rausschreiben. Heißt dass dass wir auf den Komfort und die schöne Syntax einer Monade verzichten müssen? Nein! Wir machen es einfach so, wie es schön wäre:

{% highlight haskell %}
writeAll2 :: ([Word16], [Word16]) -> Write ()
writeAll2 (ws1, ws2) = mdo
    writeWord16 pos1
    writeWord16 pos2
    pos1 <- getPosition
    writeWord16List ws1
    pos2 <- getPosition
    writeWord16List ws2
{% endhighlight %}

Nun greifen wir auf `pos1` und `pos2` zu, „bevor“ sie definiert werden. Damit das klappt, muss da statt `do` ein `mdo` stehen (wobei das `m` für µ steht, der Fixpunkt-Operator aus der Mathematik). Zusätzlich müssen wir `{# LANGUAGE RecursiveDo #-}` an den Anfang der Datei schreiben, denn dieses Feature ist eine Erweiterung der Programmiersprache.

Zusätzlich müssen wir dem Compiler sagen, wie so eine rekursive Berechnung in unserer Monade erfolgen soll. Dazu instantiieren wir die Typklasse `MonadFix` mit der Methode `mfix :: (a -> Write a) -> Write a`:

{% highlight haskell %}
instance MonadFix Write where
    mfix f = Write $ \pos -> let (bytes,x) = runWrite (f x) pos in (bytes, x)
{% endhighlight %}

Der Parameter (`f`) ist dabei einer, der einen Wert (`x`) produziert, dafür aber genau diesen Wert als Argument braucht – ein Hirnverdreher erster Güte.

Wer nicht glaub dass das funktionieren kann sehe selbst:

    execWrite $ writeAll3 ([1,2],[3,4])
    [4,0,9,0,2,1,0,2,0,2,3,0,4,0]

Tatsächlich stehen nun noch vor der ersten Liste die Position der zweiten Liste, die ja von der Länge der ersten Liste abhängt. Wie kann das funktionieren?

Das Zauberwort hier ist _Laziness_ (Bedarfsauswertung): Um die Position zu berechnen interessiert uns ja nur die _Anzahl_ der erzeugten Bytes, nicht ihr Inhalt. Der Code legt also erst mal die Listen an, aber ohne sie mit den Zahlen zu füllen. Erst wenn so die Struktur der Datei festgelegt ist und somit die Längen bekannt sind, findet die Berechnung der Positionen statt und sie werden in die Liste geschrieben.

# Fazit #

Ein Parser, der einem nebenher die Dateistruktur erklärt; eine Serialisierungshilfe, die einem jetzt schon verrät, wo was später liegt – mit handgestrickten Monaden kann man sich seinen Code schön-abstrahieren.



<!-- more end -->

