---
layout: post
description: Haskell schlägt node.js
title: "Haskell schlägt node.js"
author: stefan-wehr
tags: ["Performance", "Haskell", "NodeJS", "Netzwerk", "Sockets"]
---

Das Javascript Framework [node.js](http://nodejs.org/) ist eine auf Googles
[V8 Engine](https://code.google.com/p/v8/) basierende Platform
zur Erstellung von performanten und skalierbaren Netzwerkprogrammen. Die gute
Performance von node.js bei Netzwerkoperationen beruht vor allem auf der
Nutzung von asynchronen Programmierschnittstellen.
Allerdings machen solche Programmierschnittstellen den Entwicklern das Leben
unnötigerweise schwer und tragen nicht gerade zur guten Wartbarkeit des Codes bei.

Der heutige Blogartikel zeigt, dass sich mit der funktionalen Programmiersprache
[Haskell](http://haskell.org) Netzwerkprogramme mit
deutlich besserer Performance als mit node.js schreiben lassen, ohne dass dabei
auf ein asynchrones Programmiermodell zurückgegriffen werden muss.
Stattdessen wird im üblichen, sequenziellen Stil programmiert und das
Laufzeitsystem kümmert sich um die performante Umsetzung auf
asynchrone Primitivoperationen.

Der verwendete Benchmark ist eine einfach Serverapplikation, die jede vom Client 
geschickte Zahl verdoppelt und das Ergebnis an den Client zurückschickt. Mit diesem
Benchmark ist das vorgestellte Haskellprogramm im Durchschnitt um Faktor 1,67
schneller als das entsprechende node.js Programm. Am Ende des Artikels lesen Sie,
mit welchen Tricks das Haskell-Laufzeitsystem diesen Speedup erzielt.

<!-- more start -->

Damit der Benchmark ein bißchen mehr Pepp bekommt, wollen wir dem Client erlauben,
eine neue Zahl auch schicken zu dürfen ohne dass der Server die vorige
Anfrage beantwort hat. Außerdem kann der Client durch Senden des Strings `end`
signalisieren, dass die Verbindung beendet werden soll.
Innerhalb einer Verbindung ist also beispielsweise auch folgende
Abfolge von Ereignissen möglichen:

    Client                    Server 
            ------ 1 ------->
            ------ 9 ------->
            ------ 5 ------->
            <----- 2 --------
            <---- 18 --------
            ------ 2 ------->
            <---- 10 --------
            <----- 4 --------
            ----- end ------->

## Die Javascript Version ##

Schauen wir uns zunächst die Umsetzung des Benchmarks in node.js an. Ich habe
dabei die aktuelle Version 0.10.5 von node.js verwendet. Der entsprechende
Javascript Code sieht so aus:

{% highlight javascript %}
var net = require('net');

var server = net.createServer(function (socket) {
    var leftOver = ''; // für unvollständig gelesene Zeilen
    // Eventhandler registrieren, wird beim Eintreffen von neuen Daten aufgerufen
    socket.on('data', function(data) {
        var str = leftOver + data.toString();
        if (str === "") {
            return;
        }
        // Der String wird jetzt in Zeilen aufgespaltet und alle
        // vollständigen Zeilen werden abgearbeitet.
        var lines = str.split("\n");
        var lastLineOk = str.charAt(str.length - 1) === "\n";
        for (var i = 0; i < lines.length - 1; i++) {
            var s = lines[i];
            if (s === "end") {
                socket.write("Thank you for using the nodejs doubling service\n");
                socket.end();
            } else {
                var j = parseInt(s);
                if (isNaN(j)) {
                    socket.write("not an int: <" + s + ">\n");
                } else {
                    socket.write((j * 2) + "\n");
                }
            }
        }
        leftOver = lastLineOk ? "" : lines[lines.length - 1];
    });
    socket.on('timeout',function(){
        socket.write("timeout");
        socket.end();
    });
    socket.on('error', function() {
    });
});

server.listen(44444);
{% endhighlight %}

Das Entscheidende am obigen Javascript Code ist der Eventhandler (die Funktion
`function(data) { ... }`), der beim Eintreffen von neuen Netzwerkdaten asynchron aufgerufen
wird. Man bezeichnet diese Art zu programmieren auch häufig als ["inversion of control"](http://de.wikipedia.org/wiki/Inversion_of_Control),
da nicht mehr der Code selbst entscheidet, wann neue Daten gelesen werden sollen (durch
Aufruf einer blockierenden Funktion), sondern
diese Entscheidung von außen (in diesem Fall durch das node.js Framework) getroffen
wird. Das Programm muss dann mit den neuen Daten umgehen können, auch wenn es eigentlich
gerade mit etwas anderem beschäftigt ist. Um eine gute Performance mit node.js zu erzielen
ist es erforderlich, dass der Eventhandler niemals blockiert und dass er
nur wenig Verarbeitunszeit benötigt.

Die Maßgabe, an jeder Stelle und jederzeit mit neu eingetroffenen Daten schnell umgehen zu können,
macht Programme häufig ziemlich unleserlich, weil die Struktur des Programms nicht
mehr der logischen Struktur des Problems folgt, sondern dem Ereignisfluss unterworfen ist.
Auch wird es häufig nötig sein, globale Variablen zu verwenden um den aktuellen Zustand
des Programms innerhalb dieses Ereignisflusses zu speichern. Diese Art von Zustand geht
über den typischerweise in imperativen und objekt-orientierten Programmen benötigten
Zustand hinaus, da oftmals auch Kontrollflussinformationen explizit in Variablen
abgelegt werden müssen.

Ein kleines Beispiel: stellen Sie sich vor, Sie möchten in einer Netzwerkanwendung
abwechselnd den Vornamen und dann den Nachnamen von Personen empfangen. Mit blockierenden
Aufrufen zum Empfangen von Daten sähe das vielleicht so aus:

{% highlight javascript %}
while (true) {
    var firstName = socket.readLine(); // fiktive Funktion zum Lesen einer Zeile 
    var lastName = socket.readLine();
    doSomething(firstName, lastName);  // Verarbeite Name und Alter
}
{% endhighlight %}

Mit der in node.js erzwungenen "inversion of control" müssen wir den
Eventhandler aber so schreiben, dass wir uns in einer Variable `hasFirstName`
merken ob wir als nächsten den Vornamen (`hasFirstName === false`) oder den
Nachnamen (`hasFirstName === true`) lesen müssen. Wir machen also den Kontrollfluss
explizit und repräsentieren ihn durch eine Variable.

{% highlight javascript %}
var firstName;
var hasFirstName = false;
socket.on('data', function(data) {
    var str = data.toString();
    if (hasFirstName) {
      doSomething(firstName, str);
      hasFirstName = false;
    } else {
      firstName = str;
      hasFirstName = true;
    }
});
{% endhighlight %}

In unserem Server zum Verdoppeln von Zahlen erfüllt die Variable
`leftOver` einen ähnlichen Zweck. Bei den gezeigten Beispielen mag die "inversion of control"
gar nicht so schlimm erscheinen. Aber sobald die Kontrollflusslogik ein
bißchen komplexer wird, merkt man schnell, dass das asynchrone Programmieren
mit Eventhandlern sich rasch ziemlich kompliziert und unübersichtlich darstellt.
Mit deutlichereren Worten: ein Albtraum hinsichtlich Verständlichkeit und Wartbarkeit.
 
## Die Haskell Version ##

In Haskell kann man ohne Verwendung von asynchronen Programmierkonstrukten performante Netzwerkanwendungen
schreiben. Das sich so ergebende Programm ist lesbarer und besser wartbar und, wie der vorliegende
Benchmark zeigt, trotzdem schneller als das entsprechende asynchrone node.js Programm.

In Haskell verwenden wir zur Verarbeitung der Netztwerkdaten die [Conduit-Bibliothek](http://hackage.haskell.org/package/conduit).
Diese Bibliothek stellt Konstrukte zur Verfügung, mit denen die Datenverarbeitung ähnlich wie
mit Unix-Pipes in einzelne Bausteine aufgeteilt werden kann. In der Conduit-Bibliothek und der Ergänzung
[network-conduit](http://hackage.haskell.org/package/network-conduit) gibt es bereits Bausteine zum Lesen 
von einem Socket, zum Aufspalten der Daten am Zeilentrenner `\n` und zum Schreiben auf einen Socket. Wir müssten
dann eigentlich nur noch die Logik implementieren, die eine einzelne Zeile behandelt.

Doch zunächst bedarf es einiger Imports, die sowohl die Conduit Operationen als auch Funktionalität zum Umgang mit
[ByteStrings](http://hackage.haskell.org/package/bytestring) und mit [Textdaten](http://hackage.haskell.org/package/text)
ermöglichen.

{% highlight haskell %}
import Data.Conduit
import qualified Data.Conduit.Binary as CB
import Data.Conduit.Network

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC

import qualified Data.Text as T
import qualified Data.Text.Read as T
import qualified Data.Text.Encoding as T
{% endhighlight %}

Weil sie so schön einfach ist schreiben wir zuerst die Main-Funktion:

{% highlight haskell %}
main =
    do let settings = serverSettings 44444 HostAny
       runTCPServer settings doubleApp
    where
      doubleApp x =
          appSource x $= (CB.lines =$= processLine) $$ appSink x
{% endhighlight %}

Hier kommen fast nur Funktionen aus der Conduit-Bibliothek zum Einsatz. Die eigentliche Verarbeitungspipeline
steht in der letzten Zeile. `appSource x` ist dabei ein Bausteine der die Daten vom
Client liest. Der Teil `CB.lines` spaltet nun die gelesenen Daten am Zeilentrenner `\n`
auf, während `processLine` dann für jede Zeile eine mögliche Ausgabe produziert, die letzlich von
`appSink x` an den Client zurückgeschickt wird. Lediglich `processLine` ist dabei eine selbst-geschriebene
Funktion. Die Conduit-Bibliothek kümmert sich aber nicht nur um das Durchschleifen der Daten
durch die Verarbeitungspipeline, sondern sie lauscht auch auf dem angegebenen Port `44444` und erstellt
für jeden eintreffenden Client einen neuen Thread, so dass auch mehrere Clients gleichzeitig bedient werden können.

Doch nun zum Herzstück der Verarbeitungslogik, die Funktion `processLine`.

{% highlight haskell %}
processLine :: Monad m => Conduit BS.ByteString m BS.ByteString
processLine =
    do mBs <- await           -- blockiert bis Eingabedaten vorhanden sind
       case mBs of
         Nothing -> return () -- EOF
         Just bs
             | bs == BSC.pack "end" ->
                 do yield (BSC.pack "Thank you for using the Haskell doubling service.\n")
                    return ()
             | otherwise ->
                 do case parseInt (T.decodeUtf8 bs) of
                      Right i -> yield (BSC.pack (show (2 * i) ++ "\n"))
                      Left _ -> yield (BSC.pack "not an integer\n")
                    processLine
    where
      parseInt x =
          case T.decimal x of
            Left err -> Left err
            Right (i, rest) ->
                if T.null rest
                then Right i
                else Left ("parse error: " ++ T.unpack rest)
{% endhighlight %}

Der Typ von `processLine` weißt die Funktion als einen Conduit aus, der ByteStrings (also ein Array von rohen Bytes)
als Eingabe nimmt und auch einen ByteString als Ausgabe produziert. Der Rumpf von `processLine` ist komplett
im sequenziellen Stil geschrieben. Dies sieht man gut an der Verwendung von
`await`, was solange blockiert bis Daten für den Conduit verfügbar sind.

## Benchmarkergebnisse ##

Ich habe die Performance der beiden nun vorliegenden Implementierungen des Benchmarks mit einem selbst geschriebenen
Programm gemessen. In einem Lauf des Messprogramms werden dabei gleichzeit 500 Verbindungen zum Server erstellt
und in jeder Verbindung 10 Anfragen gestellt. Dabei werden bis zu 3 Anfragen gleichzeitig abgeschickt. Um den
Server ein bißchen mehr unter Last zu setzen, habe ich auf verschiedenen Rechnern gleichzeitig 10 Instanzen des
Messprogramms gestartet. Insgesamt wurden also quasi gleichzeitig 5000 Verbindungen von Clients an den Server gerichtet,
auf jeder Verbindung wurden 10 Requests zum Verdoppeln einer Eingabzahl gestellt.

Der Server lief dabei auf einem 8 Kern Rechner mit Intel i7 3,4 GHz CPUs, 8192 KB Cache und 8 GB Arbeitsspeicher,
Betriebssystem war überall Linux.
Ich habe Software in den momentan aktuellen Versionen verwendet: 
node.js 0.10.5,
[Glasgow Haskell Compilers](http://www.haskell.org/ghc/) 7.6.2,
conduit                                                  1.0.5,
network-conduit                                          1.0.0.

Die durchschnittliche Laufzeit des Messprogramms gegen den Haskell-Server lag bei 5,30 Sekunden, mit dem
node.js Server hingegen bei durchschnittlich 8,94 Sekunden. Die Varianz der unterschiedlichen Instanzen des Messprogramms
war jeweils vernachlässigbar. Das bedeutet dass die Haskell-Variente um *Faktor 1,67 schneller* ist als
die node.js Variante. Nicht schlecht, vor allem wenn man bedenkt dass der Code auch noch besser lesbar
und wartbar ist.

## Die Tricks des Haskell-Laufzeitsystems ##

Warum ist nun die Haskell-Variante schneller als node.js, ohne dass der Programmierer zu asynchronen Techniken
greifen muss? Nun, wie weiter oben geschrieben startet die Conduit Bibliothek für jeden Client einen neuen
Thread. Dies ermöglicht einen sequenziellen Programmierstil ohne "inversion of control".

Es ist aber zu beachten dass Haskell *keine* nativen Threads verwendet. Solche nativen Threads sind viel zu teuer,
insbesondere beim Erstellen und beim Kontextwechsel. Man kann typischerweise nur wenige Tausende nativer Threads
innerhalb einer Anwendung haben. Das Laufzeitsystem von Haskell verwendet statt nativer Threads
sogenannte ["green Threads"](http://en.wikipedia.org/wiki/Green_threads). Das sind Threads die komplett vom Laufzeitsystem
verwaltet werden und daher sehr billig sind. In Haskell ist es ohne Probleme möglich, eine Million solcher
green Threads zu erzeugen.

Die zweite wichtige Komponente für gute Performance ist Haskells
[IO-Manager](http://johantibell.com/files/hask17ape-sullivan.pdf). Der IO-Manager sorgt dafür, dass ein blockierender Aufruf
intern asynchron behandelt wird, z.B. wird unter Linux zum Lesen von einem Socket der effiziente
`epoll` Systemcall verwendet.

Zusammenfassend lässt sich also sagen: Intern verwendet Haskell ein ähnliches, asynchrones Modell wie node.js. Durch die 
Verwendung extrem billiger green Threads wird diese Komplexität aber vor dem Programmier versteckt, der stattdessen
bequem mit blockierenden Aufrufen sequenziell programmieren darf.


## Fazit ##

Der Artikel zeigt, dass man mit Haskell Netzwerkanwendungen schreiben kann, die bessere Performance (Faktor 1,67) 
bei weniger Komplexität (keine "inversion of control")
als die entsprechenden node.js Anwendungen liefern. Der gesamt Code zum Artikel kann
[hier](/files/haskell-nodejs/haskell-nodejs.zip) heruntergeladen werden.
Ich freue mich über alle Arten von Feedback.
