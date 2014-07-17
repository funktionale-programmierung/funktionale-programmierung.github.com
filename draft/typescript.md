---
layout: post
description: Ein Streifzug durch die Features von TypeScript
title: "Ein Streifzug durch die Features von TypeScript"
author: matthias-fischmann
tags: ["JavaScript", "TypeScript", "Datentypen"]
---


In den letzten Jahren hat sich die Entwicklung von Web-Anwendungen in
eine Richtung gedreht, in der immer größere Teile komplexer
Softwaresysteme im Browser leben.  Damit haben sich auch die Ansprüche
an den Browser als Softwareplattform geändert, und die Kapselungs- und
Abstraktionsmöglichkeiten der ehemals als HTML-Erweiterung angelegten
Browser-Programmiersprache
[JavaScript](https://developer.mozilla.org/en-US/docs/Web/JavaScript)
genügen nicht mehr.

Inzwischen gibt es eine Reihe von Programmiersprachen, die mit
unterschiedlichen Ansätzen an JavaScript andocken können.  In einem
Artikel über [ClojureScript]({% post_url 2014-02-14-clojurescript-react %})
wurde in diesem Blog bereits eine
Sprache aus der LISP-Familie vorgestellt.  Im vorliegenden Artikel
geht es um [TypeScript](http://www.typescriptlang.org/), eine
Alternative, die sich durch ein statisches, von C# inspiriertes
Typsystem und niedrige Einsatzhürden auszeichnet.

 <!-- more start -->

TypeScript ist eine Obermenge von JavaScript, d.h. jedes
JavaScript-Programm ist ein TypeScript-Programm.  So kann der Compiler mit wenigen
Stunden Aufwand
selbst in ein großes Softwareprojekt eingeführt werden, (fast) ohne
den Code zu ändern.  Die Vorteile der statischen Typisierung kann man
dann nach und nach intensiver nutzen.  Die Neuerungen an TypeScript
sind konservativ und orientieren sich an bestehenden Sprachen wie C#
(das Projekt stammt aus dem Hause Microsoft) und Java.

Der Compiler ist in TypeScript geschrieben, läuft in
[Node.js](http://nodejs.org), und lässt sich
genauso gut in CLI-basierte Entwicklungsumgebungen oder emacs
integrieren wie in
Eclipse oder VisualStudio.  Es unterstützt die gängigen
JavaScript-Modulsysteme und skaliert gut in großen Softwareprojekten.
Ich will im Folgenden zunächst die Sprache vorstellen und versuchen, ein
Gefühl zu dafür geben, was es heißt, in TypeScript zu denken.  Ein
Abschnitt über Entwicklungswerkzeuge und das Arbeiten mit
TypeScript-Code folgt danach.

Dem eiligen Leser, der sowohl in JavaScript als auch in C#, Java
o.ä. Routine hat, mag es genügen, die Code-Schnipsel zu lesen und den
erklärenden Text auszulassen.


## Fehler finden in JavaScript ##

TypeScript bietet einen
[Spielplatz](https://www.typescriptlang.org/Playground) ([hier ohne
SSL](http://www.typescriptlang.org/Playground)) im Netz an, auf dem
Code-Schnipsel nach JavaScript übersetzt und ausgeführt werden können.
Zum besseren Verständnis können Sie die folgenden Beispiele wärend des
Lesens hier eingeben und mit den Übersetzungen zu vergleichen.  Hier
als erstes Beispiel ein einfaches "Hello-World"-Programm:


{% highlight javascript %}
var msg = "Die Sonne scheint";
window.alert(msg)
{% endhighlight %}

So sollte das im Playground aussehen:

![(Screenshot)](fig1.png)

Zunächst fällt auf, wie durchlässig die Abstraktionsschicht zwischen
Quell- und der Zielsprache ist.  _Jedes JavaScript-Programm ist ein
TypeScript-Programm_; Wenn man also einfachen JavaScript-Code in das
TypeScript-Feld auf der linken Bildhälfte eingibt, tut der Compiler
(fast) gar nichts.
Auf der rechten Bildhälfte im generierten JavaScript-Code sieht man,
dass in der zweiten Zeile ein Semikolon angehängt wurde; ansonsten
bleibt der Code unverändert.

Dabei hat TypeScript bereits einiges geleistet: Für jeden Ausdruck im
Quellcode wurde automatisch ein statischer Typ hergeleitet und die
Korrektheit des gesamten Programms (bezüglich dieser Typen)
festgestellt.  Außerdem wurden die Typen in den Playground gereicht,
wo man sie sich anzeigen lassen kann, in dem man mit der Maus auf
einen Ausdruck zeigt (`msg` hat etwa den Typ `string`).

Auch beim Debugging leistet der Compiler bereits gute Dienste, ohne auf
Sprach-Erweiterungen zuzugreifen.  Wenn man den folgenden Code in den
Playground eingibt:

{% highlight javascript %}
var msg = {
    header: "Die Sonne scheint",
    details: [
        "Aussicht positiv",
        "schwacher Wind aus verschiedenen Richtungen"
    ];
};
window.alert(msg.haeder);
{% endhighlight %}

...  werden zwei Stellen rot unterstrichen, und durch Berührung
mit der Maus kann man sich die Fehler anzeigen lassen:

    ',' expected.

Und, nachdem das anstößige `;` entfernt oder durch ein `,` ersetzt
ist, der Buchstabendreher in `header`:

    The property 'haeder' does not exist on value of type
    '{ header: string; details: string[]; }'.

Für den Entwickleralltag bedeutet das eine enorme Verbesserung: Statt
den Code in den Browser oder in node-js zu laden und eine
Unit-Testsuite auszuführen, um Tipp- und Syntaxfehler zu finden, kann
man nun im Editor auf Knopfdruck auf den nächsten Tippfehler gesetzt
werden.  (Dazu später mehr.)


## Typen zur Dokumentation von JavaScript-Code ##

Mit _Typ-Annotationen_ bietet TypeScript eine echte Spracherweiterung
gegenüber JavaScript an.  Annotationen können an jeder Stelle stehen,
an der ein Name definiert wird, und werden mit `:` eingeleitet:

{% highlight javascript %}
var msg : string = "Die Sonne scheint";
window.alert(msg);
{% endhighlight %}

Anders als die JavaScript-Typen, die zur Laufzeit mit `typeof`
abgefragt werden können, existieren die Typ-Annotationen von
TypeScript nur in der Übersetzungsphase, wo sie der
Korrektheitsanalyse dienen.  Der JavaScript-Code enthält keine Spur
mehr von `: string`:

{% highlight javascript %}
var msg = "Die Sonne scheint";
window.alert(msg);
{% endhighlight %}

(Wenn ich im zweiten Teil dieses Artikels auf Interfaces und Klassen
eingegangen bin, wird klar werden, warum man TypeScript-Typen zur
Laufzeit behalten möchte.)

Neben den einfachen Basistypen `string`, `number`, und `boolean`, gibt
es Typen für Funktionen und (natürlich) Objekte und Arrays:

{% highlight javascript %}
var f : (number, string) => string;
f = function(msgNumber, msgHeader) {
    return msgNumber.toString() + ": " + msgHeader;
};

var msg : {
    header : string;
    details : string[];
} = {
    header: "Die Sonne scheint",
    details: [
        "Aussicht positiv",
        "schwacher Wind aus verschiedenen Richtungen"
    ]
};
{% endhighlight %}

Der aufmerksame Leser hat vielleicht bemerkt, dass die Implementierung
von `f` in ein eigenes Statement gerutscht ist.  Das liegt daran, dass
der Typ `((number, string) => string)` im Gegensatz zu `string` nicht
in Typ-Annotationen direkt verwendt werden kann.  Das ist eine
seltsame Ausnahme und ein Bruch mit der Idee der funktionalen
Programmierung, aber glücklicherweise gibt es eine spezielle
Schreibweise, die ohnehin besser zu lesen ist:

{% highlight javascript %}
var f = (msgNumber : number, msgHeader : string) : string => {
    return msgNumber.toString() + ": " + msgHeader;
}
{% endhighlight %}

Im zweiten Teil dieses Artikels werden wir mehr über Funktionen,
Typen, Klassen und Module lernen.

An dieser Stelle sei nur noch angemerkt, dass auch die etwas klobige
Typ-Annotation von `msg` im letzten Beispiel eleganter ausgedrückt
werden kann.  Objekt-Typen kann man nämlich wie JavaScript-Ausdrücke
einem Namen zuweisen:

{% highlight javascript %}
interface IMsg {
    header : string;
    details : string[];
}

var msg : IMsg = {
    header: "Die Sonne scheint",
    details: [
        "Aussicht positiv",
        "schwacher Wind aus verschiedenen Richtungen"
    ]
};
{% endhighlight %}

Ein Typsystem ist eine Meta-Sprache, in der Aussagen über die Struktur
einer darunterliegenden Programmiersprache gemacht werden können.  Das
Typsystem von TypeScript ist also eine Sprache, in der man über
JavaScript reden kann.

Wie wir bisher gesehen haben, kann der TypeScript-Compiler dies
bis zu einem gewissen grad tun, ohne uns damit zu behelligen (es sei denn natürlich,
es gibt Typfehler zu berichten).  In der Praxis interessant wird es aber erst,
wenn wir die Typ-Sprache in Form von Annotationen verwenden
können, um dem Compiler unseren Code genauer zu erklären.  Dadurch
schlägt man zwei Fliegen mit einer Klappe:

- __Maschinenlesbarkeit__: Der Compiler kann an Stellen, an denen er
  vorher einen zwar in sich stimmigen, aber nicht vom Programmierer
  beabsichtigten Typ hergeleitet hat und ein Fehler deshalb unbemerkt
  blieb, jetzt eine Fehlermeldung anzeigen.

- __Lesbarkeit für die Mitmenschen__: Typ-Annotationen sind
  Kommentare.  Der nächste (oder der selbe) Programmierer, der sich
  den Code einige Wochen später anschauen muss, erhält durch die Typen
  kompakte Zusatzinformationen darüber, was mit einem Stück Code
  "gemeint" ist.  Bonus: diese Kommentare werden immer korrekt sein,
  weil sich der Code sonst nicht mehr übersetzen lässt.


## Installation und Benutzung ##

Um den TypeScript-Compiler unter Linux zu installieren, benötigt man
[node](http://nodejs.org) und [npm](http://npmjs.org).  Beides ist in
allen verbreiteten Distributionen enthalten.  Wer sicher gehen will,
dass er die aktuellste Version hat, kann direkt von der Website
installieren:

{% highlight sh %}
$ curl -O http://nodejs.org/dist/v0.10.29/node-v0.10.29.tar.gz
$ tar xvpzf node-v0.10.29.tar.gz
$ cd node-v0.10.29
$ ./configure --prefix=$HOME/opt  # (oder ein Pfad Ihrer Wahl)
$ make
$ make install
$ node --version
{% endhighlight %}

Danach ist die Installation von TypeScript ein Kinderspiel:

{% highlight sh %}
$ npm install -g typescript
$ tsc --version
{% endhighlight %}

TypeScript wird in vielen IDEs unterstützt (insbesondere
Eclipse und VisualStudio).  Aber auch wer emacs, vi, gmake etc. als IDE
verwendet, findet sich schnell zurecht:

{% highlight sh %}
$ cat > code.ts
var msg : string = "Immer noch gutes Wetter.";
console.log(msg);
^D
$ tsc code.ts
$ node code.js
Immer noch gutes Wetter.
{% endhighlight %}

Der entstehende JavaScript-Code kann nun genau wie der von Hand
geschriebene in den Browser eingebunden werden.

`tsc` hat ein eigenes Format für Zeilen- und Spaltennummern in
Fehlermeldungen.  Wer die einzeiligen Fehlmeldungsbandwürmer nicht mag
oder seine IDE schon auf den de-Facto-Standard von `gcc` eingerichtet
hat, kann sich mit `perl` die Fehlermeldung umformatieren:

{% highlight sh %}
$ tsc code.ts
/mnt/slig-sda3/home/ghcjs/code.ts(1,5): error TS2011: Cannot convert 'string' to 'number'.
$ tsc code.ts 2>&1 | perl -pe 's/^(\S+)\((\d+),(\d+)\):(.*)$/$1:$2:$3:\n$4\n/'
/mnt/slig-sda3/home/ghcjs/code.ts:1:5:
 error TS2011: Cannot convert 'string' to 'number'.
{% endhighlight %}

`tsc` lässt sich übrigens auch im "watch"-Modus betreiben:

{% highlight sh %}
$ tsc -w code.ts
{% endhighlight %}

Nun können Sie `code.ts` im Editor bearbeiten, und bei jedem Schreiben
wird `tsc` die Datei neu übersetzen und ggf. Fehler ausgeben:

{% highlight sh %}
Recompiling (
    /mnt/slig-sda3/home/ghcjs/code.ts
    /mnt/slig-sda3/home/ghcjs/tmp/lib/node_modules/typescript/bin/lib.d.ts):
/mnt/slig-sda3/home/ghcjs/code.ts(1,5): error TS2011: Cannot convert 'string' to 'number'.

Recompiling (
    /mnt/slig-sda3/home/ghcjs/code.ts
    /mnt/slig-sda3/home/ghcjs/tmp/lib/node_modules/typescript/bin/lib.d.ts):
{% endhighlight %}


## Stay tuned... ##

Auf einige unbehandelte Themen möchte ich zum Abschluss nur kurz
verlinken:

- [sourceMaps](http://www.aaron-powell.com/posts/2012-10-03-typescript-source-maps.html),
  mit denen der Browser den TypeScript-Quellcode im debugger
  anzeigen kann, während er im Hintergrund durch die
  korrespondierenden Zeilen im generierten JavaScript-Code läuft;
- [Integration](http://www.codebelt.com/typescript/typescript-amd-with-requirejs-tutorial/)
  mit den üblichen Modulsystemen [requirejs/amd](http://requirejs.org) und
  [commonjs](http://commonjs.org);
- eine [Bibliothek von Interfaces](http://definitelytyped.org) mit einer langen
  Liste von JavaScript-Bibliotheken und Frameworks, mit denen Typfehler
  in Aufrufen in diese Bibliotheken beim Übersetzen abgefangen werden können;
- [tslint](https://github.com/palantir/tslint) für weitergehende Coding-Policy-Checks.

Der zweite Teil dieses Artikels erscheint in den nächsten Wochen.
Dort werde ich auf fortgeschrittenere und Abstraktionswerkzeuge in
TypeScript eingehen und sprachliche Feinheiten und
Designentscheidungen diskutieren.
