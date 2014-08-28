---
layout: post
description: Ein Streifzug durch die Features von TypeScript
title: "Ein Streifzug durch die Features von TypeScript (Teil 2)"
author: matthias-fischmann
tags: ["JavaScript", "TypeScript", "Datentypen"]
---


Im [ersten Teil]({% post_url 2014-07-17-typescript %}) dieses Artikels
über [TypeScript](http://www.typescriptlang.org/) ging es um die
grundlegende Idee der Sprache und den Umgang mit dem Compiler.  Im nun
vorliegenden zweiten und vorläufig letzten Teil werden wir das Typ-
und Modulsystem weiter durchleuchten und die Grenzen des Typsystems
ausloten.



## `class` ##

JavaScript ist ein seltenes Exemplar der Familie der im engeren
Wortsinn _objektorientierten_ Programmiersprachen: Im Gegensatz zu
_klassenorientierten_ Sprachen wie C# und Java bilden Objekte das
Zentrum der Programmierkonstrukte.  Klassen werden mit Hilfe von
Objekten implementiert, und zwar (weitgehend) ohne dabei die
Runtime erweitern zu müssen.  Mit TypeScript rücken Klassen wieder
mehr ins Zentrum.

TypeScript-Klassen bestehen aus Methoden, Attributen, und einem
Konstruktor, für die es jeweils eine eigene Syntax und eigene
semantische Feinheiten zu lernen gibt:

{% highlight javascript %}
class Resource {
    public data : Object;
    public path : string;
    private metaData : { creator : string; creationDate : string };

    constructor(public contentType : string) {
        this.data = {};
    }

    public getMetaData() {
        return this.metaData;
    }
}

var r = new Resource("application/png");

console.log(r.getMetaData());
{% endhighlight %}

Werfen wir einen Blick auf den generierten JavaScript-Code:

{% highlight javascript %}
var Resource = (function () {
    function Resource(contentType) {
        this.contentType = contentType;
        this.data = {};
    }
    Resource.prototype.getMetaData = function () {
        return this.metaData;
    };
    return Resource;
})();

var r = new Resource("application/png");

console.log(r.getMetaData());
{% endhighlight %}

Zur Erinnerung: JavaScript-Klassen sind Objekte, genauer Funktionen,
noch genauer Objekt-Konstruktoren.  Das JavaScript-Schlüsselwort `new`
legt ein leeres Objekt an, legt die Konstruktorfunktion unter
`.prototype` auf diesem Objekt ab, und ruft diese dann auf, wobei
`this` auf das angelegte Objekt zeigt.  Bei diesem Konstruktor-Aufruf
werden die Attribute initialisiert; die Methoden werden als Attribute
im Konstruktor registriert, und damit unter `prototype` im Objekt
abgelegt.

Attribute werden mit einem der beiden Schlüsselworte `public` oder
`private` gekennzeichnet.  TypeScript erlaubt die Deklaration von
Attributen an zwei Stellen: im Klassenblock oder in den
Konstruktorparametern.  Attribute, die dort nicht erwähnt werden,
dürfen auch sonst nirgends gesetzt oder gelesen werden.  Die folgende
`Resource`-Methode proviziert also einen Compilerfehler:

{% highlight javascript %}
    public setNixGlob() {
        this.arg = false;
    }
{% endhighlight %}

Darüberhinaus hat eine TypeScript-Attributdeklaration keine Wirkung.
`path` wird nirgends initialisiert, daher verschwindet er vollständig
aus dem generierten Code.

`public`, `private` werden ebenfalls nur für die Typchecks (und im
Fall von Konstruktorargumenten für die Kennzeichnung als Attribut)
verwendet.  Die Zeile:

{% highlight javascript %}
console.log(r.metaData);
{% endhighlight %}

provoziert zwar einen Fehler, der sich aber mit

{% highlight javascript %}
console.log((<any>r).metaData);
{% endhighlight %}

leicht umgehen lässt (siehe weiter unten).  Zu den Ecken und Kanten
des Typsystems, und wie man damit umgehen kann, kommen wir gleich.



## `extends`, `implements` ##

Klassen können von anderen Klassen erben:

{% highlight javascript %}
class PngResource extends Resource {
    constructor(imageData) {
        super("application/png");
        this.data = {
            imageData: imageData
        }
    }

    public render() {
        ...
    }
}
{% endhighlight %}

Der Konstruktor kann den Konstruktor der Superklasse über `super`
erreichen (es ist sogar ein Compiler-Fehler, wenn er das nicht tut).

Wir haben bereits
[kurz gezeigt]({% post_url 2014-07-17-typescript %}), wie man mit
Interfaces Typen von Objekten mit einem eigenen Namen versehen kann.
Interfaces sind auch bei der Entwicklung von Klassen eine wertvolle
Abstraktionshilfe.  Damit kann man z.B. einfach formulieren, dass
`PngResource` ein Attribut `.data.imageData` hat:

{% highlight javascript %}
interface HasImageData {
    data: {
        imageData: Object
    }
}

class PngResource extends Resource implements HasImageData {
...
{% endhighlight %}

Eine Klasse kann nur von einer anderen Erben (_keine_ Multiple
Vererbung), aber beliebig viele Interfaces implementieren.

Man könne dieses Beispiel auch mit einer reinen Klassenhierarchie
`Resource` - `ResourceWithImageData` - `PngResource` implementieren.
Der generierte Code ist dann aber umfangreicher: Interfaces sind wie
Typen in TypeScript und im Gegensatz zu Klassen reine
Compiler-Artefakte.  Sie verschwinden zur Laufzeit spurlos.

Durch Interfaces lassen sich viele Dinge sehr kompakt ausdrücken, für
die man mit Unit-Tests deutlich mehr schreiben muss:

{% highlight javascript %}
interface IHasTemplateURL {
    templateUrl : string;
}

class Directive implements IHasTemplateURL {
    // ...
}
{% endhighlight %}

Statt:

{% highlight javascript %}
class Directive implements IHasTemplateURL {
    // ...
}

        // ...  (an einer ganz anderen Stelle in der Jasmine Testsuite)
            it("has property 'templateUrl'", () => {
                expect((new UI.Directive(0)).templateUrl).toBeDefined();
            });
{% endhighlight %}

Interfaces können übrigens wie Klassen voneinander abgeleitet werden:

{% highlight javascript %}
interface IMsg {
    header : string;
    details : string[];
}

interface IMsgGeo extends IMsg {
    location : string;
}

var msg : IMsgGeo = {
    header: "Die Sonne scheint",
    details: [],
    location: "Wien"
};
{% endhighlight %}



## `import` ##

TypeScript unterstützt ECMAScript6-Module.  Solange diese noch nicht
von der runtime unterstützt werden, übersetzt der Compiler diese
wahlweise nach commonjs (z.B. für [Node.js](http://nodejs.org)) oder
amd (z.B. für [requirejs](http://requirejs.org)):

{% highlight sh %}
$ tsc --help | grep module
  -m KIND, --module KIND        Specify module code generation: 'commonjs' or 'amd'
{% endhighlight %}

Module erlauben ein 1:1-Mapping zwischen Modulen und Dateien.  Die
Dateien (aka Module):

{% highlight sh %}
./Database.ts
./UserMgmt/Login.ts
./UserMgmt/Register.ts
./Main.ts
{% endhighlight %}

Können durch das `import`-Schlüsselwort aufeinander zugreifen.  In
module "UserMgmt/Login" etwa:

{% highlight javascript %}
import db = require("../Database");

var handle = db.connect("[connect coordinates]");
{% endhighlight %}

Modulnamen sind also (relative) Pfadnamen im Dateisystem (ohne die
Endung `.ts`).

Zusätzlich lassen sich über eine spezielle Syntax sogenannte
Description-Dateien einbinden.  Descriptions enden auf `.d.ts` und
enhalten Typinformationen zu Bibliotheken und Frameworks, die in
JavaScript geschrieben sind und in TypeScript-Module importiert werden
sollen.  Descriptions lösen sich dem Übersetzen auf.

Die Verwendung von jquery kann dann z.B. so aussehen:

{% highlight javascript %}
/// <reference path="../lib/DefinitelyTyped/requirejs/require.d.ts"/>
/// <reference path="../lib/DefinitelyTyped/jquery/jquery.d.ts"/>

...

var storeCB = (
    data : IMsgGeo,
    status : string,
    jqXHR : JQueryXHR
) : void => {
    ...
}

$.get("api/48791", storeCB);
{% endhighlight %}

TypeScript stellt nun sichr, dass der Typ von `storeCB` den
Erwartungen von `$.get` entspricht.

Neben der Typisierung von aufrufendem Code eigenen sich
Description-Dateien erstaunlich gut zur Dokumentation.  Wenn man die
dortigen Typen und die dazugehörigen Kommentare liest, hat man oft
schon alle Informationen über eine benötigte Stelle in einer komplexen
API zusammen.

Der [TypeScript Definition Manager](http://definitelytyped.org/tsd)
ist ein Werkzeug zum Management einer Typ-Datenbank für viele
verbreitete JavaScript-Bibliotheken (eine Art "npm für Typen").  (Ich
habe in der Praxis gute Erfahrungen damit gemacht, die Datenbank
direkt aus github zu installieren und zu verwenden, allerdings steigt
das Volumen stetig an und wird wohl langsam etwas unhandlich.)



## Der `any`-Typ ##

Um mit existierendem JavaScript-Code arbeiten zu können, ist das
Typsystem nicht immer mächtig genug, und würde zu einer Vielzahl von
Typfehlern führen.  Um das kontrolliert zu vermeiden, gibt es den Typ
`any`.

Namen, die den Typ `any` haben, können in jedem Kontext ohne Typfehler
verwendet werden, d.h. sie haben insbesondere jedes Attribut, das
gewünscht wird, und alle Attribute haben immer jeden gewünschten Typ.

Dieses Feature lässt sich für verschiedene Geschmäcker unterschiedlich
einsetzen: Einerseits kann man mit dem einfachen Trick,
`(<any>someThing)` zu schreiben statt `someThing`, jeden Typfehler
ausschalten (nicht sehr sicher, aber mag z.B. beim Schreiben von
Unit-Tests, die ohnehin nicht produktiv eingesetzt werden, akzeptabel
sein).  Andererseits lässt sich über ein Compiler-Flag die
automatische Inferenz von `any` ausschalten.  Dann bekommt man so
lange Typfehler, bis man an alle Stellen, an denen die Typinferenz
fehlgeschlaten ist, einen Typ geschrieben hat.



## Generische Typen ##

TypeScript unterstützt generische Typen.  Ein generisches Interface
für Listen kann beliebige Elementtypen unterstützen, aber gleichzeitig
immer sichergestellen, dass beim Zugriff auf eine konkrete Liste nicht
der falsche Elementtyp verwendet wird.

Da dieser Artikel schon recht lange ist, sei hier nur ein kurzes
Beispiel genannt und ansonsten auf die [weitere
Dokumentation](http://www.typescriptlang.org/) verwiesen:

{% highlight javascript %}
interface List<T> {
    head: T;
    tail: T[];
    insert: T -> void;
}
{% endhighlight %}



## TypeScript Kung-Fu ##

Bei der täglichen Arbeit mit TypeScript stößt man doch noch öfters auf
Typfehler, die erst zur Laufzeit erkannt werden.  Das liegt in der
Natur der Sache, besonders wenn man beim Übersetzen auf
`--noImplicitAny` verzichtet.  Oft kann man aber mit etwas geschicktem
Refactoring doch noch einen Compiler-Fehler aus `tsc` herauskitzeln.

Das folgende Stück Code ist etwas überraschend _kein_ Fehler:

{% highlight javascript %}
var f = (count : number, optArgs : { field ?: boolean }) : void => { return; };
var x = f(2, 2);
{% endhighlight %}

Das liegt daran, dass der minimale Typ von `arg` das leere Objekt ist
(`field` ist optional).  Jeder Typ ist aber eine Spezialisierung des
leeren Objekts, also gilt auch `2 : {}`, und damit `2 : { field ?:
boolean }`.  Die Funktion `f` wird nun sehen, dass `arg.field` nicht
definiert ist und hoffentlich nicht überrascht sein.

Um dieses Problem zu vermeiden, muss mindestens ein nicht-optionales
Attribut in `arg` vorkommen, das nicht im Typ `number` enthalten ist.
Bei Funkionen mit optionalen Keyword-Argumenten z.B. kann man einfach
ein Objekt nehmen, das nicht nur die optionalen, sondern alle
Argumente der Funktion als Attribute enthält:

{% highlight javascript %}
var f = (args : { count : number; field ?: boolean }) : void => { return; };
{% endhighlight %}

Ein viel häufigeres Problem im TypeScript-Alltag, besonders wenn man
von Haskell kommt, ist, dass `null` und `undefined` immer den Typ
haben, der vom Kontext erwartet wird.  Man muss sich also weiter wie
in JavaScript immer zur Laufzeit vergewissern, dass ein Ausdruck
initialisiert ist.  Das folgende ist kein Typfehler, führt aber zu
einer Laufzeit-Exception:

{% highlight javascript %}
var x : { count : number };
var y : number = x.count;
{% endhighlight %}

Diese Klasse von Fehlern wäre in stärker getypten Sprachen wie
[Haskell]({% post_url 2014-07-25-haskell-einstieg %}) durch das
Typsystem ausgeschlossen; das Typsystem von TypScript ist aber nicht
rigoros genug, und daher hilft hier weiterhin nur
Programmierdisziplin.

Als drittes und letztes Beispiel möchte ich auf das oben skizzierte
jquery-Ajax-Beispiel zurückkommen:

{% highlight javascript %}
var storeCB = (
    data : IMsgGeo,
    status : string,
    jqXHR : JQueryXHR
) : void => {
    ...
}

$.get("api/48791", storeCB);
{% endhighlight %}

Dieser Code versucht, Typkorrektheit an der Schnittstelle zwischen
einer JavaScript-Runtime und einem Webserver am anderen Ende einer
Netzwerkverbindung herzustellen.

Das funktioniert teilweise: Wenn die Funktion `storeCB` einen
Typ-Fehler begeht, kann der Compiler das sehen und berichten; Wenn
aber der Server einen Typfehler macht, ist der Compiler machtlos.  Zur
Compilezeit werden alle Typinformationen gelöscht, und zur Laufzeit
ist `storeCB` eine dynamisch getypte Funktion ohne jegliche checks.

Dies ist eine grundsätzliche Beschränkung von statisch getypten
Sprachen: An der Schnittstelle zu anderen Softwaresystemen müssen die
Daten validiert und Fehler dynamisch abgefangen werden.  TypeScript
kann aber dabei helfen, diese Fehler an einer Stelle zu lokalisieren,
wo sie sofort nach Eintreffen der fehlerhaften Daten abgefangen
werden.  Nämlich im Konstruktor einer Klasse, die den Typ der
erwarteten Daten repräsentiert:

{% highlight javascript %}
class IMsgGeo {
    public header : string;
    public details : string[];
    public location : string;

    constructor(data : {
        header : string;
        details : string[];
        location : string;
    }) {
        this.header = data.header;
        this.details = data.details;
        this.location = data.location;
    }
}

var storeCB = (
    data : IMsgGeo,
    status : string,
    jqXHR : JQueryXHR
) : void => {
    scope.message.push(new IMsgGeo(data));
}
{% endhighlight %}

Wenn `storeCB` nun mit einem `data`-Objekt aufgerufen wird, das nicht
den Typ `IMsgGeo` hat, dann wird der Konstruktor eine Exception
werfen.

Dieser Ansatz hat auch Nachteile: Es ist schon bei diesem einfachen
Typ recht lang und mühevoll zu schreiben; und um wirklich alle
dynamischen Typfehler abzufangen, wären die Implementierung weiterer
Tests notwenig (ist `data.header` wirklich ein String, und nicht
wieder ein Objekt?  Sind alle Elemente von `data.details` Strings?).
Außerdem werden "überstehende" Attribute, die vom Server geliefert,
aber von `IMsgGeo` nicht gefordert werden, stillschweigend gelöscht,
statt Laufzeit-Typfehler zu provozieren.

Das Problem der Laufzeit-Kosten von umfassenden Typchecks kann
entschärft werden, indem die Checks nur im Development-Modus
ausgeführt und im produktiven Code ausgeschaltet werden.

Um den entstehenden Aufwand für das Schreiben von Boilerplate-Code zu
minimieren, lohnt sich vielleicht schon in einem größeren
Web-Anwendungsprojekt das Schreiben eines Tools.


## Schluss ##

TypeScript ist ein großartiges Refactoring-Tool: Mit etwas Übung kann
man sich einen Stil aneignen, in dem fast alle Fehler, die durch
Änderung oder Entfernung von Namen auftreten, direkt beim Speichern im
Editor durch den Compiler finden.

Die Dokumentation und Struktur einer großen JavaScript-Codebasis
erfordert sehr viel Disziplin.  Mit TypeScript entsteht viel Struktur
von alleine und wird bei jedem build auf Konsistenz überprüft.

Leider wurde bei TypeScript der gleiche Fehler gemacht wie bei
[mypy](http://mypy-lang.org/): `null` und `undefined` haben jeden Typ.
Der Grund liegt vermutlich in einem weiterne Manko, nämlich dem Fehlen
von Disjunktiven Typen, mit denen man immer aussagekräftige Typen für
Funktionen schreiben könnte, bei denen man das nicht sicherstellen
kann (für Leser mit Haskell-Kenntnissen: `Maybe`).

Ansätze wie [fay](https://github.com/faylang/fay/wiki),
[elm](https://en.wikipedia.org/wiki/Elm_%28programming_language%29),
[shade](https://github.com/takeoutweight/shade),
[ghcjs](https://github.com/ghcjs/ghcjs/) oder [haste](haste-lang.org)
sind TypeScript konzeptuell weit überlegen, aber (noch) nicht
ausgereift genug, um für einen Einsatz in der Praxis in Frage zu
kommen.  Außerdem ist die Einstiegshürde für das landläufige
Web-Entwicklungs-Team sehr viel höher als bei TypeScript.

Mein Fazit ist also fast durchweg positiv: Die Robustheit, Wartbarkeit
und Lesbarkeit des Codes verbessert sich enorm gegenüber JavaScript,
Werkzeuge, Bibliotheken und Frameworks aus JavaScript lassen sich
mühelos weiterverwenden, und selbst überzeugte Anwender un- oder
dynamisch getypter Programmiersprachen haben nicht allzuviele
Probleme, sich an die neuen Konzepte zu gewöhnen.
