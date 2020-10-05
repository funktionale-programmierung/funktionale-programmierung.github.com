---
layout: post
description: Ein Blick in die Untiefen eines Typsystems für JavaScript
title: "Dependently-Typed TypeScript"
author: lars-hupel
tags: ["typescript", "javascript"]
---

TypeScript ist eine Programmiersprache mit einigen Besonderheiten:
Im Gegensatz zu den allermeisten anderen getypten Programmiersprachen wurde sie als Aufsatz für JavaScript (JS) entwickelt.
Dabei hat Microsoft besonderen Wert darauf gelegt, dass sich die Sprache möglichst nahtlos in die bestehenden Ökosysteme (Node.js und Browser) integriert.
Das wird dadurch erreicht, dass die TypeScript-Syntax „bloß“ die Typen zu JavaScript hinzufügt und die Kompilierung aus dem Entfernen der Typannotationen besteht.
Damit hat sich TypeScript zum de-facto Standard entwickelt, wenn es darum geht, typsichere Anwendungen auf JS-Basis zu bauen.

<!-- more start -->

## Herausforderung JavaScript

JavaScript ist eine Sprache, deren frühe Versionen unbestritten mit der heißen Nadel gestrickt waren.
Der legendäre [Lightning-Talk “Wat” von Gary Bernhardt](https://www.destroyallsoftware.com/talks/wat) bringt dies auf den Punkt.
So kann man in JS grundsätzlich alles mit jedem addieren oder multiplizieren:

```javascript
> [] + []
''
> [] + {}
'[object Object]'
> {} * 0
NaN
```

Gewünscht ist solches Verhalten in den wenigsten Fällen.

Mittlerweile wurden gängige Features, wie Klassen-basierte Vererbung, in JavaScript eingeführt.
Diese Entwicklungen werden maßgeblich vom [TC39](https://github.com/tc39/), dem Standardisierungsgremium hinter JS, vorangetrieben.
In jedem neuen Standard sind mehr praktische Tools vorhanden, um bessere Software zu schreiben.
Als Beispiel sei die „optionale Verkettung“ genannt, die in anderen Sprachen (z. B. Kotlin) als „Elvis operator“ bekannt ist.

Das grundlegende Problem besteht aber weiterhin:
Sehr viel JavaScript-Code wird in Browsern ausgeführt.
Dort ist es schwierig bis unmöglich, mit den gängigen Observability-Praktiken Fehler festzustellen oder gar zu debuggen, da es eine unbegrenzte Anzahl an Computern gibt, die den Code im Browser ausführen.

## Typen, aber wie?

Die Idee von TypeScript ist also, die Vorteile von Typsystemen – nämlich das Erkennen von Fehlern, bevor eine Applikation ausgerollt wird – zum JavaScript-Ökosystem zu bringen.

Dummerweise ist dieses Ökosystem aber widerspenstig, denn Browser-APIs, Node.js und zahlreiche Bibliotheken sind komplett typbefreit entworfen worden.
Als Beispiel seien Event Listener in Node.js genannt:

```javascript
server.on("close", f);
server.on("request", g);
```

In beiden Fällen registrieren wir einen Callback auf dem `server`-Objekt mittels dessen `on`-Methode; zum einen für das Herunterfahren des Servers, zum anderen für eingehende Anfragen.
Allerdings haben die Callbacks unterschiedliche Typen.
`f` erhält einen optionalen Fehler.
`g` erhält ein Anfrage- und ein Antwort-Objekt, welche vom Request Handler bearbeitet werden können.

Klassische objektorientierte Sprachen wie Java können dies nicht abbilden.
Stattdessen müssen die Methoden per Name disambiguiert werden:

```javascript
server.onClose(f);
server.onRequest(g);
```

Doch in unserem Fall war JavaScript zu erst da.
Die TypeScript-Entwickler\*innen mussten diese (und viele andere Fälle) in ihrem Typsystem abbilden.

## Literale und Überladen

Die Typsignatur des `server`-Objekts sieht in TypeScript wie folgt aus:

```typescript
interface Server {
    on(event: "close", handler: (err?: any) => void): void;
    on(event: "request", handler: (request: Request, response: Response) => void): void;
}
```

In diesem oberflächlich sehr einfachen Anwendungsfall kann man bereits eine Reihe von interessanten Features erkennen.
Zum einen erlaubt TypeScript praktisch beliebiges Überladen von Methoden.
Dieses Überladen ist allerdings nur in Deklarationen zulässig.
Für jede Methode darf es nur höchstens eine Implementierung geben, die dann zur Laufzeit prüfen muss, welche konkrete Signatur aufgerufen worden ist.
Bei einem gegebenen Aufruf von `server.on` prüft der Typchecker also lediglich, ob mindestens eine überladene Deklaration passt, und überlässt den Rest der aufgerufenen Funktion.

Desweiteren erkennt man, dass der erste Parameter `event` als Typ entweder `"close"` oder `"request"` sein muss: sogenannte literale Typen.
Literale Typen sind in TypeScript für alle primitiven Werte zulässig:

```typescript
const a: 3 = 3;
const b: "foo" = "foo";
const c: 4 = 3; 
// Type '3' is not assignable to type '4'.
const d: undefined = undefined;
```

Sogar Array-Literale sind zulässig:

```typescript
const e: [1, "foo"] = [1, "foo"];
```

Diese dienen in JavaScript als Ersatz von Tupeln.

## Objekte und Eigenschaften

Während obiges z. B. auch in Scala realisierbar ist, hat TypeScripts Typsystem auch einige Alleinstellungsmerkmale.
Ein gängiges Programmiermuster in JavaScript ist es, einer Funktion ein beliebiges Objekt zu übergeben, sowie eine Liste von Strings, welche Namen von Feldern in diesem Objekt sind:

```javascript
const obj = { x: 3, y: 4, z: 5 };

validateObject(obj, ["x", "y"]);
```

Unsere Erwartung an TypeScript ist hier, dass ein Aufruf mit dem String `"a"` einen Typfehler produzieren sollte.

Bevor wir uns das genauer anschauen können, müssen wir uns kurz den Property-Zugriff in JavaScript verschaffen.
Folgende beide Zeilen sind äquivalent zueinander:

```javascript
const x = obj.x;
const x = obj["x"];
```

Objektzugriffe können ähnlich wie Arrays mit eckigen Klammern erfolgen.
In den meisten objektorientierten Programmiersprachen ist die Aussage _`"x"` ist ein Feld in `obj`_ kein richtiger Typ; mit anderen Worten, wir können diese Aussage nicht als Annotation hinschreiben.
In TypeScript ist das sehr wohl möglich.
Der Raum aller gültigen Schlüssel eines Objekttyps `T` wird in TypeScript als `keyof T` notiert.

Wir können die obige Validierungsfunktion also wie folgt mit einer Signatur versehen:

```typescript
function validateObject<T>(t: T, props: Array<keyof T>): void;
```

Bei einem falschen Aufruf beschwert sich der Compiler:

```typescript
validateObject(obj, ["a"]);
// Type '"a"' is not assignable to type '"x" | "y" | "z"'
```

Hier wurde inferiert, dass der Schlüsselraum von `obj` genau dem Summentyp `"x" | "y" | "z"` entspricht.
Allerdings hört die Mächtigkeit des Typsystems dort auf, wo man z. B. die Eindeutigkeit von Array-Elementen prüfen möchte.
Der folgende, möglicherweise ungewollte Aufruf, wird akzeptiert:

```typescript
validateObject(obj, ["x", "x"]);
```

## Enten und Typen

Das Beispiel mit der Objektvalidierung hat im Vorbeigehen auch noch ein anderes Feature von TypeScript ausgenutzt.
Wir haben `obj` als Objektliteral definiert, ohne eine Klasse oder ein Interface anzugeben.
Entsprechend inferiert TypeScript diesen Typen:

```typescript
obj: { x: number; y: number; z: number; }
```

Doch selbst wenn wir ein Interface für dieses Objekt definiert hätten:
Interfaces und sogar Klassen sind für TypeScript nichts anderes als Typ-Aliase, denn im JavaScript-Universum herrscht [_Duck Typing_](https://de.wikipedia.org/wiki/Duck-Typing).
Konsequenterweise sind sämtliche [Typprüfungen auf Objekttypen strukturell](https://wiki.c2.com/?StructuralSubtyping).
Das ist an sich nichts neues (einige ML-artige Sprachen nutzen dies für Records).
TypeScript hat es aber im großen Stil umgesetzt, denn die klassischen OOP-Sprachen nutzen ausschließlich [nominales Subtyping](https://wiki.c2.com/?NominativeAndStructuralTyping).

Für diese strukturellen Typen gibt es in TypeScript nur wenige Ausnahmen; z. B. werden gleich benannte private Felder in Klassen als verschieden betrachtet, wenn diese in separaten Dateien definiert sind.
Dies war eine gezielte Designentscheidung, um die Vermischung von Implementierungsdetails verschiedener Bibliotheken zu vermeiden.

## Narrowing

Bevor wir uns tatsächliche Berechnungen mit Typen anschauen können, möchte ich noch kurz ein weiteres Compiler-Feature zeigen, welches sonst nur aus Sprachen wie Agda und Coq bekannt ist: _Dependent Pattern Matching_.
Stellen wir uns folgende Typdefinitionen vor:

```typescript
interface Rectangle {
    type: "rect";
    height: number;
    width: number;
}

interface Circle {
    type: "circle";
    radius: number;
}

type Shape = Circle | Rectangle
```

Wollen wir nun eine Funktion schreiben, die für beliebige Formen funktioniert, dann kann man eine (Laufzeit-)Fallunterscheidung machen, bei der der Compiler anhand der Bedingung den Typen einschränkt:

```typescript
function draw(shape: Shape) {
    switch (shape.type) {
        case "rect":
            const { height, width } = shape;
            break;
        case "circle":
            const { radius } = shape;
    }
}
```

Im Fall `"rect"` muss es sich um ein `Rectangle` handeln, so dass der Compiler Zugriff auf `height` und `width` gewährt.
Die Nutzung von `switch` ist nicht notwendig; auch anderer Kontrollfluss (z. B. `if`) wird analysiert.

Obwohl der Mechanismus, statische Information aus dynamischen zu extrahieren, zum Standardrepertoire von abhängig-getypten Programmiersprachen gehört, handelt es sich bei TypeScript eher um einen "glücklichen Unfall".
Der folgende Code funktioniert nämlich nicht:

```typescript
function scale<T extends Shape>(shape: T): T {
    switch (shape.type) {
        case "rect":
            const { height, width } = shape;
            return {
                type: "rect",
                height: height * 2,
                width: width * 2
            };
        // ...
    }
}
```

Nicht nur, dass hier `shape` nicht auf `Rectangle` eingegrenzt werden kann, auch das `return`-Statement kann so nicht funktionieren.[^foot-unsound]
In bestimmten Fällen kann es aber auch umgekehrt vorkommen, dass der Compiler offensichtlich inkorrekte Ausdrücke akzeptiert, was zwei gängige Ursachen haben kann:

1. der Typ `any` wird an unerwartetet Stelle inferiert (was man durch ein Compiler-Flag verhindern kann); ein Typ, der mit allen beliebigen Ausdrücken kompatibel ist
2. man läuft in eine [Unsoundness des Compilers](https://www.typescriptlang.org/docs/handbook/type-compatibility.html)

Abgesehen von diesen Einschränkungen ist das Narrowing in TypeScript derart mächtig, dass es sehr oft in der Praxis benutzt wird.

Der Vollständigkeit halber sei noch erwähnt, dass es in der reinen Lehre der Typsysteme nicht gestattet ist, dass sich der Typ ein und desselben Symbols (hier `shape`) auf Basis seiner lexikalischen Verwendung ändert.
Doch da z. B. auch [Kotlin](https://kotlinlang.org/docs/reference/typecasts.html) mit dieser Tradition gebrochen hat, ist dieser Kampf – genau wie der Kampf gegen die Begriffe „Transpilation“ und „isomorphes JavaScript“ – längst verloren.

## Werte in Typen

Wir haben bisher einige Konstrukte gesehen, die die bei klassischen ML-artigen Typsystemen gegebene Barriere zwischen Typen und Werten aufweicht.
Als Zwischenstand können wir festhalten, dass TypeScript an einigen Stellen (Wert-)Literale in Typausdrücken zulässt.
Anhand dieser Literale können z. B. Elementtypen ausgerechnet oder Varianten einer überladenen Methode selektiert werden.

Aber hier hört es noch lange nicht auf.
Wir können auch primitive Berechnungen mit Typen machen.

Als Beispiel zum Einstieg können wir die `Server`-Definition so umschreiben, dass keine Methodenüberladung notwendig ist:

```typescript
// mit Überladung
interface Server {
    on(event: "close", handler: (err?: any) => void): void;
    on(event: "request", handler: (request: Request, response: Response) => void): void;
}

// ohne Überladung
type ServerHandler<T extends "close" | "request"> =
    T extends "close" ?
        (err?: any) => void :
        (request: Request, response: Response) => void
         

interface Server {
    on(event: "close" | "request", handler: ServerHandler<typeof event>): void;
}
```

In diesem Schnipsel taucht der berühmt-berüchtigte _ternäre Operator_ `cond ? yes : no` auf, allerdings auf Typebene.
Damit lassen sich Fallunterscheidungen je nach Subtyp durchführen; gültige Konditionen sind Prüfungen der Form `X extends Y`.
Ferner ist mit `ServerHandler<typeof event>` ein genuin abhängiger Typ in der Signatur enthalten.
Es ist nicht nur ein Literal, was in einem Typen auftaucht, sondern eine Variable.

Wie zu erwarten, wird beim Aufruf von `on` korrekt der Typ des Callbacks inferiert:

```typescript
server.on("close", (err? /* any */) => {});
server.on("request", (req /* Request */, res /* Response */) => {})
```

Diese Schreibweise hat allerdings einen Pferdefuß.
Möchte man dieses Interface implementieren, dann könnte man geneigt sein, folgendes in der `on`-Methode zu schreiben:

```
if (event === "close") {
  handler();
} else {
  // ...
}
```

Das Narrowing von TypeScript schlägt auch hier fehl: Die Einschränkung von `event` auf das Literal `"close"` wird nicht auf `handler` propagiert.

Man kann sich hier auch wieder mit Casten begnügen; andernfalls bleibt nur noch, weitere Indirektionen zu bemühen.
In diesem Artikel soll es aber eher um das Typsystem gehen, daher werde ich nicht weiter auf die Implementierung eingehen.[^foot-ts]

Zu guter Letzt möchte ich die Typen noch etwas weiter vereinfachen.
Der obige `ServerHandler`-Typ macht zwar, was er soll, skaliert aber sehr schlecht auf weitere Varianten.
Ein gängiges Muster in TypeScript ist daher, einen Phantom-Typen zu definieren:

```typescript
interface ServerHandler {
    close: (err?: any) => void;
    request: (request: Request, response: Response) => void
}
    
interface Server {
    on(event: keyof ServerHandler, handler: ServerHandler[typeof event]): void
}
```

Instanzen von `ServerHandler` werden hier nicht gebraucht; stattdessen dient das Interface nur dazu, eine Reihe von Typen an Namen zu hängen.
Dazu kann mit dem `keyof`-Konstrukt der Name des Events auf die bekannten Events eingeschränkt werden.
Statt spitzen Klammern nutzt man dann eckige Klammern, um den korrekten Handler-Typ zu selektieren.

## Fazit

Der Wunsch, möglichst viele JavaScript-Konstrukte mit Typen zu versehen, gebiert mit TypeScript ein mächtiges Typsystem, welches zweifelsohne den Sprung in den Mainstream geschafft hat.
Mühelos zieht seine Ausdrucksstärke an der von Javas Generics vorbei.
Einige Designentscheidungen wirken für die „alte Schule“ befremdlich, sind aber im JS-Kontext durchaus sinnvoll.
Da ist es verschmerzbar, wenn das Typsystem sich manchmal selbst im Wege steht.

[^foot-unsound]: Streng genommen liegt der Compiler aber hier sogar richtig, denn die angegebene Implementierung ist nicht korrekt: Man könnte `scale` mit einem echten Subtypen von `Rectangle` aufrufen, aber `scale` würde nur ein `Rectangle` zurückliefern (d.h. Objekt-Eigenschaften verwerfen); nichtsdestoweniger würde der Compiler aber auch eine korrekte Implementierung nicht ohne Casts akzeptieren.
[^foot-ts]: Tatsächlich passiert das relativ oft, dass man in TypeScript zwar komplexe Typberechnungen hinschreiben kann, aber der Compiler nicht in der Lage ist, einen Ausdruck mit einem solchen Typen korrekt zu erfassen. Das erzeugt die etwas seltsame Situation, dass manche Typen in TypeScript zwar _de jure_ bewohnt sind, _de facto_ aber nicht. Man kann das mit der Situation in Rust vergleichen, wo manche Konstrukte, die nach außen hin fein säuberlich linear getypt sind, intern mit `unsafe`-Blöcken implementiert werden müssen.

<!-- more end -->
