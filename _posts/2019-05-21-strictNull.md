---
layout: post
description: Keine Fehler wegen Null oder Undefined mit Typescript
title: "Keine Fehler wegen Null oder Undefined mit Typescript"
author: stefan-wehr
tags: ["TypeScript"]
---

Jeder Javascript Programmierer ist bestimmt schon mal über den Laufzeitfehler
_undefined is not a function_ oder auch _Cannot read property 'length' of null_ gestolpert.
Der Fehler tritt immer dann auf, wenn man mit einem Wert etwas machen möchte (z.B. als Funktion
aufrufen oder auf die `length` Property zugreifen), der Wert dann aber `undefined` oder
`null` ist. Auch Java oder C# Programmier kennen dieses Problem unter dem Name
`NullPointerException` oder `NullReferenceException`, und in C oder C++ gibt es das Problem
natürlich auch, hier stürzt das Programm gleich ab.

Die Idee einer speziellen `null`-Referenz, die überall anstelle einer "echten" Referenz verwendet
werden kann, geht auf Tony Hoare und ALGOL in den 1960er Jahren zurück. Tony Hoare nannte seine
Erfindung in der Retrospektive den
["billion dollar mistake"](https://www.infoq.com/presentations/Null-References-The-Billion-Dollar-Mistake-Tony-Hoare),
da die dadurch entstehenden Laufzeitfehler sehr häufig die Ursache von Bugs sind und somit
hohe Kosten verursachen.

Interessanterweise haben statisch getypte, funktionale Programmiersprachen diesen "billion dollar
mistake" nicht wiederholt. So gibt es z.B. in Haskell den Typ
[`Maybe`](http://hackage.haskell.org/package/base-4.12.0.0/docs/Data-Maybe.html), der die
Abwesenheit eines Werts explizit im Typsystem repräsentiert.

Aber auch in Sprachen wie Javascript hat man die Notwendigkeit erkannt, statisch erkennen
zu wollen, ob Werte `null` oder `undefined` sein können. So ist in
[Typescript](https://www.typescriptlang.org/), ein statisch getypter Javascript-Dialekt,
seit einiger Zeit die `strictNullChecks` Option verfügbar. Wir werden in diesem Artikel sehen,
wie man mit dieser Option Laufzeitfehler vermeiden kann, welche weiteren sinnvollen Optionen
es zur Fehlervermeidung gibt, wie man die `strictNullChecks` Option für eine große Codebasis
inkrementell einführen kann und ob man zwischen `null` und `undefined` unterscheiden sollte.

<!-- more start -->

## strictNullChecks ##

Schauen wir uns folgende Typescript-Funktion an, welche die Zahlen eines Arrays summiert:

``` typescript
function sumArray(arr: number[]): number {
    let sum = 0;
    arr.forEach(x => {
        sum = sum + x;
    });
    return sum;
}
```

(Typescript Code sieht so aus wie Javascript Code, mit dem Unterschied dass mittels des
Doppelpunkts Typsignaturen definiert werden, die vom Typescript Compiler überprüft werden.)

Der Aufruf `sumArray([1,2,3]` liefert dann logischerweise das Ergebnis `6`. Wenn man aber
`sumArray(null)` aufruft, kommt es zu einem Laufzeitfehler:

    TypeError: Cannot read property 'forEach' of null

Man kann diesen Fehler zur Laufzeit einfach verhindern, indem man dem Typescript-Compiler
die Option `--strictNullChecks` mitgibt. Dann bekommt man *schon beim Kompilieren* den folgenden
Fehler:

```
$ tsc --strictNullChecks sample.ts
error TS2345: Argument of type 'null' is not assignable to parameter of type 'number[]'.
sumArray(null)
         ~~~~
```

Betrachten wir einen weiteren Aufruf, nämlich `sumArray([1,2,undefined]`. Wenn man ohne
`--strictNullChecks` kompiliert und den Code dann ausführt, erhält man als Ergebnis `NaN`, also
"not a number". Dieses `NaN` entsteht durch Addition einer Zahl mit `undefined`. Wenn man
aber wieder das `--strictNullChecks` Flag verwendet, kommt es zu einem Kompilierfehler:

```
$ tsc --strictNullChecks sample.ts
error TS2322: Type 'undefined' is not assignable to type 'number'.
sumArray([1,2,undefined])
              ~~~~~~~~~
```

Bis jetzt haben wir die Verwendung von `null` oder `undefined` durch das `--strictNullChecks`
Flag verboten. Denn mit diesem Flag ist `null` oder `undefined` nicht mehr Zuweisungkompatibel
zu den Typen `number[]` und `number`. Was aber wenn die Funktion `sumArray` auch
`null` oder `undefined` als Argument oder als Array-Element unterstützen soll? Mit dem
`--strictNullChecks` Flag muss man dies explizit in den Typen modellieren. Das sieht dann so aus:

``` typescript
function sumArray2(arr: (number | null | undefined)[] | null | undefined): number {
    let sum = 0;
    arr.forEach(x => {
        sum = sum + x;
    })
    return sum;
}
```

An allen Stellen, an denen man `null` oder `undefined` erwartet, muss man das entsprechend
im Typ ausdrücken. So ist der Typ `(number | null | undefined)[]` ein Array, das Zahlen, `null`
oder `undefined` enthält. Der Typ `T[] | null | undefined` ist dann ein Array mit `T` als
Elementtyp, oder `null` oder `undefined`. (Wir werden am Schluss des Artikels sehen,
wie man diese doch etwas längliche Schreibweise kürzer bekommt, indem man nur einen der beiden
Werte `null` und `undefined` in seinen Programmen verwendet.)

Wenn man nun aber `sumArray2` mit `--strictNullChecks` kompiliert, bekommt man neue Kompilierfehler,
denn schließlich verwenden wir im Rumpf den Parameter `arr` und die Array-Elemente  ohne
zu Bedenken, dass sie `null` oder `undefined` sein könnten:

```
tsc --strictNullChecks sample.ts
error TS2533: Object is possibly 'null' or 'undefined'.
    arr.forEach(x => {
    ~~~

error TS2533: Object is possibly 'null' or 'undefined'.
    sum = sum + x;
                ~
```

Man behebt diese Kompilierfehler z.B. so:

``` typescript
function sumArray2(arr: (number | null | undefined)[] | null | undefined): number {
    if (!arr) {
        return 0;
    }
    let sum = 0;
    arr.forEach(x => {
        sum = sum + ((x === null || x === undefined) ? 0 : x);
    })
    return sum;
}
```

Durch eine Kontrollflussanalyse weiß der Typescript-Compiler, dass nach dem `if` die Variable
`arr` sicher ein Array ist und dass für die Addition der Wert der Variable `x` nur dann
verwendet wird, wenn der Wert eine Zahl ist. Damit liefert dann
`sumArray2([1,2,undefined])` das Ergebnis `3` und `sumArray2(null)` liefert `0`.

## Weitere sinnvolle Optionen zur Vermeidung von Laufzeitfehlern ##

Es gibt in Typescript eine ganze Reihe weiterer Compiler-Optionen zur Vermeidung von
Laufzeitfehlern. Eine vollständige Liste ist
[hier](https://www.typescriptlang.org/docs/handbook/compiler-options.html) zu finden. Man
kann diese Optionen entweder auf der Kommandozeile oder in der
[tsconfig.json](https://www.typescriptlang.org/docs/handbook/tsconfig-json.html)
Konfigurationsdatei angeben.

Die meiner Meinung nach wichtigsten Optionen zur Vermeidung von Laufzeitfehlern sind:

~~~
--alwaysStrict
--noFallthroughCasesInSwitch
--noEmitOnError
--noImplicitAny
--noImplicitReturns
--noImplicitThis
--strictBindCallApply
--strictFunctionTypes
--strictPropertyInitialization
--strictNullChecks
~~~

Man kann diese Liste kürzer schreiben, indem man das Metaflag `--strict` und
`--noImplicitReturns`, `--noEmitOnError` und `--noFallthroughCasesInSwitch` verwendet.

Neben dem `--strictNullChecks` Flag ist inbesondere `--strictFunctionTypes` wichtig,
denn es sorgt dafür, dass sich Funktionen gemäß Subtyping vernünftig verhalten. Dazu ist
es aber wichtig zu wissen, dass Typescript hier eine Unterscheidung zwischen
Methoden- und Funktionssyntax macht. Betrachten wir dazu folgendes Beispiel:

``` typescript
interface Name {
    firstName: string;
    lastName: string;
}

interface Formatter {
    // Important: use function syntax!
    formatName: (name: Name | undefined) => string | undefined
}

function format(fmt: Formatter) {
    fmt.formatName(undefined);
}

const myFormatter = {
    formatName(name: Name): string {
        return name.firstName + " " + name.lastName;
    }
}

format(myFormatter);
```

Wenn man den Code ohne `--strict` kompiliert, gibt es erst zur Laufzeit einen Fehler,
den `myFormatter` kann, anders als im Interface `Formatter` vorgesehen, mit dem Argument
`undefined` nicht umgehen. Mit dem Flag `--strict` gibt es aber für den Ausdruck
`format(myFormatter)` diesen Kompilierfehler:

     Type '(name: Name) => string' is not assignable to type '(name: Name | undefined) => string | undefined'.

Gemäß den üblichen Subtypregeln für Funktionen ist dieser Fehler auch richtig, denn eine Funktion
vom Typ `U => V` ist nur genau dann ein Subtyp einer Funktion vom Typ `S => T`,
wenn `V` ein Subtyp von `T` ist (covariant im Ergebnis) und `S` ein Subtyp von `U`
ist (contravariant im Argument). Dies ist hier nicht der Fall, denn `Name | undefined`
ist kein Subtyp von `Name`.

Wichtig ist hierbei allerdings, dass dieser Check nur greift wenn `--strict`
(bzw. `--strictFunctionTypes`) aktiv ist und wir außerdem Funktionssyntax
für den Typ der `formatName` Property im `Formatter` benutzen. Schreibt man `Formatter` mit
Methodensyntax

``` typescript
interface Formatter {
    formatName(name: Name | undefined): string | undefined
}
```

dann gibt es auch mit dem `--strict` Flag keinen Kompilier- sondern einen Laufzeitfehler.

## strictNullChecks inkrementell einführen ##

Unser Produkt [Checkpad](https://checkpad.de) hat nicht nur ca. 300.000 Zeilen Haskell Code
sondern inzwischen auch mehr als 200.000 Zeilen
Typescript Code. Wir haben dort alle oben erwähnten Compiler-Flags aktiviert. Allerdings
gab es zu Beginn der Einführung von Typescript in unserem Produkt die meisten der Flags noch gar
nicht, weshalb wir einen inkrementellen Weg zur Einführung finden mussten.

Manche Flags kann man einfach direkt für die ganze Codebasis anschalten, z.B. hat das
bei `--noFallthroughCasesInSwitch` oder `--noImplicitReturns` funktioniert. Aber insbesondere
das Flag `--strictNullChecks` war unmöglich auf einmal einzuführen, weil es dadurch
zu sehr vielen Kompilierfehlern gekommen wäre, die man unmöglich auf einmal fixen kann
(es sei denn man möchte für geschätzt zwei Woche die Entwicklung komplett einstellen).

Wir haben daher ein neues Kompiliertarget zur Einführung dieser Checks angelegt. Dieses
Kompiliertarget hat eine Einstiegsdatei `strictNullChecks.ts`. Diese Datei importiert
die Module, welche schon mittels `--strictNullChecks` kompiliert werden können. Um
es möglichst einfach zu machen, inkrementell neue Module zu diesem Target hinzuzufügen
haben wir initial in `strictNullChecks.ts` alle Module in auskommentierter Form importiert
und dafür gesorgt, dass Module mit wenigen Abhängigkeiten weiter oben stehen. Im Verlauf
der Einführung von `--strictNullChecks` hatte dann die `strictNullChecks.ts` Datei z.B.
so ausgesehen:

``` typescript
import "checkpad/util/array";
import "checkpad/util/datetime";
import "checkpad/util/eq";
import "checkpad/util/findefset";
import "checkpad/util/option";
import "checkpad/util/hashable";
import "checkpad/util/lazy";
import "checkpad/util/types";
import "checkpad/util/unit";
import "checkpad/util/uuid";
// import "checkpad/util/linked-list";
// import "checkpad/util/map";
// import "checkpad/util/cache";
// import "checkpad/util/color";
// import "checkpad/util/md5";
// ... viele weitere imports
```

Wenn man nun ein weiteres Module zu den `--strictNullChecks` dazu nehmen will, entfernt
man einfach den Kommentar beim obersten auskommentierten Import
(hier: `checkpad/util/linked-list`), fixt die Kompilierfehler
und ist mit diesem Modul fertig. Dadurch, dass Module mit wenigen Abhängigkeiten weiter oben
stehen, muss man immer nur die Fehler in dem Modul, welches man gerade hinzugefügt hat, fixen.
Die topologische Sortierung der Module gemäß ihrer Imports haben wir initial einmal mittels
[depcruise](https://github.com/sverweij/dependency-cruiser) berechnet. Für die
Einführung der `strict` Flags haben wir mit diesem inkrementelle Ansatz für 200.000 Zeilen
Typescript-Code effektiv ca. 5 Monate gebraucht.

## null oder undefined ##

In Javascript und damit auch in Typescript gibt es mit `null` und `undefined` zwei Konzepte,
um die Abwesenheit eines Werts zu kodieren. Die beiden Konzepte sind von der Semantik
leicht unterschiedlich, aber für die tägliche Arbeit quasi austauschbar. Wir haben
bei den Typsignaturen weiter oben gesehen, dass es etwas unhandlich ist, immer `null`
und `undefined` zu unterstützen.

Daher ist es prinzipiell wünschenswert, in einem Projekt entweder nur `null` oder nur `undefined`
zu verwenden.
[Douglas Crockford](https://www.youtube.com/watch?v=PSGEjv3Tqo0&feature=youtu.be&t=9m21s)
argumentiert, dass `undefined` das Konzept sein sollte, welches man verwendet. Der Grund hierfür ist
simpel: `undefined` ist in Javascript bzw. Typescript unvermeidbar, so sind z.B.
optionale Argument und Properties mittels undefined modelliert, die Standardfunktion
`find` auf Arrays liefert `undefined` wenn das Element nicht gefunden werden kann und
der Array-Zugriff mit ungültigem Index liefert ebenfalls `undefined`.

Wir werden daher auch in unserem Codebasis `null` durch `undefined` ersetzen. Dieser
Prozess ist aber noch nicht abgeschlossen. Bei dieser Umstellung ist es aber wichtig
das Flag `--noImplicitReturns` zu aktivieren, damit man Fälle entdeckt, in denen man
durch Weglassen eines `return` Statements aus Versehen `undefined` als Ergebnis
zurückliefert.
