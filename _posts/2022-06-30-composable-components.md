---
layout: post
description: Ein Modell für Komponierbare Webkomponenten basierend auf React
title: "Komponierbare Komponenten"
author: david-frese
tags: ["ClojureScript", "JavaScript", "Clojure", "Web", "Webkomponenten", "Reacl", "Reacl-c"]
---

Dieser Artikel stellt ein Modell für wirklich _komponierbare
Webkomponenten_ vor, aufbauend auf der beliebten Bibliothek
[React](https://reactjs.org/). Komponierbarkeit ist ein Schlüssel zu
guter Testbarkeit und maximal wiederverwendbarem Code in der
funktionalen Programmierung. Grundkenntnisse in JavaScript und React
werden vorausgesetzt.

<!-- more start -->

## Funktionskomposition

Unter Komposition versteht man in der funktionalen Programmierung ganz
allgemein aus zwei Dingen einer Art, ein Ding derselben Art zu
machen. Dabei kann es je nach Art dieser Dinge verschiedenste
Möglichkeiten der Komposition geben.

Ein klassisches Beispiel sind Funktionen. Wenn man sich auf einstellige
Funktionen beschränkt, kann man diese z. B. leicht hintereinander oder
auch nebeneinander[^juxt] ausführen:

```javascript
var f = function (x) { return g(h(x)); }       // hintereinander

var f = function (x) { return [g(x), h(x)]; }  // nebeneinander
```

Hier wird aus zwei einstelligen Funktionen `g` und `h` eine neue
einstellige Funktion `f` gebaut. Im "hintereinander" Fall wird das
Ergebnis der ersten als Argument für die zweite Funktion benutzt. Im
"nebeneinander" Fall wird ein Tupel aus den jeweiligen Ergebnissen
von `g` und `h` zurückgegeben.

Die Einschränkung auf einstellige Funktionen stellt dabei keine
grundsätzliche Einschränkung der Ausdrucksstärke dar, da man jede
Funktion mit mehreren Argumenten immer in eine Funktion mit nur
einem Argument umwandeln kann, indem man z. B. alle Argumente in ein
Tupel packt.

Ein wichtiges Merkmal der funktionalen Programmiersprachen ist, dass
Funktionen "first class values" sind, d. h. sie sind genau wie Zahlen
oder Strings _Werte_ der Programmiersprache. Und das wiederum
bedeutet, dass man sogenannte _Higher-Order-Funktionen_ schreiben
kann. Das sind Funktionen die andere Funktionen als Argument erhalten,
oder neue Funktionen erstellen und zurückgeben können. Damit kann man
auch Funktionen schreiben, die eine bestimmte Art der
Funktionskomposition implementieren. Die Hintereinanderausführung wird
dabei oft `comp` (kurz für "compose"), die Nebeneinanderausführung oft
`juxt` (kurz für "juxtapose") genannt:

```javascript
function comp(g, h) {
  return function(x) { g(h(x)) }
}
var f = comp(g, h)   // hintereinander

function juxt(g, h) {
  return function(x) { [g(x), h(x)] }
}
var f = juxt(g, h)   // nebeneinander
```

Die Ergebnisse der Kompositionen (hier jeweils `f`) sind dann die
gleichen Funktionen wie oben, aber eben viel kürzer definiert,
mithilfe der Kombinatoren `comp` bzw. `juxt`.

## Webkomponenten

Soviel zur Funktionskomposition, die recht einfach ist, wenn man sich
auf einstellige Funktionen beschränkt. In diesem Artikel soll es ja
aber um die Komposition von Webkomponenten gehen. Als Webkomponente
wollen wir hier einen Teil einer Webanwendung oder Webseite verstehen,
in die ein User über eine längere Zeit Eingaben tätigen kann, oder in
der sich ändernde Informationen angezeigt werden.[^events]

Das kann im Allgemeinen sehr komplex sein: Die eine Komponente hat
zwei Eingaben und drei Werte, die durch sie verändert werden sollen,
die nächste zwei Eingaben und eine "Ausgabe". Dies führt in der Praxis
zu sehr spezifischen Komponenten mit sehr viel langweiligem
"Boilerplate-Code" und geringer Wiederverwendbarkeit. Analog zu
Funktionen können wir diese Komplexität aber reduzieren, um eine
bessere Komponierbarkeit zu ermöglichen.

Dazu definieren wir Komponenten als etwas, das auf einem einzelnen
Wert eines bestimmten Typs operiert. Die Komponente sollte dabei einen
solchen Wert anzeigen können und, als Reaktion auf eine Aktion des
Users, eine Änderung dieses Werts veranlassen können. Dies entspricht
in etwa den sogenannten _controlled components_ in React. Das ist,
genauso wie bei den Funktionen, keine Einschränkung der Allgemeinheit
von Komponenten, da aus mehreren Werten immer ein einzelner gemacht
werden kann, und bei einer Änderung des Werts auch nur ein Teil
aktualisiert werden kann. Außerdem kann eine Webkomponente auch
_statisch_ sein, d. h. den aktuellen Wert ignorieren, und dem User
einfach keine Änderung ermöglichen.

Eine einfache Komponente zur Modifikation eines Strings durch den User
kann in diesem Modell dann z. B. so aussehen:

```jsx
function textinput(value, onChange) {
  return <input type='text'
                value={value}
                onChange={function(ev) { onChange(ev.target.value) }}/>;
}
```

Zu beachten ist, dass die Funktion `textinput` selbst die Komponente
darstellt, nicht deren Rückgabe! Jede Funktion, die einen `value` und
einen `onChange`-Callback als Argument hat, ist eine Komponente. Die
Rückgabe dieser Funktion muss dann ein fertiges "React-Element" sein;
in diesem Fall ein INPUT-Element.

Die Festlegung, dass jede Komponente so aussehen muss, ermöglicht uns
die Implementierung allgemein verwendbarer Kombinatoren und damit
die einfache Komponierbarkeit.

### Komponierbarkeit

Welche Arten von Kompositionen von Komponenten sind in diesem Modell nun
denkbar? Mit HTML-Elementen können zum Beispiel zwei Komponenten
"nebeneinander" gestellt werden. Hier mit einem DIV-Element:

```jsx
function cdiv(c1, c2) {
  function (value, onChange) = {
    return <div>{c1(value, onChange)}{c2(value, onChange)}</div>;
  }
}
```

Die Funktion `cdiv` ist dabei, analog zu den Funktions-Kombinatoren
`comp` und `juxt` von oben, ein _Kombinator für Komponenten_, die eine
neue Komponente auf Basis der beiden Komponenten `c1` und `c2`
zurückgibt.

In diesem Fall findet keine Änderung an den eingehenden oder
ausgehenden Werten der Komponenten `c1` und `c2` statt. Die erzeugte
Komponente gibt den Wert, den sie bekommt direkt "nach unten" an `c1`
und `c2` weiter, und jede Änderung, egal von welcher Komponente, wird
direkt "nach oben" durchgereicht.

Das ist natürlich selten ausreichend. Man will zum Beispiel eine
Komponente erzeugen, die, anstatt auf einem String, auf einem
bestimmten Feld eines Objekts arbeitet, das einen String enthält. Wir
können dazu eine Funktion `focus` schreiben, die uns, ausgehend vom
Namen des Feldes `field` und einer bestehenden Komponente `c`, so eine
modifizierte Komponente zurückgibt:

```javascript
function focus(field, c) {
  function (value, onChange) = {
    var c_value = goog.object.get(value, field)
    var c_onChange = function(v) {
      onChange(goog.object.set(value, field, v))
    }
    c(c_value, c_onChange)
  }
}
```

Die Funktion `focus` kann ebenfalls als Kombinator für Komponenten
bezeichnet werden: Sie nimmt eine Komponente als Argument und gibt
eine neue Komponente zurück.

Um aus der `textinput`-Komponente also nun eine Komponente zu
erzeugen, die das Feld `name` eines Objekts editierbar macht, können
wir jetzt einfach folgendes schreiben:

```javascript
focus('name', textinput)
```

Oder um zwei Input-Felder nebeneinander zu haben, von dem eins den
Nachnamen und eins den Vornamen anzeigt und für den User änderbar
macht:

```javascript
cdiv(focus('firstname', textinput),
     focus('lastname', textinput))
```

Zu beachten ist dabei, dass diese Webkomponenten auch _referenziell
transparent_ sind. D. h. obwohl die Komponente `textinput` hier zweimal
verwendet wird, erhält man natürlich zwei separate Eingabefelder in
der Webanwendung: die Komponenten haben keine Identität.

Eine Komponente muss übrigens, wie oben erwähnt, die obligatorische
Eingabe oder den Callback gar nicht benutzen. Um zum Beispiel einfach
nur einen statischen Text anzuzeigen, bietet sich folgende Definition
an:

```jsx
function text(str) {
  function (value, onChange) {
    return <>{str}</>
  }
}
```

Damit kann man neben die Input-Felder ganz leicht noch einen Text
stellen:

```javascript
cdiv(cdiv(text("Vorname:"), focus('firstname', textinput)),
     cdiv(text("Nachname:"), focus('lastname', textinput)))
```

Oder auch noch weiter abstrahieren:

```javascript
function labelled_textinput(label) {
  return cdiv(text(label), textinput)
}

cdiv(focus('fistname', labelled_textinput("Vorname:"))
     focus('lastname', labelled_textinput("Nachname:")))
```

Man beachte, dass die Verschachtelung hier eine andere ist als
vorher. Die Komponenten, die von der Funktion `text` zurückgegeben
werden, bekamen vorher ein Objekt als `value` übergeben, aber jetzt
einen String. Dadurch dass diese den Wert aber gar nicht benutzen oder
modifizieren, kann man sie quasi beliebig mit anderen Komponenten
kombinieren.

## Fazit

Wir haben jetzt also ein Modell für Komponenten gefunden, das uns
wirklich _komponierbare Komponenten_ gibt, inklusive der Definition
von Funktionen, die Komponenten als Argumente erwarten oder neue
Komponenten zurückgeben können. Dies reduziert Boilerplate-Code und
schafft mächtige Abtraktionsmöglichkeiten.

## Reacl-c

Die hier vorgestellten Implementierungen stellt nur die Grundidee für
komponierbare Komponenten in vereinfachter Form dar. Um produktiv
einsetzbar zu sein, braucht es ein etwas verfeinertes Modell und noch
eine ganze Reihe von Erweiterungen und Details in der Schnittstelle zu
React, die den Umfang dieses Artikels sprengen würden. Wir, die Active
Group GmbH, haben dafür aber eine Bibliothek namens
[reacl-c](https://github.com/active-group/reacl-c) implementiert, die
diese Idee in ClojureScript vollständig realisiert. Sie ist eine
Weiterentwicklung von
[Reacl](https://funktionale-programmierung.de/2017/06/29/reacl2.html),
und bereits in vielen Projekten für unsere Kunden produktiv im
Einsatz.

## Fußnoten:

[^juxt]: Mit "nebeneinander" ist hier nicht die gleichzeitige
    oder parallele Ausführung von Funktionen gemeint.

[^events]: Ein weiterer Aspekt sind Signale oder Ereignisse, die von
    einer Webkomponente ausgehen können, wie zum Beispiel das Klicken
    auf einen Button. Diesen Aspekt wollen wir in diesem Artikel aber
    nicht weiter betrachten.
