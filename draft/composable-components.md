---
layout: post
description: Ein Modell für Komponierbare Webkomponenten basierend auf React
title: "Komponierbare Komponenten"
author: david-frese
tags: ["ClojureScript", "JavaScript", "Clojure", "Web", "Webkomponenten", "Reacl", "Reacl-c"]
---

Dieser Artikel stellt ein Modell für wirklich _komponierbare
Komponenten_ vor, aufbauend auf der beliebten Bibliothek
[React](https://reactjs.org/). Grundkenntnisse in JavaScript und React
werden vorrausgesetzt.

<!-- more start -->

## Funktionskomposition

Unter Komposition versteht man in der funktionalen Programmierung ganz
allgemein aus zwei Dingen einer Art ein Ding derselben Art zu
machen. Dabei kann es je nach Art dieser Dinge verschiedenste
Möglichkeiten der Komposition geben.

Ein klassisches Beispiel sind Funktionen. Wenn man sich auf einstellige
Funktionen beschränkt, kann man diese z.B. leicht hintereinander als
auch nebeneinander[^juxt] ausführen:

```javascript
var f = function (x) { return g(h(x)); }       // hintereinader

var f = function (x) { return [g(x), h(x)]; }  // nebeneinander
```

Hier wird aus zwei einstelligen Funktionen `g` und `h` eine neue
einstellige Funktion `f` gebaut. Im "hintereinander" Fall wird das
Ergebnis der ersten als Argument für die zweite Funktion benutzt. Im
"nebeneinander" Fall wird ein Tupel aus den jeweiligen Ergebnissen
von `g` und `h` zurückgegeben.

Die Einschränkung auf einstellige Funktionen stellt dabei keine
grundsätzliche Einschränkung der Ausdrucksstärke dar, da man jede
Funktionen mit mehreren Argumenten immer in eine Funktion mit nur
einem Argument umwandeln kann, indem man z.B. alle Argumente in ein
Tupel packt.

Ein wichtiges Merkmal der funktionalen Programmiersprachen ist, dass
Funktionen "first class values" sind, d.h. sie sind genau wie Zahlen
oder Strings _Werte_ der Programmiersprache. Und das wiederum
bedeutet, dass man sogenannte "higher-order Funktionen" schreiben
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
var f = comp(g, h)   // hintereinader

function juxt(g, h) {
  return function(x) { [g(x), h(x)] }
}
var f = juxt(g, h)   // nebeneinander
```

Die Ergebnisse der Kompositionen (hier jeweils `f`) sind dann die
gleichen Funktionen wie oben, aber eben viel kürzer definiert,
mithilfe der Kombinatoren `comp` bzw. `juxt`.

## Webkomponenten

Soviel zur Funktionskomposition, die recht einfach ist wenn man sich
auf einstellige Funktionen beschränkt. Ausserdem ist das einzige was
Funktionen bei Benutzung "leisten", einmalig aus einem Argument ein
Ergebnis zu berechnen. In diesem Artikel soll es ja aber um die
Komposition von Webkomponenten gehen. Unter Webkomponenten versteht
man in der Regel etwas, das bei seiner Benutzung wesentlich länger
"aktiv" ist als eine aufgerufene Funktion. Webkomponenten sind Teil
einer Webanwendung, in der ein User über eine längere Zeit Eingaben
tätigt, oder sich ändernde Informationen angezeigt bekommt.

Man kann Webkomponenten daher als etwas betrachten, das wiederholt mit
neuen Daten "aufgerufen" wird, und, losgelöst davon, mehrfach neue
Daten zurückgeben kann.[^events] Das kann im Allgemeinen sehr komplex
sein: die eine Komponente hat zwei Eingaben und drei Werte die durch
sie verändert werden sollen, die nächste 2 Eingaben und eine
"Ausgabe". Dies führt in der Praxis zu sehr spezifischen Komponenten
mit sehr viel langweiligem "Boilerplate-Code" und geringer
Wiederverwendbarkeit. Analog zu Funktionen können wir diese
Komplexität aber reduzieren um eine bessere Komponierbarkeit zu
ermöglichen.

Dazu definieren wir Komponenten als etwas mit nur einer Eingabe und
einem "Rückgabewert" oder Ausgabe, vom selben Typ. Da Rückgaben mehr
als einmal möglich sein sollen (ggf. sogar 0 mal), modellieren wir die
Rückgabe mit einem Callback, der mehrfach von der Komponente
aufgerufen werden kann um einen neuen Wert zu signalisieren. Eine
einfache Komponente zur Modifikation eines Strings durch den User kann
in React dann z.B. so aussehen:

```jsx
function textinput(value, onChange) {
  return <input type='text'
                value={value}
                onChange={function(ev) { onChange(ev.target.value) }}/>;
}
```

Zu beachten ist, dass die Funktion `textinput` selbst die Komponente
darstellt, nicht deren Rückgabe. Jede Funktion die einen `value` und
einen `onChange`-Callback als Argument haben, wollen wir als
Komponente bezeichnen. Die Rückgabe dieser Funktion muß dann ein
fertiges "React-Element" sein; in diesem Fall ein INPUT-Element.

Solche Funktionen sind in React-Anwendungen gar nicht unüblich, aber
man ist dort eben nicht auf nur eine Ein-/Ausgabe beschränkt. Dadurch
gibt es allerlei "React-Komponenten", die nicht diesem Schema
folgen. Analog zu den Funktionen ermöglicht diese Einschränkung aber
die Implementierung allgemein verwendbarer Kombinatoren. Und ebenfalls
analog zu den einstelligen Funktionen, ist dies keine Einschränkung
der Ausdrucksstärke von Komponenten, da man mehrere Ergebnisse oder
Argumente immer in einen Wert zusammenpacken kann.

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

Die Funktion `cdiv` kann man analog zu "higher-order Funktionen" als
"higher-order Komponente" bezeichnen, oder auch einfach als
_Kombinator_ für Komponenten.[^hoc]

In diesem Fall findet keine Änderung an den eingehenden oder
ausgehenden Werten der Komponenten `c1` und `c2` statt. Die erzeugte
Komponente gibt den Wert den sie bekommt direkt "nach unten" an `c1`
und `c2` weiter, und jede Änderung, egal von welcher Komponente, wird
direkt "nach oben" durchgereicht.

Das ist natürlich selten ausreichend. Man will zum Beispiel eine
Komponente erzeugen, die, anstatt auf einem String, auf einem
bestimmten Feld eines Objekts arbeitet, das einen String enthält. Wir
können dazu eine Funktion `focus` schreiben, die uns ausgehend vom
Namen des Feldes `field` und einer bestehenden Komponente `c` so eine
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

Die Funktion `focus` ist zwar genau genommen kein Kombinator, aber
ebenfalls eine "higher order"-Komponente. Sie nimmt eine Komponente
als Argument und gibt eine neue Komponente zurück.

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

Eine Komponente muß übrigens die obligatorische Eingabe oder den
Callback gar nicht benutzen. Um zum Beispielt einfach nur einen
statischen Text anzuzeigen, bietet sich folgende Definiton an:

```jsx
function text(str) {
  function (value, onChange) {
    return <React.Fragment>{str}</React.Fragment>
  }
}
```

Damit kann man "neben" die Input-Felder ganz leicht noch einen Text
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
vorher. Die Komponenten die von der Funktion `text` zurückgegeben
werden, bekommen zwar auch ein `value` und ein `onChange` Argument in
die Hand, aber sie benutzen diese ja nicht. Dadurch ist es ihnen egal
ob sie ein Objekt oder ein String bekommen, und man kann man sie quasi
beliebig mit anderen Komponenten kombinieren.

## Fazit

Wir haben jetzt also ein Modell für Komponenten gefunden, das uns
wirklich _komponierbare Komponenten_ gibt, inklusive der Definition
von Funktionen die Komponenten als Argumente erwarten und neue
Komponenten zurückgeben können. Durch die Einschränkung auf genau
einen Wert und einen Callback für alle Komponenten, ist die Definition
von allgemeinen und wiederverwendbaren Kombinatoren und "higher-order
Komponenten" möglich. Dies reduziert Boilerplate-Code und schafft
mächtige Abtraktionsmöglichkeiten.

## Reacl-c

Die hier vorgestellten Implementierungen stellt nur die Grundidee für
komponierbare Komponenten in vereinfachter Form dar.  Um produktiv
einsetzbar zu sein, braucht es ein etwas verfeinertes Modell und noch
eine ganze Reihe von Erweiterungen und Details in der Schnittstelle zu
React, die den Umfang dieses Artikels sprengen würden. Wir, die Active
Group GmbH, haben dafür aber eine Bibliothek namens
[reacl-c](https://github.com/active-group/reacl-c) implementiert, die
diese Idee in ClojureScript vollständig realisiert. Sie ist in vielen
Projekten für unsere Kunden produktiv im Einsatz.

## Fußnoten:

[^juxt]: Mit "nebeneinander" ist hier nicht die gleichzeitige
    oder parallele Ausführung von Funktionen gemeint.

[^events]: Ein weiterer Aspekt sind Signale oder Ereignisse, die von
    einer Webkomponente ausgehen können, wie zum Beispiel das Klicken
    auf einen Button. Diesen Aspekt wollen wir in diesem Artikel aber
    nicht weiter betrachten.

[^hoc]: In der Dokumentation von React taucht der Begriff "higher
    order component" auch auf, wird dort aber gar nicht für
    Kompositionsfunktionen verwendet, sondern nur zur Vorbelegung von
    Eigenschaften von Klassen oder sog. "function components", die
    ohne diese Eigenschaften gar nicht benutzbar wären. Diese machen
    also nicht aus zwei Dingen einer Art eine neue Komponente der
    selben Art.
