---
layout: post
description: db-Monade in F#
title: "Zusammensetzbare Datenbank-API in F#"
author: andreas-bernauer
tags: ["F#"]
---

Für den Programmierer bietet der Einsatz einer Datenbank viele Fallstricke: Unabhängigkeit von einer bestimmten Datenbank-Implementierung, Fehlerbehandlung, verschachtelte Transaktionen, Testbarkeit, um nur ein paar zu nennen.

Wie lassen sich solche Fallstricke vermeiden oder wenigstens
entschärfen?
In unserem in F# geschriebenen Key-Value-Store [Active.Net.AddOnlyDb](https://github.com/active-group/Active.Net.AddOnlyDb), der eine schnelle und leichte Synchronisation zwischen mobilen Geräten erlaubt, haben wir eine zusammensetzbare Datenbank-API implementiert, welches sich zum Ziel gesetzt hat, genau diese Fallstricke zu vermeiden.
Anhand einer kleinen Beispiel-Funktion `update` zeige ich in diesem Artikel, wie die AddOnlyDb-API dem Programmierer hilft, die Fallstricke zu vermeiden.

<!-- more start -->

## AddOnlyDb ##

In AddOnlyDb speichert der Programmierer sogenannte Fakten, die für eine Anwendung von Interesse sind, zum Beispiel "Der Autor dieses Artikels ist 'Andreas Bernauer'".
Diese Aussage bezieht sich auf die Eigenschaft eines Objekts ("der /Autor/ dieses /Artikels/") und legt den Wert der Eigenschaft fest ("Andreas Bernauer").
AddOnlyDb repräsentiert Objekte durch eine globale eindeutige Zahl (eine GUID), Eigenschaften durch einen String ("Autor") und Werte als binären Blob (z.B. die UTF-8 Bytes von "Andreas Bernauer").

## Funktionale API ##

AddOnlyDb bietet unter anderem die Funktionen `get`, um Werte auszulesen, `put`, um Werte zu schreiben und `search`, um Werte zu suchen.

Eine einfache, naive `update`-Funktion in F# könnte also wie folgt aussehen:

{% highlight ocaml %}
let update guid prop value =
  let oldIds = search (Some guid, Some prop, None)
  put guid prop value (Meta.create()) oldIds
{% endhighlight %}

Die `update`-Funktion nimmt als Parameter die `guid` des betreffenden Objekts, die zu aktualisierende Eigenschaft `prop` und den neuen Wert `value`.
Zunächst sucht sie mit `search` die Id aller Fakten, welche sich momentan auf die zu aktualisierende Eingeschaft beziehen. Anschließend setzt sie mit `put` das neue Faktum, wobei es die alten Fakten als ungültig markiert.
`Meta.create()` liefert Meta-Information wie die aktuelle Uhrzeit, welche einem Endanwender beim Auflösen von Konflikten helfen sollen.

## Fallstricke ##

Die soeben vorgestellte `update`-Funktion würde so nicht funktionieren, denn ihr fehlt ein Verweis auf die Datenbank.
Reichen wir ihn also nach:

{% highlight ocaml %}
let update db guid prop value =
  let oldIds = search db (Some guid, Some prop, None)
  put db guid prop value (Meta.create()) oldIds
{% endhighlight %}

`update` nimmt als weiteres Argument einen Verweis `db` auf die Datenbank, den sie an die AddOnlyDb-Funktionen durchreicht. Dem Programmierer ist das vielleicht lästig, aber es scheint nicht weiter schlimm.
Allerdings wird damit der Typ der Datenbank festgelegt, was sich als hinderlich herausstellen kann, etwa wenn man zum Testen eine In-Memory-Datenbank verwenden möchte oder später die Datenbank wechseln möchte, sich jedoch nicht strikt an das generische Datenbank-Interface ODBC gehalten und nur kompatible SQL-Abfragen gestellt hat.

`update` hat ein weiteres Problem: zwischen dem Auslesen der alten Einträge und dem Setzen des neuen Eintrags sollten keine weiteren Einträge hinzukommen. `update` sollte also in einer Transaktion ausgeführt werden:

{% highlight ocaml %}
let update db guid prop value =
  startTransaction db
  let oldIds = search db (Some guid, Some prop, None)
  put db guid prop value (Meta.create()) oldIds
  commitTransaction db
{% endhighlight %}

Allerdings führen nun Fehler bei der Ausführung nicht dazu, dass ein ROLLBACK der Transaktion durchgeführt wird.

Selbst wenn wir dafür nun die passende `try...catch`-Anweisung einbauen, ist `update` immer noch nicht zufriedenstellend: da manche Datenbanken keine verschachtelten Transaktionen unterstützen, ist `udpate` nicht zusammensetzbar. 
Wenn der Programmierer `update` in einer anderen Funktion aufruft, welche selbst eine Transaktion öffnet (etwa um mehrere Eigenschaften eines Objekts gebündelt zu aktualisieren), muss der Programmierer spezielle Vorkehrungen treffen, um verschachtelte Transaktionen zu vermeiden.
Entweder er verfolgt selbst, ob eine Transaktion schon geöffnet ist oder er fragt den Datenbank-Treiber, sofern der dies unterstützt.
In jedem Fall wird `update` (und alle weiteren Funktionen, welche `update` aufrufen) abhängig von Zuständen, welche nicht in seiner Definition sichtbar sind.

## Zusammensetzbare Datenbank-API ##

In AddOnlyDb haben wir eine zusammensetzbare Datenbank-API entwickelt, welche
die dargestellten Fallstricke vermeidet und uns dazu noch erlaubt, Code zu schreiben, welcher der eingangs vorgestellten, naiven Version von `update` sehr nahe kommt:

{% highlight ocaml %}
let update guid prop value =
  atomically
  db {
    let! oldIds = search (Some guid, Some prop, None)
    do! put guid prop value (Meta.create()) oldIds
  }
{% endhighlight %}

Die Unterschiede zur naiven Version sind recht klein: `atomically`, um die Datenbank-Operationen in einer Transaktion auszuführen, `db { ... }`, welches die Datenbank-Operationen kapselt und `let!` sowie `do!`, um Ergebnisse von Datenbank-Operationen zu binden bzw. nur auszuführen.
Hier ist `db` keine Referenz auf eine Datenbank, sondern ein Konstrukt, das die Datenbank-API einleitet.
Wir kommen gleich darauf zurück.

Eine Anwendung von `update` könnte wie folgt aussehen:

{% highlight ocaml %}
let op = update guid prop value
let db = openDatabase ... // nötige Parameter
run db op
{% endhighlight %}

Die erste Zeile bindet `op` an (eine zusammengesetzte Reihe von) Datenbank-Operationen, unabhängig von der tatsächlichen Datenbank `db`, welche in der zweiten Zeile geöffnet wird.
Erst in der letzten Zeile führt `run` die Datenbank-Operationen in `op` mit der nun geöffneten Datenbank `db` zusammen und führt sie tatsächlich aus.

Hier lässt sich schon erkennen, wie die Fallstricke vermieden werden:

- In `op` stehen Datenbank-Operationen ohne Verweis auf eine Datenbank.
  Der Programmierer bleibt also bis zur tatsächlichen Ausführung unabhängig von einer bestimmten Datenbank-Implementierung.

- Da `op` unabhängig von einer bestimmten Datenbank-Implementierung ist, lässt es sich leichter testen, zum Beispiel, in dem die Datenbank-Operationen nur simuliert oder gegen eine In-Memory-Datenbank ausgeführt werden, welche in der Produktion nicht zur Verfügung steht.

- `run` orientiert sich an `atomically` und kümmert sich um das Öffnen und Schließen von Transaktionen.
  Es nimmt damit die Hürde, welche der Zusammensetzbarkeit der Datenbank-API im Wege stand.

Durch die zusammensetzbare Datenbank-API von AddOnlyDb kann sich der Programmierer auf die Funktionalität konzentrieren, welche er implementieren möchte, ohne sich um die Fallstricke einer Datenbank-Anbindung kümmern zu müssen.

## Implementierung ##

Wie haben wir die zusammensetzbare Datenbank-API implementiert?

In F# lässt sich dies in wenigen Schritten realisieren.
Man braucht hierzu vor allem zwei Dinge: 

- Einen Typ, welcher die Datenbank-Operationen repräsentiert (welche ja vorgehalten werden müssen, bis die tatsächliche Datenbank zur Verfügung steht), und 
- die Unterstützung der `db { ... }`-Syntax.

Den Typ, welcher die Datenbank-Operationen repräsentiert, nennen wir `Op`.
Welche Datenbank-Operationen wollen wir repräsentieren?
Als Minimum können wir festhalten: Daten lesen, Daten schreiben sowie Transaktionen.
Außerdem zeigt die Erfahrung, dass für die Zusammensetzbarkeit eine Operation nützlich ist, die nicht wirklich etwas tut.
Dass jede Datenbank-Operation Ergebnisse unterschiedlichen Typs liefert, stellen wir durch eine Typvariable dar.
Der `Op`-Typ sieht demnach in etwa so aus:

{% highlight ocaml %}
type 'a Op =
  | Result of 'a
  | Get of GuidT * PropertyT * (ValueT [] -> 'a Op)
  | Put of FactT * (HashT -> 'a Op)
  | Atomically of unit Op * (unit -> 'a Op)
{% endhighlight %}

`'a` ist die erwähnte Typ-Variable, `Result` die Operation, welche nicht wirklich etwas tut (sondern nur ein Ergebnis festhält), `Get` liefert Daten aus der Datenbank, `Put` schreibt Daten in die Datenbank (wobei der Typ `FactT` die Objekt-Guid usw. zusammenfasst) und `Atomically` führt eine Operation atomar aus (also in einer Transaktion).

Die Parameter für die Operation bergen bis auf die jeweils letzten wenig Überraschungen: `Get` benötigt die Objekt-Guid und Eigenschaft, die es auslesen soll, `Put` die Objekt-Guid, Eigenschaft und den Wert, den es schreiben soll und `Atomically` die Operation, welche es atomar ausführen soll.
Die jeweils letzten Parameter `(ValueT [] -> 'a Op)`, `(HashT -> 'a Op)`, sowie `(unit -> 'a Op)` stellen eine Art "callback" dar, eine Funktion, die von `run` aufgerufen wird, wenn das Ergebnis der Datenbank-Operation eingetroffen ist.
Dazu gleich mehr, sobald wir uns die Unterstützung der `db { ... }`-Syntax angeschaut haben.

## Computation Expression ##

In .Net heißen Konstrukte wie `db { ... }` ["Computation Expressions"]( https://msdn.microsoft.com/de-de/library/dd233182.aspx ) oder "Workflows".
Zur Implementierung definiert man einen Typ, der [eine Reihe von bestimmten Methoden]( https://msdn.microsoft.com/de-de/library/dd233182.aspx#Anchor_1 ) implementiert.
Für die `db { ... }`-Syntax genügt es, `Return` und `Bind` zu implementieren:

{% highlight ocaml %}
type DbBuilder() =
  member this.Return(v) = Result v             // : 'a → 'a Op
  member this.Bind(op, next) = bind op next    // : 'b Op → ('b → 'a Op) → 'a Op
{% endhighlight %}

Regelmäßige Leser unseres Blogs und sonstige Enthusiasten der Funktionalen Programmierung kommen `Return` und `Bind` bekannt vor: tatsächlich lassen sich mit Computation Expressions [Monaden](http://funktionale-programmierung.de/tags-archive.html#Monaden) implementieren.
Mit `Return` lässt sich ein Wert merken, wozu das `Result` des `Op`-Typs geeignet ist.
Mit `Bind` lassen sich zwei Datenbank-Operationen aneinanderfügen (also hintereinander ausführen), wobei `next` angibt, wie aus dem Wert vom Typ `'b` der ersten Datenbank-Operation eine Datenbank-Operation mit dem Wert vom Typ `'a` entsteht.
Die Implementierung von `bind` schauen wir uns gleich an.

Zur `db { ... }`-Syntax fehlt nur noch eine Instanz von `DbBuilder`:

{% highlight ocaml %}
let db = DbBuilder()
{% endhighlight %}

Tatsächlich ist das `db` in der `db { ... }`-Syntax also ein konkretes Objekt (das in verschiedenen Programmteilen auch ganz anders heißen könnte).

Wie kommt es von der `db { ... }`-Syntax zum eigentlichen Code?
Der F#-Compiler übersetzt den Ausdruck

{% highlight ocaml %}
db {
  let! oldIds = search (Some guid, Some prop, None)
  do! put guid prop value (Meta.create()) oldIds
}
{% endhighlight %}

beim Compilieren zu etwas wie

{% highlight ocaml %}
db.Bind(search (Some guid, Some prop, None), (fun oldIds →
  db.Bind(put guid prop value (Meta.create()) oldIds, (fun x → 
    db.Return ()))))
{% endhighlight %}

Das `let!` übersetzt der Compiler in ein `Bind`, das hier die beiden Datenbank-Operationen `search` und `put` verknüpft, wobei das Ergebnis von `search` als `oldIds` beim `put`-Aufruf zur Verfügung steht.
Das `do!` übersetzt der Compiler ebenfalls in ein `Bind`, wobei er das Ergebnis von `put` jedoch verwirft: statt dessen gibt er den leeren Wert "unit" `()` zurück.

### `bind` zum Verknüpfen von Operationen ###

Nun zur versprochenen Implementierung von `bind` und wie die "Callbacks" von `Op` ins Spiel kommen.
Die Typ-Signatur von `bind` ist praktisch durch die [Typ-Signatur von `Bind`](https://msdn.microsoft.com/de-de/library/dd233182.aspx#mt26) `'b Op → ('b → 'a Op) → 'a Op` vorgegeben.
Die Implementierung orientiert sich an der Struktur von `'b Op`:

{% highlight ocaml %}
let rec bind (opB: 'b Op) (next: 'b -> 'a Op) : ('a Op) =
  match opB with
  | Result b             -> next b
  | Get (g, p, callback) -> Get (g, p, (fun v →
                                        let cbOp = callback v
                                        bind cbOp next))
  | Put (d, cb)          -> Put (d, (fun v -> bind (cb v) next))
  | Atomically (op, cb)  -> Atomically (op, (fun () -> bind (cb ()) next))
{% endhighlight %}

Im Falle von `Result` gibt `bind` den gespeicherten Wert `b` an `next` weiter.
Interessanter sind die folgenden Fälle, wo `bind` den Callback der Operation mit `next` verbindet.
`bind` ersetzt hier den Callback durch einen, der den ursprünglichen Callback aufruft, um die Datenbank-Operation zu erhalten, welche er dann (durch endrekursiven Aufruf von `bind`) an `next` weiterreicht.
Zur besseren Illustration ist dies im Fall von `Get` ausgeschrieben.

### `run` zum Auswerten von Operationen ###

Bis jetzt haben wir nur Datenbank-Operationen gebaut und zusammengesetzt.
Zur eigentlichen Ausführung kommt es erst durch einen Aufruf von `run`.
`run` ist in AddOnlyDb mit den verschränkt rekursiven Hilfsfunktionen `runLoop` und `runTransaction` implementiert.

{% highlight ocaml %}
let run db (op0:'a Op) : 'a =
    runLoop db false op0
{% endhighlight %}

`runLoop` nimmt als Parameter einen Verweis auf die Datenbank, ein Flag, das angibt, ob sich die Ausführung in einer Transaktion befindet und die Datenbank-Operation, die es ausführen soll.
Das Ergebnis ist der Wert, der sich aus der Datenbank-Operation ergibt.
Die Implementierung von `runLoop` orientiert sich wieder an der Struktur von `Op`:

{% highlight ocaml %}
let rec runLoop<'a> db (inTransaction:bool) (op:'a Op) : 'a =
  match op with
  | Result v -> v
  | Get (guid, prop, cb) →
                        let resultOfGet = get db guid prop
                        let nextOp = cb resultOfGet
                        runLoop db inTransaction nextOp)
  | Put ((guid, prop, value, meta, obsoletes), cb) -> // ...
  | Atomically (op, cb) ->
      let res = runTransaction db inTransaction op
      runLoop db inTransaction (cb res)
{% endhighlight %}

Bei `Get` ruft `runLoop` die Datenbank-Funktion `get` auf, um aus der Datenbank zu lesen, erzeugt mit dem Ergebnis und dem Callback die nächste Datenbank-Operation, und berechnet rekursiv dessen Ergebnis.
Bei `Result` ist das Ergebnis der Wert, der im `Result` abgelegt ist.
Bei `Put` geht `runLoop` ähnlich vor.
Bei `Atomically` geht `runLoop` ebenfalls ähnlich vor, wobei das Ergebnis die andere Hilfsfunktion `runTransaction` berechnet:

{% highlight ocaml %}
and runTransaction<'b> db (inTransaction:bool) (op: 'b Op) : 'b =
  if inTransaction
  then runLoop db true opB
  else
    try
      openTransaction db
      let res = runLoop db true opB
      commitTransaction db
      res
    with
    | e ->
      rollbackTransaction db
      raise e
{% endhighlight %}

Falls schon eine Transaktion offen ist, muss `runTransaction` nichts weiter tun als mit `runLoop` das Ergebnis der Datenbank-Operation zu berechnen.
Andernfalls berechnet `runTransaction` das Ergebnis in einer Transaktion, die im Erfolgsfall abgeschlossen, im Fehlerfall rückabgewickelt wird.

Möchte der Programmierer nun die Datenbank-Operationen gegen eine andere Datenbank laufen lassen oder nur simulieren, kann er eine andere `run`-Funktion verwenden.
Diese könnte zum Beispiel das Ergebnis der `Get`-Operation aus einem Array oder einer Datei lesen.

## Zusammenfassung ##

In diesem Blog-Post habe ich gezeigt, wie in [AddOnlyDb](https://github.com/active-group/Active.Net.AddOnlyDb) die zusammensetzbare Datenbank-API aussieht, wie sie implementiert ist und wie sie die üblichen Fallstricke von Datenbank-Zugriffen vermeidet.
Möchte man selbst etwas ähnliches implementieren, überlegt man sich einen geeigneten Typ, der die auszuführenden Operationen repräsentiert, implementiert die relevanten Methoden eines Computation-Expression-Typs und definiert passende Auswertungsfunktionen.
Übrigens: AddOnlyDb gibt es auch als fertiges [nuget-Paket](https://www.nuget.org/packages/Active.Net.AddOnlyDb/) zum Einbinden in Ihr Projekt in Visual Studio.

<!-- more end -->

