---
layout: post
description: 
title: "MyOwnSafe - Funktionale Programmierung in der Praxis"
author: david-frese
tags: ["Praxis", "OCaml", "XML"]
---

Die Active Group hat die Webanwendung
[MyOwnSafe](http://www.myownsafe.de/) für die MyOwnSafe GmbH
entwickelt. Der Anwender kann in dieser Anwendung Informationen und
Dokumente zu seinen Versicherungen, seinem Vermögen und sonstige
persönlichen Informationen ablegen und pflegen, sowie Vorkehrungen
treffen um diese Informationen im Todes- oder Krankheitsfall
bestimmten Personen zugänglich zu machen.

Durch den Einsatz funktionaler Programmierung in der Sprache
[OCaml](http://ocaml.org/) konnte das Projekt extrem kostengünstig und
termingerecht umgesetzt werden. Die Kernentwicklung wurde von einer
Person in gerade einmal sechs Monaten abgeschlossen.

Dieser Artikel zeigt an einem kleinen Ausschnitt der Software, wie
funktionale Programmierung in OCaml die Entwicklung vereinfachen und
damit auch beschleunigen konnte.

<!-- more start -->

OCaml ist eine bereits seit über fünfzehn Jahren etablierte und
gefestigte Variante aus der ML-Sprachfamilie. Sie unterstützt neben
dem funktionalen Programmieren auch imperative und objekt-orientierte
Paradigmen, ein statisches Typsystem mit besonders mächtiger
Typinferenz, sowie ein gutes Modulsystem. Es gibt eine Vielzahl von
Bibliotheken, insbesondere zur Web- und Netzwerkprogrammierung, und
der Compiler erzeugt sehr performanten nativen Code. Letzteres gab auch
den Ausschlag zu der Entscheidung für diese Sprache in diesem Projekt,
da ein Betrieb des Servers als einfache CGI-Anwendung hinter einem
simplen Webserver einen deutlichen Sicherheitsgewinn gegenüber einer
komplexeren, und damit fehleranfälligeren Architektur wie PHP oder
einem Java Application Server verspricht.

## Hintergrund

Höchste Vertraulichkeit dieser sehr sensiblen Daten der Kunden ist
durch Client-seitige RSA- und AES-Verschlüsselung bereits im Browser
sichergestellt - bisher einzigartig in diesem Bereich. Nicht einmal
der Betreiber selbst kann die Informationen also einsehen, wodurch
eine kostengünstige und skalierbare Speicherung der Daten in der
Amazon Cloud (Amazon Webservices) möglich wurde.

Für die Speicherung von Kreditkarten- und Kontodaten der Kunden, und
die Abbuchung von Beträgen über diese Bezahlverfahren, wurde ebenfalls
ein externer Dienstleister in Anspruch genommen, und zwar die Firma
[ExperCash](http://www.expercash.de). Da ExperCash eine Schnittstelle
anbietet, bei der MyOwnSafe niemals selbst Kenntnis der
Kreditkarten-Daten erlangt, ist keine teure Zertifizierung durch die
Kreditkarten-Unternehmen notwendig. Ein großer Kostenvorteil für ein
Start-Up.

Diese Anbindung an den externen Zahlungsdienstleister ist es auch, die
in diesem Artikel als Beispiel dienen soll.

## Aufgabe

Um einen bestimmten Betrag vom Kunden einzuziehen, leitet die
Client-Software den Kunden zunächst auf eine Seite von Expercash
weiter, auf der er über eine SSL-verschlüsselte Verbindung seine
Konto- oder Kreditkartendaten eingibt, und den Betrag autorisiert. Als
Ergebnis davon erhält die MyOwnSafe-Software lediglich einen
eindeutigen Schlüssel, den sogenannten _Payment-Authorization-Key_.

Die eigentliche Ausführung der Abbuchung erfolgt dann vom Server aus
über einen HTTP-Request, bei dem einer der Parameter dieser
_Payment-Authorization-Key_ ist:

   https://xyz.de/pay?key=authkey4711

Als Ergebnis erhält man eine XML-Struktur, aus der man noch ablesen
muss, ob die Abbuchung erfolgreich war, oder welcher Fehlergrund
vorliegt. Wenn sie erfolgreich war, erhält man außerdem noch eine
sogenannte _Payment-Id_, mit der man den Abbuchungsvorgang später
identifizieren kann:

{% highlight xml %}
   <payresult>
     <rc>300</rc>
   </payresult>
{% endhighlight %}

oder im Erfolgsfall:

{% highlight xml %}
   <payresult>
     <rc>100</rc>
     <payment_id>pid1233</payment_id>
   </payresult>
{% endhighlight %}

## Implementierung

Man benötigt also eine Funktion, die einen `payment_key` nimmt, und
entweder zurückliefert dass ein Fehler mit einem bestimmten Fehlercode
passiert ist, oder dass der Aufruf erfolgreich war und eine bestimmte
`payment_id` vergeben wurde.

Die möglichen Rückgabewerte bestehen aus einer festen Anzahl von
Möglichkeiten (zwei) mit jeweils unterschiedlichen zugehörigen Daten.
Für diese _Aufzählungstypen_ gibt es in OCaml, wie in vielen funktionalen
Programmiersprachen, eine sehr komfortable Unterstützung. Die Syntax
dafür sieht folgendermaßen aus:

{% highlight ocaml %}
  type payment_result = PaymentSuccess of string | PaymentFailure of int
{% endhighlight %}

`PaymentSuccess` und `PaymentFailure` bezeichnet man dabei als
Konstruktoren für Werte vom Typ `payment_result`, die in diesem Fall
jeweils einen String, beziehungsweise eine Zahl als Parameter erwarten.

Ebenso leicht kann man Typ-Aliase definieren, also alternative Namen
für existierende Typen, wie zum Beispiel für den Parameter der
Funktion:

{% highlight ocaml %}
  type payment_auth_key = string
{% endhighlight %}

Die Signatur der zu implementierenden Funktion ist damit also in der
OCaml-Syntax:

{% highlight ocaml %}
  val do_payment : payment_auth_key -> payment_result
{% endhighlight %}

Zur Implementierung verwenden wir eine HTTP-Client-Bibliothek, sowie
eine XML-Parser-Bibliothek. Zur Vereinfachung gehen wir davon aus,
dass die HTTP-Client-Bibliothek folgende Schnittstelle bietet:

{% highlight ocaml %}
  type http_result = int * string
  type url = string

  val http_get : url -> http_result
{% endhighlight %}

Zwei Typen mit einem Stern dazwischen definieren in OCaml den Typ von
Tupeln aus Werten der jeweiligen Typen. Der Ausdruck für die Erzeugung
von Tupeln besteht dann einfach aus zwei Ausdrücken getrennt durch ein
Komma, wie wir weiter unten gleich sehen werden.

Zunächst noch zum XML-Parser, von dem wir vereinfachend annehmen, dass er
folgende Typen und Funktionen anbietet:

{% highlight ocaml %}
  type xml_node =
    Elem of (string * xml_node list)
    | Attr of (string * string)
    | Text of string

  val parse_xml : string -> xml_node
{% endhighlight %}

Das besonders komfortable Feature von OCaml, das einem bei der nun
folgenden Implementierung der Funktion `do_payment` hilft, ist das
sogenannte _Pattern-Matching_:

{% highlight ocaml %}
  let do_payment authkey =
    match http_get (ec_url authkey) with
      (200, body) -> parse_ec_body body
      | _ -> failwith "HTTP call failed"
{% endhighlight %}

Zur Erklärung ein Wort zu Funktionen: `let f p1 p2 = ...` definiert
eine Funktion `f` mit zwei Parametern, und der Ausruck `f 1 2` ruft
diese Funktion mit den beiden Zahlen als Argumente auf.

Das Pattern-Matching selbst ist der Ausdruck `match ... with ...`
innerhalb der Funktion `do_payment`. Er definiert eine sehr mächtige
_Fallunterscheidung_ abhängig von dem Wert des Funktionsaufrufs von
`http_get` mit dem Rückgabewert einer kleinen Hilfsfunktion `ec_url`
als Argument (die Definition folgt weiter unten). Die _Tests_ sind
dabei jeweils auf der linken Seite den Pfeils `->`. Von dem ersten
Fall der _passt_ wird der Ausdruck rechts vom Pfeil ausgewertet. Das
Pipe-Symbol `|` trennt dabei zwei Fälle und kann als _oder_ gelesen
werden.

Pattern-Matching leistet dabei drei Dinge:

1. Dekonstruktion von Werten: In diesem Fall ist das Ergebnis von
   `http_get` ein Tupel. Mit der gleichen Syntax mit dem Tupel
   konstruiert werden, können sie innerhalb des Pattern-Matching wieder
   in die beiden Bestandteile, eine Zahl und einen String,
   _dekonstruiert_ werden.

2. Vergleich mit konstanten Werten: Der Test des ersten Falls besteht
   zum Beispiel aus dem konstanten Ausdruck `200`. Dies bewirkt, dass
   der Test nur erfolgreich ist, wenn von http_get zurückgegebene
   Status-Code gleich 200 ist.

3. Bindung von Bestandteilen an neue Namen: Hier ist das der Rumpf der
   HTTP-Antwort, den wir in dem Test nicht genauer prüfen, sondern
   durch die Angabe eines Bezeichners (`body`) angeben, dass wir diesen
   Bestandteil des Tupels auf der rechten Seite dieses Falls noch
   weiter verwenden möchten; genauer als Argument für die Funktion
   `parse_ec_body` die gleich folgt.

Letzter Fall eines Pattern-Matching ist wie hier häufig ein
_else_-Clause, der in OCaml durch eine sogenannte anonyme Bindung mit
dem Unterstrich angegeben werden kann. In diesem Fall wird hier eine
Exception mit `failwith` ausgelöst.

Nun noch zu den fehlenden Hilfsfunktionen und der Funktion zum
Analysieren der XML-Struktur, die ebenfalls von Pattern-Matching
gebrauch macht:

{% highlight ocaml %}
  let ec_url authkey = "https://xyz.de/pay?key=" ^ authkey

  let parse_ec_body body =
    let fields =
      match parse_xml body with
        Elem ("payresult", children) -> List.map xml_field children
        | _ -> failwith "Unexpected XML structure"
    in
    match List.assoc "rc" fields with
      "100" -> PaymentSuccess (List.assoc "payment_id" fields)
      | rc -> PaymentFailure (int_of_string rc)
 
  let xml_field node =
    match node with
      Elem (tagname, [Text v]) -> (tagname, v)
      | _ -> failwith "Unexpected content in XML structure"
{% endhighlight %}

Im ersten Pattern-Matching auf das Ergebnis von `parse_xml body` sieht
man, dass Pattern-Matching nicht nur auf eingebaute Werte wie Tupel
möglich ist, sondern, ohne weiteren Code schreiben zu müssen, auch auf
den neu definierten Aufzählungstyp `xml_node`. In diesen Fällen
_zerlegt_ man die Werte häufig anhand der Konstruktoren des Typs; hier
interessieren uns allerdings nur Element-Knoten mit dem Namen
`"payresult"` und deren Liste der Kind-Knoten.

Mit dieser Liste der Kind-Knoten führen wir ein _map_ aus, d.h. wir
werten die Funktion `xml_field` auf jeden einzelnen Kind-Knoten aus,
und sammeln die Ergebnisse in einer neuen Liste. Diese Liste wird dann
aus Tupeln bestehen, und ist damit eine sogenannte Assoziations-Liste,
eine einfachen Art von Dictionary, in dem man mit der Funktion `assoc`
aus dem Modul `List` nach dem ersten Teil der Tupel suchen kann und
den zweiten Teil zurück erhält.

In der Funktion `xml_field` geschieht eine weitere Art von
Pattern-Matching, und zwar auf eine Liste von Werten. In diesem Fall
passt der erste Test nur, wenn `node` ein Element-Knoten mit
beliebigem Namen (gebunden an `tagname`) ist, und seine
Kindknoten-Liste aus einem einzigen Text-Knoten besteht, dessen Inhalt
nicht weiter getestet wird, aber an den Namen `v` gebunden werden
soll.

## Fazit

Das Zerlegen von Datenstrukturen, sowie das Überprüfen und
Weiterverarbeiten einzelner Bestandteile, ist eine immer
wiederkehrende häufige Aufgabe von Programmen bzw. deren
Programmierern. In Sprachen ohne Pattern-Matching benötigt man für so
eine Zerlegung in der Regel sehr viele Zeilen Code, mit entsprechnd
hoher Wahrscheinlichkeit für Fehler. Pattern-Matching wie in Ocaml
bietet demgegenüber eine extrem kurze, prägnante Syntax für dieses
häufige Muster, und trägt damit besonders zur Beschleunigung der
Entwicklung und Minimierung der Fehlerrate bei.

Die Active Group setzt daher auch weiterhin auf OCaml und andere
mächtige funktionale Programmiersprachen, um robuste Programme so
effizient wie möglich zu entwickeln.
