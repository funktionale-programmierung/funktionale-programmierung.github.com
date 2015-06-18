---
layout: post
description: "XML in Haskell - Datentypen und Serialisierung"
title: "XML in Haskell - Datentypen und Serialisierung"
author: david-leuschner
tags: ["Haskell"]
---

Die Frage "Wie parse oder erzeuge ich ein XML-Dokument mit Haskell?"  ist
mir schon oft gestellt worden und ich stelle sie mir auch selbst immer
wieder.  Es gibt viele XML-Libraries für Haskell, so dass man oft gar
nicht weiss wo man als erstes schauen sollte.  In dieser Serie von
Blogartikeln möchten wir zeigen, wie man XML-Dokumente erzeugt, wie man
vorhandene Dokumente in eigene Datenstrukturen einliest und wie man in
Dokumenten suchen und diese transformieren kann.

In der Welt des World Wide Web werden Daten fast nur noch im
[JSON-Format](http://www.json.org) ausgetauscht.  In der Industrie spielt
XML jedoch weiterhin eine sehr wichtige Rolle, so dass sich wahrscheinlich
jeder irgendwann mit dem Erzeugen oder Verarbeiten von XML-Dokumenten
beschäftigen wird.  Da wir nicht davon ausgehen, dass jeder bereits
Erfahrung mit XML hat, stellen wir in jedem Artikel auch XML-Technologien
unabhängig von Haskell vor.

Dieser erste Artikel der Serie erklärt die Grundbegriffe von XML, zeigt
auf wie die zugehörigen Haskell Datentypen aus dem Paket
[xml-conduit](https://hackage.haskell.org/package/xml-conduit) aussehen
und veranschaulicht die Nutzung durch Erzeugung und Serialisierung eines
XML-Dokument mit Hilfe dieser Datentypen.


# Was ist XML und warum sollte ich XML überhaupt verwenden?

Die
[eXtensible Markup Language](http://www.w3.org/standards/xml/core.html)
ist ein Regelwerk, das genutzt werden kann, um textbasierte
Datenaustauschformate zu beschreiben.  XML kann also genutzt werden um zum
Beispiel Blogartikel, Rechnungen oder Arztbriefe maschinenlesbar zu
beschreiben.  Wenn man Daten speichern oder austauschen möchte ist es
vorteilhaft XML zu verwenden, weil es für jede Programmiersprache gute
Bibliotheken zum Einlesen und Ausgeben von XML-Dokumenten gibt.  Was der
Inhalt eines XML-Dokuments bedeutet und wie die Daten strukturiert sind,
wird durch die [XML-Spezifikation](http://www.w3.org/TR/REC-xml/) nicht
festgelegt.  Jede Anwendung, die XML benutzen möchte, muss also selbst
festlegen wie ein XML-Dokument strukturiert sein darf.  XML gibt vor wie
die Kodierung der Struktur erfolgen muss.  Hier ein Beispiel für ein
XML-Dokument aus unserer digitalen Krankenakte
[Checkpad MED](http://www.cpmed.de):

# Ein Beispieldokument
{% highlight xml %}
<?xml version="1.0" encoding="UTF-8"?>
<patient xmlns="http://www.factisresearch.com/ns/checkpad" id="Patient/432442">
  <name>
    <given-name>Jana</given-name>
    <family-name>Braun</family-name>
    <display-name>Jana Braun</display-name>
  </name>
  <case-number>432442</case-number>
  <patient-number>55555502</patient-number>
  <birth-date>1952-07-18</birth-date>
  <sex>female</sex>
  <admission>2011-02-10T08:41:00+01:00</admission
></patient>
{% endhighlight %}

Dieses Dokument enthält Daten zu unserer Demo-Patientin Jana Braun.  Zu
diesen Daten gehören der Name, Identifikationsnummern für die Patientin
und den Krankenhausaufenthalt, Geburtsdatum, Geschlecht und Aufnahmedatum.
Aus Sicht des XML-Parsers sind die eigentlichen Daten irrelevant - es
zählt nur, dass das Dokument syntaktisch korrekt ist.  Man sagt auch: Das
XML-Dokument ist *well-formed*.  Ein Dokument darf (aber muss nicht) mit
einem Prolog `<?xml version="1.0" encoding="UTF-8"?>` beginnen, der
festlegt welcher XML-Spezifikation das Dokument entspricht und in welcher
Zeichenkodierung (hier UTF-8) es abgelegt ist.  Was jedes XML-Dokument
aber braucht ist ein Wurzelelement, das hier `patient` heisst.

# Elemente und Attribute

Ein Element wird durch ein Start-Tag in spitzen Klammern (hier `<patient
...>`) geöffnet und durch ein End-Tag (hier `</patient>`) wieder
geschlossen.  Durch diese beiden Tags wird der Inhalt des Elements, die
sogenannten Kindknoten, umschlossen.  Kindknoten eines Elements können
weitere Elemente oder Textknoten sein.  Das Start-Tag kann zwischen dem
Namen und der schliessenden, spitzen Klammer noch Attribute enthalten, die
durch beliebig viele Leerzeichen getrennt werden.  Das Start-Tag des
`patient` Element hat hier die Attribute mit dem Namen `xmlns` und `id`
deren Wert nach dem Gleichheitszeichen in einfachen oder doppelten
Anführungszeichen angegeben werden muss.

In Haskell sieht die Datentypdefinition zur Beschreibung von XML so aus:

{% highlight haskell %}

-- Ein `Document` ist ein Record mit den drei Feldern `documentPrologue`
-- (alles vor dem Root-Element), `documentRoot` (dem Root-Element) und
-- `documentEpilogue` (alles nach dem Root-Element).  Vor und nach dem
-- Root-Element stehen keine Inhalte sondern nur Kommentare und
-- Verarbeitungsanweisungen.
data Document
  = Document
  { documentPrologue :: Prologue
  , documentRoot :: Element
  , documentEpilogue :: [Miscellaneous]
  }

-- Der Prolog eins XML-Dokuments besteht aus einer Liste von Kommentaren
-- und Verarbeitungsanweisungen (Miscellaneous), einer optionalen Referenz
-- auf eine Document Type Definition und weiteren Kommentaren und
-- Verarbeitungsanweisungen.

data Prologue
  = Prologue
  { prologueBefore :: [Miscellaneous]
  , prologueDoctype :: Maybe Doctype
  , prologueAfter :: [Miscellaneous]
  }

-- Die genaue Definition der Typen Doctype und Miscellaneous lassen wir
-- hier aus.
data Doctype = Doctype
data Miscellaneous = Miscellaneous

-- Ein `Element` ist ein Record mit drei Feldern.  `elementName` für den
-- Namen des Elements. `elementAttributes` ist ein Mapping von Attributname
-- auf den Textwert des Attribut.  `elementNodes` ist die Liste der
-- Kindknoten vom Typ `Node`.
data Element
  = Element
  { elementName :: Name
  , elementAttributes :: Map Name Text
  , elementNodes :: [Node]
  }

-- Ein Knoten ist entweder ein `NodeElement` mit dem einem Element als
-- Inhalt oder ein `NodeContent` und enthält dann nur einen Textstring.
data Node
  = NodeElement Element
  | NodeContent Text
{% endhighlight %}

Das Element `name` aus unserem Beispiel hat drei Kindelemente, nämlich
`given-name`, `family-name` und `display-name`.  Das sind aber nicht alle
Kindknoten des `name` Element.  Es sind nur die Elemente, denn zwischen
der schließenden, spitzen Klammer des Start-Tag `<name>` und der
öffnenden, spitzen Klammer des Start-Tag `<given-name>` ist auch noch ein
Textknoten (`NodeContent`), der einen Zeilenumbruch und einige Leerzeichen
enthält.  Für unsere Anwendung Checkpad MED sind diese Textknoten
irrelevant und könnten ignoriert werden. Da der XML-Parser aber die Daten
nicht versteht wird er diese Textknoten nicht ignorieren. Es könnte beim
Inhalt ja auch um den Programmcode einer Programmiersprache handeln, wo
solches Whitespace zur Einrückung wichtig ist.

Was ist aber mit dem Zeilenumbruch innerhalb des End-Tag des Elements
`admission'?  Hier ist sogar ein Zeilenumbruch enthalten.  Whitespace
innerhalb von Tags wird vom XML-Parser ignoriert und nicht an die XML
verarbeitende Anwendung zurückgeliefert.  Welche Informationen genau von
einem Parser an die Anwendung zur Weiterverarbeitung geliefert werden
müssen und was ignoriert werden darf, ist in der Spezifkation des
[XML Information Set](http://www.w3.org/TR/xml-infoset/) definiert.

# Namen

Jedes Element und jedes Attribut hat einen
[XML-Namen](http://www.w3.org/TR/REC-xml-names/), der aus drei
Bestandteilen besteht:

* dem `local name` (hier z.B. `patient`)
* einem optionalen `namespace name` (hier `http://www.factisresearch.com/ns/checkpad`) und
* einem optionalen `namespace prefix` (im Beispiel gibt es keine)

Ein Präfix wird vor den Namen des Elements geschrieben und durch einen
Doppelpunkt abgetrennt.  Zu jedem Präfix muss mit Hilfe eines Attributs
ein Namespace definiert werden.  Ein Attribut zur Zuordnung eines
Namespace zu einem Präfix trägt das Präfix `xmlns` und hat den lokalen
Namen des Präfix das definiert werden soll. `xmlns:foo=...` bindet also
das Namespacepräfix `foo`. Ein Namespace, der keinem Präfix zugeordnet
ist, definiert den Default-Namespace der für alle Elemente gilt, die kein
Präfix haben.  Er wird mit `xmlns=...` festgelegt.  Ein Namespace gilt
immer ab dem Element in dessen Start-Tag er definiert wird für das Element
und alle enthaltenen Knoten.  Wenn kein Default-Namespace definiert ist,
dann ist das Element keinem Namespace zugeordnet.  Das Präfix wird
normalerweise von XML verarbeitenden Anwendungen ignoriert und es wird nur
der Name des Namespace zur Identifizierung verwendet.  Das folgende
Dokument würde daher von unserer Anwendung gleich verarbeitet werden:

{% highlight xml %}
<?xml version="1.0" encoding="UTF-8"?>
<cpm:patient cpm:xmlns="http://www.factisresearch.com/ns/checkpad" id="Patient/432442">
  <name xmlns="http://www.factisresearch.com/ns/checkpad">
    <given-name>Jana</given-name>
    <family-name>Braun</family-name>
    <cpm:display-name>Jana Braun</cpm:display-name>
  </name>
  <cpm:case-number>432442</cpm:case-number>
  <cpm:patient-number>55555502</cpm:patient-number>
  <cpm:birth-date>1952-07-18</cpm:birth-date>
  <test:sex xmlns:test="http://www.factisresearch.com/ns/checkpad">female</test:sex>
  <cpm:admission>2011-02-10T08:41:00+01:00</cpm:admission>
</cpm:patient>
{% endhighlight %}

Als Haskell Datentyp sieht ein Name dann so aus:

{% highlight haskell %}
data Name
  = Name
  { nameLocalName :: Text
  , nameNamespace :: Maybe Text
  , namePrefix :: Maybe Text
  }
{% endhighlight %}

# Das Beispieldokument als Haskell-Wert

Wir können nun das Beispieldokument mit den definierten Haskell-Datentypen
repräsentieren.  Wir definieren dazu aber zunächst einige Hilfsfunktionen.

{% highlight haskell %}
import Text.XML
import qualified Data.Text as T
import qualified Data.Map as Map

-- Erzeugt einen XML-Namen mit dem angegebenen lokalen Namen, ohne Präfix
-- im Checkpad Namespace.
mkName :: T.Text -> Name
mkName localName =
  Name
  { nameLocalName = localName
  , nameNamespace = Just "http://www.factisresearch.com/ns/checkpad"
  , namePrefix = Nothing
  }

-- Erzeugt ein Element mit angegebenem Namen und genau einem Textknoten
mkTextElem :: T.Text -> T.Text -> Element
mkTextElem n txt = Element (mkName n) Map.empty [NodeContent txt]

-- Erzeugt ein Element mit dem angegebenem Namen und den angegebenen
-- Elementen als Kindelemente.
mkElemWithElems :: T.Text -> [Element] -> Element
mkElemWithElems n els = Element (mkName n) Map.empty (map NodeElement els)
{% endhighlight %}

Mit diesen Hilfsfunktionen ist es nun einfach das Dokument aufzubauen.
Beginnen wir mit dem Element `name`:

{% highlight haskell %}
name :: Element
name =
  mkElemWithElems "name"
  [ mkTextElem "given-name" "Jana"
  , mkTextElem "family-name" "Braun"
  , mkTextElem "display-name" "Jana Braun"
  ]
{% endhighlight %}

Nun können wir schon das Wurzelelement definieren:

{% highlight haskell %}
patient :: Element
patient =
  mkElemWithElems "patient"
  [ name
  , mkTextElem "case-number" "432442"
  , mkTextElem "patient-number" "55555502"
  , mkTextElem "birth-date" "1952-07-18"
  , mkTextElem "sex" "female"
  , mkTextElem "admission" "2011-02-10T08:41:00+01:00"
  ]
{% endhighlight %}

Jetzt haben wir nur das `id` Attribut von `patient` noch nicht
hinzugefügt.  Dafür können wir eine Hilfsfunktion definieren und beim
Erzeugen des gesamten Dokuments aufrufen:

{% highlight haskell %}
-- Fügt ein Attribut mit lokalen Namen und Wert zu einem Element hinzu
addAttr :: T.Text -> T.Text -> Element -> Element
addAttr key value elem =
  let oldAttrs = elementAttributes elem
      newAttrs = Map.insert (Name key Nothing Nothing) value oldAttrs
  in elem { elementAttributes = newAttrs }

doc :: Document
doc =
  Document
  { documentPrologue = Prologue [] Nothing []
  , documentRoot = addAttr "id" "Patient/432442" patient
  , documentEpilogue = []
  }
{% endhighlight %}

# Serialisierung des Dokuments

Das so definierte Dokument kann man nur einfach als Datei speichern.  Dazu
stellt das Modul `Text.XML` die Funktion `writeFile :: RenderSettings ->
FilePath -> Document -> IO ()` zur Verfügung:

{% highlight haskell %}
main :: IO ()
main = Text.XML.writeFile def "patient.xml" doc
{% endhighlight %}

Da wir die Textknoten zwischen dem End-Tag von `patient` und dem
Start-Tag von `name` in Haskell nicht eingefügt haben sieht die Ausgabe
nicht exakt so aus wie das oben stehende Beispiel, sondern alles steht in
einer Zeile.

So, das war's für heute. Das nächste Mal schauen wir uns an, wie man
XML-Dateien einlesen und verarbeiten kann. Ich freue mich über Feedback!
