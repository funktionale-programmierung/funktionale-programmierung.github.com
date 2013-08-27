---
layout: post
title: "Ein Parser für HL7-Nachrichten in weniger als 180 Zeilen Haskell-Code"
author: stefan-wehr
tags: ["haskell", "parser", "hl7"]
---

Ein Argument gegen die Benutzung funktionaler Sprachen in einem kommerziellen Produkt
ist oft, dass es für solche "exotischen" Sprachen nicht genügend Bibliotheken gäbe. Für
ausgewachsene funktionale Sprachen wie Scala oder Haskell zieht dieses Argument
längst nicht mehr, denn es gibt für beide Sprache eine Menge qualitativ hochwertiger
Bibliotheken und Komponenten. Und sollte es doch mal vorkommen dass für ein etwas
ungewöhnlicheres Problem keine Unterstützung durch eine Bibliothek vorhanden ist,
kann man meistens in kurzer Zeit Abhilfe schaffen und sich selbst eine kleine Bibliothek
zusammenstellen.

Heute wollen wir das anhand eines Parsers für HL7-Nachrichten durchexerzieren.
HL7 ist ein weitverbreiteter [Standard](http://www.hl7.org/implement/standards/) zum Austausch
medizinischer Daten. Wir setzen diesen Standard bei uns in der Firma sehr
oft im Rahmen unseres Produktes [Checkped MED](http://cpmed.de) ein, über das
wir schon an [anderer Stelle](/2013/07/17/medizin-funktional.html) in diesem
Blog berichtet haben. Da der Checkpad-Server hauptsächlich in Haskell
geschrieben ist, benötigen wir in Haskell natürlich auch einen Parser
für HL7-Nachrichten. Da es einen solchen nicht fertig erhältlich gibt, haben wir
in kurzerhand selbstgeschrieben. Sie werden in diesem Artikel sehen,
dass wir eine performante und robuste Implementierung in weniger als 180 Zeilen Haskell-Code
erstellen können, und diese Zeilen enthalte auch die Datentypdefinition für
die HL7-Nachrichten sowie Tests. Der Code des eigentlichen Parsers ist weniger
als 50 Zeilen lang.

<!-- more start -->

Bevor wir uns in die Implementierung stürzen, vielleicht noch ein paar Worte
zu HL7. Wie oben beschrieben ist HL7 ein Standard zum Austausch medizinischer Daten.
Solche Daten können z.B. Stammdaten von Patienten (Name, Geburtsdatum, Angehörige, Hausarzt,
Informationen zur Versicherung usw.) oder auch Daten zu Befunden (Laborergebnisse, 
Begutachtung von radiologischen Bildern usw.) sein. Obwohl der
[Hl7-Standard](http://www.hl7.org/implement/standards/) neben der Syntax von Hl7-Nachrichten
auch deren Semantik spezifiziert ist dieser Teil der Spezifikation in der Praxis
nicht viel Wert. So gibt es z.B. nicht einmal Einigkeit bezüglich der [Zeitzone von
Zeit- und Datumswerten](http://www.hl7standards.com/blog/2008/07/25/hl7-time-zone-qualification/).

Doch selbst bei der Spezifikation der Syntax bzw. bei der Umsetzung davon gibt es Lücken, 
was sich z.B. in der fehlenden Spezifikation für das Encoding von HL-Nachrichten oder der fehlenden
Unterstützung für [Escape-Sequenzen](http://www.hl7standards.com/blog/2006/11/02/hl7-escape-sequences/) 
in vielen Implementierungen äußert.

Nichtsdestotrotz stellen wir Ihnen heute einen in Haskell geschrieben HL7-Parser vor,
der mit Nachrichten in beliebigen Encodings umgehen kann und auch Escape-Sequenzen unterstützt.

Zunächst mal zum Aufbau von HL7-Nachrichten. Eine solche Nachricht besteht aus
mehreren durch `\r` getrennte *Segmenten*, wobei jedes Segment einen Namen und beliebig viele
*Felder* hat. Der Name eines Segments steht am Anfang des Segments und ist typischerweise
ein dreizeichiger Buchstabencode.  Ein Feld kann mehrere *Feldwiederholungen* enthalten,
eine Wiederholung besteht aus *Komponenten*, welche wiederum
in *Subkomponenten* unterteilt werden können. Die Trennzeichnen zwischen Feldern,
Wiederholungen, Komponenten und Subkomponenten sind am Anfang der Nachricht festgelegt,
es wird aber typischerweise immer `|` zur Feldtrennung, `~` zur Abgrenzung von Wiederholungen,
`^` zur Trennung von Komponenten und `&` zur Trennungen von Subkomponenten verwendet.

Schauen wir uns ein einfaches Beispiel an (ich ersetze dabei `\r` durch das Symbol für "neue Zeile"):

~~~
MSH|^~\&|EPIC|EPICADT|SMS|SMSADT|199912271408|CHARRIS|ADT^A04|1817457|D|2.5|
PID||0493575^^^2^ID 1|454721||DOE^JOHN^^^^
PV1||O|168 ~219~C~PMA^^^^^^^^^||||277^ALLEN MYLASTNAME^BONNIE^
~~~

Das erste Segment trägt immer den Namen `MSH`, was für "Message Header" steht. Direkt nach dem
Namen werden dann die Trennzeichen sowie das Escapesymbol `\` definiert. Das MSH-Segment
enthält Information über den Absender und den Adressaten der Nachricht sowie über die Nachricht selbst.
So steht z.B. in Feld 9 der Typ der Nachricht `ADT^A04`, also ein Feldinhalt mit zwei Komponenten
`ADT` und `A04`. ADT steht dabei für "Admission Discharge Transfer", es handelt sich also
um eine Stammdatennachricht. Die zweite Komponente `A04` schränkt diesen Typen weiter ein.
In den anderen beiden Segmenten finden sich Information zum Patienten selbst (PID Segment) sowie
zu einem Krankenhausaufenthalt (PV1 Segment).

Beginnen wir nun mit der Modellierung von HL7-Nachrichten als Haskell Datentyp (der komplette
Quellcode inklusiver alle Import-Statements ist [hier](/files/hl7-parser/Hl7Parser.hs) erhältlich):

~~~haskell
newtype Message
    = Message
    { msg_segments :: Vector Segment
    } deriving (Show, Eq)

data Segment
    = Segment
    { seg_name :: T.Text
    , seg_fields :: !(Vector Field)
    } deriving (Show, Eq)

data Field
    = EmptyField
    | TextField !T.Text
    | StructuredField !(Vector FieldRep)
    deriving (Show, Eq)

data FieldRep
    = EmptyFieldRep
    | TextFieldRep !T.Text
    | StructuredFieldRep !(Vector Component)
    deriving (Show, Eq)

data Component
    = EmptyComponent
    | TextComponent !T.Text
    | StructuredComponent !(Vector SubComponent)
    deriving (Show, Eq)

data SubComponent
    = EmptySubComponent
    | TextSubComponent !T.Text
    | ValueSubComponent !(Vector Value)
    deriving (Show, Eq)

data Value
    = TextValue !T.Text
    | EscapeValue!T.Text
      deriving (Show, Eq)
~~~

Aus Effizienzgründen verwenden wir `Vector` statt Listen, `Text` statt `String` und versehen 
jedes Feld mit einer Striktheitsannotation `!`. Ebenfalls aus Effizienzgründen sind die Typen
`Field`, `FieldRep`, `Component` und `SubComponent` mit Spezialfällen für leeren und rein
textuellen Inhalt ausgestattet. Dies macht Sinn da diese Fälle sehr häufig in HL7-Nachrichten
aus der echten Welt vorkommen.

Als nächstes führen wir Konstrukturfunktionen für die eben definierten Datentypen ein. Die 
Konstruktorfunktionen kümmern sich um eine normalisierte Darstellung. Zum einen sorgen sie dafür,
dass in den richtigen Fällen die Konstruktoren für leere oder rein textuelle Inhalte aufgerufen werden,
zum anderen entfernen sie leere Felder, leere Feldwiederholungen, leere Komponenten und leere Subkomponenten,
falls diese ganz am Ende vorkommen. Das ist konsistent mit dem HL7-Standard, denn leere Element dürfen
am Ende beliebig weggelassen oder auch hinzugefügt werden.

~~~haskell
mkMessage :: Segment -> [Segment] -> Message
mkMessage x xs = Message (Vector.fromList (x:xs))

mkSegment :: T.Text -> [Field] -> Segment
mkSegment name fields = Segment name (mkVector EmptyField fields)

mkField :: [FieldRep] -> Field
mkField [] = EmptyField
mkField [EmptyFieldRep] = EmptyField
mkField [TextFieldRep v] = TextField v
mkField reps = StructuredField (mkVector EmptyFieldRep reps)

mkFieldRep :: [Component] -> FieldRep
mkFieldRep [] = EmptyFieldRep
mkFieldRep [EmptyComponent] = EmptyFieldRep
mkFieldRep [TextComponent v] = TextFieldRep v
mkFieldRep comps = StructuredFieldRep (mkVector EmptyComponent comps)

mkComponent :: [SubComponent] -> Component
mkComponent [] = EmptyComponent
mkComponent [EmptySubComponent] = EmptyComponent
mkComponent [TextSubComponent v] = TextComponent v
mkComponent subComps = StructuredComponent (mkVector EmptySubComponent subComps)

mkSubComponent :: [Value] -> SubComponent
mkSubComponent [] = EmptySubComponent
mkSubComponent [TextValue t] =
    if t == "\"\"" then TextSubComponent "" else TextSubComponent t
mkSubComponent vs = ValueSubComponent (Vector.fromList vs)

mkVector :: Eq a => a -> [a] -> Vector a
mkVector empty l = Vector.fromList (L.dropWhileEnd (== empty) l)
~~~

So, jetzt aber zum eigentlichen Parser. Wir verwenden dafür die Bibliothek
[attoparsec](http://hackage.haskell.org/package/attoparsec), eine weitverbreite,
hochperformante *Bibliothek für Parserkombinatoren*. Was ist denn nun schon wieder
ein Parserkombinator? Einigen von Ihnen wird sicher der Begriff *Parsergenerator* ein
Begriff sein. Dieser Ansatz wird in imperativen Sprachen häufig benutzt, um aus einer
externen Beschreibung einer Grammatik und ein paar Codeschnipseln ein Parser zu generieren.

In funktionalen Sprachen gibt es selbstverständlich auch Parsergeneratoren,
allerdings werden Parser wesentlich häufiger direkt in der Sprache selbst implementiert.
Zur Beschreibung der Grammatik kommen dabei die Parserkombinatoren zum Einsatz. Wir werden
gleich ein Beispiel sehen, vorher sei mir aber noch erlaubt darauf hinzuweisen, dass
es in unserem Fall nur schwer möglich wäre, einen Parsergenerator zu verwenden, denn
schließlich ist die HL7-Grammatik durch die frei konfigurierbaren Trennzeichen
in gewissem Sinne "selbst-modifizierend", eine Eigenschaft die mit
Parsergeneratoren nur schwer in den Griff zu bekommen ist.

Nun aber zum Parser selbst, den ich jetzt schrittweise vorstellen möchte. Der 
Parser ist dabei in [monadischem Stil](http://funktionale-programmierung.de/2013/04/18/haskell-monaden.html)
geschrieben.

~~~haskell
parseMessage :: Parser Message
parseMessage =
    do string "MSH"
       fieldSep <- anyChar
       compSep <- anyChar
       repSep <- anyChar
       escChar <- anyChar
       subSep <- anyChar
~~~

Wir beginnen mit dem Parsen des Headers und den Trennzeichnen. Der Kombinator `string`
ist dabei ein Parser der erfolgreich den angegebenen String wegparst. Findet er den
String nicht am Anfang der Eingabe schlägt der Parser fehlt. Danach benutzen wir
fünfmal den `anyChar` Kombinator, um die einzelnen Trennzeichen zu parsen.

Der folgende Parser für Felder parst eine Sequenz von HL7-Feldern, die alle mit dem Feldtrennzeichen
(typischerweise `|`) beginnen. Dabei drückt der Kombinator `many` eine beliebig lange Wiederholung
aus (schließlich kann ein Segment auch eine beliebige Anzahl von Feldern enthalten).
Der Parser `char` ist erfolgreich wenn als nächstes in der Eingabe das angegebene Zeichen, in diesem
Fall das Feldtrennzeichen, auftaucht.

~~~haskell
       let fields =
               many (do char fieldSep
                        field)
~~~

Als nächstes folgt die Definition des Parsers für ein einzelnes Feld. Wir benutzen dabei
den Kombinator `sepBy` um auszudrücken dass ein Feld aus
einzelnen Wiederholungen getrennt durch `repSep` besteht.

~~~haskell
           field =
               do reps <- fieldRep `sepBy` char repSep
                  return (mkField reps)
~~~

Der Inhalt einer Feldwiederholung wird vom `fieldRep`-Parser geparst. Diese funktioniert, genauso
wie der Parser für Komponenten, analog zum `field`-Parser.

~~~haskell
           fieldRep =
               do comps <- component `sepBy` char compSep
                  return (mkFieldRep comps)
           component =
               do subComps <- subComponent `sepBy` char subSep
                  return (mkComponent subComps)

~~~

Der Parser für Subkomponenten ist etwas spannender, den hier müssen wir mit textuellen Inhalten und
Escapesequenzen umgehen. Um hier eine Auswahl zu treffen, benutzen wir den `choice` Kombinator,
der aus einer Liste von Parsern die einzelnen Parser der Reihe nach ausprobiert und das
Ergebnis des ersten erfolgreichen zurückliefert (oder fehlschlägt wenn alle fehlschlagen).
Der `takeWhile1` Kombinator parst solange die Zeichen des Eingabestroms (mindestens aber eines), bis
das übergebene Prädikat falsch wird.

~~~haskell
           subComponent =
               do xs <- many $ choice [regularValue, escapeSequence]
                  return (mkSubComponent xs)
           regularValue =
               do x <- takeWhile1 notSpecial
                  return (TextValue x)
           escapeSequence =
               do char escChar
                  x <- takeWhile1 notSpecial
                  char escChar
                  return (EscapeValue x)
           notSpecial x = (x /= fieldSep && x /= compSep && x /= subSep &&
                           x /= lineSep && x /= repSep && x /= escChar)
~~~

So, jetzt können wir den Parser komplettieren:

~~~haskell
       mshFields <- fields
       let segment =
               do name <- takeWhile1 (/= fieldSep)
                  fs <- fields
                  return (mkSegment name fs)
       otherSegments <- many (do eol
                                 segment)
       return (mkMessage (mkSegment "MSH" mshFields) otherSegments)
    where
      lineSep = '\r'
      eol = char lineSep
~~~

Zum Abschluss definieren wir noch eine Funktion, die den soeben definierten Parser auf einen
Eingabestring anwendet:

~~~haskell
parseMessageFromText :: T.Text -> Either String Message
parseMessageFromText = parseOnly parseMessage
~~~

Fertig! Halt, ein Test wäre noch gut, hier kommt er schon:

~~~haskell
test_parseMessage =
    assertEqual (Right expectedMsg) (parseMessageFromText msg)
    where
      expectedMsg =
          Message (Vector.fromList
                        [Segment "MSH" (Vector.fromList [TextField "FOO"]),
                         Segment "PID" (Vector.fromList [EmptyField,
                                                    EmptyField,
                                                    TextField "454721",
                                                    EmptyField,
                                                    StructuredField (Vector.fromList
                                                                          [StructuredFieldRep
                                                                           (Vector.fromList [TextComponent "DOE",
                                                                                        TextComponent "JOHN"])])]),
                         Segment "PV1" (Vector.fromList [EmptyField,
                                                    StructuredField (Vector.fromList
                                                                          [TextFieldRep "0",
                                                                           StructuredFieldRep
                                                                           (Vector.fromList [TextComponent "1",
                                                                                        TextComponent "2"])]),
                                                    StructuredField (Vector.fromList
                                                                          [StructuredFieldRep
                                                                           (Vector.fromList [StructuredComponent
                                                                                        (Vector.fromList [EmptySubComponent,
                                                                                                     TextSubComponent "bar"])])]),
                                                    StructuredField (Vector.fromList
                                                                          [StructuredFieldRep
                                                                           (Vector.fromList [StructuredComponent
                                                                                        (Vector.fromList [ValueSubComponent
                                                                                                     (Vector.fromList [TextValue "string",
                                                                                                                  EscapeValue "F",
                                                                                                                  TextValue "escape"])])])]),
                                                    StructuredField (Vector.fromList
                                                                          [StructuredFieldRep
                                                                           (Vector.fromList [EmptyComponent,
                                                                                        TextComponent ""])])])])

      msg = T.intercalate "\r"
            ["MSH|^~\\&|FOO",
             "PID|||454721||DOE^JOHN^",
             "PV1||0~1^2|&bar&|string\\F\\escape|^\"\""]
~~~

Damit haben wir in weniger als 180 Zeilen Code eine kompletten Parser für HL7-Nachrichten inklusive Test geschrieben.
Der Parser hat sich so auch bei uns im produktiven Einsatz bewährt.

Viel Spaß beim Experimentieren mit Haskell und Parserkombinatoren. Ich freue mich über Rückmeldungen jeder Art!
