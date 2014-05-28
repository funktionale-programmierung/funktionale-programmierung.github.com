---
layout: post
description: Eine Einführung in GADTs
title: "Mehr Typsicherheit durch GADTs"
author: andres-loeh
tags: ["haskell", "datatypes", "GADTs"]
---

In diesem Beitrag wollen wir uns mit GADTs beschäftigen. GADTs, das steht für
"Generalized Algebraic Data Type", zu Deutsch "generalisierter algebraischer
Datentyp".

Dazu muss man wissen, das schon ganz normale Datentypen, wie sie zum Beispiel
in Haskell allgegenwärtig sind, den Namen "algebraische Datentypen" tragen.
Zum Beispiel definiert
{% highlight haskell %}
data Expr = Num Int
          | Add Expr Expr
          | Equal Expr Expr
          | Or Expr Expr
{% endhighlight %}
einen neuen Datentyp `Expr`, der den abstrakten Syntaxbäumen einer sehr simplen
Sprache arithmetischer Ausdrücke entspricht. Die einzigen unterstüzten Sprachkonstrukte
sind

  * numerische Literale, repräsentiert durch den Konstruktor `Num`, der einen Wert
    vom Typ `Int` als Argument nimmt,

  * die Addition zweier Ausdrücke mittels `Add`,

  * der Vergleich zweier Ausdrücke mit Hilfe von `Equal`,

  * sowie die Disjunktion zweier Ausdrücke, dargestellt durch `Or`.

Einen Ausdruck, der in möglicher konkreter Syntax `7 == 3 + 4` entspricht,
kann man dann als Haskell-Wert vom Typ `Expr` als
{% highlight haskell %}
Equal (Num 7) (Add (Num 3) (Num 4))
{% endhighlight %}
hinschreiben.

Man kann allerdings auch unsinnige Ausdrücke repräsentieren, wie etwa `3 || (4 == 5)`
als
{% highlight haskell %}
Or (Num 3) (Equal (Num 4) (Num 5))
{% endhighlight %}
Bei `Or` würden wir zwei Boolsche Ausdrücke als Argument erwarten, aber `3` ist eine Zahl.

Es wäre schön, wenn wir (je nach Bedarf) solche ungewollten Kombinationen von vornherein,
unter Verwendung von Haskells Typsystem, ausschließen könnten. Es wäre auch schön, wenn
wir bei einer Liste, in der wir mittels eines Index einen Wert suchen, schon im Typsystem
festhalten könnten, dass der Index zur Länge der Liste passen muss. Ähnlich wäre es auch
schön, wenn wir bei einem Binärbaum auf Wunsch im Typsystem festhalten könnten, dass er
vollständig balanciert sein muss. Solche zusätzlichen Eigenschaften lassen sich mit
normalen ADTs nicht oder nur sehr schwer ausdrücken, aber mit GADTs ist es auf einfache
Art und Weise möglich.

Im Rest dieses ersten Teils werde ich ein weiteres Beispiel ausführlich behandeln und
an Hand dessen beleuchten, wie genau der Bedarf für GADTs in einem konkreten Programm
entstehen könnte und wie man so einen GADT dann definiert und verwendet.

Im zweiten Teil werden wir genauer diskutieren, wie man GADTs verwenden kann und auch
auf die Beispiele, die ich gerade genannt habe, zurückkommen.

<!-- more start -->

## Frage und Antwort

Stellen wir uns vor, wir wollen ein System zur Erstellung, Verwaltung und
Beantwortung von Fragebögen entwickeln. Den Kern eines solchen Systems stellt
sicherlich die Repräsentation von Fragebögen und zugehörigen Antworten da.

Eine naheliegende Option in Haskell ist es, einen Fragebogen als eine Liste
von Fragen zu sehen, und die Bearbeitung eines solchen Fragebogens als eine
Liste von Antworten:
{% highlight haskell %}
type Questions = [Question]
type Answers   = [Answer]
{% endhighlight %}
Vielleicht haben wir mehrere Arten von Fragen, wie etwa

> Wer wird Fussball-Weltmeister? <br/>
> Wie endet das Spiel Deutschland -- Portugal?

Zu diesem Zweck können wir den Datentyp `Question` so definieren, dass
mehrere Fragetypen möglich sind:
{% highlight haskell %}
data Question =
    CountryQuestion Text
  | ResultQuestion  Text
{% endhighlight %}
Jetzt lassen sich obige Fragen wie folgt darstellen:
{% highlight haskell %}
exampleQuestions :: Questions
exampleQuestions = [
    CountryQuestion "Wer wird Fussball-Weltmeister?"
  , ResultQuestion  "Wie endet das Spiel Deutschland -- Portugal?"
  ]
{% endhighlight %}
Natürlich benötigen wir Antwort-Möglichkeiten, die zu den Fragetypen passen:
{% highlight haskell %}
data Answer =
    CountryAnswer  Country
  | ResultAnswer   Int Int

data Country = Germany | Netherlands | Brazil | Spain | Other
  deriving (Read, Show)
{% endhighlight %}
Sollten wir uns bei unseren Antworten festlegen wollen auf Deutschland als
Weltmeister und das Ergebnis 3 : 1, so können wir das jetzt darstellen als:
{% highlight haskell %}
exampleAnswers :: Answers
exampleAnswers = [
    CountryAnswer Germany
  , ResultAnswer 3 1
  ]
{% endhighlight %}
Nehmen wir ferner an, dass wir nach einiger Zeit eine Liste mit tatsächlich
"korrekten" Antworten zur Verfügung haben. Da nicht alle Antworten vorliegen
müssen, definieren wir
{% highlight haskell %}
type CorrectAnswers = [Maybe Answer]
{% endhighlight %}
und dann
{% highlight haskell %}
possiblyCorrectAnswers :: CorrectAnswers
possiblyCorrectAnswers = [
    Nothing
  , Just (ResultAnswer 2 2)
  ]
{% endhighlight %}
Die Verwendung von `Nothing` zeigt an, dass die richtige Antwort noch nicht
vorliegt, die Verwendung von `Just` transformiert einen Wert vom Typ `Answer`
in einen Wert vom Typ `Maybe Answer`.

## Berechnungen auf Fragebögen

Wir haben jetzt alles beisammen, um ein paar Funktionen zu implementieren.
Zum Beispiel können wir die zu einem Fragebogen gehörenden Antworten 
(interaktiv) einlesen:
{% highlight haskell %}
getAnswers :: Questions -> IO Answers
getAnswers = mapM getAnswer
  where
    getAnswer :: Question -> IO Answer
    getAnswer (CountryQuestion txt) = do
      Text.putStrLn txt
      CountryAnswer <$> readLn
    getAnswer (ResultQuestion txt) = do
      Text.putStrLn txt
      ResultAnswer <$> readLn <*> readLn
{% endhighlight %}
Natürlich kann man das wesentlich robuster und besser gestalten. In der Praxis
würde man die Antworten z.B. eventuell über ein Web-Formular oder eine REST-Anfrage
erhalten. Darum soll es hier und heute aber nicht gehen.

Haben wir Fragen sowie zugehörige Antworten, so können wir den Fragebogen auch
wieder ausgeben:
{% highlight haskell %}
displayQAs :: Questions -> Answers -> IO ()
displayQAs (q : qs) (a : as) = do
  displayQA q a
  displayQAs qs as
displayQAs []       []       = return ()
displayQAs _        _        = fail "incompatible questions and answers"

displayQA :: Question -> Answer -> IO ()
displayQA (CountryQuestion txt) (CountryAnswer a) = do
  putStr txt
  putStr " "
  print a
displayQA (ResultQuestion txt) (ResultAnswer m n) = do
  putStr txt
  putStr " "
  putStrLn (show m ++ " : " ++ show n)
displayQA _ _ = fail "incompatible questions and answers"
{% endhighlight %}
Wir durchlaufen beide Listen synchron. Für jede Frage und zugehörige Antwort
rufen wir `displayQA` auf. Hier wiederum erwarten wir, dass Frage- und Antwort
zueinander passen. Falls wir Inkompatibilitäten in den Listen finden, bleibt
uns nichts, als eine Exception zu werfen und den Vorgang abzubrechen.

Ein ähnliches Problem ergibt sich, wenn wir Antworten und zugehörige "korrekte"
Antworten haben, und daraus eine Gesamtpunktzahl berechnen wollen:
{% highlight haskell %}
computeTotalScore :: Answers -> CorrectAnswers -> Int
computeTotalScore (q : qs) (a : as) = computeScore q a + computeTotalScore qs as
computeTotalScore []       []       = 0
computeTotalScore _        _        = error "incompatible questions and answers"

computeScore :: Answer -> Maybe Answer -> Int
computeScore _                  Nothing                     = 0
computeScore (CountryAnswer c)  (Just (CountryAnswer c'))   = if c == c' then 1 else 0
computeScore (ResultAnswer m n) (Just (ResultAnswer m' n')) = if (m, n) == (m', n') then 1 else 0
computeScore _                  _                           =
  error "incompatible questions and answers"
{% endhighlight %}
Das Muster ist wiederum sehr ähnlich, und abermals müssen wir zur Laufzeit damit
Rechnung tragen, dass die übergebenen Listen evtl. unterschiedlicher Länge sein
könnten, oder die Antworttypen nicht zueinander passen.

## Dynamisches Testen ist suboptimal

Tatsächlich würden wir unser Programm wohl so entwickeln, dass bereits beim Einlesen
der Antworten zu einem Fragebogen sichergestellt wird, dass die Antworten zu den Fragen
passen. Ebenso beim Einlesen der "korrekten" Antworten. Aber selbst wenn wir Funktionen
{% highlight haskell %}
verifyAnswers        :: Questions -> Answers -> Bool
verifyCorrectAnswers :: Questions -> CorrectAnswers -> Bool
{% endhighlight %}
definieren, hilft uns das nur bedingt weiter: Falls wir diese Funktionen tatsächlich
konsequent aufrufen, bevor wir Antworten weiterverarbeiten, können wir uns als
Programmierer etwas sicherer fühlen, dass im laufenden Betrieb keine Fehler auftreten
werden. Aber der Haskell-Compiler bzw. das Typsystem sieht das nicht. Bei der
Definition von Funktionen wie `displayQA` oder `computeTotalScore` bleibt das ungute
Gefühl, dass sie eventuell mit inkompatiblen Argumenten aufgerufen werden könnten und
dann einen ungewollten Laufzeitfehler erzeugen könnten.

Dies ist nicht wirklich zufriedenstellend. Es wäre viel schöner, wenn nach dem Testen
nicht nur wir, sondern auch der Haskell-Compiler *wüsste*, dass die überprüften Listen
zueinander kompatibel sind, und wenn das Typsystem beim Aufruf von `displayQA` und
`computeScore` *erwarten* würde, dass wir kompatible Listen übergeben, was uns als
Programmierer wiederum erlauben würde, bei der Implementierung dieser Funktionen
*vorauszusetzen*, dass die Eingaben den Anforderungen genügen.

## GADTs zur Rettung

Die gerade beschriebene Situation ist vorzüglich für den Einsatz von GADTs geeignet.
Wenn wir präzise machen wollen, wann Fragen und Antworten zueinanderpassen, müssen
wir die Form eines Fragebogens im Typ sichtbar machen. Also statt einfach von
`Questions` zu reden, werden wir `Questions` über ihre Form parametrisieren. Was aber
soll die "Form" eines Fragebogens? Hier bleibt uns ein gewisser Spielraum, aber für
den Moment wollen wir wissen,

  * wie viele Fragen ein Fragebogen enthält,

  * und von welcher Art jede Frage ist.

Wir können zum Beispiel einen Datentyp
{% highlight haskell %}
data QuestionType = CountryType | ResultType
{% endhighlight %}
definieren und dann die Art einer Frage durch einen Wert des Typs `QuestionType`
beschreiben, sowie die Form eines gesamten Fragebogens durch eine Liste mit Elementen
vom Typ `QuestionType`. Die Form unseres Beispielfragebogens `exampleQuestions` ist
etwa
{% highlight haskell %}
[CountryType, ResultType]
{% endhighlight %}
da die erste Frage nach einem Teilnehmer ist, die zweite nach einem Spielergebnis.

Mit Hilfe von GADTs können wir so einen Wert jetzt tatsächlich an einen Datentyp
knüpfen. Dazu müssen wir die Datentypen, die wir verwendet haben, neu und präziser
definieren. Beginnen wir mit `Question`, also einer einzelnen Frage:
{% highlight haskell %}
data Question :: QuestionType -> * where
  CountryQuestion :: Text -> Question CountryType
  ResultQuestion  :: Text -> Question ResultType
{% endhighlight %}

Die Syntax der Datentypdefinition hat sich geändert. Statt Alternativen mit `|`
voneinander zu trennen, listen wir nun die Typsignaturen der Konstruktoren untereinander
auf. Der Typ `Question` ist jetzt parametrisiert über einen Wert vom Typ `QuestionType`,
und je nach Konstruktor nimmt dieser Parameter einen anderen spezifischen Wert an.
Diese Parametrisierung und Einschränkung des Parameters für verschiedene Konstruktoren
ist es, was GADTs ausmacht.

Wenn wir im Interpreter nachfragen, können wir jetzt am Typ ablesen, welchen
Konstruktor wir verwendet haben. Bislang hatten wir:
{% highlight haskell %}
GHCi> :t CountryQuestion "Wer wird Fussball-Weltmeister?"
CountryQuestion "Wer wird Fussball-Weltmeister?" :: Question
{% endhighlight %}
Jetzt bekommen wir:
{% highlight haskell %}
GHCi> :t CountryQuestion "Wer wird Fussball-Weltmeister?"
CountryQuestion "Wer wird Fussball-Weltmeister?" :: Question CountryType
{% endhighlight %}

Interessanter wird es, sobald wir uns Listen von Fragen zuwenden:
{% highlight haskell %}
data Questions :: [QuestionType] -> * where
  QNil  :: Questions \'[]
  QCons :: Question t -> Questions ts -> Questions (t ': ts)
{% endhighlight %}
Wir verwenden nicht mehr die eingebauten Haskell-Listen für eine Liste von Fragen,
sondern definieren uns unseren eigenen listenähnlichen Datentyp. Eine Liste von Fragen
ist entweder leer (`QNil`), und dann ist ihre Form auch die leere Liste `[]`. Haben
wir bereits eine Liste der Form `ts` und eine einzelne Frage vom Typ `t`, dann können
wir mittels `QCons` die Frage vorne an den Fragebogen anfügen und erhalten insgesamt
einen Fragebogen, der die Form `t : ts` hat. (Die Verwendung von Anführungszeichen
in der Datentyp-Definition dient der Auflösung syntaktischer Ambiguitäten, da die
Listensyntax in Haskell leider in vielfacher Hinsicht überladen ist.)

Unseren Beispielfragebogen können wir nun wie folgt definieren:
{% highlight haskell %}
exampleQuestions = 
  QCons (CountryQuestion "Wer wird Fussball-Weltmeister?") (
  QCons (ResultQuestion  "Wie endet das Spiel Deutschland -- Portugal?") (
  QNil))
{% endhighlight %}
Das ist etwas hässlicher als zuvor, weil die Syntax für die eingebauten Listen
unschlagbar einfach ist. Man kann das auch hier noch etwas aufhübschen, aber darauf
will ich mich im Moment nicht konzentrieren. Wichtig ist, dass wir noch immer eine
listenartige Struktur haben, die wir durch das Anfügen zweier einzelner Fragen an
die leere Liste `QNil` erzeugen. Ich habe die Typsignatur für `exampleQuestions`
weggelassen. Den Typ soll der Typinferenz-Algorithmus von Haskell für uns ermitteln.
Im Interpreter erfragen wir diesen wie folgt:
{% highlight haskell %}
GHCi> :t exampleQuestions
exampleQuestions :: Questions '[CountryType, ResultType]
{% endhighlight %}
Wir sehen, dass der Typparameter von `Questions` uns wir gewünscht anzeigt, wie viele
und welche Art von Fragen der Fragebogen enthält.

## Ein GADT für Antworten

Für Antworten können wir nun ganz entsprechend vorgehen und diese ebenfalls mit
einem Wert vom Typ `QuestionType` assoziieren:
{% highlight haskell %}
data Answer :: QuestionType -> * where
  CountryAnswer   :: Country -> Answer CountryType
  ResultAnswer    :: Int -> Int -> Answer ResultType

data Answers :: [QuestionType] -> * where
  ANil  :: Answers {::nomarkdown}'{:/}[]
  ACons :: Answer t -> Answers ts -> Answers (t ': ts)
{% endhighlight %}

Der entscheidende Punkt ist, dass wir für unsere Beispielantworten
{% highlight haskell %}
exampleAnswers =
  ACons (CountryAnswer Germany) (
  ACons (ResultAnswer 3 1) (
  ANil))
{% endhighlight %}
nun *dieselbe* Form erhalten wir für die Beispielfragen:
{% highlight haskell %}
GHCi> :t exampleAnswers
exampleAnswers :: Answers '[CountryType, ResultType]
{% endhighlight %}

Jetzt haben wir endlich die Möglichkeit, klar und deutlich auszudrücken, was es
heisst, dass wir *kompatible* Fragen und Antworten erwarten: sowohl Fragen als auch
Antworten müssen dieselbe Form haben.

Verarbeitende Funktionen können diese Annahme jetzt explizit machen:
{% highlight haskell %}
displayQAs :: Questions ts -> Answers ts -> IO ()
displayQAs (QCons q qs) (QCons a as) = do
  displayQA q a
  displayQAs qs as
displayQAs QNil         QNil         = return ()

displayQA :: Question t -> Answer t -> IO ()
displayQA (CountryQuestion txt) (CountryAnswer a) = do
  putStr txt
  putStr " "
  print a
displayQA (ResultQuestion txt) (ResultAnswer m n) = do
  putStr txt
  putStr " "
  putStrLn (show m ++ " : " ++ show n)
{% endhighlight %}
Die Typen von `displaQAs` sowie `displayQA` drücken jetzt aus, dass wir kompatible
Fragen und Antworten erwarten. Das Typsystem prüft dies. Ein Aufruf wie etwa
{% highlight haskell %}
GHCi> displayQAs exampleQuestions ANil
{% endhighlight %}
erzeugt jetzt einen Typfehler zum Zeitpunkt des Übersetzens! In der Implementation
können wir uns als Konsequenz darauf verlassen, dass die beiden Argumente durch
kompatible Konstruktoren erzeugt wurden. Wir müssen den Fehlerfall nicht mehr überprüfen --
er *kann* zur Laufzeit nicht mehr eintreten.

In ähnlicher Weise können wir auch `computeTotalScore` und `computeScore` mit
präziseren Typen reimplementieren und uns die Fehlerüberprüfungen sparen:
{% highlight haskell %}
computeTotalScore :: Answers ts -> CorrectAnswers ts -> Int
computeScore :: Answer t -> Maybe (Answer t) -> Int
{% endhighlight %}

## Eine Zwischenbilanz

Wie wir an unserem Frage- und Antwortbeispiel stellvertretend gesehen haben, haben
wir oft mit Daten zu tun, die gewisse Zusatzanforderungen erfüllen müssen, oder die
in gewisser Hinsicht zueinander kompatibel sein sollten.

Mit Hilfe von GADTs können wir solche Zusatzanforderungen ausdrücken, indem wir
Datentypen zusätzliche Parameter geben, die von den Konstruktoren des Datentyps
eingeschränkt werden. Auf diese Art und Weise können wir zum Beispiel sicherstellen,
dass Frage- und Antworttypen zueinander passen.

Manchmal, aber nicht immer, könnte man den Bedarf für GADTs dadurch umgehen, dass man
Daten in einer bestimmten Form abspeichert. Würde man in unserem Beispiel-Szenario
jede einzelne Frage mit einer Liste zugehöriger abgegebener Antworten kombinieren,
könnte man dem Kompatibilitätsproblem entgehen. Fragen und Antworten treten dann immer
zusammen auf, man bräuchte nur zwei Konstruktoren und könnte jeweils sicherstellen,
dass die Typen kompatibel sind:
{% highlight haskell %}
data QuestionWithAnswers =
    CountryQAs Text [Country]
  | ResultQAs  Text [(Int, Int)]
{% endhighlight %}
Allerdings kann man schon an diesem kleinen Beispiel sehen, dass man das Problem
dadurch verschiebt, aber nicht behebt. Während man sich bei dieser Repräsentation
darauf verlassen kann, dass Fragen und zugehörige Antworten kompatibel sind, wissen
wir nicht mehr, dass wir zu jeder Frage gleichviele Antworten speichern. Will man
also die von einem Teilnehmer gegebenen Antworten extrahieren, braucht man nun
eigentlich die Information, dass alle Listen `[Country]` und `[(Int, Int)]`, die
in der Sammlung auftauchen, dieselbe Länge besitzen (und in derselben Reihenfolge
abgespeichert sind).

Durch die Verwendung von GADTs ist unser Code etwas komplizierter geworden. Zum
Teil liegt dies daran, dass wir auf die Schönheit eingebauter Typen wie etwa
Listen verzichten müssen. Zum Teil auch daran, dass wir weiter abstrahieren und
dadurch wieder vereinfachen können (zum Beispiel `Questions` und `Answers` zu
einem einzigen Datentyp). Aber während die Typen etwas ausdruckstärker und
präziser und dadurch eventuell etwas komplizierter geworden sind, gilt das
nicht für den Code. Qualitativ sind die Reimplementierungen von Funktionen wie
`displayQAs` beinahe identisch zu den ursprünglichen Versionen -- lediglich die
ohnehin unerwünschten Fehlerfälle können wir uns jetzt sparen.

<!-- more end -->

