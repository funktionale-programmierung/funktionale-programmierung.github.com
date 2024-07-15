---
layout: post
description: Ein kurze Einführung in Elixirs Patter Matching
title: "Pattern Matching in Elixir"
author: marco-schneider
tags: ["Elixir"]
---

Die funktionale Programmiersprache Elixir bietet hervorragende Möglichkeiten zum Einsatz von Pattern Matching.
In diesem Artikel geben wir eine kurze Einführung am Beispiel eines JSON-Parsers.

<!-- more start -->

Seit ihrer Veröffentlichung 2012 erhält Elixir, eine auf Erlang basierende,
funktionale Programmiersprache, zurecht einiges an Lob.
Auch bei uns in der Active Group kommt sie mittlerweile gerne zum Einsatz.
Eine der herausragenden Eigenschaften dieser Sprache ist ihre Unterstützung zum
Pattern Machting. Das wollen wir uns hier genauer anschauen.

<!-- Das ist auch die Syntax für Kommentare, die im HTML nachher
auftauchen. -->

## Musterabgleich in Elixir ##

Unter Pattern Matching (zu deutsch in etwa "Musterabgleich") versteht
man die Möglichkeit, im Programmcode diskrete Daten(-strukturen) anhand ihres "Musters"
zu identifizieren und verarbeiten zu können.
In vielen Fällen im Alltag ist es sehr hilfreich, die Definition einer Funktion als
Musterabgleich zu notieren: Es hilft der/dem Leser*in beim Verstehen und hat
darüber hinaus häufig eine große Ähnlichkeit zur mathematischen Notation der
Fallunterscheidung. Dass dadurch die Definitionen häufig auch kürzer und klarer
werden ist natürlich auch ein nicht zu verachtender Vorteil.

Sehr grob formuliert ist in Elixir *fast alles* ein Pattern Match. In diesem Sinne
gibt es keine klassische Wertzuweisung. Möchte man Werte binden, so bedient man
sich des `=`-Operators (hier nicht Zuweisung sondern **match**). Im folgenden
sehen wir einige Beispiele:

```elixir
# 1. 
n = 42  # => 42
42 = n  # => 42
23 = n  # => ** (MatchError) no match of right hand side value: 42

# 2.
xs = [1,2,3]    # => [1,2,3]
[x | ys] = xs   # => [1,2,3]
x               # => 1
ys              # => [2,3]
[1 | zs] = xs   # => [1,2,3]
[42 | zs] = xs  # => [1,2,3]

# 3.
foobar = "foobar"                          # => "foobar"
"foo" <> rest = foobar                     # => "foobar"
rest                                       # => "bar"
<<foo :: binary-size(3)>> <> bar = foobar  # => "foobar"
foo                                        # => "foo"
bar                                        # => "bar"
```

Der Nummerierung im Code nach:

1. Wir matchen die Variable `n` auf den Wert `42`. Anschließend wissen wir, dass
   das Pattern `n` auf 42 zutrifft. Daher ist der nächste Match `42 = n` auch
   erfolgreich ("Der Wert passt zum Muster"). Für `23` ist das nicht der Fall,
   daher Antwortet Elixir mit einem `MatchError`.
2. Hier matchen wir zuerst das Muster `xs` auf die Liste `[1,2,3]`.
   Anschließend matchen wir das Muster bestehend aus einem erstem Element `x` und einem Rest 
   `ys` mit `xs`. `x` passt nun auf den Wert `1`, `ys` auf den Rest der Liste
   `[2,3]`.
   Der Match `[1 | zs] = xs` ist demnach wieder erfolgreich, während `[42 | zs] = xs`
   uns wieder mit einem `MatchError` begrüßt.
3. Auch Strings (in Elixir sind Strings binäre Daten) lassen sich matchen.
   Interessant ist hier speziell der letzte Fall: Möchte ich einen Teilstring
   matchen, so muss ich Elixir mitteilen, wie viele Zeichen ich erwarte.
   Die Syntax ist etwas ungelenk, sollte aber verständlich sein.
   
Soweit, sogut. Elixir erlaubt es uns auch, den Musterabgleich in 
Funktionsdefinitionen zu verwenden. Hierfür können wir belibig viele 
Implementierungen einer Funktion angeben und Elixir sucht sich die **erste** aus
bei dem das Muster und der übergebene Wert übereinstimmen. Hier ein kleiner
Klassiker: die Fakultätsfunktion mit Musterabgleich.

```elixir
# If we get zero, just return 1.
def factorial(0) do
  1
end
# Oder, in Kurzschreibweise
# def factorial(0), do: 1

# In any other case, recursively call `factorial`.
def factorial(n) do
  n * (factorial (n - 1))
end
```

Mit ein wenig Fantasie sieht das der mathematischen Notation recht ähnlich.
Um damit nun richtig Spaß zu haben benutzen wir das schon Gelernte um zu zeigen, 
wie einfach es damit ist, eine Parser für JSON zu schreiben!

## Ein einfacher JSON-Parser ##

**Anmerkung**: Gleich vorneweg möchte ich sagen, dass das hier keine produktionsreife
Implementierung ist. Der Einfachheit halber sparen wir uns das Abfangen von
Fehlern, etc. und konzentrieren uns nur auf die Idee.

JSON unterstützt folgende Daten:

* Null: der primitive Wert `null`
* Strings: von zwei `"` umschlossene Zeichenkette, z. B. `"foobar"`
* Booleans: die Werte `true` und `false`
* Zahlen: Fließkomma und Ganzzahlen, z.B. `42`, `-23`, `8.5` oder `3.7e-5`
* Arrays: Werte beliebiger JSON-Typen, umschlossen von `[]`, z. B. `[1, 2, 3, true, false, "foobar"]`
* Objekte: Paare von Strings und einem beliebigen JSON-Wert, umschlossen von `{}`,
  z. B. `{"foo": "asdf", "bar": [1,2,3], "fizz": 42, "buzz": true"}` usw.

Zuerst definieren wir eine kleine Hilfsfunktion zum Überspringen von Leerzeichen
in Strings (natürlich mit Hilfe von Pattern Matching!). Um es noch einfacher zu
machen verwenden wir hier Elixirs **Guard**s. Das sind Prädikate, die wir an den
Header unserer Funktion anheften können und bewirken, dass das Pattern nur dann
zutrifft, wenn zusätzlich die Bedingung im Guard-Ausdruck erfüllt ist (näheres 
hierzu [gibt es in der offiziellen Dokumentation (Link)](https://hexdocs.pm/elixir/master/guards.html)).

```elixir
# Skip any whitespace characters and return the resulting string.
defp skip_white(<<char>> <> rest) when char in '\s\n\t\r', do: skip_white(rest)
defp skip_white(string), do: string
```

Beginnen wir nun mit den einfachen Fällen und definieren eine `parse`-Funktion für
den leeren String, `null`, `true` und `false`. Das Ergebnis soll jeweils ein 
Tupel aus dem erkannten Wert und dem noch nicht bearbeiteten Rest sein. 
Der `<>`-Operator ist in Elixir die Stringverknüpfung und kann natürlich auch
in einem Pattern Match verwendet werden. Das `_`-Symbol im Pattern steht für einen
beliebigen Wert; damit signalisieren wir, dass sich dort zwar ein Wert befindet,
dieser hier für uns aber keine Rolle spielt.

```elixir
defp parse("" <> _),         do: {nil, ""}
defp parse("null" <> rest),  do: {nil, rest}
defp parse("true" <> rest),  do: {true, rest}
defp parse("false" <> rest), do: {false, rest}
```

Das war einfach! Nun bleiben noch die etwas komplexeren Fälle. 

Als nächstes nehmen wir uns Strings vor. Da Strings immer mit einem `"` anfangen
haben wir schon eine Ahnung, was jetzt passieren muss.

```elixir
defp parse("\"" <> rest), do: parse_string(rest, [])

defp parse_string("\"" <> rest, acc) do
  # When we encounter the closing \", we're done.
  {IO.iodata_to_binary(acc), rest}
end

defp parse_string(string, acc) do
  # Count the length of the partial string.
  # See github repo for full code.
  count = string_chunk_size(string, 0)
  <<chunk :: binary-size(count), rest :: binary>> = string
  parse_string(rest, [acc, chunk])
end
```

Um Strings gut matchen zu können müssen wir jeweils wissen, wie lang die Repräsentation
eines Zeichens ist (das sagt uns die hier nicht gezeige Funktion `string_chunk_size`).
Anschließend gleichen wir `chunk` mit dem ersten Zeichen in `string` ab und `rest`
mit dem Rest.
Am Ende, wenn der String komplett analysiert ist, wird das akkumulierte Ergebnis 
als Binärstring zurück gegeben.


Weiter geht es mit Arrays. Für unsere `parse`-Funktion bedeutet das, dass wir
auf ein `[` matchen wollen.
In `parse_array` parsen wir dann Schritt für Schritt alle Werte. Wenn wir am
Ende angekommen sind geben wir das akkumulierte Ergebnis und den unverarbeiteten
Rest zurück. Hier sehen wir auch den Pipe-Operator `|>` im Einsatz. Dieser
erlaubt es, das Ergebnis der "linken" Seite als **erstes** Argument der Funktion
auf der "rechten" Seite zu verwenden und ist häufig nützlich, um ein Codefragment
leichter lesbar und kürzer zu machen. Dazu gibt es [hier](https://elixirschool.com/en/lessons/basics/pipe-operator/)
weitere Beispiele.

```elixir
defp parse("[" <> rest), do: skip_white(rest) |> parse_array([])

defp parse_array("]" <> rest, acc), do: {Enum.reverse(acc), rest}
defp parse_array("," <> rest, acc), do: parse_array(rest, acc)
defp parse_array(string, acc) do
  {value, rest} = skip_white(string) |> parse()
  skip_white(rest) |> parse_array([value | acc])
end
```

Bevor wir zu den Zahlen kommen kümmern wir uns noch um JSON-Objekte. Diese sind
sehr ähnlich zu Arrays; wir suchen nach einem String, der mit `{` beginnt.
Danach gehen wir durch den String und parsen zuerst Schlüssel, dann Wert.
Die weiteren Implementierungen prüfen auf `","` für weitere Paare oder `"}"` für
das Ende des Objekts.
`Map.new` erstellt aus einer Liste von Tupeln eine neue Elixir-Map,
wird es ohne Argumente aufgerufen gibt es eine leere Map zurück.

```elixir
defp parse("{" <> rest), do: skip_white(rest) |> parse_object([])

defp parse_object("}" <> rest, acc), do: {Map.new(acc), rest}
defp parse_object("," <> rest, acc), do: skip_white(rest) |> parse_object(acc)
defp parse_object("\"" <> rest, acc) do
  {name, rest} = parse_string(rest)  # Parse the name (key)
  ":" <> rest = skip_white(rest)
  {value, rest} = skip_white(rest) |> parse()  # Parse the value
  acc = [{name, value} | acc]
  parse_object(rest, acc)
end
```

Jetzt fehlen nur noch Zahlen. Diese sind im Vergleich mit den anderen Werttypen
etwas komplexer. Um eine Zahl zu erkennen nutzen wir wieder ein Guard-Statement.
Das Muster, das wir erwarten, ist eine Ziffer oder ein Minus `-`, gefolgt von einem
beliebigen Wert.

```elixir
defp parse(<<char, _ :: binary>> = string) when char in '-0123456789' do
  parse_number(string)
end
```

Im folgenden müssen wir diese Fälle unterscheiden:

1. Ganzzahlen: Ein Zahlenstring, der sich nur aus den Ziffern 0-9 zusammensetzt.
2. Kommazahlen: Ein Zahlenstring, der sich aus den Ziffern 0-9, einem Punkt und
   eventuellen Exponenten zusammen setzt.

```elixir
# If it starts with a minus, it could be both.
defp parse_number("-" <> rest) do
  case rest do
    "0" <> rest -> parse_frac(rest, ["-0"])
    rest -> parse_int(rest, [?-])
  end
end

# If it starts with a 0, it's always a fraction.
defp parse_number("0" <> rest), do: parse_frac(rest, [?0])

# In any other case, it's an integer.
defp parse_number(string), do: parse_int(string, [])
```
Eine Ganzzahl parsen wir, indem wir sicherstellen, dass es mit einer Zahl zwischen
1 und 9 beginnt. Dann sammeln wir einfach die restlichen Ziffern auf und parsen
den Zahlenwert aus dem akkumulierten Ergebnis.

```elixir
defp parse_int(<<char, _ :: binary>> = string, acc) when char in '123456789' do
  {digits, rest} = parse_digits(string)
  parse_frac(rest, [acc, digits])
end

defp parse_digits(<<char>> <> rest = string) when char in '0123456789' do
  count = count_digits(rest, 1)
  <<digits :: binary-size(count), rest :: binary>> = string
  {digits, rest}
end

# Count the number of digits.
defp count_digits(<<char>> <> rest, acc) when char in '0123456789' do
  count_digits(rest, acc + 1)
end
defp count_digits(_, acc), do: acc
```

Wieder ähnlich bei Kommazahlen. 

```elixir
defp parse_frac("." <> rest, acc) do
  {digits, rest} = parse_digits(rest)
  parse_exp(rest, true, [acc, ?., digits])
end
defp parse_frac(string, acc), do: parse_exp(string, false, acc)

defp parse_exp(<<e>> <> rest, frac, acc) when e in 'eE' do
  e = if frac, do: ?e, else: ".0e"
  case rest do
    "-" <> rest -> parse_exp_rest(rest, frac, [acc, e, ?-])
    "+" <> rest -> parse_exp_rest(rest, frac, [acc, e])
    rest -> parse_exp_rest(rest, frac, [acc, e])
  end
end

defp parse_exp(string, frac, acc), do: {parse_number_complete(acc, frac), string}

defp parse_exp_rest(rest, _, acc) do
  {digits, rest} = parse_digits(rest)
  {parse_number_complete([acc, digits], true), rest}
end

defp parse_number_complete(iolist, false), do: IO.iodata_to_binary(iolist) |> String.to_integer
defp parse_number_complete(iolist, true),  do: IO.iodata_to_binary(iolist) |> String.to_float
```

Geschafft! Das verarbeiten der Zahlenstrings ist etwas aufwändiger, allerdings
hilft uns das Pattern Matching auch hier, die Übersich zu behalten. In allen Fällen
"sammeln" wir zuerst alle Ziffern zusammen bevor wir sie am Schluss mit Hilfe
von `parse_number_complete` und den eingebauten `String.to_*` in Zahlen überführen.

Um jetzt noch die API aufzuräumen definieren wir eine nicht-private Funktion
`parse!` (In Elixir ist es üblich, Funktionen, die Fehler werfen und von "außen"
aufgerufen werden können mit einem Ausrufezeichen als solche zu markieren).

```elixir
@doc """
`parse!` takes a JSON-string and returns the corresponding Elixir data structure.
"""
def parse!(string) do
  {result, _} = skip_white(string) |> parse()
  result
end

# Example
parse!("{\"foo\": 42, 
         \"bar\": true, 
         \"fizz\": 3.7e-5, 
         \"buzz\": [42, true, [\"done\"]]}")
# =>  %{"bar" => true, 
        "buzz" => [42, true, ["done"]], 
        "fizz" => 3.7e-5, 
        "foo" => 42}
```

## Fazit ##

In diesem Artikel haben wir uns mit Pattern Matching in Elixir beschäftigt. 
Anhand eines Parsers haben wir es verwendet, um eine kurze und übersichtliche
Implementierung zu bauen.
Weiter haben wir Guards kennen gelernt, die noch mehr Kontrolle über den
Musterabgleich geben.

Wir bei der Active Group verwenden mittlerweile gerne Elixir für die
Programmierung - natürlich nicht nur wegen des Pattern Matchings. Elixir bietet
die volle Mächtigkeit von [Erlang und der zugehörigen VM (BEAM) sowie OTP](http://www.erlang.org/), was hochgradig
nebenläufige Programmierung ermöglicht und unterstützt dabei hervorragend das
Arbeiten mit funktionaler Programmierung.

Der Code für diesen Artikel kann [hier auf GitHub angesehen werden](https://github.com/neshtea/elixir-json-parser-example).
