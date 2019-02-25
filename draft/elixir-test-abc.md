---
layout: post
description: Tests schreiben und ausführen mit Elixir
title: "Test-ABC mit Elixir"
author: tim-digel
tags: ["Elixir", "Erlang", "Test", "Testing", "ExUnit", "Mix"]
---

In der Regel schreibt keiner gerne Tests. Es ist einfacher mit etwas Selbstsicherheit zu behaupten, dass man das Programm gleich richtig schreibt und sich Tests sparen kann.  

Weit gefehlt wie wir alle wissen. Tests werden überall benötigt. Wir schauen uns heute die Möglichkeit an mit der jungen Sprache _Elixir_, aufbauend auf der _Erlang Virtual Machine_, Tests zu schreiben. Elixir bietet uns mit _Mix_ und _ExUnit_ ein sehr gutes Tooling, um einfach, übersichtlich und schnell Tests schreiben und ausführen zu können.  

In diesem Artikel legen wir zuerst ein neues Elixir-Projekt an und schreiben einige Tests um eine Einführung in das Test-Tooling zu bekommen. Weiter lernen wir einfache Möglichkeiten, um im Entwickleralltag schneller und effizienter mit Tests arbeiten zu können.
<!-- more start -->

## Bevor es los geht

Wer schon ein bestehendes Projekt hat kann dieses Kapitel überspringen. Für alle anderen erstellen wir uns zuerst eine Spielwiese. Wir verwenden Elixir in Version 1.8 auf Erlang 21, zumal die Versionen keine große Bedeutung für unsere Tests haben werden. Wie man Elixir & Co schnell installieren kann haben wir bereits in [Mit Nix raus aus der Versionshölle](https://funktionale-programmierung.de/2018/02/19/nix.html) gesehen.
Nun legen wir in einem Verzeichnis mit `mix new fehlerfrei` ein Projekt mit dem Namen _Fehlerfrei_ an. Mix erstellt uns einige hilfreiche Dinge, wie z. B. auch eine Projekt-Liesmich oder die Gitignore-Datei.

## Tests ausführen

In einem frischen Projekt gibt es bereits zwei Beispieltests. Führen wir innerhalb unseres Ordner `fehlerfrei` nun `mix test` aus, so erhalten wir:
```console
user@pc:~/fehlerfrei$ mix test
..

Finished in 0.03 seconds
1 doctest, 1 test, 0 failures

Randomized with seed 414377
```
Mix zeichnet für jeden erfolgreichen Test einen Punkt, für jeden gescheiterten Test wird eine großzügige Beschreibung über den fehlerhaften Test ausgegeben. Am Ende wird noch die Gesamtanzahl der durchgeführten, fehlgeschlagenen und ggf. übersprungenen Tests ausgegeben. Die letzte Zeile gibt den verwendeten Seed an. Diese Zahl bestimmt die zufällig gewählte Reihenfolge der Tests. Im Folgenden kürzen wir die Ausgabe der Testdurchläufe um irrelevante Teile.  

`mix test` führt alle Tests in allen Dateien innerhalb des Ordners `test/` aus die auf `_test.exs` enden. Die übliche Konvention besagt im Test-Ordner die gleiche Struktur wie für die Moduldefinitionen aufzubauen, jeweils ergänzt um `_test.exs` im Dateinamen, sowie `Test` im Modulnamen innerhalb der Testdatei. Das Modul `Fehlerfrei` hat das zugehörige Test-Modul `FehlerfreiTest`.

## Tests schreiben

Um etwas mehr Material zu haben, definieren wir uns in der Datei `lib/fehlerfrei.ex` die folgende Funktion:
```elixir
  @doc "Sum the integers from 1 to n."
  @spec gausssum(pos_integer()) :: pos_integer()
  def gausssum(1), do: 1
  def gausssum(n), do: n + gausssum(n - 1)
```

Schreiben wir einige Tests in der dazugehörigen Test-Datei unter `test/fehlerfrei_test.exs`:
```elixir
  test "that 5 is greater than 4" do
    assert 5 >= 4
  end

  test "that 6 divided by 2 is the same as 3" do
    assert 6 / 2 == 3
    refute 12 == 4 * 4
  end
  
 test "gausssum" do
    assert Fehlerfrei.gausssum(1) == 1
    assert Fehlerfrei.gausssum(100) == 5050
    assert Fehlerfrei.gausssum(78) == 78 * 79 / 2
  end
```
Ein Test hat immer einen Titel und kann dann beliebig viele Behauptungen (`assert`) oder Widerlegungen (`refute`) haben. 
Testen können wir beliebige Wahrheitswerte oder `nil` und Sachen die nicht `nil` sind. `refute` ist gleichbedeutend mit `assert !` (_behaupte nicht_), liest sich aber viel besser. Im zweiten Test sehen wir, dass wir uns in der Regel nicht um Sachen wie 3.0 ungleich 3 kümmern müssen. Wenn wir `mix test` ausführen, sehen wir, dass alle Tests erfolgreich sind.  

Ein Testfall kann nicht nur `assert`- oder `refute`-Anweisungen beinhalten, sondern auch beliebige Codeanweisungen:
```elixir
  test "that Lagerregal is a palindrom" do
    a = "Lagerregal"

    assert a == String.reverse(a)
  end
```
Wenn wir die Tests erneut ausführen erhalten wir jetzt unseren ersten Fehlerfall:
```console
user@pc:~/fehlerfrei$ mix test
..

  1) test that Lagerregal is a palindrom (FehlerfreiTest)
     test/fehlerfrei_test.exs:18
     Assertion with == failed
     code:  assert a == String.reverse(a)
     left:  "Lagerregal"
     right: "lagerregaL"
     stacktrace:
       test/fehlerfrei_test.exs:21: (test)

...

Finished in 0.1 seconds
1 doctest, 5 tests, 1 failure
```
Die im Deutschen korrekte Großschreibung macht unser Palindrom kaputt. Wir ändern unseren Test ab zu
```elixir
  test "that Lagerregal is a palindrom" do
    a = String.downcase("Lagerregal")

    assert a == String.reverse(a)
  end
```
um die Zeichenkette vorher klein zu kriegen. Wir können nun mit `mix test --failed` den Durchlauf wiederholen und dabei nur die zuvor fehlgeschlagenen Tests erneut laufen lassen.

## Tests zielgerichtet ausführen

Wenn ein Projekt mit der Zeit größer wird können schnell mehrere Hundert Tests zusammenkommen. Schauen wir uns an wie wir die Zeit verkürzen können, um nicht ewig auf den Bildschirm starren zu müssen.  

Den Schalter `--failed` haben wir eben schon kennen gelernt, er führt alle zuvor fehlgeschlagenen Tests erneut aus. Diese Option gibt es erst seit Elixir 1.8.  

Mit Angabe von Dateien, Ordnern oder Mustern (z. B. `test/*_sql_test.exs`) können wir die Ausführung auf bestimmte Testdateien einschränken. So führt zum Beispiel `mix test test/fehlerfrei_test.exs` nur die Tests in dieser Datei aus. Duplizieren wir unsere Testdatei und führen wir dann zuerst alle Tests und anschließend eingeschränkt auf eine Datei aus, sehen wir den Unterschied:
```console
user@pc:~/fehlerfrei$ cp test/fehlerfrei_test.exs test/duplikate_test.exs # Testdatei duplizieren, dann Modulnamen ändern
user@pc:~/fehlerfrei$ mix test
............

Finished in 0.1 seconds
2 doctests, 10 tests, 0 failures
user@pc:~/fehlerfrei$ mix test test/fehlerfrei_test.exs
......

Finished in 0.1 seconds
1 doctest, 5 tests, 0 failures
```
Zugegeben, man erkennt es nur an den Punkten und an der Gesamtanzahl. Hier schafft uns eine ausführlichere Ausgabe mehr Durchblick. Mit `--trace` können wir sehen, welche Tests ausgeführt werden. Bei _Trace_ werden die erfolgreichen Tests nicht mit einem Punkt, sondern auch mit ihrem Titel aufgelistet.  

Möchten wir nur genau einen Test ausführen können wir zusätzlich noch eine Zeilennummer angeben:
```console
user@pc:~/fehlerfrei$ mix test --trace test/fehlerfrei_test.exs:24 
Excluding tags: [:test]
Including tags: [line: "24"]

FehlerfreiTest
  * test that 5 is greater than 4 (excluded)
  * doctest Fehlerfrei.hello/0 (1) (excluded)
  * test that 6 divided by 2 is the same as 3 (excluded)
  * test that Lagerregal is a palindrom (excluded)
  * test greets the world (excluded)
  * test gausssum (0.00ms)

Finished in 0.03 seconds
1 doctest, 5 tests, 0 failures, 5 excluded
```

## Tests auslassen & Standardeinstellungen

Oft schleppt man unvollendete oder kaputt gegangene Tests mit sich her. Damit diese nicht jedes Mal als fehlgeschlagen angezeigt werden, können wir diese mit `@tag :skip` in der Zeile oberhalb des Tests überspringen. Weiter können wir eigene Tags vergeben, z. B. ist ein Tag `:fixme` sinnvoll. Wir schreiben nun in die Test-Helfer-Datei unter `test/test_helper.exs`, dass Tests mit `@tag :fixme` generell übersprungen werden:
```elixir
ExUnit.start([exclude: :fixme])
```
Hier können wir übrigens alle Optionen verwenden, die `mix test` auch als Kommandozeilenschalter interpretiert. Möchten wir z. B. standardmäßig die ausführliche Ausgabe können wir `trace: true` in die Liste der Optionen hinzufügen. Rufen wir nun `mix test` mit `--include fixme` auf werden auch die Fixme-Tests mit ausgeführt.  

Die Test-Helfer-Datei wird vor jedem Testdurchlauf ausgeführt. Wir können sie zum Beispiel benutzen um Konfigurationen auszugegeben oder welche Version der Datenbank gerade benutzt wird.  


## Fazit

Da Elixir recht jung ist, wurde das Tooling um die Tests sehr strukturiert und umfassend aufgebaut. Insbesondere die Detailliertheit bei Fehlern oder die Steuerung der Ausgabe mit Einschränkungen, _trace_, etc. machen Tests schreiben etwas angenehmer.  

Das Test-Tooling von Elixir bietet uns noch einiges mehr: Wir können Fehlermeldungen oder Logging sehr einfach mit testen oder mit Test-Kontexten für jeden Test einen vordefinierten Ausgangszustand bereitstellen. Diese Themen werden wir in einem späteren Artikel betrachten.
<!-- more end -->
