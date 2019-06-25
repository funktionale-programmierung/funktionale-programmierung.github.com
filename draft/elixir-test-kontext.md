---
layout: post
description: Wie man sich Daten für Tests zurecht legen kann
title: "Testen in Elixir mit Beispieldaten"
author: tim-digel
tags: ["Elixir", "Erlang", "Test", "Testing", "ExUnit", "Mix", "Setup", "Context", "Describe"]
---

Elixir bietet uns eine einfache Möglichkeit, Testdaten übergreifend zu nutzen. Mit sogenannten Kontexten können wir eine Startbasis an Daten definieren, die unsere Tests komplett oder in Teilen verwenden können. Wir lernen das Konstrukt `setup` kennen. Neben Beispieldaten können wir auch Funktionsaufrufe mit Seiteneffekten im Setup ausführen, z. B. Löschen von vorherigen Testerzeugnissen auf der Festplatte oder Starten von externen Diensten.  

Wer neu in Elixir ist, kann mit [Test-ABC mit Elixir](https://funktionale-programmierung.de/2019/03/27/elixir-test-abc.html) eine kurze Einführung in die Elixir-Test-Welt bekommen.
<!-- more start -->

## Bevor es los geht

Wer schon ein bestehendes Projekt hat, kann dieses Kapitel überspringen. Wir erstellen uns zuerst eine Spielwiese. Dabei verwenden wir Elixir in Version 1.8 auf Erlang 21, wobei die Versionen keine große Bedeutung für unsere Tests haben werden. Wie man Elixir & Co schnell installieren kann, haben wir bereits in [Mit Nix raus aus der Versionshölle](https://funktionale-programmierung.de/2018/02/19/nix.html) gesehen.
Nun legen wir in einem Verzeichnis mit `mix new fehlerfrei` ein Projekt mit dem Namen _Fehlerfrei_ an. Mix erstellt uns einige hilfreiche Dinge, wie z. B. auch eine Projekt-Readme oder die Gitignore-Datei.

## Basis für Tests

In einführenden Beispielen lernt man immer nur einfache 3-Zeiler-Tests kennen. In realen Anwendung kommt man schnell zu dem Punkt, an dem ein Test die letzten herausfordernden 10% einer Funktion oder eines Programms testen soll. Die ersten 90% über 10 Tests hinweg sind dabei meistens gleich. Möchte man das ewige Beispiel eines Onlineshops bemühen, braucht man zum Testen für Funktionalität wie _Übersicht der Produkte_, _Produkte in den Warenkorb legen_, _Kauf abschließen als registrierter Benutzer_ oder _Benutzerkonto löschen_ einiges an Vorarbeit. Auf jeden Fall schließt dies eine Reihe an Produkten und ein Benutzerkonto als Datenobjekte ein. Dafür definieren wir uns zwei zusammengesetzte Datentypen, _Product_ und _Account_:
```elixir
  defmodule Account do
    @enforce_keys [:email]
    defstruct email: nil,
      password: "",
      address: ""

    @type t() :: %__MODULE__{
      email: String.t() | nil,
      password: String.t(),
      address: String.t()
    }
  end

  defmodule Product do
    @enforce_keys [:id, :price]
    defstruct id: nil,
      title: "",
      description: "",
      price: 0,
      available: 0

    @type t() :: %__MODULE__{
      id: String.t(),
      title: String.t(),
      description: String.t(),
      price: float(),
      available: non_neg_integer()
    }
  end
```
Diese Definitionen hätten wir normalerweise einzeln in eigenen Dateien gepackt, wir können diese aber auch direkt vor dem Modul `Fehlerfrei` in `lib/fehlerfrei.ex` definieren. Mit `defstruct` definieren wir den Datentyp. `@type t()` legt eine Typsignatur für `%__MODULE__{}` fest, also für `%Account{}` bzw. `%Product{}`. 

Damit erstellen wir uns jetzt Beispieldaten. Da wir bei Tests in einem Modul sind, könnten wir diese Definitionen von Testdaten einfach außerhalb der Tests definieren, würden dann aber schnell die Übersicht verlieren.
Elixir bietet uns daher die Möglichkeit, einen Test-Kontext aufzubauen. Wir definieren uns am Anfang ein `setup`, z. B. mit Datenobjekten als _Structs_. In unserem Beispiel legen wir Instanzen von Produkten und Benutzerkonten fest:
```elixir
 setup do
    pRasen = %Product{id: "10000815",
                      title: "Rasenmäher",
                      description: "Mit Turbofunktion",
                      price: 399.99,
                      available: 10}
    pKabel = %Product{id: "10000816",
                      title: "Verlängerungskabel",
                      description: "10 Meter",
                      price: 24.99,
                      available: 0}

    aMargo = %Account{email: "margo.musterman@example.com",
                      password: "00e3261a6e0d79c329445acd540fb2b07187a0dcf6017065c8814010283ac67f",
                      address: "Margo Mustermann, Hauptstraße 12, 01234 Musterhausen"}

    aRobin = %Account{email: "robin.mustermann@example.com",
                      password: "54d03b8c1bea08ef8896747edc304ff22fbe71a9d764ef9a3ee7b1a4ea60a622",
                      address: ""}

    {:ok,

     pRasen: pRasen,
     pKabel: pKabel,

     aMargo: aMargo,
     aRobin: aRobin
    }
  end
```
Am Ende von `setup` steht der Rückgabewert. Das ist ein Tupel mit `:ok` und z. B. einer Keyword-Liste (die eckigen Klammern werden hier oft weg gelassen, zum besseren Verständnis: `{:ok, [a1: objekt1, ...]}`). Dieser Wert wird jedem Test als Parameter übergeben. In unseren Tests binden wir diesen übergebenen Kontext an eine Variable, z. B. an `context` im folgendem Testfall, der die zuvor definierte Funktion `change_availability` überprüft:
```elixir
  @doc "Change availability of a products."
  @spec change_availability(%Product{}, integer()) :: %Product{}
  def change_availability(%Product{} = p, new_amount) do
    %Product{p | available: p.available + new_amount}
  end

  test "new products were delivered", context do
    pRasenNew = change_availability(context.pRasen, 99)
    assert pRasenNew.available == 109
  end
```
In `change_availability` benutzen wir die Pipe-Syntax (`|`) innerhalb eines Structs, mit der man einen vorhandenen Struct in seinen Feldern verändern kann.  

Für einen weiteren Test betrachten wir nur die beiden Benutzerkonten. Wir müssen nicht zwangsweise den ganzen übergebenen Kontext im Test verfügbar machen. Mithilfe von Pattern Matching, können wir einzelne Teile des Kontexts direkt an Variablen binden und andere überhaupt nicht verwenden.
```elixir
  @doc "Check if a account has a non-empty address"
  @spec has_address?(%Account{}) :: boolean()
  def has_address?(%Account{address: ""}), do: false
  def has_address?(%Account{}), do: true

  test "the account has_address? functionality", %{aMargo: account_with_address,
                                                   aRobin: account_without_address} do
    assert has_address?(account_with_address)
    refute has_address?(account_without_address)
  end
```

## Kontexte staffeln, ändern, ...

Oft kommt es vor, dass man für eine Reihe an Tests eine minimal geänderte Grundlage braucht. Man könnte alle Objekte im zuvor kennengelernten Setup duplizieren und dann abändern. Dadurch würde man viel Doppelung bekommen und viel Übersicht verlieren. Mit Hilfe von `describe` können wir Tests zu einem Block zusammenfassen. Dies ist oft schon für eine bessere Übersicht sinnvoll. Innerhalb von `describe` können wir dann ein Setup anhand von Funktionen definieren, die den Kontext erstellen, verändern, erweitern oder reduzieren.  

Dafür brauchen wir eine Funktion, die einen Kontext entgegennimmt und wieder zurückgibt. Wir definieren uns zwei Funktionen: Eine, die die Produkte auf Verfügbarkeit 0 setzt und eine, die eine Liste mit allen Produkten dem Kontext hinzufügt:
```elixir
  # Make all products unavailable
  defp context_products_unavailable(%{pRasen: pRasen, pKabel: pKabel} = context) do
    %{context |
      pRasen: Map.put(pRasen, :available, 0),
      pKabel: Map.put(pKabel, :available, 0)}
  end

  # Put all products in a list
  defp context_put_products_to_list(%{pRasen: pRasen, pKabel: pKabel} = context) do
    Map.merge(
      context,
      %{all_products: [pRasen, pKabel]})
    # This duplicates the data in the context, we don't delete context.pRasen & context.pKabel
  end
```
Im ersten Beispiel verwenden wir erneut die Pipe-Syntax, um `pRasen` und `pKabel` zu überschreiben. In `context_put_products_to_list` vereinen wir den alten Kontext mit einer Map, die unser neues Feld enthält. Wir möchten die folgenden zwei Implementierungen testen:
```elixir
  @doc "Check if we can buy a product"
  @spec buyable?(%Product{}) :: boolean()
  def buyable?(%Product{available: n, price: s}) do
    n > 0 and s > 0
  end

  @doc "Returns list of products we need to redorder"
  @spec need_reorder([%Product{}]) :: [%Product{}]
  def need_reorder(products) do
    Enum.filter(products, fn p -> p.available <= 0 end)
  end
```
Wir können uns jetzt zwei getrennte Gruppen mit `describe` definieren: Tests mit nicht verfügbaren Produkten bekommen als Setup einen Kontext, der durch `context_products_unavailable` und `context_put_products_to_list` geschleift wurde. Tests mit verfügbaren Produkten verwenden hingegen den Kontext nur bearbeitet durch die Funktion, welche die Produkte in eine Liste packt.
```elixir
  describe "functionality with no available products:" do
    setup [:context_products_unavailable, :context_put_products_to_list]

    test "can't buy unavailable products", %{pRasen: pRasen,
                                             pKabel: pKabel} do
      refute buyable?(pRasen)
      refute buyable?(pKabel)
    end

    test "list products which are not available", %{all_products: products} do
      assert need_reorder(products) == products
    end
  end

  describe "functionality with some available products:" do
    setup [:context_put_products_to_list]

    test "list products which are not available", %{all_products: products,
                                                    pKabel: pKabel} do
      assert need_reorder(products) == [pKabel]
    end
  end
```
Die bei `describe` angegebenen Titel werden an den eigentlichen Testtitel vorangestellt (siehe `mix test --trace`). Mit dem Aufruf von `setup` am Anfang des `describe`-Blocks geben wir eine Liste von Funktionsnamen als Atome an. Durch diese Funktion wird der globale Kontext von links nach rechts durchgereicht, bevor er einem Testfall übergeben wird.  

## Fazit

Mit `setup` können wir Beispieldaten oder andere Vorarbeiten übersichtlich zu Kontexten struktuieren. Für jeden Testfall wird dieser Kontext neu erstellt und wie wir oben gesehen haben, ganz oder teilweise mittels Pattern Matching verfügbar gemacht. Mit Funktionen, die einen Kontext konsumieren und einen Kontext zurück geben, können wir mit Hilfe von `describe` Tests gruppieren und mit beliebigen Kontext-Funktionen die Beispieldaten modifizieren. Darüber hinaus kann man sich viele weitere Anwendungsfälle für ein Test-Setup vorstellen. Eine Idee wäre, eine externe Datenbank zu löschen und inital mit Daten zu befüllen. Jeder Test hätte dadurch einen festen Datenbestand und funktioniert unabhängig von anderen Tests. In diesem Fall muss aber darauf geachtet werden, dass die Tests nicht parallel ausgeführt werden, was der Standardeinstellung entspricht.
<!-- more end -->
