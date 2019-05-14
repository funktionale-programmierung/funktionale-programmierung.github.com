---
layout: post
description: Wie man sich Daten für Tests zurecht legen kann
title: "Testen in Elixir mit Beispieldaten"
author: tim-digel
tags: ["Elixir", "Erlang", "Test", "Testing", "ExUnit", "Mix", "Setup", "Context", "Describe"]
---

Elixir bietet uns eine einfache Möglichkeit Testdaten übergreifend zu benutzen. Mit sogenannten Kontexten können wir eine Startbasis an Daten und anderen Dingen definieren, die unsere Tests komplett oder in Teilen benutzen können. Wer neu in Elixir ist, kann mit [Test-ABC mit Elixir](https://funktionale-programmierung.de/2019/03/27/elixir-test-abc.html) ein kurze Einführung in die Elixir-Test-Welt bekommen.
<!-- more start -->

## Bevor es los geht

Wer schon ein bestehendes Projekt hat kann diesen Abschnitt überspringen. Für alle anderen erstellen wir uns zuerst eine Spielwiese. Wir verwenden Elixir in Version 1.8 auf Erlang 21, zumal die Versionen keine große Beudetung für unsere Tests haben werden. Wie man Elixir & Co schnell installieren kann haben wir bereits in [Mit Nix raus aus der Versionshölle](https://funktionale-programmierung.de/2018/02/19/nix.html) gesehen.
Nun legen wir in einem Verzeichnis mit `mix new fehlerfrei` ein Projekt mit dem Namen _Fehlerfrei_ an. Mix erstellt uns einige hilfreiche Dinge, wie z. B. auch eine Projekt-Liesmich oder die Gitignore-Datei.

## Basis für Tests

In einführenden Beispielen lernt man immer nur einfache 3-Zeiler Tests kennen. In realen Anwendung kommt man schnell zu dem Punkt, wo ein Test die letzten 10% einer Funktion oder eines Programms testen soll. Die ersten 90% über 10 Tests hinweg sind dabei meistens gleich. Möchte man das ewige Beispiel eines Onlineshops bemühen, braucht man für Tests wie _Übersicht der Produkte_, _Produkte in den Warenkorb legen_, _Kauf abschließen als registrierter Benutzer_ oder _Benutzerkonto löschen_ einiges vor ab. Auf jeden Fall sind dies eine Reihe an Produkten und ein Benutzerkonto als Datenobjekt. Dafür definieren wir uns kurz zwei zusammengesetzten Datentypen, _Product_ und _Account_:
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
Diese Definitionen hätten wir normalerweise einzeln in eigenen Dateien gepackt, können wir hier aber auch direkt vor unser Modul `Fehlerfrei` in `lib/fehlerfrei.ex` setzen. 

Damit können wir uns jetzt Beispieldaten erstellen. Da wir bei Tests in einem Modul sind, könnten wir diese einfach außerhalb der Tests definieren und verwenden. Wir verlieren dann schnell die Übersicht. Elixir bietet uns die Möglichkeit einen Test-Kontext aufzubauen. Wir definieren uns am Anfang ein `setup`, z. B. mit Datenobjekte als _Structs_. In unserem Beispiel legen wir Instanzen von Produkten und Benutzerkonten fest:
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
Am Ende von `setup` steht der Rückgabewert, das ist ein Tupel mit `:ok` und z. B. einer Keyword-Liste (die eckigen Klammern werden hier oft weg gelassen, zum besseren Verständnis: `{:ok, [a1: objekt1, ...]}`). In unseren Tests können wir jetzt den übergebenen Kontext an eine Variable binden, z. B. `context` im Testfall, der die zuvor definierte Funktion `change_availability` überprüft:
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
In `change_availability` benutzen wir den Pipe-Syntax (`|`) innerhalb eines Structs mit dem man einen vorhandenen Struct in seinen Feldern verändern kann.  

Für einen weiteren Test betrachten wir z. B. nur die beiden Benutzerkonten. Wir müssen nicht zwangsweise den ganzen Kontext in den Test einbinden. Da Elixir Pattern Matching anwendet, können wir einzelne Teile des Kontext direkt an Variablen binden und andere gar nicht verwenden.
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

Oft kommt es vor, dass man für eine Reihe an Tests eine minimal geänderte Grundlage braucht. Man könnte alle Objekte im zuvor kennen gelernten Setup duplizieren und dann abändern. Dadurch würde man viel Doppelung bekommen und viel Übersicht verlieren. Mit Hilfe von `describe` können wir Tests zu einem Block zusammenfassen. Dies ist oft schon für eine bessere Übersicht sinnvoll. Innerhalb `describe` können wir dann ein Setup anhand Funktionen definieren, die den Kontext erstellen, verändern, erweitern oder reduzieren.  

Dafür brauchen wir eine Funktion, die einen Kontext nimmt und wieder zurück gibt. Wir definieren uns zwei Funktionen: Eine, die alle Produkte auf Verfügbarkeit 0 setzt und eine, die eine Liste mit allen Produkten dem Kontext hinzufügt:
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
Im ersten Beispiel verwenden wir wieder den Pipe-Syntax, um `pRasen` und `pKabel` zu überschreiben. In `context_put_products_to_list` vereinen wir den alten Kontext mit einer Map, die unser neues Feld enthält. Wir möchten die folgenden beiden Implementierungen testen:
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
Wir können uns jetzt zwei getrennte Gruppen mit `describe` definieren: Tests mit nicht verfügbaren Produkten bekommen als Setup ein Kontext, der durch `context_products_unavailable` und `context_put_products_to_list` geschleift wurde. Tests mit verfügbaren Produkten bekommen hingegen nur den Kontext bearbeitet durch die Funktion, welche die Produkte in eine Liste packt.
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
Die bei `describe` angegebenen Titel werden an den eigentlichen Testtitel voran gestellt (siehe `mix test --trace`). Mit `setup` am Anfang von `describe` geben wir eine Liste von Funktionsnamen als Atome an. Durch diese Funktion wird der globale Kontext von links nach rechts durch gereicht, bevor er einem Testfall übergeben wird.  

## Fazit

Mit Setup können wir Beispieldaten oder andere Vorarbeiten übersichtlich struktuieren. Für jeden Testfall wird dieser Kontext neu erstellt und wie wir oben gesehen haben, ganz oder teilweise mittels Pattern Matching verfügbar gemacht. Mit Funktionen, die einen Kontext konsumieren und einen Kontext wieder zurück geben, können wir mit `describe` Tests gruppieren und mit beliebigen Kontext-Funktionen die Beispieldaten modifizieren. Darüber hinaus kann man sich viele weitere Anwendungsfälle für ein Test-Setup vorstellen. Eine Idee wäre, eine externe Datenbank zu löschen und inital mit Daten zu befüllen. Jeder Test hätte dadurch einen festen Datenbestand und funktioniert unabhängig anderer Tests. In diesem Fall muss aber darauf geachtet werden, die Tests nicht parallel auszuführen, was der Standardeinstellung entspricht.
<!-- more end -->
