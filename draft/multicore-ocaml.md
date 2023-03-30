---
layout: post
description: Multicore-Programmierung mit OCaml 5
title: "Multicore-Programmierung mit OCaml 5"
author: michael-sperber
tags: ["OCaml", "Nebenläufigkeit", "Parallelität]
---

In diesem Post geht es endlich mal wieder um
[OCaml](https://ocaml.org/), eine großartige funktionale Sprache, die
es schon seit 1996 gibt.  (Wenn man den Vorgänger Caml mitzählt sogar
schon seit 1985.)

OCaml hat wie viele andere funktionale Sprachen seinen Ursprung in der
Forschung am INRIA in Frankreich, ist aber seit ca. 20 Jahren
zunehmend auch in industriellen Projekten im Einsatz, zum Beispiel bei
[Facebook](https://github.com/facebook/flow), [Jane
Street](https://blog.janestreet.com/why-ocaml/) und
[Bloomberg](https://www.bloomberg.com/company/preqss/open-source-at-bloomberg-introducing-bucklescript/).

Zum Erfolg von OCaml haben allem vor das elegante Modulsystem, die
ergonomische Syntax, der schnelle Byte-Code- und der effiziente
Native-Code-Compiler beigetragen.  Ein Manko plagte aber die
OCaml-Programmierung seit den Anfangstagen: OCaml unterstützte bis
vor kurzem keine Multicore-Implementierung, zumindest nicht direkt.

Mit dem Release von [OCaml 5.0.0](https://ocaml.org/news/ocaml-5.0) im
Dezember 2022 ist das Problem allerdings ausgeräumt:
Multicore-Programmierung sowie Nebenläufigkeit werden unterstützt.
Das Multicore-OCaml-Team um
[KC Sivaramakrishnan](https://kcsrk.info/) hat sich viel Zeit
gelassen, um wirklich alles richtig zu machen.

"Multicore OCaml" hat eine ganze Reihe von Facetten, heute geht es
speziell um die Nutzung von Betriebssystem-Threads.  In späteren Posts
werden wir uns auch noch die Unterstützung von Nebenläufigkeit mit
"effect handlers" anschauen sowie die effiziente parallele
Programmierung mit "Reagents".

<!-- more start -->

## Was ist eigentlich ein Thread?

Um die Multicore-Unterstützung in OCaml einordnen zu können, klären
wir zunächst ein paar Begriffe:

Programmiersprachen, die Parallelismus oder Nebenläufigkeit
unterstützen, bieten häufig an, Code in "Threads" auszuführen, wobei
mehrere Threads "gleichzeitig" laufen können.  Allerdings verstehen
unterschiedliche Sprachen beziehungsweise Laufzeitumgebungen darunter
sehr unterschiedliche Dinge:

- "Betriebssystem-Threads", die einen Thread in der
  Programmiersprache auf einen Thread des Betriebssystems abbilden,
  das für diesen dann das Scheduling insbesondere auf
  Multicore-Systemen erledigt.

- ["Green Threads"](https://en.wikipedia.org/wiki/Green_thread), von
  denen mehrere innerhalb eines Betriebssystem-Threads laufen können,
  und deren Scheduling von der Laufzeitumgebung der Programmiersprache
  erledigt wird.
  
Warum braucht man Green Threads?
Betriebssystem-Threads sind oft vergleichsweise teuer, was den
Platzverbrauch und den Zeitaufwand beim Start betrifft.  (Weshalb es
dann zum Beispiel in Java "Thread Pools" gibt, um die teuer erzeugten
Threads zu rezyklieren.)  "Green Threads" kommen in vielen
Programmiersprachen mit deutlich geringerem Overhead aus, insbesondere
in funktionalen Sprachen wie Erlang, Scheme oder Haskell.

Traditionell waren diese beiden Modelle eine
Entweder-Oder-Entscheidung: Programmiersprachen-Implementierungen mit
Green Threads haben traditionell beim Start eine feste Menge
Betriebssystem-Threads gestartet und auf denen dann das Scheduling der
Green Threads erledigt.  Damit wurde aber der Scheduling-Prozess als
ganzer oft komplex und intransparent und nicht für alle Anwendungen
passend.

OCaml geht einen anderen Weg und gibt Programmierys direkten,
transparenten Zugriff auf die Betriebssystem-Threads.  Das OCaml-Team
hat dabei besonders großen Wert darauf gelegt, dass alter Code ohne
nennenswerten Laufzeitverlust auch auf Multicore-OCaml läuft.

Multicore OCaml kommt außerdem mit einem "Green Thread Construction
Kit": Man kann sich "Green Threads" selber bauen, gegebenenfalls
maßgeschneidert für einen bestimmten Anwendungszweck.  Darüber mehr,
wie gesagt, in einem zukünftigen Blog-Post.

In diesem Post geht es also um Betriebssystem-Threads in Multicore
OCaml.  Rudimentäre OCaml-Kenntnisse setzen wir voraus.

## Domains = Betriebssystem-Threads

Um Begriffskonfusion zu vermeiden, heißt die OCaml-Schnittstelle zu
Betriebssystem-Threads
["Domains"](https://v2.ocaml.org/api/Domain.html).  Eine Domain ist eine
Berechnung, die in einem Betriebsystem-Thread abläuft und ein Ergebnis
produziert.  Das`Domain`-Modul definiert entsprechend einen Typ für
Domains:

```ocaml
type 'a t
```

Ein OCaml-Modul kann also unter dem Damen `Domain.t` auf diesen Typ zugreifen.
Der Typparameter `'a` ist der Typ des Ergebnisses.

Die beiden wesentlichen Funktionen zu Domains erlauben, eine Domain
zu starten und auf deren Beendigung zu  warten:

```ocaml
val spawn : (unit -> 'a) -> 'a t
val join : 'a t -> 'a
```

Der Code in einer Domain kann mit anderen Domains über
Standard-Mechanismen der parallelen Programmierung kommunizieren -
über geteilten Zustand sowie
[Mutexe](https://v2.ocaml.org/api/Mutex.html) und
[Semaphoren](https://v2.ocaml.org/api/Semaphore.html) und
[Condition-Variablen](https://v2.ocaml.org/api/Condition.html), um den
Zugriff darauf zu koordinieren.  

Diese Low-Level-Mechanismen sind für die tägliche Programmierung
allerdings ziemlich umständlich und fehleranfällig.  Darum gibt es
eine praktische Library namens
[Domainslib](https://github.com/ocaml-multicore/domainslib), die auf
der primitiven Domain-Funktionalität aufbaut.  Sie bietet:

- [Async/await-Operationen](https://www.ocaml.org/p/domainslib/0.3.2/doc/Domainslib/Task/index.html)
- [parallele Iteration](https://www.ocaml.org/p/domainslib/0.3.2/doc/Domainslib/Task/index.html)
- [Tasks und Promises](https://www.ocaml.org/p/domainslib/0.3.2/doc/Domainslib/Task/index.html)
- [Channels für Message-Passing](https://www.ocaml.org/p/domainslib/0.3.2/doc/Domainslib/Chan/index.html)

## Kanäle

Für unser Code-Beispiel benutzen wir
[Channels](https://www.ocaml.org/p/domainslib/0.3.2/doc/Domainslib/Chan/index.html).
Ein Channel kann für die Übertragung von Nachrichten zwischen
zwei Domains benutzt werden.  Der dazugehörige Typ ist:

```ocaml
type 'a t
```

Außerdem gibt es Funktionen zum Senden und Empfangen von Nachrichten:

```ocaml
val send : 'a t -> 'a -> unit
val recv : 'a t -> 'a
```

Nun ist jeder Channel mit einem Puffer versehen, in dem `send` Nachrichten
zunächst speichert, damit `recv` sie danach abholen kann.
Diesen Puffer gibt es in zwei Varianten, je nachdem, welche Funktion
zur Konstruktion verwendet wird:

```ocaml
val make_bounded : int -> 'a t
```

Diese Funktion macht einen Channel mit einem Puffer fester Größe.
Läuft er voll, blockiert `send` so lange, bis wieder Platz ist.  Es
ist möglich, einen *synchronen* Channel zu machen – mit Puffergröße 0.
Dieser blockiert bei `send` immer, bis ein `recv` erfolgt,
beziehungsweise umgekehrt.

```ocaml
val make_unbounded : unit -> 'a t
```

Diese Funktion macht einen Puffer unbeschränkter Größe – `send` kann
hier gar nicht blockieren.

Von `send` und `receive` gibt es auch Varianten, die "pollen", also
nicht blockieren können:

```ocaml
val send_poll : 'a t -> 'a -> bool
val recv_poll : 'a t -> 'a option
```

`send_poll` liefert `true`, wenn das Senden geklappt hat, und sonst
`false`.  Bei `recv_poll` kommt `Some <message>` zurück, wenn eine
Nachricht wartete, sonst `None`.


## Dinierende Philosophen

Um die Funktionalität von Multicore OCaml zu demonstrieren, nehmen wir
uns das
[Philosophenproblem](https://de.wikipedia.org/wiki/Philosophenproblem)
vor – ein Klassiker der nebenläufigen Programmierung, ursprünglich
formuliert von Edsger W. Dijkstra.

Wikipedia beschreibt es so:

"Fünf Philosophen, nummeriert von 0 bis 4, leben in einem Haus, in dem
der Tisch für sie gedeckt ist, wobei jeder Philosoph seinen eigenen
Platz am Tisch hat. Ihr einziges Problem – neben dem der Philosophie –
ist, dass es sich bei dem servierten Gericht um eine sehr schwierige
Sorte Spaghetti handelt, die mit zwei Gabeln gegessen werden
muss. Zwischen den Tellern befindet sich jeweils eine Gabel, so dass
dies für einen einzelnen Philosophen kein Problem
darstellt. Allerdings können zwei Nachbarn nicht gleichzeitig essen."

Wir realisieren die [Lösung von Chandy und
Misra](https://www.cs.utexas.edu/users/misra/scannedPdf.dir/DrinkingPhil.pdf),
die folgendermaßen funktioniert:

- Die Gabeln können die Zustände "sauber" und "schmutzig" annehmen.
  Am Anfang sind alle Gabeln schmutzig.
- Wenn einer der Philosophen essen will, müssen sie eine Anfrage an
  den entsprechenden Nachbarn schicken.
- Wenn ein Philosoph mit einer Gabel eine Anfrage bekommt, beantwortet
  er diese, sobald die Gabel schmutzig ist – dann macht er sie
  zunächst sauber und gibt sie dann dem Anfragenden.
- Wenn ein Philosoph zwei saubere Gabeln hat, isst er und macht beide
  Gabeln so schmutzig.
  
Wir erledigen erstmal die Domänenmodellierung.  Bei den Gabeln wird
zwischen linker und rechter Gabel unterschieden:

```ocaml
type which_fork = Left | Right
```

Wir modellieren außerdem den "Zustand" jeder Hand eines Philosophen
entsprechend dem Paper von Chandy/Misra:

```ocaml
type hand = No_fork | Requested | Dirty | Clean
```

Die Hand kann leer sein (`No_fork`), der Philosoph könnte den
Nachbar-Philosophen eine Anfrage auf die entsprechende Gabel geschickt
haben (`Requested`) und in der Hand könnte eine schmutzige oder eine
saubere Gabel liegen (`Dirty` beziehungsweise `Clean`).

Wir nummerieren die Philosophen durch und statten sie mit einem Kanal
aus, über den sie Gabeln oder Anfragen auf Gabeln bekommen können.
Wir nehmen außerdem eine Nachricht auf, die den Philosophen anweist,
seine Domain zu beenden:

```ocaml
type philosopher = { number: int; channel: message Domainslib.Chan.t }
and message =
  | Heres_fork of which_fork
  | Want_fork of philosopher * which_fork
  | Die
```

Zur Bedeutung der Nachrichten: 

- Mit der `Heres_fork`-Nachricht schickt ein Philosoph A einem
  Philosophen B die Gabel, und zwar mit den Links-/Rechts-Seiten von
  Philosoph B.
- Mit der `Want_fork`-Nachricht schickt Philosoph A einem Philosophen
  B, wenn er eine Gabel braucht – auch hier mit den
  Links-/Rechts-Seiten von Philosoph B.  In die Nachricht steckt
  Philosoph A außerdem sein eigenes Objekt.
  
Um ein Philosophen-Objekt zu konstruieren, machen wir eine kleine
Convenience-Funktion, die den Channel erzeugt:

```ocaml
let make_philosopher number =
  { number = number; channel = Domainslib.Chan.make_unbounded () }
```

Kommen wir nun zur eigentlichen Lebensführung der Philosophen bei
Tisch – diese erledigt die Funktion `run_philosopher`.  Der geben wir
den "aktuellen" Philosophen mit, den linken und den rechten Nachbarn,
sowie die Anfangszustände der beiden Hände:

```ocaml
let run_philosopher
      (me: philosopher) (left_neighbor: philosopher) (right_neighbor: philosopher)
      (left_hand: hand) (right_hand: hand) =
```

Jeder Philosoph befindet sich in einer Schleife, in der er die
Hände-Zustände mitführt.  Außerdem merkt er sich, ob der linke oder
rechte Nachbar um eine Gabel gebeten hat:

```ocaml
  let rec loop
      (left_hand: hand) (right_hand: hand)
      (wants_left: philosopher option) (wants_right: philosopher option) =
```

In jeder Runde von `loop` schauen wir erstmal, ob wir eine
Nachricht bekommen haben und bearbeiten diese gegebenenfalls:
```
    match Domainslib.Chan.recv_poll me.channel with
    | Some Die -> ()
    | Some (Heres_fork Left) ->
       loop Clean right_hand wants_left wants_right
    | Some (Heres_fork Right) ->
       loop left_hand Clean wants_left wants_right
    | Some (Want_fork (who, Left)) ->
       loop left_hand right_hand (Some who) wants_right
    | Some (Want_fork (who, Right)) ->
       loop left_hand right_hand wants_left (Some who)
```

Falls keine Nachricht anliegt, kümmern wir uns um die möglichen
Zustandskombinationen.  Zunächst mal der Fall, wenn der Philosoph
beide Gabeln hat und deshalb essen kann:

```ocaml
    | None ->
       match (left_hand, right_hand, wants_left, wants_right) with
       | (Clean, Clean, _, _) ->
          loop Dirty Dirty wants_left wants_right
```

Wenn die linke Gabel schmutzig ist und der linke Nachbar sie will,
schickt der Philosoph sie ihm mit einer `Heres_fork`-Nachricht:

```ocaml
       | (Dirty, _, Some left_philosopher, _) ->
          begin
            Domainslib.Chan.send left_philosopher.channel (Heres_fork Right);
            loop No_fork right_hand None wants_right
          end
```

Andersherum: Die rechte Gabel ist schmutzig und der rechte
Nachbar will sie:

```ocaml
       | (_, Dirty, _, Some right_philosopher) ->
          begin
            Domainslib.Chan.send right_philosopher.channel (Heres_fork Left);
            loop left_hand No_fork wants_left None
          end
```

Jetzt fehlt noch die Gegenseite, wo dem Philosophen die linke oder
rechte Gabel fehlt – dann schickt der Philosoph eine
`Want_fork`-Anfrage an den entsprechenden Nachbarn:

```ocaml
       | (No_fork, _, _, _) ->
          begin
            Domainslib.Chan.send left_neighbor.channel (Want_fork (me, Right));
            loop Requested right_hand wants_left wants_right
          end
       | (_, No_fork, _, _) ->
          begin
            Domainslib.Chan.send right_neighbor.channel (Want_fork (me, Left));
            loop left_hand Requested wants_left wants_right
          end
```

Der Default macht einfach weiter in der Hoffnung, dass irgendwann eine
Nachricht kommt:

```ocaml
       | _ -> loop left_hand right_hand wants_left wants_right
```

Schließlich müssen wir die `loop`-Schleife noch in einer neuen Domain
starten:

```ocaml
  in Domain.spawn (fun _ -> loop left_hand right_hand None None)
```

Fertig!  Also fast ... so wie's ist, gibt's noch zwei Probleme:

- Philosophen sterben nie.
- Wir bekommen nicht mit, was sie tun.

Für's erste Problem können wir schonmal eine Funktion schreiben:

```ocaml
let kill (philosopher: philosopher) =
  Domainslib.Chan.send philosopher.channel Die
```

Fürs zweite Problem wollen wir jeden Philosophen Meldung erstatten lassen,
wenn er isst.  Man ist versucht, das mit `Printf.printf` zu machen -
da unser Programm mehrere Betriebssystem-Threads startet, würden die
Ausgaben aber durcheinanderkommen.  Wir verschieben das Problem
erstmal und abstrahieren über einer Funktion `print`, die einen Text
ausdruckt.  Das gibt uns Gelegenheit, die ganze
`run_philosopher`-Funktion nochmal im Zusammenhang abzudrucken:

```ocaml
let run_philosopher (print: string -> unit)
      (me: philosopher) (left_neighbor: philosopher) (right_neighbor: philosopher)
      (left_hand: hand) (right_hand: hand) =
  let rec loop
      (left_hand: hand) (right_hand: hand)
      (wants_left: philosopher option) (wants_right: philosopher option) =
    match Domainslib.Chan.recv_poll me.channel with
    | Some Die -> ()
    | Some (Heres_fork Left) ->
       loop Clean right_hand wants_left wants_right
    | Some (Heres_fork Right) ->
       loop left_hand Clean wants_left wants_right
    | Some (Want_fork (who, Left)) ->
       loop left_hand right_hand (Some who) wants_right
    | Some (Want_fork (who, Right)) ->
       loop left_hand right_hand wants_left (Some who)
    | None ->
       match (left_hand, right_hand, wants_left, wants_right) with
       | (Clean, Clean, _, _) ->
          begin
            print (Printf.sprintf "philosopher %d ate" me.number);
            loop Dirty Dirty wants_left wants_right
          end
       | (Dirty, _, Some left_philosopher, _) ->
          begin
            Domainslib.Chan.send left_philosopher.channel (Heres_fork Right);
            loop No_fork right_hand None wants_right
          end
       | (_, Dirty, _, Some right_philosopher) ->
          begin
            Domainslib.Chan.send right_philosopher.channel (Heres_fork Left);
            loop left_hand No_fork wants_left None
          end
       | (No_fork, _, _, _) ->
          begin
            Domainslib.Chan.send left_neighbor.channel (Want_fork (me, Right));
            loop Requested right_hand wants_left wants_right
          end
       | (_, No_fork, _, _) ->
          begin
            Domainslib.Chan.send right_neighbor.channel (Want_fork (me, Left));
            loop left_hand Requested wants_left wants_right
          end
       | _ -> loop left_hand right_hand wants_left wants_right
  in Domain.spawn (fun _ -> loop left_hand right_hand None None)
```

Um die `print`-Funktion zu realisieren, erledigen wir das Ausdrucken
in einer separaten Domain, der wir über einen Channel die zu
druckenden Texte übermitteln:

```ocaml
let start_print_domain () =
  let chan = Domainslib.Chan.make_unbounded () in
  let rec loop () =
    let text = Domainslib.Chan.recv chan in
    begin
      Printf.printf "%s\n" text;
      flush stdout;
      loop ()
    end
  in
  let domain = Domain.spawn (fun _ -> loop ()) in
  (domain, fun text -> Domainslib.Chan.send chan text)
```

(Warum `make_unbounded`, fragen Sie vielleicht.  Dazu gleich.)

Jetzt können wir die fünf Philosophen starten.  Dabei müssen wir
darauf achten, dass die Zustände keinen Zyklus bilden – sonst werden
immer nur Gabeln herumgereicht und jeder hat immer nur eine.  Darum
bekommt der erste Philosophen zwei Gabeln und der letzte keine:

```ocaml
let philosophers_5 () =
  let (print_domain, print_text) = start_print_domain () in
  let p1 = make_philosopher 1 in
  let p2 = make_philosopher 2 in
  let p3 = make_philosopher 3 in
  let p4 = make_philosopher 4 in
  let p5 = make_philosopher 5 in
  let run = run_philosopher print_text in
  let domain1 = run p1 p5 p2 Dirty Dirty in
  let domain2 = run p2 p1 p3 No_fork Dirty in
  let domain3 = run p3 p2 p4 No_fork Dirty in
  let domain4 = run p4 p3 p5 No_fork Dirty in
  let domain5 = run p5 p4 p1 No_fork No_fork in
  ()
```

Allerdings hat das Programm noch ein Manko: Es terminiert nicht.
Das ist im Philosophenproblem durchaus so angelegt, aber irgendwann
reicht's auch.  Wir lösen das so, dass wir die `print`-Funktion
begrenzen, so dass sie nach einer bestimmten Anzahl von Ausgaben
Schluss macht:

```ocaml
let start_print_domain (n: int) =
  let chan = Domainslib.Chan.make_unbounded () in
  let rec loop (n: int) =
    if n > 1
    then
      let text = Domainslib.Chan.recv chan in
      begin
        Printf.printf "%s\n" text;
        flush stdout;
        loop (n-1)
      end
    else ()
  in
  let domain = Domain.spawn (fun _ -> loop n) in
  (domain, fun text -> Domainslib.Chan.send chan text)
```

Jetzt können wir darauf warten, dass die `print`-Domain fertig ist,
dann töten wir die ganzen Philosophen und warten, bis sie auch gestorben
sind:

```ocaml
let philosophers_5 () =
  let (print_domain, print_text) = start_print_domain 1000 in
  let p1 = make_philosopher 1 in
  let p2 = make_philosopher 2 in
  let p3 = make_philosopher 3 in
  let p4 = make_philosopher 4 in
  let p5 = make_philosopher 5 in
  let run = run_philosopher print_text in
  let domain1 = run p1 p5 p2 Dirty Dirty in
  let domain2 = run p2 p1 p3 No_fork Dirty in
  let domain3 = run p3 p2 p4 No_fork Dirty in
  let domain4 = run p4 p3 p5 No_fork Dirty in
  let domain5 = run p5 p4 p1 No_fork No_fork in
  begin
    Domain.join print_domain;
    kill p1; kill p2; kill p3; kill p4;  kill p5;
    Domain.join domain1;
    Domain.join domain2;
    Domain.join domain3;
    Domain.join domain4;
    Domain.join domain5
  end
```

Ach so: Ich wollte noch schreiben, warum der Puffer in
`start_print_domain` mit `make_unbounded` erzeugt wird: Nehmen wir an,
dass er es nicht ist – dann würden die `print`-Aufrufe der Philosophen
blockieren und sie kämen nie dazu, die `Die`-Nachricht zu verarbeiten.

Um es nochmal zusammenfassen:

- Seit Version 5.0 unterstützt OCaml Multicore-Programmierung.
- Existierende OCaml-Programme laufen unverändert, ohne
  Performance-Verlust.
- OCaml unterstützt Parallelität mit Domains, die
  Betriebssystem-Threads abbilden.
- Nebenläufigkeit und "Green Threads" werden mit Hilfe von "effect
  handlers" unterstützt, zu denen wir noch separat schreiben werden.

Das war's jetzt aber – viel Vergnügen mit Multicore OCaml!

<!-- more end -->
