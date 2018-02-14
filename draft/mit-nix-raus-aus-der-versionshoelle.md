---
layout: post
description: Entwicklungsumgebungen mit der Nix-Shell
title: "Mit Nix raus aus der Versionshölle"
author: tim-digel
tags: ["Nix", "Elixir", "Erlang"]
---

Wer kennt es nicht: Man startet ein neues Softwareprojekt oder steigt bei der Entwicklung eines bestehenden Projektes mit ein und muss erst mal zahlreiche Compiler, Interpreter, Editoren, Abhängigkeiten und Weiteres installieren. Dabei heißt es nicht selten, man soll davon die Version 1.2.24-50rc4 mit Bugfix-Patch 19 installieren, sonst funktioniert es nicht.  

Mit dem Nix-Paketmanager eröffnet sich uns die Möglichkeit deklarativ zu beschreiben, wie unser lokales Setup auszusehen hat. Dabei spielt es (fast) keine Rolle, auf was für einem unixbasiertem Betriebssystem wir unterwegs sind.

In diesem Artikel sehen wir beispielhaft an einer Entwicklungsumgebung für Elixir, wie einfach, schnell, non-invasiv und versionierbar wir eine Entwicklungsumgebung bereitstellen können. Wir lernen weiter eine Möglichkeit kennen, bestimmte Versionen eines Pakets zu installieren.
<!-- more start -->

## Der Nix-Paketmanager

Beim Begriff _Paketmanager_ denken viele schon an Chaos und Abneigung, möchte doch jeder gern seinen bisher liebgewonnen Paketmanager behalten. [Nix](https://nixos.org/nix/) ist ein Paketmanager, der sich parallel zum Systempaketmanager auf Benutzerebene installieren lässt. Nix ist ebenso eine funktionalie Programmiersprache. Sämtliche Pakte innerhalb Nix sind als deaklarative _Nix-Expressions_ formuliert. In den allermeisten Fällen gibt die Deklaration an, wie das Paket anhand dem Quellcode gebaut wird. Durch ein ausgeklügeltes Cache-System ist der eigene Rechner aber nicht stundenlang mit Bauen von Paketen beschäftigt, sondern bedient sich aus den fertig gebauten Ergebnissen des Nix-Cache.

Eine weitere Perle von Nix ist die Tatsache, dass jedes Paket sein eigenes Fundament an Abhängigkeiten hat. So können wir z. B. Programme installieren, die vershiedene Versionen von Java oder Python benötigen. Weiter können wir sogar von einem Tool verschiedene Versionen installieren (siehe Abschnitt [Versionen überschreiben](#versionen)).

## Installation von Nix

Wir führen einmalig mit unserem normalen Benutzer ein Shell-Skript aus: 
```
curl https://nixos.org/nix/install | sh
```
Nix fügt in der Regel automatisch zwei Zeilen unser `.profile`-Datei hinzu, wodurch die Nix-Tools und alle installierten Nix-Pakete in unserem Pfad verfügbar sind. Bei MacOS sind womöglich andere Schritte notwendig.  

Um immer die neusten Versionen zu erhalten wechseln wir vom Stable-Zweig (entspricht einem Release vom letzten April oder Oktober) auf den Master-Channel:
```
nix-channel --add https://nixos.org/channels/nixos-unstable nixpkgs
nix-channel --update
```

Möchten wir ein Paket dauerthaft installieren, so können wir das mit `nix-env --install PAKETNAME` tun, z. B.: 
```
nano --version # Zeigt Version 2.5.3 von der Systeminstallation
nix-env --install nano
```
In einer neuen Konsole zeigt uns `nano --version` jetzt die Version _2.9.2_ aus den Nix-Paketen.

## Nicht-invasive Installationen mit der Nix-Shell

Hier sehen wir, dass wir mit `nano` nicht mehr so einfach an unser _nano_ vom Betriebssystem ran kommen. Eine Abhilfe schafft hier die Nix-Shell. Deinstallieren wir zuerst die globale _nano_-Installation aus den Nix-Paketen und wechseln anschließend in eine Nix-Shell, in der wir das Paket _nano_ verfügbar haben möchten:
```
nix-env --uninstall nano
nix-shell -p nano
```
Wir befinden uns jetzt in einer Umgebung, die uns _nano_ zusätzlich bereitstellt. Wir verlassen die Nix-Shell wie üblich mit `exit` oder _STRG+D_. 

Wir haben die Möglichkeit auch mehrere Pakete anzugeben und können uns so eine aufwendigere Umgebung bauen. Um nicht jedes mal ein Befehl mit einer Reihe von Paketen als Argumente angeben zu müssen, legen wir in unserem Projekt Verzeichnis eine Datei `default.nix` an. Diese beschreibt unsere Umgebung, insbesondere die Pakete, die wir verfügbar haben möchten.

## Umgebung für eine Elixir-Anwendung

Elixir ist eine recht neue funktionale Programmiersprache, die auf Erlang aufbaut. Mit Elixir kann man direkt auf Erlang-Bibliotheken zugreifen. Daher ist oft nicht nur die Elixir-Version selbst von Bedeutung, sondern auch die zugrundeliegende Erlang-Version. Die Nix-Pakete bieten uns hierfür schon fertige Pakete an, die verschiedene Elixir-Versionen mit verschiedenen Erlang-Versionen bereitstellen. Eine `default.nix` mit Erlang, Elixir, eine ältere Version von _NodeJS_ und ein paar Pakete um von Hand Quellcode komplilieren zu können (_autoconf_, _automake_, ...) könnte so aussehen:
```
with import <nixpkgs> {}; {
   myEnv = stdenv.mkDerivation {
      name = "myEnv";
      shellHook = ''
         export PS1="[myEnv:\w]$ "
      '';
      nativeBuildInputs = [
         autoconf
         automake
         erlangR20
         beam.packages.erlangR20.elixir_1_6
         nodejs-6_x
         ];
      src = null;
   };
}
```
Die Nix-Shell bietet uns zudem die Möglichkeit, die Konsolen-Benennung zu ändern, indem wir die Variable _PS1_ mit einer Bezeichnung (in unserem Beispiel `[myEnv:]$` belegen. Dies funktioniert meistens nur zuverlässig, wenn man systemweit kein anderes Skript (wie z. B. die Anzeige des aktuellen Git-Zweiges) für die Shell-Bezeichnung aktiv hat.

Innerhalb des Verzeichnis mit der `defaul.nix` starten wir unsere Umgebung mit:
```
nix-shell
```
Beim ersten Start werden die Pakete kompiliert oder fertig gebaut heruntergeladen, was etwas Zeit in Anspruch nimmt. Jeder weitere Start mit `nix-shell` geschieht dann unmittelbar. Innerhalb der Nix-Shell stehen uns jetzt die installierten Pakete zur Verfügung:
```
user@rechner:/tmp/my-env$ iex -v
Erlang/OTP 20 [erts-9.2] [source] [64-bit] [smp:4:4] [ds:4:4:10] [async-threads:10] [hipe] [kernel-poll:false]

IEx 1.6.0 (compiled with OTP 20)
```

## Versionen überschrieben <a id="versionen"></a>

In der Regel halten die Paketmanager der Betriebssysteme keine verschiedenen Versionen von einem Paket bereit und gewiss keine Versionen die sich nur in der dritten Stelle der Version unterscheiden. Will man bestimmte Verisonen von einem Paket haben, bleibt einem nur der Schwenk hin zum selber Kompilieren, was auch alles andere als komfortabel ist.  

Nix bietet meistens auch keine verschiedene Versionen an, mit Ausnahme bei einigen Paketen mit großer Nachfrage (bei Erlang z. B. pro Hauptversion). Wir können aber die vorhandene Version einfach überschreiben und uns so eine Version aussuchen. Dazu legen wir eine Datei `config.nix` an. Diese Datei beinhaltet eine Nix-Expression, welche die Map `pkgs` verändert.  

Möchten wir z. B. Erlang in Version 20.2.2 und Elixir in Version 1.6.1 so kommen wir zu dieser `config.nix`:
```
{
   packageOverrides = pkgs: rec {
      erlangv2022 = pkgs.stdenv.lib.overrideDerivation pkgs.erlangR20 (oldAttrs: rec {
         name = "erlang-" + version;
         version = "20.2.2";
         src = pkgs.fetchFromGitHub {
            owner = "erlang";
            repo = "otp";
            rev = "OTP-${version}";
            sha256 = "1cns1qcmmr00nyvcvcj4p4n2gvliyjynlwfqc7qzpkjjnkb7fzl6";
         };
      });

      erlangR2022 = pkgs.beam.packagesWith erlangv2022;

      elixirv161 = erlangR2022.elixir_1_6.overrideAttrs (oldAttrs: rec {
         name = "elixir-${version}";
         version = "1.6.1";
         src = pkgs.fetchFromGitHub {
            owner = "elixir-lang";
            repo = "elixir";
            rev = "v1.6.1";
            sha256 = "01q5nxpgbpkiw9wk7na6arxc5s75sc3qh8gw8xwnrgxg9iabkqcf";
         };
      });
   };
}
```
Im ersten Block definieren wir die Variable `erlangv2022` indem wir die Funktion `pkgs.stdenv.lib.overrideDerivation` auf das Originalpaket _erlangR20_ anwenden und dabei einige Attribute überschreiben. Hier hilft es jetzt in die originale Paketdefinition von Erlang bei [Github](https://github.com/NixOS/nixpkgs) anzuschauen. An sich folgt es aber immer dem gleichen Schema: Name, Version und Downloadquelle überschreiben. Erlang und Elixir benutzen `fetchFromGitHub`, hier genügt es dann einfach die Version und die dazu passende SHA-256-Kontrollsumme anzupassen. Für normale Downloads kann mit dem Shell-Kommando `nix-prefetch-url` die Kontrollsumme ermittelt werden. Bei `fetchFromGitHub` scheint der einfachste Weg zu sein, die alte Kontrollsumme zu behalten und auf die Fehlermeldung zu warten, wie die Kontrollsumme eigentlich lauten müsste.  

Mit unserer eigenen Version von Erlang benutzen wir die eingebaute Funktion `pkgs.beam.packagesWith`, die uns alle Beam-Pakete mit der übergebenen Erlang-Version als Unterbau präsentiert. Dies definieren wir als `erlangR2022` und übergeben der Funktion unser gerade definiertes Paket `erlangv2022`.

In der so gewonnen Map mit Paketen auf Basis unserer eigenen Erlang-Version, überschreiben wir jetzt schließlich noch das `elixir_1_6`-Paket analog und definieren es als `elixirv161`.  

In unserer `default.nix` können wir jetzt unser _elixirv161_-Paket verwenden (statt _beam.packages.erlangR20.elixir\_1\_6_). Dafür müssen wir `nix-shell` noch unsere `config.nix`-Datei bekannt geben, am einfachsten mit der Umgebungsvariable _NIXPKGS\_CONFIG_:
```
export NIXPKGS_CONFIG=/tmp/my-env/config.nix
```

Legt man neben der `default.nix` auch die `config.nix` mit in die Projektversionsverwaltung, so ist zu jedem Zeitpunkt die richtige Version der Entwicklungsgebung definiert. Aktualisiert man irgendwann z. B. auf eine neue Erlang-Hauptversion braucht man sich nicht vor Fehlerbehebungen in alten Versionen drücken. Mit der `default.nix` auf dem alten Stand präsentiert die Nix-Shell ebenfalls wieder die alte Erlang-Version. Ein kurzes verlassen und wiederstarten der Nix-Shell reicht für den Wechsel.

## Fazit

Der Nix-Paketmanager zusammen mit der Nix-Shell bietet uns einen non-invasiven Paketmanager, den man parallel zum systemeigenen Paketverwalter verwenden kann. Durch die Möglichkeit Pakete nur innerhalb einer Shell laufen zu lassen und diese in einer Datei fest zu schreiben, macht die Installation für neue Teammitglieder einfach und vorallem nachvollziehbar.  
Mit der Option Pakete in der Version zu überschreiben, ersparen wir händische Kompilierungsarbeit und können dadurch auch eine Vielzahl an Versionen gleichzeitig bauen. Ein Wechsel der Versionen folgt dann auf Knopfdruck mit der `nix-shell` oder man benutzt mit zwei Konsolen schlicht weg mehrere Versionen gleichzeitig. 
<!-- more end -->
