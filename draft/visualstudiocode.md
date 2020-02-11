---
layout: post
description: Wir entwickeln F#-Anwendungen für Windows ohne Windows als Entwicklungssystem einzusetzen.
title: "F# ohne Windows mit Visual Studio Code"
author: tim-digel
tags: ["F#", "F Sharp", "Visual Studio Code", "VS Code", "Einführung", "Erste Schritte", "Projekt", "Start"]
---

In [Einstieg in Visual Studio mit F#](/2020/01/23/f-sharp-visual-studio-erste-schritte.html) haben wir im Schnelldurchgang die ersten Schritte im Zusammenhang mit Anwendungen in F# kennen gelernt. Wir haben dabei auf Visual Studio zurück gegriffen und uns somit auf Windows oder MacOS beschränkt.  
Mit Visual Studio **Code*** bietet Microsoft hingegen einen plattformunabhängigen Editor an. In diesem Blogpost zeigen wir ähnliche erste Schritte in F#, diesmal in Visual Studio Code unter einem linuxbasierten Betriebssystem. Dabei erhalten wir auch Einblicke in die Bedienung mit Kommoandozeilentools, die in nahezu gleicherweise unter Windows und MacOS anwendbar sind. Dieser Artikel geht an manchen Stellen weniger ins Detail als in [Einstieg in Visual Studio mit F#](/2020/01/23/f-sharp-visual-studio-erste-schritte.html), weswegen wir diesen Blogpost allen vorab empfehlen.
<!-- more start -->

## Installation

Unter Windows genügt es Visual Studio zu installieren und die Entwicklung kann beginnen. Unter Linux benötigen wir etwas mehr. Wir installieren neben Visual Studio **Code** noch _.NET SDK_, _Mono_ und einige Plugins für Visual Studio Code. Abhängig des verwendeten Betriebssystem muss hierfür jeder anders vorgehen. In unserem Blogartikel verwenden wir den [Nix-Paket-Manager](https://nixos.org/nix/) (siehe auch [Mit Nix raus aus der Versionshölle](/2018/02/19/nix.html)). Wir stellen unser Setup in einer nicht-invasiven Nix-Shell her, dazu führen wir in einem Terminal
```sh
NIXPKGS_ALLOW_UNFREE=1 nix-shell -p dotnet-sdk_3 -p vscode -p fsharp -p mono
```
aus. Da _.NET SDK_ nicht unter freier Lizenz steht, muss mit `NIXPKGS_ALLOW_UNFREE=1` die Installation explizit erlaubt werden. Wir befinden uns nun in einer virtuellen Umgebung mit den installierten Paketen. Wir erstellen uns einen leeren Ordner und starten Visual Studio Code:
```
mkdir -p ersteschritte
cd ersteschritte
code .
```
Als nächste installieren wir die Erweiterungen _Ionide-fsharp_ und _C#_. In unserem Fall haben wir noch zusätzlich _German Language Pack for Visual Studio Code_ installiert.  

# Projekt erstellen

Mit dem Tastenkürzel _STRG + UMSCHALT + P_ erscheint eine Kommandoeingabezeile. Wir tippen _F#: New Project_. In den folgenden Abfragen wählen wir _Console Application_ als Anwendungstyp, `.` (aktuelles Verzeichnis) als Projektordner und _ErsteSchritte_ als Projektname.  

Wir sehen links im _Explorer_ einige erstellte Dateien, unter anderem die _Program.fs_ mit einer beispielhaften _Main_-Methode. Unter _Terminal_, _Neues Terminal_ erhalten wir eine Betriebssystemkonsole. Wir führen unser Programm erstmalig aus:
```sh
dotnet run
```
Wir erhalten als Rückgabe wie erwartet `Hello World from F#!`.

[![Visual Studio Code Übersicht](/files/2020-02-15-f-sharp-visual-studio-code/run-project.png "Visual Studio Code Übersicht")](/files/2020-02-15-f-sharp-visual-studio-code/run-project.png)

In der obigen Abbildung sehen wir, dass die Typsignatur von `main` als `// string [] -> int` berechnet und angezeigt wird. Halten wir mit der Maus über eine Definition erhalten wir ebenfalls diese Information.

# F# Interactive

Auch in Visual Studio Code können wir die interaktive Repl von F# nutzen. Dazu drücken wir in einer Zeile oder nach einem markierten Codeblock _ALT + ENTER_. Der entsprechende Code wird mit dem Kommandozeilentool `fsharpi` ausgeführt. In dem offenbleibenden _F# Interactive_-Fenster können wir auch eigene Anweisungen (z. B. `14 * 5;;`) eingeben. Dabei muss jede Eingabe mit `;;` gefolgt von _ENTER_ beendet werden.

# Debugging & Haltepunkte

Um unsere Anwendung in Visual Studio debuggen zu können, müssen wir einmalig eine Konfiguration anlegen. Wir gehen auf _Debuggen_, _Debuggen starten_ oder drücken _F5_. Als Umgebung wählen wir _.NET Core_. Darauf folgt eine Fehlermeldung, dass keine _.NET_-Konfiguration angelegt werden konnte. Ebenso öffnet sich die Datei `.vscode/launch.json`. Hier fügen wir unsere Konfiguration ein.  

Im Wert von `configurations` drücken wir zwischen den eckigen Klammern Enter und tippen `.NET`. In der erscheinenden Vorschlagsliste wählen wir _.NET: Launch .NET Core Console App_. Wir müssen die beiden Werte `<target-framework>` und `<project-name.dll>` eintragen. Wenn wir im Explorer von Visual Studio Code den Ordner `bin/Debug` öffnen, sehen wir das verwendete Framework inklusive Version. In diesem Ordner befindet sich auch die _dll_-Datei. Diese heißt normalerweise gleich wie das Projekt selbst. In unserem Fall sieht die _launch.json_ wie folgt aus:
```json
{
    "version": "0.2.0",
    "configurations": [
       {
           "name": ".NET Core Launch (console)",
           "type": "coreclr",
           "request": "launch",
           "preLaunchTask": "build",
           "program": "${workspaceFolder}/bin/Debug/netcoreapp3.1/ErsteSchritte.dll",
           "args": [],
           "cwd": "${workspaceFolder}",
           "stopAtEntry": false,
           "console": "internalConsole"
       }
    ]
}
```
Wir speichern `launch.json` und wählen erneut _Debuggen starten_. Es kommt die Meldung, dass der Task _build_ nicht gefunden wurde. In _launch.json_ haben wir `build` als `preLaunchTask` festgelegt, da wir unser Projekt vor dem Debuggen bauen müssen.  
Wir klicken auf _Aufgabe konfigurieren_ gefolgt von _Datei task.json aus Vorlage erstellen aus_. Als Aufgabenvorlage nehmen wir `.NET Core`. Die automatisch erzeugte Vorlage ist für unseren Fall direkt passend:
```fsharp
{
    "version": "2.0.0",
    "tasks": [
        {
            "label": "build",
            "command": "dotnet",
            "type": "shell",
            "args": [
                "build",
                "/property:GenerateFullPaths=true",
                "/consoleloggerparameters:NoSummary"
            ],
            "group": "build",
            "presentation": {
                "reveal": "silent"
            },
            "problemMatcher": "$msCompile"
        }
    ]
}
```
Das Debuggen ist jetzt eingerichtet. Wir setzen in _Program.fs_ einen Haltepunkt auf `0`. Dazu klicken wir links neben die entsprechende Zeilennummer oder drücken _F9_ während der Cursor in dieser Zeile steht. Es erscheint ein roter Punkt am Anfang der Zeile.  

[![Visual Studio Code Debuggen](/files/2020-02-15-f-sharp-visual-studio-code/debugging.png "Visual Studio Code Debuggen")](/files/2020-02-15-f-sharp-visual-studio-code/debugging.png)

Mit _Debuggen starten_ wird unser Projekt gebaut und ausgeführt. Die Ausführung bleibt am Haltepunkt stehen. Links werden die Werte der bisher berechneten Variablen angezeigt. Oben erscheint eine kleine Videorecorder-Navigation, mit der wir fortsetzen, pausieren, abbrechen oder weiter springen können. Wir drücken einmal auf _Weiter_. Der Haltepunkt wird durchlaufen und das Programm ist beendet. Wir entfernen den Haltepunkt wieder.

## Neue Datei hinzufügen

Wir möchten eine neue Datei hinzufügen. Im Explorer-Bereich wählen wir _Neue Datei_ im Kontextmenü und benennen sie _MeinModul.fs_. Wir geben der Datei einen Namensraum und definieren uns ein Modul mit einem gerundeten Wert von Pi:
```fsharp
namespace ErsteSchritte

module MeinModul = 

    /// Pi als abgerundete Dezimalzahl
    let pi : decimal = 
        3.141M
```
Weiter ändern wir in der Zeile `printfn` der _Program.fs_ ab zu:
```fsharp
printfn "%A" ErsteSchritte.MeinModul.pi
```
IntelliSense, unsere ständige Echtzeitkompilierung, unterstreicht unmittelbar den Aufruf von `ErsteSchritte`. Wir müssen die neue Datei noch unserem Projekt hinzufügen. Dazu öffnen wir _ErsteSchritte.fsproj_ und fügen die Zeile
```xml
<Compile Include="MeinModul.fs" />
```
oberhalb der gleichlautenden Zeile für _Program.fs_ ein. Die Reihenfolge ist hier maßgebend. Die _Program.fs_ beinhaltet den Einstiegspunkt und muss als letztes definiert sein. Ggf. müssen wir _Program.fs_ neu öffnen. Der Aufruf _ErsteSchritte.MeinModul.pi_ sollte nun bekannt sein. Starten wir das Programm indem wir `dotnet run` in der Konsole ausführen. Wir erhalten:
```sh
user@rechner:~/ersteschritte$ dotnet run
3.141M
```

## Pakete installieren

Im nächsten Schritt möchten wir Testfälle definieren. Dafür benutzen wir mit _NUnit_ und _FSUnit_ entsprechend Test-Frameworks. Um die benötigten Pakete zu installieren, bearbeiten wir `ErsteSchritte.fsproj` und fügen vor `</project>` folgende Pakete an:
```xml
  <ItemGroup>
    <PackageReference Include="FsUnit" Version="3.8.0" />
    <PackageReference Include="Microsoft.NET.Test.Sdk" Version="16.4.0" />
    <PackageReference Include="NUnit" Version="3.12.0" />
    <PackageReference Include="NUnit3TestAdapter" Version="3.16.1" />
  </ItemGroup>
```
Bei der nächsten Ausführung von `dotnet build` werden die Pakete heruntergeladen und referenziert.

## Testfall definieren und ausführen

Im Explorer von Visual Studio Code legen wir einen neuen Ordner `Test` an und darin die Datei `MeinModulTest.fs`. Wir fügen das folgende Modul mit einem Testfall für unsere Funktion `MeinModul.pi` ein:
```fsharp
namespace ErsteSchritte.Test

module MeinModulTest =
    open NUnit.Framework
    open FsUnit

    [<Test>]
    let ``Defintion of Pi`` () =
        ErsteSchritte.MeinModul.pi |> should equal 3.141M
```
Wir müssen die neue Datei noch als Referenz in unser Projekt aufnehmen. Wir fügen daher in `ErsteSchritte.fsproj`
```xml
    <Compile Include="Test/MeinModulTest.fs" />
```
zwischen die Zeilen mit _MeinModul.fs_ und _Program.fs_ ein.  

Um die Tests auszuführen führen wir den Befehl `dotnet test` aus. Wir erhalten in etwa:
```sh
user@rechner:~/ersteschritte 1$ dotnet test
Testlauf für "/home/td/ersteschritte/bin/Debug/netcoreapp3.1/ErsteSchritte.dll" (.NETCoreApp,Version=v3.1)
Microsoft (R) Testausführungs-Befehlszeilentool Version 16.3.0
Copyright (c) Microsoft Corporation. Alle Rechte vorbehalten.

Die Testausführung wird gestartet, bitte warten...

Insgesamt 1 Testdateien stimmten mit dem angegebenen Muster überein.
                                                                                          
Der Testlauf war erfolgreich.
Gesamtzahl Tests: 1
     Bestanden: 1
 Gesamtzeit: 0,9498 Sekunden
```

## Fazit

Auch unter Betriebssystemen abseits von Windows können wir _.NET-Core_-Anwendungen mit F# entwickeln. Visual Studio Code mit dem Plugin _Ionide_ bieten uns hierfür eine gute Basis. Im Vergleich zu Visual Studio unter Windows müssen wir mehr mit der Konsole arbeiten und einige Einstellungen von Hand vornehmen. Am Ende können aber Dinge wie das Projekt zu bauen, Debuggen mit Haltepunkten oder Testfälle ähnlich einfach genutzt werden.  

In einem weiteren Blogartikel werden wir die Sprache F# selbst kennen lernen. Dabei spielt es dann keine Rolle mehr, ob wir mit Visual Studio unter Windows oder wie hier gezeigt mit Visual Studio Code arbeiten.
