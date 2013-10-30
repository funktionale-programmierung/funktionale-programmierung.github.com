---
layout: post
title: "Continuations in der Praxis"
author: stefan-wehr
tags: ["continuations", "Haskell", "Konzepte", "Praxis"]
---

Gewisse Konzepte, die in der funktionalen Programmierung häufig vorkommen,
sind in imperativen oder objekt-orientierten Sprachen nahezu unbekannt.
Eines dieser Konzepte sind *Continuations*. Kurz gesprochen ermöglichen
Continuations den Zugriff auf den "Rest" einer Berechnung. Diesen Rest
kann man sich dann beispielsweise merken und zu einem späteren Zeitpunkt
ausführen.

In diesem Artikel möchte ich Continuations anhand eines praktischen
Beispiels erklären. In einem [früheren Artikel](http://funktionale-programmierung.de/2013/07/17/medizin-funktional.html) 
hatte ich schonmal die Architekur von Checkpad MED vorgestellt, eine elektronische
Krankenakte deren Serverkomponente fast vollständig mit Haskell
realisiert ist. In dieser Serverkomponente gibt es einen Teil,
welcher aus Krankhausdaten die Elemente der Benutzeroberfläche
generiert. Nun soll sich natürlich bei Änderung der Krankenhausdaten
auch die Benutzeroberfläche ändern. Und genau dabei spielen Continuations
eine wichtige Rolle...

<!-- more start -->

Schauen wir uns doch mal zunächst einen kleinen Teil der
Dokumentengenerierung an. (Die Dokumentengenerierung
ist der Teil der aus Krankenhausdaten
Elemente der Benutzeroberfläche erzeugt.)

    labDocGen = mkGen "labDocGen" 42 $ \(patId, labId) ->
      do Just pat <- getImportData patId  
         Just lab <- getImportData labId
         return $ LabDoc { 
             title = patName pat ++ " Labor  ++ labDate lab
          , sections = [renderLabParams (labParams lab)]
        }

Die Variable `labDocGen` ist ein sogenannter Generator. Ein Generator hat einen
Namen (hier `"labDocGen"`) und eine Versionsnummer für den Code (hier `42`).
Die Versionsnummer wird benötigt damit der Generator nicht nur bei
Daten- sonder auch bei Codeänderungen erneut ausgeführt werden kann.

Außerdem hat der Generator natürlich auch noch eine Generierungsfunktion.
Diese nimmt zwei Parameter, wobei `patId` die ID eines Patienten und
`labId` die eines Laborbefunds ist. Im Rumpf des Generators wird dann 
ein `LabDoc` erzeugt, welches später in der Patientenakte angezeigt werden
kann. Um an die benötigten Daten zu kommen benutzt der Generator
zweimal die Funktion `getImportData`.

Ein wichtige Eigenschaft des obigen Generatorcodes ist, dass sich die
Generatorfunktion einzig um das Erzeugen des Ausgabedokuments kümmert.
Sie sehen nirgends Logik, die sich um das Neuausführen des Generators
bei Änderung der Eingabedaten kümmert.

Die Verantwortung für die Neuausführung der Generatoren bei Code- oder
Datenänderungen liegt einzig beim darunterliegenden Framework. Und genau
an dieser Stelle werden Continuations relevant. Schauen wir uns mal
die Generatorfunktion genauer an. Zuerst holt sie sich mit 
`getImportData patId` die Daten zum richtigen Patienten. Wenn sich nun
die Daten des Patienten ändern (weil z.B. jemand in der Verwaltung
den Namen des Patienten korrigiert) muss der restliche Code
der Generatorfunktion neu ausgeführt werden.

Was ist denn nun dieser restliche Code? Dazu erinnern wir uns,
dass die `do`-Notation in Haskell lediglich eine Kurzschreibweise ist.
Ausgeschrieben sieht der Code des Generators wie folgt aus:

    labDocGen = mkGen "labDocGen" 42 $ \(patId, labId) ->
      getImportData patId >>= \pat ->
      getImportData labId >>= lab ->
         return $ LabDoc { 
             title = patName pat ++ " Labor  ++ labDate lab
          , sections = [renderLabParams (labParams lab)]
        }

Jetzt sieht man deutlich, dass der nach `getImportData patId` ausgeführte
Teil des Code alles nach `>>= \pat -> ...` ist. Um also bei Aktualisierung
der unter der `patId` abgelegten Patientendaten das Ausgabedokument
aktuell zu halten, muss sich das Framework also nur diesen Teils des
Codes merken und zu den richtigen Zeiten wieder ausführen. Und da die
Generatorfunktion in einer vom Framework bereitgestellten Monade läuft
ist der Zugriff auf den Code auch einfach möglich.

So, das war's für heute. Wir werden uns sicher in einem späteren Artikel
Continuations nochmal genauer anschauen und dabei auch kennenlernen, dass
Continuations in Haskell auch nur eine Monade sind.

Ich freu' mich über Feedback jedweder Art!

<!-- more end -->

