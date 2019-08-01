---
layout: post
title: "Actions & Snapshots"
author: bastian-senst
tags: []
---

Heute geht es um ein Datenmodell für verteilte Anwendung zur Speicherung von fortlaufenden
Benutzereingaben in einem verteilten System.
In der Praxis hat sich dieses Datenmodell seit mehreren Jahren in unserem
Produkt Checkpad MED sehr bewährt.
Im Krankenhaus ist Checkpad unter anderem eine Kollaborationsplattform.
Formulare und Checklisten werden ausgefüllt, Medikamente angeordnet, Aufgaben werden erstellt und
bearbeitet. All das passiert auch an mobilen Endgeräten direkt am Patientenbett.
Die mobilen Geräte können dabei jederzeit auch offline benutzt werden. Offline-Nutzung ist dabei nicht
nur lesend, sondern auch schreibend möglich.

Wie entscheidet man also, welche Daten sich ergeben, wenn mehrere Benutzer gleichzeitig oder
durch Offline-Nutzung voneinander getrennt eine Änderung durchführen.
Einfache verteilte Systeme lösen dieses Problem gar nicht. Der Benutzer, der als letztes einen
Datensatz schreibt, gewinnt und überschreibt mögliche Änderungen anderer Benutzer. Eine
Offline-Nutzung mit mehreren Benutzern ist hier für die meisten Einsatzzwecke nicht denkbar.

In der Praxis haben sich häufig Locks etabliert, z.B. bei der Bearbeitung von gemeinsamen
Office-Dokumenten auf einem Fileshare. Auf der Implementierungsseite gibt es hier einige — zum Teil
gelöste — Schwierigkeiten. Man muss die Locks z.B. rechtzeitig wieder freigeben.
Wie unterscheidet man
dabei einen Client, der nicht mehr erreichbar ist, von einem, der gerade offline an dem Dokument
arbeitet? Für die Bearbeitung von Dokumenten mag dieses Verfahren ein akzeptabler Weg sein, unser
Datenmodell soll aber die verschiedensten Anwendungsfälle unterstützen.

Wir haben uns in Checkpad für eine andere Lösung entscheiden: Für jede Änderung speichern wir nur
das, was nötig ist, um aus dem alten Zustand, den die Benutzerin gesehen
hat, den neuen Zustand berechnen zu können. Diese Änderungsinformation nennen wir _Action_. Durch
Hintereinanderausführung der Actions kommt man dann zu einem _Snapshot_, den wir dem Benutzer anzeigen
können. Um einen konsistenten Zustand zu erreichen, werden die Daten schließlich von einer zentralen
Instanz, in unserem Fall dem Checkpad-Server, in eine verbindliche Reihenfolge gebracht.

Design
------

Dieser Artikel beschreibt das Design des Datenmodells anhand einer simplen Checkliste am Patienten,
die bei der Aufnahme ausgefüllt werden soll.

```
Aufnahmebogen ausgefüllt und unterschrieben         [ja] [nein] [entfällt]
Arztbrief angelegt                                         [ja] [entfällt]
Tumorkonferenz bereits erfolgt und dokumentiert?    [ja] [nein] [entfällt]
Therapieabweichung? Neue Erkenntnisse?              [ja] [nein] [entfällt]
```

Die gespeicherten Daten für diese konkrete Liste sind wie folgt:
```json
{
    "aufnamebogen": null,
    "arztbrief": "entfaellt",
    "tumorkonferenz": "ja",
    "therapieabweichung": "nein"
}
```

Daten werden in Checkpad an _Entities_ gespeichert. In unserem Fall ist die Entity der
Patient, für den die Checkliste ausgefüllt wurde: `{ kind: "Patient", id: "4567042" }`.
Zu jeder Art von Daten gibt es einen _Namespace_, in unserem Beispiel: `check_admission_onk`.
Zu jeder Entity kann es dann für jeden Namespace ein _Datafield_ geben. In diesem Datafield
stehen dann jeweils die Daten, die wir _Snapshot_ nennen.
Zur Veranschaulichung kann man die Daten zu einer Entity so darstellen, `diagnosis` ist dabei ein
weiterer Namespace.

```json
{
    "entity": { "kind": "Patient", "id": "4567042" },
    "namespaces": {
        "diagnosis": "Rezidiv ITP",
        "check_admission_onk": {
            "aufnamebogen": null,
            "arztbrief": "entfaellt",
            "tumorkonferenz": "ja",
            "therapieabweichung": "nein"
        }
    }
}
```

Änderungen an diesen Daten werden durch _Operationen_ beschrieben, die Parameter haben.
Für unser Beispiel mit der Checkliste gibt es eine Operation `setValue` mit den Parametern
Feld-ID (`key`), alter Wert (`oldValue`) und neuer Wert (`value`).
_Actions_ sind dann schließlich konkrete instanziierungen einer Operation.
In unserem Beispiel sieht
eine Action so aus:
```json
{
    "entity": { "kind": "Patient", "id": "1" },
    "namespace": "check_admission_onk",
    "operation": "setValue",
    "args": {
        "key": "aufnahmebogen",
        "oldValue": null,
        "value": "ja"
    }
}
```
Eine Operation ist letztendlich mit der Logik assoziiert, die die Action auf den Snapshot anwendet
und daraus eine neue Version des Snapshots erstellt.

```json
{
    "aufnagmebogen": "ja",
    "arztbrief": "entfaellt",
    "tumorkonferenz": "ja",
    "therapieabweichung": "nein",
}
```

Wendet man eine Action auf eine Entity an, bei dem das Datafield noch nicht existiert, gibt es
keinen Fehler. In der Spezifikation des Namespace wird ein Wert angegeben,
der in diesem Fall verwendet werden soll.

```json
{
    "aufnahmebogen": null,
    "arztbrief": null,
    "tumorkonferenz": null,
    "therapieabweichung": null
}
```

Bei Serverkontakt überträgt der Client die gespeicherten Actions an den Server.
Der Server bestätigt die empfangenen Actions. Wenn mehrere Clients gleichzeitig oder innerhalb
einer Offline-Periode für dasselbe Datafield (Kombination aus Entity und Namespace) Actions
übertragen haben, legt der Server die Reihenfolge der Actions fest. Dabei darf er aber niemals die
Reihenfolge der Actions ändern, die ein Client ihm für ein Datafield geschickt hat. Es können aber
sehr wohl zwischen den Actions eines Clients Actions anderer Clients eingefügt werden.
Solange der Client nicht vom Server bestätigte Actions hat, ist er selbst für das Anwenden der
Actions verantwortlich.

Eigenschaften und Annahmen
----------

Persistent müssen jeweils nur die Actions gespeichert werden. Der Snapshot kann jederzeit durch
Hintereinanderausführung der Actions neu berechnet werden. Aus Performancegründen wird am Server
aber trotzdem der Snapshot gespeichert. Der Client erhält, ebenfalls aus Performancegründen, auch
nur diesen Snapshot und die Information, welche Actions dort schon berücksichtigt sind.

Actions können von verschiedenen Codeständen unterschiedlich
interpretiert werden, wenn man z.B. die Darstellung der Snapshots ändern möchte.
Möchte man die Bedeutung von einer Operation oder deren Parameter aber ändern,
ist es notwendig eine neue Operation einzuführen. Alte Actions müssen weiterhin korrekt
interpretierbar sein.
Actions dürfen nicht fehlschlagen, auch nicht, wenn der Zustand, auf den sich die Action bezieht,
nicht mehr sichtbar ist. Gibt es einen Konflikt, kann dieser entweder
beim Anwenden der Action automatisch gelöst werden oder man repräsentiert den Konfliktzustand
im Snapshot und überlässt die Konfliktlösung dem Benutzer. Ein Konflikt kann z.B. durch einen
automatischen Merge oder die einfachere Strategie „der letzte gewinnt“.
Dies kann je nach Anwendungsfall
separat entschieden und an genau einer Stelle implementiert werden.

Unterschiedliche Funktionen haben diesbezüglich komplett unterschiedliche Anforderungen.
Zum Beispiel bei einer Aufgabe möchte man als Benutzer sehen, wenn die Aufgabe von einem anderen
Benutzer geändert wurde, nachdem man diese als erledigt markiert hatte. Bei einem Formular möchte
man bei einem unterschiedlichen Eintrag im gleichen Formularfeld die Formulare zusammenführen
können. Bei einem einfachen Chat sind die Aktionen, die Chat-Nachrichten einfügen aber im
Wesentlichen unabhängig und nur die Reihenfolge der Nachrichten könnte von der Reihenfolge der
Actions abhängen.
