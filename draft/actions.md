---
layout: post
title: "Actions & Snapshots"
author: bastian-senst
tags: []
---

Thema dieses Artikels ist das Datenmodell, wie [Checkpad MED](https://www.checkpad.de/)
Benutzereingaben und deren
Änderungen speichert. Im Krankenhaus ist Checkpad unter anderem eine Kollaborationsplattform.
Formulare und Checklisten werden ausgefüllt, Medikamente angeordnet, Aufgaben werden erstellt und
bearbeitet. All das passiert auch an mobilen Endgeräten direkt am Patientenbett.

Die mobilen Geräte können dabei jederzeit auch offline benutzt werden. Offline-Nutzung ist dabei nicht
nur lesend, sondern auch schreibend möglich. Dadurch, dass es sich bei Checkpad um ein verteiltes
System handelt, hat man ähnliche Fragestellungen wie bei der verteilen Bearbeitung von
Quellcode oder Textdateien, für die sich über die Zeit verschiedene Versionsverwaltungssysteme
entwickelt haben (RCS, CVS bis hin zu Git).

Design
------

Dieser Artikel beschreibt das Design anhand einer simplen Checkliste am Patienten, die bei der
Aufnahme des Patienten ausgefüllt werden soll.

```
Aufnahmebogen ausgefüllt und unterschrieben         [ja] [nein] [entfällt]
Arztbrief angelegt                                         [ja] [entfällt]
Tumorkonferenz bereits erfolgt und dokumentiert?    [ja] [nein] [entfällt]
Therapieabweichung? Neue Erkenntnisse?              [ja] [nein] [entfällt]
```

Die gespeicherten Daten für diese konkrete Liste sieht so aus:
```json
{
    "aufnamebogen": null,
    "arztbrief": "entfaellt",
    "tumorkonferenz": "ja",
    "therapieabweichung": "nein"
}
```

Daten werden in Checkpad an _Entities_ gespeichert. In unserem Fall ist die Entity der
Patient für die die Checkliste ausgefüllt wurde: `{ kind: "Patient", id:  "1"}`.
Zu jeder Art von Daten gibt es einen _Namespace_. Hier hießt der Namespace: `check_admission_onk`.
An dieser Stelle ist der Namespace erstmal nur dieser Name.
Zu jeder Entity kann es dann für jeden Namespace ein _Datafield_ geben. In diesem Datafield
stehen dann jeweils die Daten, die wir _Snapshot_ nennen.
Zur Veranschaulichung kann man die Daten zu einer Entity so darstellen:

```json
{
    "entity": { "kind": "Patient", "id": "1" },
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
Für unser Beispiel mit der Checkliste, gibt es eine Operation `setValue` mit den Parametern
Feld-ID (`key`), alter Wert (`oldValue`) und neuer Wert (`value`).
Eine konkrete Instanziierung von einer Operation nennen wir _Action_. In unserem Beispiel sieht
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

Wendet man eine Action auf eine Entity an, bei dem das Datafield noch nicht existiert, wird
im Code der Wert angenommen, der in diesem Fall genommen werden soll.

```json
{
    "aufnahmebogen": null,
    "arztbrief": null,
    "tumorkonferenz": null,
    "therapieabweichung": null
}
```

Bei Serverkontakt überträgt der Client die gespeicherten Actions an den Server.
Der Server bestätigt die empfangenen Actions. Wenn mehrere Client gleichzeitig oder innerhalb
einer Offline-Periode für dasselbe Datafield (Kombination aus Entity und Namespace) Actions
übertragen haben, legt der Server die Reihenfolge der Actions fest.
Solange der Client nicht bestätigte Actions hat, ist er selbst für das Anwenden der Actions
verantwortlich.

Eigenschaften und Annahmen
----------

Persistent **müssen** jeweils nur die Actions gespeichert werden. Der Snapshot kann jederzeit neu
berechnet werden aus Hintereinanderausführung der Actions. Aus Performancegründen wird am Server
aber trotzdem der Snapshot gespeichert. Der Client erhält, ebenfalls aus Performancegründen auch
nur diesen Snapshot und die Information, welche Actions dort schon berücksichtigt sind.

Actions können und werden, z.B. zu Migrationszwecken, von verschiedenen Codeständen unterschiedlich
interpretiert. Möchte man die Bedeutung von einer Operation oder deren Parameter aber ändern,
ist es notwendig eine neue Operation einzuführen. Alte Actions müssen weiterhin korrekt
interpretierbar sein.
Actions dürfen nicht fehlschlagen, auch nicht, wenn der Zustand, auf den sich die Action bezieht,
nicht mehr so sichtbar ist. Gibt es einen Konflikt, kann dieser Konflikt entweder
beim Anwenden der Action automatisch gelöst werden oder man repräsentiert den Konfliktzustand
im Snapshot und überlässt die Konfliktlösung dem Benutzer. Dies kann je nach Anwendungsfall
separat entschieden und an genau einer Stelle implementiert werden.

Unterschiedliche Funktionen haben diesbezüglich komplett unterschiedliche Anforderungen.
Zum Beispiel bei einer Aufgabe möchte man als Benutzer sehen, wenn die Aufgabe von einem anderen
Benutzer geändert wurde, nachdem man diese als erledigt markiert hat. Bei einem Formularfeld möchte
man bei einem unterschiedlichen Eintrag im gleichen Formularfeld die Formulare zusammenführen
können. Bei einem einfachen Chat sind die Aktionen, die Chat-Nachrichten einfügen aber im
Wesentlichen Unabhängig und nur die Reihenfolge der Nachrichten könnte von der Reihenfolge der
Actions abhängen.
