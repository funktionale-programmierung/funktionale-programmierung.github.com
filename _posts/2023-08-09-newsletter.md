---
layout: post
title: "'Was ist denn gerade der Stand?' und andere Fragen, die der Newsletter beantworten kann"
author: marco-schneider
---

Wer hätte das gedacht: Der gute alte Newsletter hat still und heimlich seinen
Weg zurück in mein Leben gefunden.  Aus diesem Anlass wollen wir heute mal die
funktionale Progammierung beiseite lassen und uns einen Aspekt der
Softwareentwicklung anschauen, der uns alle angeht: Kommunikation mit
Teamkolleg:innen und Kund:innen.

<!-- more start -->

Möglicherweise haben Sie es schon bemerkt:
[funktionale-programmierung.de](https://funktionale-programmierung.de) wird
schon seit geraumer Zeit im Wesentlichen von der [Active Group
GmbH](https://www.active-group.de) betrieben.  Um dem Rechnung zu tragen, werden
wir hier nach und nach ein paar Änderungen vornehmen.  Zentral ist, dass das
Blog zukünftig auch ganz offiziell unser Firmenblog sein wird.  Für Sie als
Leser:in wird sich nichts grundlegendes ändern: Wir werden weiterhin gut
informierte Artikel zum Thema "Funktionale Programmierung" verfassen.
Allerdings wollen wir diese Veränderung auch dazu nutzen, neben der Technik auch
unsere Arbeitsweise und soziotechnische Aspekte unserer Arbeit mit Ihnen zu
teilen.  In diesem Sinne ist das hier der erste Post, der sich explizit *nicht*
mit Softwaretechnik beschäftigt.

Als Aufschlag möchte ich heute Situationen aus tatsächlichen Projekten
beleuchten, die mir und unseren Teams immer wieder zu schaffen machen und
zeigen, was ein ganz einfacher Newsletter daran geändert hat.

# "Was ist denn der Stand?"

Es soll wohl eine perfekte Welt geben, in der Entwicklungsteams, POs,
Projektmanager:innen und Kund:innen eine perfekte Kommunikationssymbiose
eingehen.  Die Regeltermine sind produktiv, Fragen werden schnell beantwortet,
der Stand des Projekts ist zu jedem Zeitpunkt allen klar.  Das mag für Projekte
wie aus dem Lehrbuch so sein.  Meine Erfahrung ist, dass das eher selten
tatsächlich der Fall ist.  Häufig ist es doch so: Sie arbeiten an mehreren
Projekten und kennen nicht immer den aktuellen Stand.  Ihre Kund:innen sind
ebenfalls an vielen Baustellen beschäftigt und können schlicht nicht ständig den
Issuetracker verfolgen.

Ich sehe mich häufig mit diesen Fragen konfrontiert:

- Was ist denn der Stand?
- Wie viele Stunden sind noch auf dem Stundenkonto?
- Wann bist du noch mal im Urlaub?

Man könnte sagen, dass diese Art von Kommunikation "Pull-basiert" ist.  Kund:in
fragt nach und bekommt eine Antwort.

# "Das ist aber nicht so, wie wir uns das vorgestellt haben"

Das ist aber noch nicht alles.  Achtung, jetzt kommen die Dinge, die sich leider
oft nicht technisch argumentieren oder strukturell vollständig lösen lassen!

Ein Beispiel: Wir haben vor einiger Zeit ein Feature gebaut und ausgeliefert.
Der Projektabschnitt wurde zum Festpreis angeboten und die fertige Software vom
Kunden auch so abgenommen.  Ein dreiviertel Jahr später regt sich aber Unmut.
Das Feature ist doch nicht so, wie es sein sollte.  Der Kunde erwartet
Nachbesserung, wir sind der Meinung, dass das abgenommene Feature lange genug
Zeit hatte, beanstandet zu werden und hier auch keine Kulanz mehr drin ist.  Was
tut man in so einer Situation?

Ganz konkret konnten wir die Situation damals so auflösen: Ich erstellte eine
detaillierte Timeline der Projektabschnitte, jeweils mit

- Lasten (was war gefordert)
- Pflichten (was haben wir konkret angeboten)
- Auslieferungsdatum
- Abnahmedatum (wann hat der Kunde signalisiert, dass es keine Beanstandungen
  gibt, gegebenenfalls durch Nichtstun)

Da wir alle vernünftige Menschen sind, konnten wir anhand der Timeline
feststellen, dass uns in diesem konkreten Fall keine Schuld zukommt.  Das Ende
vom Lied war, dass wir uns einigen konnten.  Wunderbar.

Allerdings: Auch hier handelt es sich gewissermaßen wieder um "Pull-basierte"
Kommunikation.

# Gemeinsamkeiten

Wir haben schon gesehen, dass es sich hier jeweils um Kommunikation handelt, die
man als "Pull-basiert" bezeichnen könnte.  Warum?

Im ersten Fall ist es klar.  Kund:in fragt nach einer bestimmten Information und
wir liefern sie.  Das ist an sich in Ordnung, kann aber über die Zeit
problematisch werden.  Das *Warum* sehen wir an Fall zwei.

Hier haben wir es schon mit einem waschechten Problem zu tun.  Die:der Kund:in
ist verständlicherweise nicht glücklich, da sie eine Software bekommen haben,
die nicht ihrem Usecase oder ihrer Vorstellung entspricht.  Wie es dazu kam ist
für uns hier uninteressant.  Wichtig ist, dass die Situation (und das ist meine
These) überhaupt erst entstanden ist, weil zum entscheidenden Zeitpunkt nicht
nachgefragt wurde.  Der Pull ist gewissermaßen ausgelassen worden, sodass im
entscheidenden Moment -- nämlich im Moment der Abnahme -- die richtige
Information gefehlt hat.

# Pull-Kommunikation als Problemherd

Ich hoffe, dass ich zeigen konnte, dass Pull-Kommunikation zumindest in
bestimmten Fällen zu Problemen führt.  Mir ist natürlich klar, dass es da
draußen viele Methoden gibt, die dem entgegenwirken sollen.  Aber seien wir mal
ehrlich: Die Chance, dass der Projektethos auf beiden Seiten einer Beauftragung
gleich ist, konvergiert gegen Null.  Klar, wir können unseren Prozessen Namen
geben, zum Beispiel uns auf "agile Arbeit" verständigen.  Wenn der Projektethos
der Kunden intern aber trotzdem eher dem Wasserfallmodell gleicht, bringt es
letztlich nicht viel, an der "Schnittstelle" zwischen Kunde und Dienstleister so
zu tun, als sei man "agil".  Wenn es beim Kunden keine "Abnahmekultur" gibt,
weil Projekte zwar in Paketen bestellt, eigentlich aber immer erst am Schluss
als Ganzes abgenommen werden, hilft das auch nicht -- und das kennen Sie sicher
auch.

# Eine unerwartete Lösung

Wie gesagt sind das hier echte Probleme, die ich im vergangenen Jahr erlebt
habe.  Sicher kennen Sie selbst solche Fälle.  Die Lösung für unser Pull-Problem
ist genau so simpel wie unerwartet: Wir schreiben seit einiger Zeit einen
wöchentlichen Projekt-Newsletter!  Der sieht ungefähr so aus:

> `To: person@kunde.de`
> `Cc: team@dienstleister, chef@dienstleiter, vorgesetzter@kunde.de`
> `Subject: [news] Weekly Newsletter KW-<N>`

> Liebe:r `name/n`,

> diese Woche haben wir die Arbeit an folgenden Tickets abgeschlossen:

> - `ticketbeschreibung` (`link-zum-issuetracker`)
> - ...

> Weiter sind folgende Tickets aktuell in Arbeit:

> - `ticketbeschreibung` (`link-zum-issuetracker`)
> - ...

> Neuigkeiten diese Woche:

> - Headsup: `name` ist vom 10. bis einschließlich 15. Juli im Urlaub.  Wenn es
>   etwas gibt, dass nur `name` beantworten kann, wendet euch bitte bis dahin
>   noch an sie.
> - Die Notizen zu unserem Weekly findet ihr hier (`link-zu-den-notizen`).
> - ...
  
> Folgende Punkte sind offen geblieben:

> - Wir hatten letzte Woche Probleme mit `x`.  Es hat sich in Zusammenarbeit mit
>   `name` herausgestellt, dass das nur ein Symptom für `y` war.  `admin` hat
>   das jetzt auf dem Schirm und kümmert sich darum.
> - ...

> Auf dem Stundenkontingent haben wir Stand heute noch circa `n` Stunden übrig.
> Das entspricht etwas über `m` Tagen.  Ich empfehle, dass wir da bald etwas
> neues beauftragen, da wir in circa `l` Wochen das Kontingent erschöpft haben.

> Beste Grüße und ein schönes Wochenende

Die ersten Newsletter habe ich den Newsletter kommentarlos und ohne Vorwarnung
abgeschickt und prompt von allen beteiligten positive Rückmeldung bekommen.
Warum?  Ich glaube die Gründe sind:

- Ohne Nachzufragen wissen wirklich alle Beteiligten ganz genau, welche Tickets
  bearbeitet wurden und was noch in Arbeit ist.  Für uns Entwickler:innen
  vielleicht überflüssig, weil "steht ja im Issuetracker".  Aber nicht alle
  können oder wollen ständig in den Tracker schauen!  Gerade `chef` und
  `vorgesetzter` haben vielleicht Besseres zu tun und eventuell nur an Problemen
  oder der buchhalterischen Seite interessiert ("Was kann abgerechnet werden?")
- Neuigkeiten wie Urlaube sind etwas, das häufig per Flurfunk abläuft.  Lange
  genug (und vor allem ohne nachfragen zu müssen) noch mal schriftlich darüber
  informiert zu werden hilft sowohl Team als auch Kund:in.
- Oft fliegen Themen herum, die nur bilateral auftreten.  "Der Loginserver hat
  gesponnen" ist vielleicht etwas, was ein Teammitglied und ein Admin gemeinsam
  angeschaut haben und lösen konnten.  Da ist die Gefahr hoch, dass
  
  1. das Teammitglied erst mal eine Woche nach der richtigen Person suchen
     musste
  1. niemand davon etwas mitbekommt
  2. das Problem wieder auftaucht und die suche von 1. von vorne losgeht

  Stattdessen haben es hier alle schriftlich und können zukünftig auf
  gemeinsames Wissen zugreifen, das sonst nirgends aufgeschrieben worden wäre.
- Auch ganz toll: Neue Kolleg:innen im Projekt können sehen, was in der
  Vergangenheit passiert ist.  Und zwar nicht nur technisch sondern auch
  "kulturell" (wie wird im Projekt kommuniziert, wer ist am Projekt auch
  beteiligt außer denen, die offiziell dabei sind, und so weiter).

Besonders reizvoll für mich ist noch, dass dadurch auch die Timeline ganz von
selbst entsteht.  Man mag entgegnen, dass es für all diese Themen auch Werkzeuge
gibt, die das besser lösen.  Ich sage: ein Newsletter ist 

- leicht zu schreiben
- enthält hochkonzentrierte Informationen
- ist asynchron und von allen überall lesbar: E-Mail ist ein extrem
  niederschwelliges Medium das alle benutzen können
- und erzählt eine nachvollziehbare Projektgeschichte abseits von den rein
  technischen Fragen

# Fazit

Ich würde gerne allen ans Herz legen, den Projekt-Newsletter mal selbst
auszuprobieren.  Schreibt ihn am besten immer schon während der Woche und checkt
ihn in ein Versionskontrollsystem ein.  Macht es euch einfach und macht ein
Template, das ihr nur noch ausfüllt.  Ich glaube, dass das vielen Projekten gut tun
wird!
