Schreiben von Artikeln
==

Verkürzung des Artikels für die Übersicht
===

Das Verkürzen muß manuell passieren, und geschieht mithilfe von zwei HTML-Kommentaren im Artikel:

    <h1>Mein Artikel</h1>

    Mein Artikel behandelt... bla bla.

    <!-- more start -->

    Die Details sind folgende...

    <!-- more end -->

Der Teil zwsichen `more start` und `more end` ist dann auf der Übersichtsseite nicht sichtbar.
