---
layout: post
description:
title: "Warum funktional?"
author: stefan-wehr
tags: []
meta_description:
---

Warum haben wir als Firma entschieden Softwareentwicklung fast
ausschließlich in funktionalen Programmiersprachen durchzuführen?  Dieses
Blog gibt auf diese Fragen jede Woche eine neue Antwort. Und wir hoffen
mit unseren Antworten Softwareentwickler und Manager von funktionaler
Programmierung zu überzeugen.

Uns ist aber auch klar dass nicht in jedem
Projekt eine funktionale Sprache zum Einsatz kommen kann, sei es aus
politischen Gründen oder aufgrund externer Zwängen. So werden beispielsweise
iOS-Apps typischerweise in Objective-C geschrieben und eine moderene
Webanwendung wird mit großer Wahrscheinlichkeit in Javascript entwickelt.

Heute möchte ich eine Antwort auf die Frage "warum funktional?"
anhand eines Beispiels aus der Praxis geben. Das Beispiel
stammt aus meiner alltäglichen Arbeit und zeigt, wie man auch in einer
imperativen Sprache wie Objective-C durch 
funktionale Denkweise und die Prinzipien der funktionalen Programmierung
einfacheren, besser wartbaren Code schreiben kann.

<!-- more start -->

### Warum also funktional?

Der für mich wichtigste Grund ist die Begrenztheit meiner geistigen
Resourcen. Bei der Entwicklung eines Projekts oder Produkts gilt es viele
Punkte im Auge zu behalten: Anforderungen des Kunden, Korrektheit,
Performance, Usability und und und. Da bin ich sehr froh dass funktionale
Programme typischerweise auf Seiteneffekte verzichten und ich daher über
eine ganze Kategorien von weiteren Problemen gar nicht erst nachdenken muss.

Erst kürzlich ist mir wieder deutlich geworden, wieviel geistige Resourcen
das Nachdenken über veränderbare Datenstrukturen, Zuweisungen oder
Variablenaliasing kostet. Meine Aufgabe war es, für unsere iOS-App eine
Komponente zu entwickeln, die grob gesagt beim Auftreten gewisser
Ereignisse eine vorhandene Menge von IDs verändert und diese
Änderung als Diff in einem Baum propagiert.

Ich hatte zunächst versucht, die Komponente mit Hilfe der in iOS verfügbaren
Datenstrukturen zu lösen. Leider sind alle diese Datenstrukturen
destruktiv, d.h. man muss höllisch aufpassen dass man nicht aus Versehen
eine Datenstruktur ändert obwohl man zu einem späteren Zeitpunkt noch den
alten Zustand benötigt. Beim Entwickeln habe ich dann gemerkt dass es
relativ viel Zeit kostet über alle solche Auswirkungen einer destruktiven
Operation nachzudenken, ohne dass man dabei dem eigentlichen Ziel wirklich näher
kommt. Daher habe ich mich nach kurzer Zeit entschieden 
[*persistente Datenstrukturen*](http://en.wikipedia.org/wiki/Persistent_data_structure)
zu verwenden. Solche Datenstrukturen sind aus der
funktionalen Programmierung wohlbekannt und haben die schöne Eigenschaft,
dass Änderungsoperationen nie die Datenstruktur selbst ändern, sondern
eine neue, geänderte Sicht zurückliefern und die alte Datenstruktur
unberührt lassen.

Als Beispiel möchte ich eine vereinfachte Version der Stelle innerhalb der
Komponente zeigen, an der das Diff zwischen der alten und der neuen Menge
von IDs berechnet wird. Unter dem Diff zweier Menge `old` und `new`
verstehe ich dabei die Information, die nötig ist, um Änderungen
zwischen `old` und `new` im Baum propagieren zu können.  In
meinem Anwendungsfall sind die Mengen selbst typischerweise relativ groß,
die Änderungen zwischen den Mengen aber ziemlich klein. Daher liegt es
nahe, das Diff zwischen `old` und `new` als ein Paar `(toAdd, toRemove)`
zu repräsentieren, so dass bei Anwenden des Diffs auf eine Menge `s` die
Elemente aus `toAdd` zu `s` hinzugefügt werden, während die Elemente aus
`toRemove` von `s` entfernt werden.

Mein erster, destruktiver Ansatz sah in etwa so aus:

{% highlight objective-c %}
@interface DestructiveSetDiff : NSObject
@property (nonatomic, strong) NSSet *toAdd;
@property (nonatomic, strong) NSSet *toRemove;
+ (DestructiveSetDiff *)diffNew:(NSMutableSet *)new withOld:(NSMutableSet *)old;
- (void)applyDiff:(NSMutableSet *)s;
@end
@implementation DestructiveSetDiff
+ (DestructiveSetDiff *)diffNew:(NSMutableSet *)new withOld:(NSMutableSet *)old
{
    NSMutableSet *toAdd = [NSMutableSet setWithSet:new]; // (*) Kopie erstellen
    [toAdd minusSet:old];
    [old minusSet:new];
    DestructiveSetDiff *diff = [[DestructiveSetDiff alloc] init];
    diff.toAdd = toAdd;
    diff.toRemove = old;
    return diff;
}
- (void)applyDiff:(NSMutableSet *)set
{
    [set minusSet:self.toRemove];
    [set unionSet:self.toAdd];
}
@end
{% endhighlight %}

Wie in Objective-C üblich steht zwischen `@interface` und `@end` die
Deklarationen der öffentlichen Eigenschaften und Methoden der Klasse
`DestructiveSetDiff`, während der `@implementation` Block dann die
Methoden implementiert.  Beachten Sie, dass in der mit `(*)` markierten
Zeile ein Kopie der Menge `new` erstellt wird. Dies ist nötig, um
Korrektheit sicherzustellen. Die beiden anderen Parameter `old` und `set`
werden hingegen destruktiv modifiziert, zum einen aus Gründen der
Effizienz zum anderen einfach weil eine imperative Sprache wie Objective-C
diese Implementierung nahe legt. Eine Inspektion meines übrigen Codes
hatte mich zunächst davon überzeugt, dass diese destruktiven Modifikationen
in Ordnung sind.

Allerdings habe ich dann später noch Änderungen vorgenommen und plötzlich
hatten die destruktiven Änderungen fatale Folgen, da die dort modifizierten
Mengen an anderer Stelle mit Annahme des alten Zustands verwendet
wurden. Konkret ist dieses Problem aufgetreten als ich eine Optimierung
an der Serialisierung vorgenommen hatte. 

Mit jedem Knoten im Baum sind nämlich mehrere Mengen von IDs assoziert,
die Auswahl der richtigen Menge geschieht über einen Schlüssel
vom Typ `NSString`.
Vor der Optimierung hatte ich einfach alle mit einem Knoten assozierten
Mengen serialisiert und auf Platte geschrieben.  Da es aber auch durchaus
vorkommen kann, dass sich gewisse Menge nicht ändern, wollte ich diesen
Umstand ausnützen und nur die veränderten Mengen speichern.  Dazu habe ich
eine Klasse `SerializableNodeData` eingeführt, der man explizit mitteilen
muss, dass sich eine Menge geändert hat, und die bei der
Serialisierung nur die tatsächlich geänderten Mengen speichert.  Die
Schnittstelle der Klasse sieht in etwa so aus:

{% highlight objective-c %}
@interface SerializableNodeData
- (NSMutableSet *)valueForKey:(NSString *)key;
- (void)setValue:(NSSet *)set forKey:(NSString *)key;
- (void)serialize;
@end
{% endhighlight %}

Die Implementierung von `setValue:forKey:` hat folgende Form, wobei
`self.dirtyKeys` die Menge der Schlüssel repräsentiert, deren Werte verändert wurden,
und `self.values` die aktuellen Werte speichert.

{% highlight objective-c %}
- (void)setValue:(NSSet *)set forKey:(NSString *)key
{
    NSSet *old = [self.values objectForKey:key];
    if (![set isEqualToSet:old]) {
        [self.dirtyKeys addObject:key];
        [self.values setObject:set forKey:key];
    }
}
{% endhighlight %}

Bei der Serialisierung werden dann nur die Schlüssel gespeichert die
in `self.dirtyKeys` enthalten sind. Mein Code zum Anwenden des Diffs und zum
Speichern des neuen Werts sah dann nach der Optimierung in etwa so aus:

{% highlight objective-c %}
    SerializableNodeData *nd;
    NSString *key;
    DestructiveSetDiff *diff;
    // ...
    NSMutableSet *old = [nd valueForKey:key];
    [diff applyDiff:old];
    [nd setValue:old forKey:key];
    [nd serialize];
{% endhighlight %}

In dieser kompakten Darstellung und mit etwas Abstand ist offensichtlich was hier schiefgeht:
keiner der Schlüssel von `SerializableNodeData` wird jemals als "dirty" markiert werden,
denn der Vergleich `[set isEqualToSet:old]` in der Methode `setValue:forKey:` liefert immer `true`
da `set` und `old` Aliase für dasselbe Objekt sind. Daher passierte mit meiner
Optimierung bei der Serialisierung gar nichts.

Nach dieser Einsicht habe ich mich dann an meine funktionalen Wurzeln erinnert und eine
persistente Version `PersistentSetDiff` geschrieben. In dieser Version
werden keine Parameter destruktiv modifiziert und die Methode `applyDiff:`
liefert ihr Ergebnis als Rückgabewert. Damit war der oben geschilderte Bug auch sofort
behoben.

{% highlight objective-c %}
@interface PersistentSetDiff : NSObject
@property (nonatomic, strong) PersistentSet *toAdd;
@property (nonatomic, strong) PersistentSet *toRemove;
- (PersistentSetDiff *)diffNew:(PersistentSet *)new withOld:(PersistentSet *)old;
- (PersistentSet *)applyDiff:(PersistentSet *)s;
@end
@implementation PersistentSetDiff
- (PersistentSetDiff *)diffNew:(PersistentSet *)new withOld:(PersistentSet *)old
{
    PersistentSetDiff *diff = [[PersistentSetDiff alloc] init];
    diff.toAdd = [new minusSet:old];
    diff.toRemove = [old minusSet:new];
    return diff;
}
- (PersistentSet *)applyDiff:(PersistentSet *)s
{
    PersistentSet *s0 = [s minusSet:self.toRemove];
    return [s0 unionSet:self.toAdd];
}
@end
{% endhighlight %}

Sie sehen außerdem, dass ich statt `NSSet` und `NSMutableSet` nun überall
eine selbstgeschriebene Klasse `PersistentSet` verwende. Das Interface
von `PersistentSet` sieht wie folgt aus:

{% highlight objective-c %}
@interface PersistentSet : NSObject <NSFastEnumeration>
+ (FRPersistentSet *)empty;
+ (FRPersistentSet *)setWithObject:(id)object;
+ (FRPersistentSet *)setWithObjects:(id)firstObj, ...;
+ (FRPersistentSet *)setWithSet:(NSSet *)set;
+ (FRPersistentSet *)setWithArray:(NSArray *)arr;
- (NSInteger)count;
- (FRPersistentSet *)unionSet:(FRPersistentSet *)other;
- (FRPersistentSet *)minusSet:(FRPersistentSet *)other;
- (FRPersistentSet *)addObject:(id)obj;
- (FRPersistentSet *)removeObject:(id)obj;
- (NSArray *)array;
- (id)member:(id)obj;
@end
{% endhighlight %}

Momentan ist die Implementierung von `PersistentSet` naiv und basiert auf
einem `NSMutableSet`, das an den richtigen Stellen kopiert wird. Falls es
mit der naiven Implementierung irgendwann mal Performanceprobleme geben
sollte, könnte ich die naive Implementierung durch eine deutlich
performantere ersetzen.  Dazu bieten sich z.B. ["Hash Tries"](http://lampwww.epfl.ch/papers/idealhashtrees.pdf) an, wie sie
beispielsweise auch in [Clojure](http://clojure.org/)
([Blogartikel](http://blog.higher-order.net/2009/09/08/understanding-clojures-persistenthashmap-deftwice/))
oder [Scala](http://scala-lang.org)
([Sourcecode](https://github.com/scala/scala/blob/v2.10.1/src/library/scala/collection/immutable/HashSet.scala#L1))
zum Einsatz kommen.

Neben `PersistentSetDiff` habe ich bei der Implementierung meiner
Komponente auch noch an vielen anderen Stellen persistente Datenstrukturen
anstatt destruktiver verwendet. Dadurch konnte ich die Komponente
schneller entwickeln, da weniger Nachdenken über mögliche Auswirkungen
destruktiver Operationen nötig war. Außerdem wurde der Code dadurch besser
wartbar und weniger fehleranfällig.

So, das war's für heute mit meiner ganz persönlichen Einschätzung warum es
sich lohnt in die funktionale Programmierung reinzuschauen. Ich freue mich
auf ihr Feedback und ihre Fragen!
