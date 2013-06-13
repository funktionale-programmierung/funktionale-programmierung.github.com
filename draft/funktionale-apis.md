---
layout: post
description: Design funktionaler APIs am Beispiel JasperReports
title: "Funktionale APIs"
author: david-frese
tags: ["API Design", "JasperReports", "Scala"]
---

[JasperReports](http://www.jaspersoft.com/reporting) ist eine beliebte
Java-Bibliothek zur Erstellung von Reports, in der Regel in PDF oder
HTML-Form. Reports sind Auszüge oder Zusammenfassung aus größeren
Datenbeständen in Form von Tabellen, Diagrammen und begleitenden
Texten.

JasperReports bietet nun unter anderem eine API an, mit der man
programmatisch einen Report zusammenbauen kann. Diese API lässt aber
einiges zu wünschen übrig, was auch die Macher der Bibliothek
[DynamicJasper](http://dynamicjasper.com/) erkannt haben, die aber
immer noch sehr imperativ ist. Das hat uns dazu veranlasst eine rein
funktionale API in Scala zu implementieren, die auf JasperReports
aufbaut, aber den Prinzipien der Nicht-Mutierbarkeit und der
_Kompositionalität_ folgt.

In diesem Beitrag demonstriere ich die Probleme mit der
JasperReports-API, wie sie gelöst wurden, und welche funktionalen
Grundprinzipien beim Design von APIs beachtet werden sollten.

<!-- more start -->

Als Einstieg direkt ein kleines Beispiel für die Verwendung der
JasperReports-API, in der Programmiersprache [Scala]:

{% highlight java %}
JRDesignBand myCompanyBanner() {
  JRDesignBand band = new JRDesignBand();
  band.setHeight(30);
  
  JRDesignStaticText t = new JRDesignStaticText();
  t.setFontName("Helvetica");
  t.setFontSize(12);
  t.setHeight(12);
  t.setWidth(200);
  t.setX(0);
  t.setY(0);
  t.setText("My Company");
  band.addElement(t);

  return band;
}

JasperDesign myReport() {
  JasperDesign d = new JasperDesign();
  d.setName("myreport");

  JRDesignBand banner = myCompanyBanner();
  d.setPageHeader(banner);

  return d;
}
{% endhighlight %}

Der Code definiert zunächst eine Funktion die ein sogenanntes
_Band_ mit dem Schriftzug unserer Beispiel-Firma erzeugt und
zurückgibt. Ein Band ist in JasperReports eine Art Abschnitt des
Reports, der immer die volle Seitenbreite, aber nur eine bestimmte
Höhe einnimmt. Viele Band-Elemente hintereinander gehängt ergeben den
gesamten Report. Anschließend nutzt die Funktion `myReport` dieses
Firmen-Banner als Kopfzeile eines ansonsten leeren Reports.

## CRUD vs. Nicht-Mutierbarkeit

Wie man sieht ist die JasperReports-API imperativ gestaltet,
indem die Komponenten dem sogenannten [_CRUD_-Pattern](http://en.wikipedia.org/wiki/Create,_read,_update_and_delete) folgen: CRUD steht
für Create-Read-Update-Delete, und zeigt sich hier darin dass alle
Objekte zunächst "leer" erzeugt werden, und anschließend mit vielen
Set- und Add-Funktionen mit dem Inhalt gefüllt werden müssen, den wir
haben wollen.

Das ist, für einen funktionalen Programmierer, aber zunächst mal nur
lästig und fördert einen unübersichtlichen "flachen" Code (siehe auch
[hier](http://code.jaspersoft.com/svn/repos/jasperreports/tags/jr-5-1-0/jasperreports/demo/samples/noxmldesign/src/NoXmlDesignApp.java)
für ein längeres Beispiel in den JasperReport-Sourcen). Allerdings
wird das in sehr vielen APIs dieser Art an der einen oder anderen
Stelle zum Problem. Entweder dadurch, dass "Back-References"
hinzugefügt werden: ein Beispiel dafür ist in der weit verbreiteten
API [XML-DOM](http://www.w3schools.com/dom/) zu finden. Jedes
XML-Element hat dort eine Referenz auf den Vater-Knoten im XML-Baum.
Dadurch kann man das selbe XML-Element-Objekt nicht an mehrere Stellen
in den XML-Baum hängen. Das führt zu umständlichen Abstraktionen und
dazu, dass viel zu oft tiefe Kopien und "Imports" von Knoten gemacht
werden, um auf "Nummer sicher" zu gehen.

Eine andere Folge des CRUD-Patterns ist, dass Bibliotheks-Entwickler
offenbar zu gerne noch weiteren interen Zustand in die (ohnehin schon)
mutierbaren Objekte einfügen. Das sieht man den Klassen und Objekten
dann überhaupt nicht mehr an, und führt im besten Fall noch zu einem
Kommentar in der Referenz-Dokumentation, wie beispielsweise in der
Klasse
[GridData](http://help.eclipse.org/indigo/index.jsp?topic=%2Forg.eclipse.platform.doc.isv%2Freference%2Fapi%2Forg%2Feclipse%2Fswt%2Flayout%2FGridData.html)
aus dem Eclipse-Projekt, oft aber zu obskuren Fehlern.

## Beispiel: Styles

In jedem erzeugten Report-Element immer wieder neu die Schriftarten,
Abstände, Rahmen und viele weitere Eigenschaften, die das Aussehen
betreffen, zu setzen ist selbstverständlich nicht praktikabel. Was wäre
zum Beispiel, wenn wir über den Stil abstrahieren möchten. Wenn man in
die [API-Referenz](http://jasperreports.sourceforge.net/api/net/sf/jasperreports/engine/JRPrintElement.html#setStyle%28net.sf.jasperreports.engine.JRStyle%29) guckt, findet man:

{% highlight java %}
public void setStyle(JRDesignStyle style)
{% endhighlight %}

Und die Klasse [`JRDesignStyle`](http://jasperreports.sourceforge.net/api/net/sf/jasperreports/engine/design/JRDesignStyle.html) scheint alles zu enthalten was wir
brauchen, also schreiben wir doch eine Funktion die ein solches
Style-Objekt erzeugt, and ändern unsere Funktion `myCompanyBanner`
folgendermaßen:

{% highlight java %}
JRDesignStyle boldSmallText() {
  JRDesignStyle st = new JRDesignStyle();
  st.setName("bold-small");
  st.setFontName("Helvetica");
  st.setFontSize(12);
  st.setBold(true);
  return st;
}

JRDesignBand myCompanyBanner() {
  ...
  JRDesignStaticText t = new JRDesignStaticText();
  JRDesignStyle st = boldSmallText();
  t.setStyle(st);
  t.setHeight(12);
  ...
}
{% endhighlight %}

Aber was passiert wenn man aus dem ereugten Report ein PDF generieren will:

     net.sf.jasperreports.engine.JRRuntimeException: Could not resolve style(s): bold-small
     ...

Was ist passiert? Jasper will den Namen 'bold-small' auflösen, und
konnte das nicht? Tatsächlich ist es so, dass für das Aussehen des
Text-Elements die Eigenschaften des Style-Objekts, das an `setStyle`
übergeben wird, überhaupt keine Rolle spielen! Die einzige Eigenschaft
die er davon nutzt ist der `Name` des Stils. Der Gesamt-Report, das
`JasperDesign`-Objekt, hat dann wiederum eine "globale" Liste von
Style-Objekten. In diesem sucht die JasperReports-Engine nach einem
Objekt mit dem selben Namen und nur dessen Eigenschaften sind dann
relevant um das Text-Element auszugestalten.

Man kann also diese Styles nicht verwenden, wenn man gleichzeitig die
konkrete Gestalt des Firmen-Banners in einer Funktion "verstecken"
will. Entweder muß man der Funktion übergeben welche Styles sie
verwenden kann, oder sie muß zurückgeben welche sie verwendet hat. Das
freie "Kapseln" ist aber gerade eine Essenz der funktionalen
Programmierung - man ruft eine Funktion auf die ein Band erzeugt, und
kann dieses Band frei verwenden ohne sich darüber Sorgen machen zu
müssen wie es entstanden ist, oder wo es sonst noch verwendet wird.

# Eine kompositionale API

Diese Problemen für die Nutzer einer API, beziehungsweise
Fettnäpfchen für die Weiterentwicklung einer Bibliothek, kann man sehr
leicht vermeiden, indem man die Schnittstellen _rein funktional_
gestaltet. Objekte sollten nicht mutierbar sein, also nach der
Erzeugung nicht mehr verändert werden können. Dies erleichtert das
Verständnis der API, ermöglicht eine freie (Wieder-) Verwendung der
Objekte, erleichtert Tests und ermöglicht nicht zuletzt den Zugriff
auf die Objekte aus mehreren Threads heraus.

Eine weitere Eigenschaft der Nicht-Mutierbarkeit ist, dass über die
Konstruktoren bereits alle Eigenschaften eines Objekts gesetzt werden
können. Dadurch ergibt sich eine Verschachtelung des Codes, die direkt
die resultierende Baum-Struktur der Report-Elemente wiederspiegelt.
Dies ist wesentlich lesbarer und verständlicher als die flache
Struktur des Java-Codes:

{% highlight scala %}
val myCompanyBanner = Band(
  height = 30 px,
  content = StaticText(
    x = 0 px,
    y = YPos.float(0 px),
    width = 200 px,
    height = Height.fixed(12 px),
    text = "My Company"
  )
)

val myReport = Report(
  name = "myreport",
  page = Page(
    header = Some(myCompanyBanner)
  )
)
{% endhighlight %}

Diese Beschreibung des Reports und seiner Elemente, transformiert
unsere Bibliothek anschließend in ein Report-Objekt aus der
Jasper-Bibliothek, das dann weiter verarbeitet werden kann.

Der obige Beispielcode ignoriert noch das Thema der Schriftart, bzw.
der Stils, was wir in folgendem Abschnitt nachholen.

## Styles

In der funktionalen Bibliothek, die wir entwickelt haben, gibt es das
oben erwähnte Problem mit den Styles nicht. Man kann Styles frei
definieren, kombinieren und verwenden, ohne sich darüber Gedanken zu
machen wo, wie oft, und in welchen Reports sie verwendet werden:

{% highlight scala %}
val boldSmallText = Style(
  font = Font(
    fontName = Some("Helvetica"),
    fontSize = Some(12),
    bold = Some(true)
  )
)

val myCompanyBanner = Band(
  height = 30 px,
  content = StaticText(
    ...
    style = boldSmallText,
    text = "My Company"
  )
)
{% endhighlight %}

Entscheidend ist die sogenannte _Komponierbarkeit_ oder
_Kompositionalität_ der Elemente. Der Darstellungsstil ist durch das
Text-Element selbst definiert, und hängt nicht davon ab in welchen
Report es eingebaut wird. Das ermöglicht dann auch die Abstraktion, in
dem Sinn dass man hier nicht wissen muss aus was `myCompanyBanner`
besteht, um es benutzen zu können.

## Kompositionalität

Zum Abschluss noch etwas theoretischer Hintergrund:

Der Begriff Kompositionalität stammt aus der Semantikforschung Anfang
des 20. Jahrhunderts, und ist dort auch als das
[Frege-Prinzip](http://de.wikipedia.org/wiki/Frege-Prinzip) bekannt.
Er steht dafür, dass sich die Bedeutung eines komplexen Ausdrucks allein aus
den Bedeutungen der Teilausdrücke und den Kombinatoren zusammensetzt,
mit denen diese Teilausdrücke zum Gesamtausdruck zusammengefügt sind.

Andersherum bedeutet es damit auch, dass sich die Bedeutung eines
Ausdrucks nicht ändert, wenn er in verschiedenen Kontexten in größeren
Ausdrücken verwendet wird.

Daraus folgt eine wichtige Eigenschaft für das Programmieren, nämlich
dass man "Gleiches durch gleiches ersetzen" kann. Bei einer
kompositionalen API macht es keinen Unterschied, ob man ein neues
Objekt mit gewissen Eigenschaften erzeugt, oder ein bestehendes
_gleiches_ Objekt wiederverwendet. Dies ist das Sprungbrett für
mächtige Funktionen und Tools, die einem das Erstellen von Reports
noch weiter erleichtern können.

## ScalaJasper

Diese und weitere Probleme der JasperReports-API haben wir mit
[ScalaJasper](https://github.com/active-group/ScalaJasper) bereinigt;
teils durch Erweiterung um neue Features, aber genauso auch durch
Entfernen von "Irrwegen" aus der JasperReports-API.

Der Sourcecode der Bibliothek ist seit kurzem frei zugänglich bei
GitHub, wobei sich bis zum ersten Release (hoffentlich in den nächsten
Wochen) noch immer viel verändern kann - wozu auch der Name gehört.
