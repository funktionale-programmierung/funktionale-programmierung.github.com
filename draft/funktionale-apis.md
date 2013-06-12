---
layout: post
description: Design funktionaler APIs am Beispiel JasperReports
title: "Funktionale APIs"
author: david-frese
tags: ["API Design", "JasperReports", "Scala"]
---

[JasperReports](http://www.jaspersoft.com/reporting) ist eine beliebte
Java-Bibliothek zur Erstellung von Reports, in der Regel in PDF oder
HTML Form. Reports sind Auszüge oder Zusammenfassung aus größeren
Datenbeständen in Form von Tabellen, Diagrammen und begleitenden
Texten.

JasperReports bietet nun unter anderem eine API an, mit der man
programmatisch einen Report zusammenbauen kann. Diese API lässt aber
einiges zu wünschen übrig, was uns dazu veranlasst hat eine rein
funktionale API in Scala davor zu schalten, die dem Prinzip der
_Kompositionalität_ folgt.

In diesem Beitrag demonstriere ich die Probleme mit der
JasperReports-API, wie sie gelöst wurden, und welche funktionalen
Grundprinzipien beim Design von APIs beachtet werden sollten.

<!-- more start -->

Als Einstieg direkt ein kleines Beispiel für die Verwendung der
JasperReports-API, in der Programmiersprache [Scala]:

{% highlight scala %}
def myCompanyBanner() = {
  val band = new JRDesignBand()
  band.setHeight(30)
  
  val t = new JRDesignStaticText()
  t.setFontName("Helvetica")
  t.setFontSize(12)
  t.setHeight(12)
  t.setWidth(200)
  t.setX(0)
  t.setY(0)
  t.setText("My Company")
  band.addElement(t)

  band
}

def myReport() = {
  val d = new JasperDesign()
  d.setName("myreport")

  val banner = myCompanyBanner()  
  d.setPageHeader(banner)

  d
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

Wie man sieht ist die JasperReports-API sehr imperativ gestaltet,
indem die Komponenten dem sogenannten _CRUD_-Pattern folgen: CRUD steht
für Create-Read-Update-Delete, und zeigt sich hier darin dass alle
Objekte zunächst "leer" erzeugt werden, und anschließend mit vielen
Set- und Add-Funktionen mit dem Inhalt gefüllt werden müssen, den wir
haben wollen.

Das ist, für einen funktionalen Programmierer, aber zunächst mal nur
lästig und fördert einen unübersichtlichen "flachen" Code (siehe auch
[hier](http://code.jaspersoft.com/svn/repos/jasperreports/tags/jr-5-1-0/jasperreports/demo/samples/noxmldesign/src/NoXmlDesignApp.java)
für ein längeres Beispiel in den JasperReport Sourcen). Allerdings
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

Diese Problemen für die Nutzer einer API, beziehungsweise
Fettnäpfchen für die Weiterentwicklung einer Bibliothek, kann man sehr
leicht vermeiden, indem man die Schnittstellen _rein funktional_
gestaltet. Objekte sollten nicht mutierbar sein, also nach der
Erzeugung nicht mehr verändert werden können. Dies erleichtert das
Verständnis der API, ermöglicht eine freie (Wieder-) Verwendung der
Objekte, erleichtert Tests und ermöglicht nicht zuletzt den Zugriff
auf die Objekte aus mehreren Threads heraus.

## Beispiel: Styles

In jedem erzeugten Report-Element immer wieder neu die Schriftarten,
Abstände, Rahmen und viele weitere Eigenschaften, die das Aussehen
betreffen, zu setzen ist selbstverständlich nicht praktikabel. Was wäre
zum Beispiel, wenn wir über den Stil abstrahieren möchten. Wenn man in
die API-Referenz guckt, findet man:

{% highlight java %}
public void setStyle(JRDesignStyle style)
{% endhighlight %}

Und die Klasse 'JRDesignStyle' scheint alles zu enthalten was wir
brauchen, also schreiben wir doch eine Funktion die ein solches
Style-Objekt erzeugt, and ändern unsere Funktion `mkCompanyBanner`
folgendermaßen:

{% highlight scala %}
def boldSmallText() = {
  val st = new JRDesignStyle()
  st.setName("bold-small")
  st.setFontName("Helvetica")
  st.setFontSize(12)
  st.setBold(true)
  st
}

def myCompanyBanner() = {
  ...
  val t = new JRDesignStaticText()
  val st = boldSmallText()
  t.setStyle(st)
  t.setHeight(12)
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

In der funktionalen Bibliothek, die wir entwickelt haben, gibt es
dieses Problem nicht. Man kann Styles frei definieren, kombinieren und
verwenden, ohne sich darüber Gedanken zu machen wo, wie oft, und in
welchen Reports sie verwendet werden:

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
  splitType = SplitTypeEnum.STRETCH,
  content = StaticText(
    x = 0 px,
    y = YPos.float(0 px),
    width = 200 px,
    height = Height.fixed(12 px),
    style = boldSmallText,
    text = "My Company"
  )
)

def myReport() = Report(
  name = "myreport",
  page = Page(header = Some(myCompanyBanner))
)
{% endhighlight %}

Entscheidend ist die sogenannte _Komponierbarkeit_ der Elemente. Der
Darstellungsstil ist durch das Text-Element selbst definiert, und
hängt nicht davon ab in welchen Report es eingebaut wird. Das
ermöglicht dann auch die Abstraktion, in dem Sinn dass man hier nicht
wissen muss aus was `myCompanyBanner` besteht, um es benutzen zu
können.

## Positionen und Größen

Was in obigem Beispiel schon zu sehen ist, ist die Erweiterung um
die Möglichkeit Positionen und Größen nicht nur in Pixeln anzugeben.
Weitere vordefinierte Möglichkeiten sind `mm`, `cm` und `inch`. Eine
größere Erleichterung für die Komponierbarkeit ist allerdings die
Möglichkeit für Breite und horizontale Position prozentuale Angaben
machen zu können:

{% highlight scala %}
StaticText(
  width = 100 percent,
  height = Height.fixed(12 px),
  style = boldSmallText,
  text = "My Company"
)
{% endhighlight %}

Ein solches Text-Element nimmt immer die komplette zur Verfügung
stehende Breite ein, egal was der Report für Seitenränder definiert,
ob Hoch- oder Querformat oder das Text-Element in einer kleinen
Tabellenspalte verwendet wird.

## ScalaJasper

Diese und weitere Probleme der JasperReports-API haben wir mit
[ScalaJasper] bereinigt; teils durch Erweiterung um neue Features,
aber genauso auch durch Entfernen von "Irrwegen" aus der
JasperReports-API.

Der Sourcecode der Bibliothek ist seit kurzem frei zugänglich, wobei
sich bis zum ersten Release (hoffentlich in den nächsten Wochen) noch
immer viel verändern kann - wozu auch der Name gehört.
