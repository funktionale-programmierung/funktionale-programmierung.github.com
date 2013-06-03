---
layout: post
description: Moderne Webanwendungen mit Haskell und Javascript: clientseite Implementierung
title: "Moderne Webanwendungen mit Haskell und Javascript: clientseite Implementierung"
author: alexander-thiemann
tags: ["web", "Haskell", "JavaScript", "SOY", "HTTP", "PHP"]
---

Im [letzten Teil](http://funktionale-programmierung.de/2013/04/04/webanwendung-haskell.html) des Artikels haben wir einen einfachen Server mit Rest-API in Haskell geschrieben.
Nun möchten wir noch ein einfaches Frontend dafür bauen. Wie bereits erwähnt wollen wir hier unter
anderem die [Soy-Templates-Sprache von Google](https://developers.google.com/closure/templates/) und deren
Kompiler verwenden, um über Templates mit JavaScript HTML zu erzeugen. Außerdem werden wir einen einfachen Controller in JavaScript schreiben, der die entsprechenden Funktionen zusammenfügt und mit Daten versorgt.

<!-- more start -->

Zunächst möchte ich mich nocheinmal kurz auf den letzten Teil beziehen: Mit Hilfe von [TemplateHaskell](http://www.haskell.org/ghc/docs/7.0.2/html/users_guide/template-haskell.html) und der JSON-Biliothek [aeson](https://github.com/bos/aeson) haben für unsere
Datentypen Persistenz und JSON Serialisierung erzeugt. Dann haben wir mit [scotty](https://github.com/xich/scotty) auf einfache Art und Weise HTTP-Routen erzeugt um auf unsere persitenten serialisierten Objekte zugreifen zu können. Zum Schluss hatten wir einen fertigen Server mit REST-API, den man wie folgt ansteuern konnte:

Beiträge hinzufügen:
{% highlight bash %}
$ curl --data '{"title": "Test", "author": "Alex", "tags": ["a", "b"], "content": "Test Beitrag"}' http://localhost:8085/news
true
{% endhighlight %}

Liste aller Beiträge anzeigen:
{% highlight bash %}
$ curl http://localhost:8085/news
[{"title":"Test","author":"Alex","tags":["a","b"],"id":5,"content":"Test Beitrag"}]
{% endhighlight %}

Beginnen wir nun mit den HTML Frontend für unsere Anwendung. Ich denke, das statische HTML und CSS Grundgerüst muss an dieser Stelle nicht weiter erläutert werden, es ist im [GitHub Repository des Mini Blogs](https://github.com/agrafix/HaskellBlog/blob/master/static/index.html) zu finden.

Schreiben wir nun also mit der SOY-Template Sprache unsere Templates:

{% highlight html %}
{namespace BlogUI autoescape="true"}
{% endhighlight %}

Der Namespace (in Wirklichkeit einfach ein JavaScript Objekt), in dem später alle Templates als JavaScript Funktionen definiert sind, wird `BlogUI` genannt. Mit `autoescape="true"` aktivieren wir die sehr nützlichen autoescape Features von SOY-Templates. Wie diese genau funktioniert, kann man [hier](https://developers.google.com/closure/templates/docs/security) nachlesen.

{% highlight html %}
/**
 * Render a list of news
 *
 * @param news
 */
{template .news}
{foreach $entry in $news}
    {call .newsBit data="$entry" /}
{/foreach}
{if (length($news) == 0)}
<i>Keine Beiträge vorhanden.</i>
{/if}
{/template}
{% endhighlight %}

Unser erstes Template erzeugt eine Seite mit Blogbeiträgen. Zunächst ist vor jedem Template ein Kommentar notwendig. Dieser enthält eine optionale Beschreibung des Templates, und die Liste aller Parameter. Mit `{template .news}` beginnen wir nun ein neues Template im aktuellem Namespace
und nennen es `news`. Die foreach-Syntax ist an die von JavaScript angelehnt, in obiger Form kann man die Schleife als ein Haskell `map` verstehen: Auf alle Elemente in der Liste `$news` wird das Template `newsBit` angewendet. Zum Schluss prüfen wir noch, ob es überhaupt Beiträge gibt, und wenn nicht geben wir die Meldung "Keine Beiträge vorhanden" aus - damit der Blog nicht komplett leer ist. Zur Erinnerung: die JSON-Struktur eines Beitrags sieht wie folgt aus:

{% highlight javascript %}
{ "title":"Test",
  "author":"Alex",
  "tags":["a","b"],
  "id":5,
  "content":"Test Beitrag"
}
{% endhighlight %}

Das werden wir nun zum Rendern von Beiträgen ausnutzen:

{% highlight html %}
/**
 * Render a news entry
 *
 * @param id
 * @param title
 * @param content
 * @param tags
 * @param author
 */
{template .newsBit}
<div class="newsBit">
    <h2>{$title}</h2>
    <p>
        <i>geschreiben von {$author}</i> <br />
        {$content|changeNewlineToBr}
    </p>
    <span class="tags">
        Tags: {foreach $tag in $tags}{$tag}{if (not isLast($tag))}, {/if}{/foreach}
    </span>
    <h3>Kommentar verfassen:</h3>
    <form id="addCommentFor{$id}">
        <input type="text" placeholder="Ihr Name" id="commentAuthor{$id}" /> <br />
        <textarea id="commentText{$id}"></textarea> <br />
        <input type="submit" value="Speichern" />
    </form>
    <br />
    <a href="javascript:Blog.showCommentsClick({$id});" 
       id="commentLink{$id}" 
       class="showComments commentsClosed">
        Kommentare anzeigen
    </a>
    <div id="commentsFor{$id}" style="display:none;"></div>
</div>

<script>
$('#addCommentFor{$id}').submit(function (e) /*{literal}*/{/*{/literal}*/
   e.preventDefault();
   Blog.addComment({$id});
/*{literal}*/});/*{/literal}*/
</script>
{/template}
{% endhighlight %}

Das Template für einen einzelnden Blogbeitrag zeigt weitere Funktionen der SOY-Templates: Mit `{$variable}` liest man den Inhalt einer
Variable und zeigt ihn an - hier kommt dann auch unser autoescape ins Spiel! Enthält die Variable `$title` etwa den Wert `<script>alert('alert');</script>`, wird die Ausgabe automatisch "escaped". Außerdem kann man der Anzeige von Variablen noch so genannte [Print Directives](https://developers.google.com/closure/templates/docs/functions_and_directives#print_directives) mitgeben, wie zum Beispiel bei `{$content|changeNewlineToBr}`. In diesem Fall wird aus einem `\n` Zeilenumbruch ein HTML `<br>`.  Alles weitere ist einfach HTML und JavaScript, bis auf den `{literal}`-Blocks. Diese sorgen dafür, dass der SOY-Kompiler dessen Inhalt ignoriert, und es keine Probleme mit den geschweiften Klammern gibt. Mit der jQuery Notation `$('...')` kann man den DOM-Baum traversieren und ein Element wählen. Mehr zu der Syntax findet man [hier](http://api.jquery.com/category/selectors/). Das `Blog` Objekt ist unser Controller, welcher weiter unten erklärt wird.

{% highlight html %}
/**
 * Render a list of comments
 *
 * @param comments
 */
{template .comments}
{foreach $comment in $comments}
    {call .commentBit data="$comment" /}
{/foreach}
{if (length($comments) == 0)}
<i>Keine Kommentare vorhanden.</i>
{/if}
{/template}

/**
 * Render a comment
 *
 * @param comment
 * @param author
 */
{template .commentBit}
<div class="commentBit">
    <p>
        <b>{$author}:</b> {$comment|changeNewlineToBr}
    </p>
</div>
{/template}
{% endhighlight %}

Die Templates für das Anzeigen von Kommentaren unter Blogbeiträgen funktionieren analog zu denen der Blogbeiträge.

Mit dem Soy-Compiler können wir nun daraus JavaScript machen:

{% highlight bash %}
java -jar SoyToJsSrcCompiler.jar --outputPathFormat static/templates.js static/templates.soy
{% endhighlight %}

Jetzt fehlt nur noch die Logik, die die *REST-API* mit unseren Templates/Views verbindet. Das geht mit Hilfe von *jQuery* auch relativ einfach:

{% highlight javascript %}
/**
 * Haskell Blog
 * app.js
 */

var Blog = {};
{% endhighlight %}

Wir erzeugen mithilfe von einem Objekt einen "Namespace" `Blog` für unsere Funktionen.

Mit `$.get("url", function () {})` laden wir unsere JSON-Newsliste vom Server und generieren mit unseren Templates
dann dessen HTML-Repräsentation. Diese wird dann in unser `<div id="main">` geladen:

{% highlight javascript %}
Blog.loadEntries = function () {
    $.get("/news", function (n) {
        var html = BlogUI.news({"news": n});
        $('#main').html(html);
    });
};
{% endhighlight %}

Folgende weitere Funktionen unterstützt unserer Controller. Da die Implementierung relativ einfach ist
haben wir sie hier aus Gründen der Übersichtlichkeit weggelassen. Sie finde aber den kompletten Code
im [GitHub Repository des Mini Blogs](https://github.com/agrafix/HaskellBlog/blob/master/static/app.js).
{% highlight javascript %}
Blog.addEntry = function (author, title, content, tags) { /* ... */ }
Blog.storeComment = function (newsId, author, text, onOk) { /* ... */ }
Blog.resetAddForm = function () { /* ... */ }
Blog.showComments = function (id) { /* ... */ }
Blog.showCommentsClick = function (id) { /* ... */ }
Blog.addComment = function (id) { /* ... */ }
{% endhighlight %}

Zum Schluss registrieren wir einen `onload` Handler, also eine Funktion die nach dem Laden der Seite aufgerufen wird, die alle Blogbeiträge lädt und mit `.submit()` abfängt wenn das "Neuen Beitrag anlegen" Formular abgeschickt wird. Wir lesen dann die Felder aus und schicken sie mit unserer Hilfsfunktion an den Server.

{% highlight javascript %}
$(function () {
    Blog.loadEntries();
    $('#newNews').submit(function (e) {
        e.preventDefault();
        Blog.addEntry($('#author').val(),
                      $('#title').val(),
                      $('#content').val(),
                      $('#tags').val().split(",")
        );
    });
});
{% endhighlight %}

Nun wäre das Grundgerüst für den Blog fertig! Wir haben also mit rund 200 Zeilen *Haskell*, etwa 100 Zeilen *JavaScript* und 150 Zeilen *HTML* einen kleinen Blog implementiert, der gut skaliert und sicher gegen XSS und SQL-Injections ist. Versuchen wir etwa HTML-Code in einen Kommentar zu schmuggeln, sorgt das AutoEscaping der SOY-Templates dafür, dass aus beispielsweise `<script>alert("HALLO!");</script>` `&lt;script&gt;alert("HALLO!");&lgt;</script&gt;` wird. Wenn wir einen Kommentar schreiben, mit dem wir versuchen das eigentliche SQL-Query was dahinter steckt zu manipulieren (zB: `', NULL, NULL, 'ASDF') --`, schlägt das ebenfalls fehl: Das persistent Framework kennt nämlich die Typen unserer SQL-Felder und escaped die Eingaben entsprechend.

Den gesammten Code für den Blog findet man im bereits erwähten [GitHub Repository](https://github.com/agrafix/HaskellBlog/) mit entsprechender Cabal-Datei. Den Blog kann man also einfach mit `cabal configure && cabal build` bauen und dann starten.

Natürlich kann man einen Blog mit noch weniger Haskell-Code schreiben. Wie das geht und wie man dem Blog noch Benutzerauthentifizierung und Sessions spendiert werde ich in einem weiteren Beitrag erklären.
