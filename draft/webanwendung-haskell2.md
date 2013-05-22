---
layout: post
description: Moderne Webanwendungen mit Haskell (Teil 2)
title: "Moderne Webanwendungen mit Haskell"
author: alexander-thiemann
tags: ["web", "Haskell", "JavaScript", "SOY", "HTTP", "PHP"]
---

Im letzten Teil des Artikels haben wir einen einfachen Server mit Rest-API in Haskell geschrieben.
Nun möchten wir noch ein einfaches Frontend dafür bauen. Wie bereits erwähnt wollen wir hier unter
anderem die [Soy-Templates-Sprache von Google](https://developers.google.com/closure/templates/) und deren
Kompiler verwenden, um über Templates mit JavaScript HTML zu erzeugen. Außerdem werden wir einen einfachen Controller in JavaScript schreiben, der die entsprechenden zusammenfügt und mit Daten versorgt.

<!-- more start -->

Beginnen wir also mit einer einfachen Index-Datei `index.html`, die alle benötigten JavaScripts lädt und statische HTML Elemente beschreibt.

{% highlight html %}
<!DOCTYPE html>
<html>
<head>
    <title>Haskell Blog</title>
    <script src="/jquery.min.js" type="text/javascript"></script>
    <script src="/soyutils.js" type="text/javascript"></script>
    <script src="/templates.js" type="text/javascript"></script>
    <script src="/app.js" type="text/javascript"></script>
{% endhighlight %}
`jquery.min.js`  ist die bekannt jQuery-Bibliothek für JavaScript, die DOM Manipulationen sehr einfach macht ([jQuery](http://jquery.com)). `soyutils.js` ist eine Hilfsfunktionsbiliothek von Soy-Templates, die mit dem Google-Soy-Compiler geliefert wird (siehe [hier](https://developers.google.com/closure/templates/)). Nach `templates.js` werden später unsere Soy-Templates kompiliert, und in `app.js` werden wir unseren Controller schreiben.

{% highlight html %}
    <style type="text/css">
    body {
        font: 9pt Verdana;
    }
    #container {
        width: 800px;
        margin: 10px auto;
    }

    .newsBit {
        border: 1px solid black;
        padding: 10px;
        margin-bottom: 10px;
    }

    .newsBit h2 {
        margin: 0;
    }
    </style>
</head>
{% endhighlight %}

Nur ein bisschen CSS, damit das ganze am Ende nicht zu langweilig aussieht.

{% highlight html%}
<body>

<div id="container">
    <h1>Haskell Blog</h1>
    <div id="main">
        Lade ...
    </div>
{% endhighlight %}

Wir erzeugen ein DIV mit der id `main`, in das später vom Controller die Blogposts und Kommentare geladen werden sollen.

{% highlight html %}
    <h1>Neuer Beitrag</h1>
    <form id="newNews">
        <label for="author">
            Dein Name:
        </label>
        <input type="text" placeholder="Autor" id="author" /> <br />
        <label for="author">
            Überschrift:
        </label>
        <input type="text" placeholder="Titel" id="title" /> <br />
        <label for="content">
            Beitrag:
        </label> <br />
        <textarea id="content"></textarea> <br />

        <label for="tags">
            Tags:
        </label>
        <input type="text" placeholder="Tags (mit Komma trennen)" id="tags" /> <br />
        <input type="submit" value="Speichern" />
    </form>
</div>

</body>
</html>
{% endhighlight %}

Zum Schluss noch HTML-Code, der das statische Formular zum Hinzufügen weiterer Blogbeiträge enthält. Wie bereits erwähnt haben wir keinerlei Benutzerauthentifizierung, das heißt Momentan kann jeder Besucher des Blogs auch neue Beiträge schreiben. Dieses "Problem" werden wir dann in einem dritten Teil behandeln.

Schreiben wir nun mit der SOY-Template Sprache unsere Templates:

{% highlight html %}
{namespace BlogUI autoescape="true"}
{% endhighlight %}

Der Namespace, dass heißt das JavaScript Objekt/"Modul", in dem später alle Templates als JavaScript Funktionen definiert sind, wird `BlogUI` genannt. Mit `autoescape="true"` aktivieren wir die sehr nützlichen autoescape Features von SOY-Templates. Wie diese genau funktioniert, kann man [hier](https://developers.google.com/closure/templates/docs/security) nachlesen.

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
und nennen es `news`. Die foreach-Syntax ist an die von JavaScript angelehnt, in obiger Form kann man die Schleife als ein Haskell `map` verstehen: Auf alle Elemente in der Liste `$news` wird das Template `newsBit` angewendet. Zum Schluss prüfen wir noch, ob es überhaupt Beiträge gibt, und wenn nicht geben wir die Meldung "Keine Beiträge vorhanden" aus - damit der Blog nicht komplett leer ist.

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
    <a href="javascript:Blog.showCommentsClick({$id});" id="commentLink{$id}" class="showComments commentsClosed">
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
Variable und zeigt ihn an - hier kommt dann auch unser autoescape ins Spiel! Enthält die Variable `$title` etwa den Wert `<script>alert('alert');</script>`, escaped das SOY automatisch bei der Ausgabe für uns. Außerdem kann man der Anzeige von Variablen noch so genannte [Print Directives](https://developers.google.com/closure/templates/docs/functions_and_directives#print_directives) mitgeben, wie zum Beispiel bei `{$content|changeNewlineToBr}`. In diesem Fall wird aus einem `\n` Zeilenumbruch ein HTML `<br>`.  Alles weitere ist einfach HTML und JavaScript, bis auf den `{literal}`-Blocks. Diese sorgen dafür, dass der SOY-Kompiler dessen Inhalt ignoriert, und es keine Probleme mit den geschweiften Klammern gibt.

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

{% highlight javascript %}
Blog.loadEntries = function () {
    $.get("/news", function (n) {
        var html = BlogUI.news({"news": n});
        $('#main').html(html);
    });
};
{% endhighlight %}

Mit `$.get("url", function () {})` laden wir unsere JSON-Newsliste vom Server und generieren mit unseren Templates
dann dessen HTML-Repräsentation. Diese wird dann in unser `<div id="main">` geladen.

{% highlight javascript %}
Blog.addEntry = function (author, title, content, tags) {
    $.ajax({
        type: "POST",
        url: "/news",
        data: JSON.stringify({
            author: author,
            title: title,
            content: content,
            tags: tags
        }),
        success: function () {
            Blog.resetAddForm();
            Blog.loadEntries();
        },
        error: function () {
            alert("Beitrag konnte nicht angelegt werden");
        }
    });
};

Blog.storeComment = function (newsId, author, text, onOk) {
    $.ajax({
        type: "POST",
        url: "/comments",
        data: JSON.stringify({
            author: author,
            comment: text,
            news: "" + newsId
        }),
        success: onOk,
        error: function () {
            alert("Kommentar konnte nicht gespeichert werden");
        }
    });
};
{% endhighlight %}

Einfache Wrapperfunktionen, die mit einen `POST` Request an unseren Server das Hinzufügen von Kommentaren und Beiträgen ermöglicht.

{% highlight javascript %}
Blog.resetAddForm = function () {
    $('#author').val(""); $('#title').val(""); $('#content').val(""); $('#tags').val("");
};
{% endhighlight %}

Diese Funktion setzt das "Beitrag hinzufügen"-Formular zurück. Mit `$("[SELECTOR]")` findet man ein Element im DOM-Baum, mit `.val("[WERT]")` kann
man `<input>` und `<textarea>` Elementen neue Werte zuweisen.

{% highlight javascript %}
Blog.showComments = function (id) {
    $.get("/comments/" + id, function (c) {
        var html = BlogUI.comments({"comments": c});
        $('#commentsFor' + id).html(html).slideDown();
    });
};
{% endhighlight %}

Hier laden wir die Kommentare zu einem Beitrag mit der ID `id` vom Server und zeigen sie im richtigen `div` unter dem Beitrag an.

{% highlight javascript %}
Blog.showCommentsClick = function (id) {
    var el = $('#commentLink' + id);

    if (el.hasClass('commentsClosed')) {
        Blog.showComments(id);
        el.text("Kommentare verbergen");
        el.removeClass('commentsClosed');
    } else {
        el.text("Kommentare anzeigen");
        $('#commentsFor' + id).slideUp();
        el.addClass('commentsClosed');
    }
};
{% endhighlight %}

Eine weitere Hilfsfunktion zum ein- und ausblenden von Kommentaren. Mit `.text("[TEXT]")` kann man den Inhalt von HTML-Elementen ändern. Alles wird escaped, dh. wir können kein HTML-Markup übergeben. `.slideDown()` und `.slideUp()` sind übrigens zwei Funktionen, mit denen man HTML-Elemente mit einem Schiebeffekt ein- und ausblenden kann.

{% highlight javascript %}
Blog.addComment = function (id) {
    var authorEl = $('#commentAuthor' + id);
    var textEl = $('#commentText' + id);

    Blog.storeComment(id, authorEl.val(), textEl.val(), function () {
        authorEl.val(""); textEl.val("");
        Blog.showComments(id);
    });

};
{% endhighlight %}

Wir lesen hier aus dem Formular zum Hinzufügen von Kommentaren und schicken den Kommentar an den Server zum Speichern. Dann laden wir alle Kommentare neu.

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

Zum Schluss registrieren wir einen `onload` Handler, also eine Funktion die nach dem Laden der Seite aufgerufen wird, die alle Blogbeiträge lädt und mit `.submit()` abfängt wenn das "Neuen Beitrag anlegen" Formular abgeschickt wird. Wir lesen dann die Felder aus und schicken sie mit unserer Hilfsfunktion an den Server.

Nun wäre das Grundgerüst für den Blog fertig! Wir haben also mit rund 200 Zeilen *Haskell*, etwa 100 Zeilen *JavaScript* und 150 Zeilen *HTML* einen kleinen Blog implementiert, der gut skaliert und sicher gegen XSS und SQL-Injections ist. Dass das ganze noch kürzer geht und wie Benutzerauthentifizierung und Sessions funktionieren, werde ich in weiteren Beiträgen erklären.
