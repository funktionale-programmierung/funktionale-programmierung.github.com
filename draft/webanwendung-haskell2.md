---
layout: post
description: Moderne Webanwendungen mit Haskell (Teil 2)
title: "Moderne Webanwendungen mit Haskell"
author: alexander-thiemann
tags: ["web", "Haskell", "JavaScript", "SOY", "HTTP", "PHP"]
---
Nun schreiben wir also den Code für die index.html:

{% highlight html %}
<!DOCTYPE html>
<html>
<head>
    <title>Haskell Blog</title>
    <script src="/jquery.min.js" type="text/javascript"></script>
    <script src="/soyutils.js" type="text/javascript"></script>
    <script src="/templates.js" type="text/javascript"></script>
    <script src="/app.js" type="text/javascript"></script>

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
<body>

<div id="container">
    <h1>Haskell Blog</h1>
    <div id="main">
        Lade ...
    </div>

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

Zum *HTML* Code gibt‘s denke ich nicht viel zu sagen, es ist lediglich ein einfaches Grundgerüst was alle notwendigen *JavaScript*s lädt und einen sehr
einfachen *CSS*-Style enthält.

Schreiben wir nun mit der *SOY*-Template Sprache unsere Templates:

{% highlight html %}
{namespace BlogUI autoescape="true"}

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
{literal}$('#addCommentFor{/literal}{$id}{literal}').submit(function (e) {
   e.preventDefault();
   Blog.addComment({/literal}{$id}{literal});
});{/literal}
</script>
{/template}

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

Ich denke auch hier versteht mal relativ schnell wie *SOY*-Templates funktionieren: Zunächst wird ein namespace angegeben, in dem die kompilierten
Template-Funktionen später landen sollen. Dann definieren wir unsere Template-Funktionen. Über die Kommentare werden die Parameter gesteuert (`@param xx`)
und dann folgt eben das Template. Mit dem Soy-Compiler können wir nun daraus JavaScript machen:

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

Blog.loadEntries = function () {
    $.get("/news", function (n) {
        var html = BlogUI.news({"news": n});
        $('#main').html(html);
    });
};

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

Blog.resetAddForm = function () {
    $('#author').val(""); $('#title').val(""); $('#content').val(""); $('#tags').val("");
};

Blog.showComments = function (id) {
    $.get("/comments/" + id, function (c) {
        var html = BlogUI.comments({"comments": c});
        $('#commentsFor' + id).html(html).slideDown();
    });
};

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

Blog.addComment = function (id) {
    var authorEl = $('#commentAuthor' + id);
    var textEl = $('#commentText' + id);

    Blog.storeComment(id, authorEl.val(), textEl.val(), function () {
        authorEl.val(""); textEl.val("");
        Blog.showComments(id);
    });

};

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

Nun wäre der Blog also komplett! Wir haben also mit rund 200 Zeilen *Haskell*, etwa 100 Zeilen *JavaScript* und 150 Zeilen *HTML* einen kleinen Blog
implementiert, der gut skaliert und sicher gegen XSS und SQL-Injections ist.
