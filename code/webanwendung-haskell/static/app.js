/**
 * Haskell Blog
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