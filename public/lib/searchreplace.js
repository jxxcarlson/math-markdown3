// view-source:http://web.mit.edu/xavid/Public/bazki/lib/ext/codemirror/demo/search.html

function search() {
  unmark();
  var text = document.getElementById("query").value;
  if (!text) return;
  for (var cursor = editor.getSearchCursor(text); cursor.findNext();)
    marked.push(editor.markText(cursor.from(), cursor.to(), "searched"));

  if (lastQuery != text) lastPos = null;
  var cursor = editor.getSearchCursor(text, lastPos || editor.getCursor());
  if (!cursor.findNext()) {
    cursor = editor.getSearchCursor(text);
    if (!cursor.findNext()) return;
  }
  editor.setSelection(cursor.from(), cursor.to());
  lastQuery = text; lastPos = cursor.to();
}

function replace() {
  unmark();
  var text = document.getElementById("query").value,
      replace = document.getElementById("replace").value;
  if (!text) return;
  for (var cursor = editor.getSearchCursor(text); cursor.findNext();)
    editor.replaceRange(replace, cursor.from(), cursor.to());
}
