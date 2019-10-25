// NAVIGATION

// Inform app of browser navigation (the BACK and FORWARD buttons)
window.addEventListener('popstate', function () {
    console.log("popstate, location", location.href)
    app.ports.onUrlChange.send(location.href);
});

// Change the URL upon request, inform app of the change.
app.ports.pushUrl.subscribe(function(url) {
    console.log("push, url", url)
    console.log("push, location", location.href)
    history.pushState({}, '', url);
    app.ports.onUrlChange.send(location.href);
});
