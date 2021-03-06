// OUTSIDE

app.ports.infoForOutside.subscribe(msg => {

    console.log("I am here!")
    // console.log("infoForOutside:: ", msg.tag);

    switch(msg.tag) {


       case "AskToReconnectUser":
        console.log("AskToReconnectUser")
        if (sessionStorage.currentUser != null) {
            app.ports.infoForElm.send({tag: "ReconnectUser", data: JSON.parse(sessionStorage.currentUser)})
        }
        break;

        case "AskForClipBoard":
        console.log("I am logging this: AskForClipBoard")
        navigator.clipboard.readText()
          .then(text => {
            console.log('Clipboard (outside):', text);
            app.ports.infoForElm.send({tag: "GotClipboard", data:  text})
          })
          .catch(err => {
            console.error('Failed to read clipboard: ', err);
          });

        break;

        case "UserData":
        console.log("UserData")
        if (msg.data != null) {
          sessionStorage.setItem("currentUser", JSON.stringify(msg.data));
        }
        break;

        case "DestroyUserData":
        console.log("DestroyUserData")
        sessionStorage.clear();
        break;


        case "AskForDequeData":
            console.log("AskForDequeData")
            if (localStorage.deque != null) {
                app.ports.infoForElm.send({tag: "UuidList", data: JSON.parse(localStorage.deque)})
            }
            break;
                    case "DequeData":
        console.log("DequeData")
        if (msg.data != null) {
          localStorage.setItem("deque", JSON.stringify(msg.data));
        }
        break;

       case "GetSelection":
           console.log("GetSelection (2)")
           var value =  window.getSelection()
           console.log ("value: ", value)
           console.log("NODE:", value.focusNode.data )
           var jsonData = {selection: value.focusNode.data}
           console.log ("json", jsonData)
           app.ports.infoForElm.send({tag: "GotSelection", data: jsonData})
       break;

       case "GetSelectionForSyncOutside":
            console.log("GetSelectionForSyncOutside (yay!)")
            var value =  window.getSelection()
            console.log ("value fo SYNCs: ", value)
            console.log("NODE:", value.focusNode.data )
            var jsonData = {selection: value.focusNode.data}
            console.log ("json", jsonData)
            app.ports.infoForElm.send({tag: "GotSelectionForSync", data: jsonData})
       break;


      case "ScrollToLine":
         console.log("ScrollToLine", msg.data)  //, JSON.stringify(msg.data))
      break;

       case "AskForClipBoard":
        console.log("AskForClipBoard")

        navigator.clipboard.readText()
          .then(text => {
            console.log('Clipboard (outside):', text);
            app.ports.infoForElm.send({tag: "GotClipboard", data:  text})
          })
          .catch(err => {
            console.error('Failed to read clipboard: ', err);
          });

        break;

       case "WriteToClipboard":
            console.log("WriteToClipboard", JSON.stringify(msg.data))

            navigator.permissions.query({name: "clipboard-write"}).then(result => {
              if (result.state == "granted" || result.state == "prompt") {
                updateClipboard(JSON.stringify(msg.data))
              }
            });


       break;

    }

    function updateClipboard(newClip) {
      navigator.clipboard.writeText(newClip).then(function() {
        console.log("Wrote to clipboard");
      }, function() {
        console.log ("Clipboard write failed");
      });
    }

})



// This is needed so that selected text is sent
// from the browser back to the Elm app using
// GetSelection-GotSelection (see above)

document.addEventListener('selectionchange', () => {
  console.log("Selection: " + document.getSelection());
});