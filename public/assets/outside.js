// OUTSIDE

app.ports.infoForOutside.subscribe(msg => {

    console.log("app.ports.infoForOutside")

    switch(msg.tag) {

        case "UserData":
        console.log("UserData")
        if (msg.data != null) {
          sessionStorage.setItem("currentUser", JSON.stringify(msg.data));
        }
        break;

        case "AskToReconnectUser":
        console.log("AskToReconnectUser")
        if (sessionStorage.currentUser != null) {
            app.ports.infoForElm.send({tag: "ReconnectUser", data: JSON.parse(sessionStorage.currentUser)})
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

    }

})




document.addEventListener('selectionchange', () => {
  console.log("Selection: " + document.getSelection());
});