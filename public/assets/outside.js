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


    }

})

