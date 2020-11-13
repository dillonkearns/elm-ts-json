import { Elm } from "./src/Main";

const app = Elm.Main.init({
  node: document.querySelector("main"),
  flags: null,
});

app.ports.fromElm.subscribe((fromElm) => {
  console.log({ fromElm });

  switch (fromElm.tag) {
    case "alert":
      alert(fromElm.message);
      break;
    case "sendPresenceHeartbeat":
      console.log("sendPresenceHeartbeat");
      break;
    case "bugsnag":
      console.log("context", fromElm.context);
      console.log("message", fromElm.message);
      break;
  }
});
