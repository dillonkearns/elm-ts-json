import { Elm } from "./src/Main";

const app = Elm.Main.init({
  node: document.querySelector("main"),
  flags: null,
});

app.ports.fromElm.subscribe((fromElm) => {
  console.log({ fromElm });

  switch (fromElm.tag) {
    case "Alert":
      alert(fromElm.message);
      break;
    case "SendPresenceHeartbeat":
      // ...
      break;
  }
});
