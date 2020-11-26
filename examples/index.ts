import { Elm } from "./src/Main";
import Bugsnag, { NotifiableError, Event } from "@bugsnag/js";

const app = Elm.Main.init({
  node: document.querySelector("main"),
  flags: {
    first: "Dillon",
    last: "Kearns",
    severity: "error",
  },
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
      {
        // const error: NotifiableError = { name: "", message: "" };
        // error.setUser("3", "bugs.nag@bugsnag.com", "Bugs Nag");
        // const event: Event = { app: "", device: "" };
        // const event: Event = Event.create();
        // error.setUser("3", "bugs.nag@bugsnag.com", "Bugs Nag");
        // Bugsnag.notify(event);
      }
      console.log("context", fromElm.context);
      console.log("message", fromElm.message);
      break;
    case "scrollIntoView":
      document.getElementById(fromElm.id)?.scrollIntoView(fromElm.options);
      break;
  }
});
