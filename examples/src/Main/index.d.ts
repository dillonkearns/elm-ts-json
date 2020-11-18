type FromElm = { tag : "alert"; message : string } | { tag : "bugsnag"; context : string[]; message : string } | { tag : "sendPresenceHeartbeat" }

type Flags = { severity : "info" | "warning" | "error"; first : string; last : string }

export interface ElmApp {
  ports: {
    fromElm: {
      subscribe(callback: (fromElm: FromElm) => void): void;
    };
    toElm: {
      send(data: string): void;
    };
  };
}

declare const Elm: {
  Main: {
    init(options: { node?: HTMLElement | null; flags: Flags }): ElmApp;
  };
};
export { Elm };
    