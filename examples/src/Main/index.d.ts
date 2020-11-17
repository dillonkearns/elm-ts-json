type FromElm = { tag : "alert"; message : string } | { tag : "bugsnag"; context : string[]; message : string } | { tag : "sendPresenceHeartbeat" }

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
    init(options: { node?: HTMLElement | null; flags: null }): ElmApp;
  };
};
export { Elm };
    