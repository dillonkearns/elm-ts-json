export type FromElm =
  | { tag: "Alert"; message: string }
  | { tag: "SendPresenceHeartbeat" };

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
