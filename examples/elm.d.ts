export type FromElm =
  | { tag: "Alert"; message: string }
  | { tag: "SendPresenceHeartbeat" };

export namespace Elm {
  namespace Main {
    export interface App {
      ports: {
        fromElm: {
          subscribe(callback: (fromElm: FromElm) => void): void;
        };
        toElm: {
          send(data: string): void;
        };
      };
    }
    export function init(options: {
      node?: HTMLElement | null;
      flags: null;
    }): Elm.Main.App;
  }
}
