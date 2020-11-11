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

declare namespace Elm {
  interface Main {
    init(options: { node?: HTMLElement | null; flags: null }): ElmApp;
  }
}

// declare const Elm: {
//   Main: {
//     init(options: { node?: HTMLElement | null; flags: null }): ElmApp;
//   };
// };

export default Elm;
