type FromElm = { tag : "alert"; message : string } | { tag : "bugsnag"; message : string; context : string[] } | { tag : "sendPresenceHeartbeat" } | { tag : "scrollIntoView"; options : { behavior? : "smooth" | "auto"; block? : "nearest" | "end" | "center" | "start"; inline? : "nearest" | "end" | "center" | "start" }; id : string }

type Flags = { severity : "info" | "warning" | "error"; first : string; last : string }

export type JsonObject = {[Key in string]?: JsonValue};
export interface JsonArray extends Array<JsonValue> {}
/**
Matches any valid JSON value.
Source: https://github.com/sindresorhus/type-fest/blob/master/source/basic.d.ts
*/
export type JsonValue = string | number | boolean | null | JsonObject | JsonArray;

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
    