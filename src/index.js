#!/usr/bin/env node

const { compileToStringSync } = require("node-elm-compiler");
const fs = require("fs");
const moduleName = "Main";

const data = compileToStringSync(["src/CodeGenTarget.elm"], {});

(function () {
  const warnOriginal = console.warn;
  console.warn = function () {};

  eval(data.toString());
  const app = Elm.CodeGenTarget.init();
  app.ports.log.subscribe((typeDefinitions) => {
    fs.writeFileSync(
      `./src/${moduleName}/index.d.ts`,
      declarationFileContent(typeDefinitions)
    );
  });
  console.warn = warnOriginal;
})();

function declarationFileContent({ fromElm, flags }) {
  return `type FromElm = ${fromElm}

type Flags = ${flags}

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
  ${moduleName}: {
    init(options: { node?: HTMLElement | null; flags: Flags }): ElmApp;
  };
};
export { Elm };
    `;
}
