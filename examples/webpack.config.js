const webpack = require("webpack");
const path = require("path");
const MODE =
  process.env.npm_lifecycle_event === "build" ? "production" : "development";

module.exports = function (env) {
  return {
    mode: MODE,
    entry: "./index.ts",
    output: {
      path: path.resolve(__dirname, "dist"),
      filename: "bundle.js",
    },
    plugins:
      MODE === "development"
        ? [
            // Suggested for hot-loading
            // new webpack.NamedModulesPlugin(),
            // Prevents compilation errors causing the hot loader to lose state
            new webpack.NoEmitOnErrorsPlugin(),
          ]
        : [],
    module: {
      rules: [
        {
          test: /\.html$/,
          exclude: /node_modules/,
          loader: "file-loader",
        },
        {
          test: [/\.elm$/],
          exclude: [/elm-stuff/, /node_modules/],
          use: [
            { loader: "elm-hot-webpack-loader" },
            {
              loader: "elm-webpack-loader",
              options: MODE === "production" ? {} : { debug: true },
            },
          ],
        },
        { test: /\.ts$/, loader: "ts-loader" },
      ],
    },
    resolve: {
      extensions: [".js", ".ts", ".elm"],
    },
  };
};
