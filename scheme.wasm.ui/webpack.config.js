const path = require("path");
const { CleanWebpackPlugin: CleanWebpack } = require("clean-webpack-plugin");
const CopyWebpack = require("copy-webpack-plugin");
const HtmlWebpack = require("html-webpack-plugin");

module.exports = (env, argv) => {
  // Selects a different template file for the start page based on whether
  // this is a "production" build or not
  const isProduction = argv.mode === "production";
  const index_template = isProduction
    ? "./index.prod.html"
    : "./index.dev.html";

  return {
    entry: "./src/index.tsx", // The entry point into the bundle
    output: {
      path: path.join(__dirname, "/dist"),
      filename: "[name].[contenthash].js", // The name of the output bundle,
      // this is referenced from the start
      // page.
    },
    devServer: {
      port: 8080,
      host: "0.0.0.0",
      static: {
        publicPath: path.join(__dirname, "/dist"),
      },
    },
    devtool: "source-map",
    resolve: {
      extensions: [".ts", ".tsx", ".js"],
    },

    module: {
      rules: [
        {
          test: /\.ts(x?)$/,
          exclude: /node_modules/,
          use: "ts-loader",
        },
        {
          enforce: "pre",
          test: /\.js$/,
          loader: "source-map-loader",
        },
        {
          test: /\.css$/,
          use: ["style-loader", "css-loader"],
        },
      ],
    },

    externals: {
      react: "React",
      "react-dom": "ReactDOM",
    },

    plugins: [
      new CleanWebpack(),
      new HtmlWebpack({
        title: "scheme.wasm UI", // Title is injected into the page template
        filename: "scheme.wasm.html", // The output name when built
        template: index_template,
      }),
      new CopyWebpack({
        patterns: [
          { from: "../scheme.wasm/dist/scheme.wasm", to: "wasm" },
          { from: "../scheme.wasm/dist/unicode/blocks.json.gz", to: "unicode" },
          { from: "./about.html", to: "static" },
        ],
      }),
    ],
  };
};
