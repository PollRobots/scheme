const path = require("path");
const { CleanWebpackPlugin: CleanWebpack } = require("clean-webpack-plugin");
const CopyWebpack = require("copy-webpack-plugin");
const HtmlWebpack = require("html-webpack-plugin");
const TerserWebpack = require("terser-webpack-plugin");
const { SubresourceIntegrityPlugin } = require("webpack-subresource-integrity");
const GenerateFileWebpack = require("generate-file-webpack-plugin");

module.exports = (env, argv) => {
  // Selects a different template file for the start page based on whether
  // this is a "production" build or not
  const isProduction = argv.mode === "production";

  const defaultSources = ["self"];
  const scriptSources = isProduction
    ? [
        "self",
        // error handling inline script in index.html
        "sha256-TO1LBWpNZpKvSV7k/GXrQi3Dr3W6DM4HiyBZ34t9NZY=",
        // monaco loader inline script in index.html
        "sha256-L3Q56wz8wKAd/Sv4L0XQVvnS9NTbuc6bMFdjybWP7W0=",
        // required to allow wasm compilation
        "unsafe-eval",
      ]
    : [
        "self",
        // error handling inline script in index.html
        "sha256-CaJQ9XvDeqT1hFm5G9xK1m9KdzTb15u78m3/yqU3kI4=",
        // monaco loader inline script in index.html
        "sha256-mBDwcg6uDyAcx3h6QPxb9yGjK5ak26IXZ/9Xk/VsJM4=",
        // required to allow wasm compilation
        "unsafe-eval",
      ];
  const fontSources = ["self"];
  const imageSource = ["self"];
  const styleSources = [
    "self",
    // needed because of the way we handle css in general. This still won't allow
    // accessing external urls (i.e. no tracking images in inline styles)
    "unsafe-inline",
  ];
  // fflate useses a worker loaded from a blob, so worker sources needs to
  // include the blob: scheme
  const workerSources = ["self", "blob:"];

  const sourceMap = (sources, type) =>
    [type, ...sources.map((el) => (el.endsWith(":") ? el : `'${el}'`))].join(
      " "
    ) + ";";
  const cspHeader = [
    sourceMap(defaultSources, "default-src"),
    sourceMap(scriptSources, "script-src"),
    sourceMap(fontSources, "font-src"),
    sourceMap(imageSource, "img-src"),
    sourceMap(styleSources, "style-src"),
    sourceMap(workerSources, "worker-src"),
  ].join(" ");
  const cspHeaderFile = `add_header Content-Security-Policy "${cspHeader}" always;`;

  return {
    entry: "./src/index.tsx", // The entry point into the bundle
    output: {
      path: path.join(__dirname, "/dist"),
      filename: "[name].[contenthash].js",
      crossOriginLoading: "anonymous",
    },
    devServer: {
      port: 8080,
      host: "0.0.0.0",
      static: {
        publicPath: path.join(__dirname, "/dist"),
      },
      headers: {
        "Content-Security-Policy": cspHeader,
        "Cross-Origin-Opener-Policy": "same-origin",
        "Cross-Origin-Embedder-Policy": "require-corp",
      },
    },
    devtool: isProduction ? undefined : "source-map",
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
          exclude: /node_modules/,
          loader: "source-map-loader",
        },
        {
          test: /\.css$/,
          exclude: /node-modules/,
          use: ["style-loader", "css-loader"],
        },
        {
          test: /\.ttf$/,
          use: ["file-loader"],
        },
      ],
    },

    externals: {
      "monaco-editor": [],
    },

    optimization: {
      minimize: isProduction,
      minimizer: [new TerserWebpack()],
      realContentHash: true,
      splitChunks: {
        chunks: "all",
      },
    },

    plugins: [
      new CleanWebpack(),
      new SubresourceIntegrityPlugin(),
      new HtmlWebpack({
        filename: "scheme.wasm.html", // The output name when built
        template: "./index.html",
      }),
      new CopyWebpack({
        patterns: [
          { from: "../scheme.wasm/dist/scheme.wasm", to: "wasm" },
          { from: "../scheme.wasm/dist/unicode/blocks.json.gz", to: "unicode" },
          { from: "../scheme.wasm/src/scheme", to: "scheme" },
          { from: "./about.html", to: "static" },
          { from: "./favicon", to: "static" },
          { from: "./fonts", to: "fonts" },
          {
            from: "../node_modules/monaco-editor/min/vs",
            to: "vs",
            globOptions: {
              ignore: ["**/basic-languages/**", "**/language/**"],
            },
          },
          {
            from: "../node_modules/monaco-editor/min-maps",
            to: "min-maps",
          },
        ],
      }),
      GenerateFileWebpack({
        file: "csp.conf",
        content: cspHeaderFile,
      }),
    ],
  };
};
