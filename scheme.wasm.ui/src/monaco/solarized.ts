import monaco from "monaco-editor";
import { BaseColors, Theme } from "./theme";

const kSolarizedColors: BaseColors = {
  base03: "#002b36", // 'brblack',
  base02: "#073642", // 'black',
  base01: "#586e75", // 'brgreen',
  base00: "#657b83", // 'bryellow',
  base0: "#839496", // 'brblue',
  base1: "#93a1a1", // 'brcyan',
  base2: "#eee8d5", // 'white'
  base3: "#fdf6e3", // 'brwhite'
  yellow: "#b58900", // 'yellow',
  orange: "#cb4b16", // 'brred',
  red: "#dc322f", // 'red',
  magenta: "#d33682", // 'magenta',
  violet: "#6c71c4", // 'brmagenta',
  blue: "#268bd2", // 'blue',
  cyan: "#2aa198", // 'cyan',
  green: "#859900", // 'green',
};

export const kSolarizedDark: Theme = {
  name: "SolarizedDark",
  ...kSolarizedColors,
  background: kSolarizedColors.base02,
  foreground: kSolarizedColors.base2,
  boldBackground: kSolarizedColors.base03,
  boldForeground: kSolarizedColors.base3,
};

export const kSolarizedLight: Theme = {
  name: "SolarizedLight",
  ...kSolarizedColors,
  background: kSolarizedColors.base2,
  foreground: kSolarizedColors.base02,
  boldBackground: kSolarizedColors.base3,
  boldForeground: kSolarizedColors.base03,
};

export function defineThemes() {
  // @ts-ignore
  window.monaco.editor.defineTheme(kSolarizedDark.name, {
    base: "vs-dark",
    inherit: false,
    rules: [
      { token: "keyword", foreground: kSolarizedDark.green },
      { token: "constant", foreground: kSolarizedDark.orange },
      { token: "identifier", foreground: kSolarizedDark.yellow },
      { token: "delimiter.vector", foreground: kSolarizedDark.yellow },
      { token: "string.character", foreground: kSolarizedDark.cyan },
      { token: "string", foreground: kSolarizedDark.cyan },
      { token: "number", foreground: kSolarizedDark.blue },
      { token: "comment", foreground: kSolarizedDark.base1 },
      { token: "operators", foreground: kSolarizedDark.green },
      { token: "delimiter", foreground: kSolarizedDark.base01 },
      { token: "variable", foreground: kSolarizedDark.green },
      { token: "bracket", foreground: kSolarizedDark.base01 },
      {
        token: "",
        foreground: kSolarizedDark.foreground,
        background: kSolarizedDark.background,
      },
    ],
    colors: {
      "editor.foreground": kSolarizedDark.foreground,
      "editor.background": kSolarizedDark.background,
      "editorLineNumber.foreground": kSolarizedColors.base00,
    },
  });
  // @ts-ignore
  window.monaco.editor.defineTheme(kSolarizedLight.name, {
    base: "vs",
    inherit: false,
    rules: [
      { token: "keyword", foreground: kSolarizedLight.green },
      { token: "constant", foreground: kSolarizedLight.orange },
      { token: "identifier", foreground: kSolarizedLight.yellow },
      { token: "delimiter.vector", foreground: kSolarizedLight.yellow },
      { token: "string.character", foreground: kSolarizedLight.cyan },
      { token: "string", foreground: kSolarizedLight.cyan },
      { token: "number", foreground: kSolarizedLight.blue },
      { token: "comment", foreground: kSolarizedLight.base1 },
      { token: "operators", foreground: kSolarizedLight.green },
      { token: "delimiter", foreground: kSolarizedLight.base01 },
      { token: "variable", foreground: kSolarizedLight.green },
      { token: "bracket", foreground: kSolarizedLight.base01 },
      {
        token: "",
        foreground: kSolarizedLight.foreground,
        background: kSolarizedLight.background,
      },
    ],
    colors: {
      "editor.foreground": kSolarizedLight.foreground,
      "editor.background": kSolarizedLight.background,
      "editorLineNumber.foreground": kSolarizedColors.base00,
    },
  });
}
