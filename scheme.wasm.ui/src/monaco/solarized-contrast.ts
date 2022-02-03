import monaco from "monaco-editor";
import { BaseColors, Theme } from "./theme";

const kSolarizedContrastColors: BaseColors = {
  base03: "#001014", // 'brblack',
  base02: "#031b20", // 'black',
  base01: "#425257", // 'brgreen',
  base00: "#596c73", // 'bryellow',
  base0: "#91a0a1", // 'brblue',
  base1: "#adb8b8", // 'brcyan',
  base2: "#f5f1e6", // 'white'
  base3: "#fefaf1", // 'brwhite'
  yellow: "#b58900", // 'yellow',
  orange: "#cb4b16", // 'brred',
  red: "#dc322f", // 'red',
  magenta: "#d33682", // 'magenta',
  violet: "#6c71c4", // 'brmagenta',
  blue: "#268bd2", // 'blue',
  cyan: "#2aa198", // 'cyan',
  green: "#859900", // 'green',
};

export const kSolarizedContrastDark: Theme = {
  name: "SolarizedContrastDark",
  ...kSolarizedContrastColors,
  background: kSolarizedContrastColors.base02,
  foreground: kSolarizedContrastColors.base2,
  boldBackground: kSolarizedContrastColors.base03,
  boldForeground: kSolarizedContrastColors.base3,
};

export const kSolarizedContrastLight: Theme = {
  name: "SolarizedContrastLight",
  ...kSolarizedContrastColors,
  background: kSolarizedContrastColors.base2,
  foreground: kSolarizedContrastColors.base02,
  boldBackground: kSolarizedContrastColors.base3,
  boldForeground: kSolarizedContrastColors.base03,
};

export function defineThemes() {
  // @ts-ignore
  window.monaco.editor.defineTheme(kSolarizedContrastDark.name, {
    base: "vs-dark",
    inherit: false,
    rules: [
      { token: "keyword", foreground: kSolarizedContrastDark.green },
      { token: "constant", foreground: kSolarizedContrastDark.orange },
      { token: "identifier", foreground: kSolarizedContrastDark.yellow },
      { token: "delimiter.vector", foreground: kSolarizedContrastDark.yellow },
      { token: "string.character", foreground: kSolarizedContrastDark.cyan },
      { token: "string", foreground: kSolarizedContrastDark.cyan },
      { token: "number", foreground: kSolarizedContrastDark.blue },
      { token: "comment", foreground: kSolarizedContrastDark.base1 },
      { token: "operators", foreground: kSolarizedContrastDark.green },
      { token: "delimiter", foreground: kSolarizedContrastDark.base01 },
      { token: "variable", foreground: kSolarizedContrastDark.green },
      { token: "bracket", foreground: kSolarizedContrastDark.base01 },
      {
        token: "",
        foreground: kSolarizedContrastDark.foreground,
        background: kSolarizedContrastDark.background,
      },
    ],
    colors: {
      "editor.foreground": kSolarizedContrastDark.foreground,
      "editor.background": kSolarizedContrastDark.background,
      "editorLineNumber.foreground": kSolarizedContrastColors.base00,
    },
  });
  // @ts-ignore
  window.monaco.editor.defineTheme(kSolarizedContrastLight.name, {
    base: "vs",
    inherit: false,
    rules: [
      { token: "keyword", foreground: kSolarizedContrastLight.green },
      { token: "constant", foreground: kSolarizedContrastLight.orange },
      { token: "identifier", foreground: kSolarizedContrastLight.yellow },
      { token: "delimiter.vector", foreground: kSolarizedContrastLight.yellow },
      { token: "string.character", foreground: kSolarizedContrastLight.cyan },
      { token: "string", foreground: kSolarizedContrastLight.cyan },
      { token: "number", foreground: kSolarizedContrastLight.blue },
      { token: "comment", foreground: kSolarizedContrastLight.base1 },
      { token: "operators", foreground: kSolarizedContrastLight.green },
      { token: "delimiter", foreground: kSolarizedContrastLight.base01 },
      { token: "variable", foreground: kSolarizedContrastLight.green },
      { token: "bracket", foreground: kSolarizedContrastLight.base01 },
      {
        token: "",
        foreground: kSolarizedContrastLight.foreground,
        background: kSolarizedContrastLight.background,
      },
    ],
    colors: {
      "editor.foreground": kSolarizedContrastLight.foreground,
      "editor.background": kSolarizedContrastLight.background,
      "editorLineNumber.foreground": kSolarizedContrastColors.base00,
    },
  });
}
