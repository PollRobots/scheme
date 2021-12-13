import { Monaco } from "@monaco-editor/react";

const kSolarizedColors: Record<string, string> = {
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

export const kThemeName = "r7rsSchemeTheme";

export function defineTheme(monaco: Monaco) {
  monaco.editor.defineTheme(kThemeName, {
    base: "vs-dark",
    inherit: false,
    rules: [
      { token: "keyword", foreground: kSolarizedColors.blue },
      { token: "constant", foreground: kSolarizedColors.orange },
      { token: "identifier", foreground: kSolarizedColors.base2 },
      { token: "delimiter.vector", foreground: kSolarizedColors.yellow },
      { token: "string.character", foreground: kSolarizedColors.violet },
      { token: "string", foreground: kSolarizedColors.orange },
      { token: "number", foreground: kSolarizedColors.orange },
      { token: "comment", foreground: kSolarizedColors.base00 },
      { token: "operators", foreground: kSolarizedColors.cyan },
      { token: "bracket", foreground: kSolarizedColors.base0 },
      { token: "delimiter", foreground: kSolarizedColors.base3 },
      { token: "variable", foreground: kSolarizedColors.green },
      {
        token: "",
        background: kSolarizedColors.base02,
        foreground: kSolarizedColors.base2,
      },
    ],
    colors: {
      "editor.foreground": kSolarizedColors.base2,
      "editor.background": kSolarizedColors.base02,
      "editorLineNumber.foreground": kSolarizedColors.base00,
    },
  });
}
