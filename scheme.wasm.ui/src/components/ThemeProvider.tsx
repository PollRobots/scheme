import React from "react";
import { kSolarizedDark } from "../monaco/solarized";
import { Theme } from "../monaco/theme";

const kDefaultTheme: Theme = kSolarizedDark;

type EditorTheme = Theme | false;

export const ThemeContext = React.createContext<Theme>(kDefaultTheme);

export const ThemeProvider = ThemeContext.Provider;

export const EditorThemeContext = React.createContext<EditorTheme>(false);

export const EditorThemeProvider = EditorThemeContext.Provider;
