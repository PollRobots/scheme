import React from "react";
import { Logo } from "../icons/logo";
import { Theme } from "../monaco/theme";
import { FocusContext } from "./FocusContext";
import { RuntimeStatus } from "./RuntimeStatus";
import { ThemeContext } from "./ThemeProvider";
import { ToggleSwitch } from "./ToggleSwitch";

export type ThemeDescriptor =
  | "Dark"
  | "Light"
  | "ContrastDark"
  | "ContrastLight";
export type ExtendedThemeDescriptor = ThemeDescriptor | "Same";

export interface SettingsBase {
  theme: ThemeDescriptor;
  editorTheme: ExtendedThemeDescriptor;
  highlighting: boolean;
  inspector: boolean;
  fontSize: number;
  persist: boolean;
}

interface SettingsProps extends SettingsBase {
  onChange?: (update: SettingsBase) => void;
  onAbout?: () => void;
}

const kSettingsSubHeading: React.CSSProperties = {
  fontWeight: 500,
  fontSize: "1.25em",
  lineHeight: "2em",
  marginTop: "1rem",
};

const kFontSizes: number[] = [
  6, 7, 8, 9, 10, 11, 12, 14, 18, 24, 30, 36, 48, 60, 72, 96,
];

function selectStyle(theme: Theme): React.CSSProperties {
  return {
    fontSize: "inherit",
    fontFamily: "inherit",
    width: "10em",
    height: "1.5em",
    borderRadius: "0.25em",
    appearance: "none",
    padding: "0 0.25em",
    background: theme.background,
    color: theme.foreground,
  };
}

export const Settings: React.FunctionComponent<SettingsProps> = (props) => {
  const theme = React.useContext(ThemeContext);
  const focus = React.useContext(FocusContext);
  return (
    <div style={{ fontSize: `${props.fontSize}pt` }}>
      <Logo style={{ width: "9em", margin: "0 auto", display: "block" }} />
      <div style={kSettingsSubHeading}>Appearance</div>
      <div style={{ lineHeight: "2em" }}>
        REPL Theme:{" "}
        <select
          style={selectStyle(theme)}
          disabled={!focus}
          value={props.theme}
          onChange={(e) => {
            if (props.onChange) {
              props.onChange({
                ...props,
                theme: e.target.value as ThemeDescriptor,
              });
            }
          }}
        >
          <option value="Dark">Dark</option>
          <option value="Light">Light</option>
          <option value="ContrastDark">High Contrast Dark</option>
          <option value="ContrastLight">High Contrast Light</option>
        </select>
      </div>
      <div style={{ lineHeight: "2em" }}>
        Editor Theme:{" "}
        <select
          style={selectStyle(theme)}
          disabled={!focus}
          value={props.editorTheme}
          onChange={(e) => {
            if (props.onChange) {
              props.onChange({
                ...props,
                editorTheme: e.target.value as ExtendedThemeDescriptor,
              });
            }
          }}
        >
          <option value="Same">Same as REPL</option>
          <option value="Dark">Dark</option>
          <option value="Light">Light</option>
          <option value="ContrastDark">High Contrast Dark</option>
          <option value="ContrastLight">High Contrast Light</option>
        </select>
      </div>
      <div style={{ lineHeight: "2em" }}>
        Font Size:{" "}
        <select
          style={selectStyle(theme)}
          disabled={!focus}
          value={props.fontSize}
          onChange={(e) => {
            if (props.onChange) {
              props.onChange({ ...props, fontSize: Number(e.target.value) });
            }
          }}
        >
          {kFontSizes.map((el) => (
            <option value={el} key={el}>
              {el}pt
            </option>
          ))}
        </select>
      </div>
      <div style={{ lineHeight: "2em" }}>
        <span style={{ marginRight: "0.5em" }}>Syntax Highlighting</span>
        <ToggleSwitch
          on={props.highlighting}
          disabled={!focus}
          onChange={(on) => {
            if (props.onChange) {
              props.onChange({ ...props, highlighting: on });
            }
          }}
        />
      </div>
      <div style={kSettingsSubHeading}>Debug</div>
      <div style={{ lineHeight: "2em" }}>
        <span style={{ marginRight: "0.5em" }}>Show inspector</span>
        <ToggleSwitch
          on={props.inspector}
          disabled={!focus}
          onChange={(on) => {
            if (props.onChange) {
              props.onChange({ ...props, inspector: on });
            }
          }}
        />
      </div>
      <div style={{ lineHeight: "2em" }}>
        <span style={{ marginRight: "0.5em" }}>Persist Settings</span>
        <ToggleSwitch
          on={!!props.persist}
          disabled={!focus}
          onChange={(on) => {
            if (props.onChange) {
              props.onChange({ ...props, persist: on });
            }
          }}
        />
      </div>
      <div style={kSettingsSubHeading}>Runtime</div>
      <RuntimeStatus disabled={!focus} />
      <div
        style={{
          ...kSettingsSubHeading,
          cursor: "pointer",
          textDecoration: "underline",
        }}
        onClick={(e) => {
          if (props.onAbout) {
            props.onAbout();
          }
        }}
      >
        About
      </div>
    </div>
  );
};
