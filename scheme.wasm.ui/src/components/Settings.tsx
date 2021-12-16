import React from "react";

interface SettingsBase {
  theme: string;
  editorTheme: string;
}

interface SettingsProps extends SettingsBase {
  onChange?: (update: SettingsBase) => void;
  onAbout?: () => void;
}

const kSettingsSubHeading: React.CSSProperties = {
  fontSize: "1.25em",
  lineHeight: "2em",
};

export const Settings: React.FunctionComponent<SettingsProps> = (props) => {
  return (
    <div>
      <div
        style={{
          fontWeight: "bolder",
          fontSize: "1.5em",
          margin: "1rem 0",
        }}
      >
        Settings
      </div>
      <div style={kSettingsSubHeading}>Theme</div>
      <div style={{ lineHeight: "2em" }}>
        REPL Theme:{" "}
        <select
          style={{
            fontSize: "inherit",
            fontFamily: "inherit",
            width: "10em",
            height: "1.5em",
            borderRadius: "0.25em",
            appearance: "none",
            padding: "0 0.25em",
          }}
          value={props.theme}
          onChange={(e) => {
            if (props.onChange) {
              props.onChange({ ...props, theme: e.target.value });
            }
          }}
        >
          <option value="Dark">Dark</option>
          <option value="Light">Light</option>
        </select>
      </div>
      <div style={{ lineHeight: "2em" }}>
        Editor Theme:{" "}
        <select
          style={{
            fontSize: "inherit",
            fontFamily: "inherit",
            width: "10em",
            height: "1.5em",
            borderRadius: "0.25em",
            appearance: "none",
            padding: "0 0.25em",
          }}
          value={props.editorTheme}
          onChange={(e) => {
            if (props.onChange) {
              props.onChange({ ...props, editorTheme: e.target.value });
            }
          }}
        >
          <option value="Same">Same as REPL</option>
          <option value="Dark">Dark</option>
          <option value="Light">Light</option>
        </select>
      </div>
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
