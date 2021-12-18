import React from "react";
import { ThemeContext } from "./ThemeProvider";

interface ToggleSwitchProps {
  on: boolean;
  labelOn?: string;
  labelOff?: string;
  onChange?: (on: boolean) => void;
}

function generateId(): string {
  const array = new Uint8Array(12);
  window.crypto.getRandomValues(array);
  return btoa(
    Array.from(array)
      .map((el) => String.fromCodePoint(el))
      .join("")
  );
}

const kToggleSwitchStyle: React.CSSProperties = {
  position: "relative",
  display: "inline-block",
  verticalAlign: "text-bottom",
  userSelect: "none",
};

const kToggleLabelStyle: React.CSSProperties = {
  display: "block",
  overflow: "hidden",
  cursor: "pointer",
  borderWidth: 1,
  borderStyle: "solid",
  borderRadius: "0.5em",
  width: "3em",
  height: "1em",
  margin: 0,
  transition: "background-color 0.1s 0.1s",
};

const kToggleInnerStyle: React.CSSProperties = {
  display: "block",
  transition: "all 0.3s",
  height: "1em",
  borderRadius: "0.5em",
  width: "1.5em",
  boxSizing: "border-box",
  position: "relative",
};

export const ToggleSwitch: React.FunctionComponent<ToggleSwitchProps> = (
  props
) => {
  const theme = React.useContext(ThemeContext);
  const id = React.useRef(generateId());
  return (
    <div style={kToggleSwitchStyle}>
      <input
        style={{ display: "none" }}
        type="checkbox"
        id={id.current}
        checked={props.on}
        onChange={() => {
          if (props.onChange) {
            props.onChange(!props.on);
          }
        }}
      />
      <label
        htmlFor={id.current}
        style={{
          ...kToggleLabelStyle,
          border: `1px solid  ${theme.background}`,
          backgroundColor: props.on ? theme.blue : theme.foreground,
          borderColor: theme.base00,
        }}
      >
        <span
          style={{
            position: "absolute",
            display: "block",
            left: props.on ? "0" : "1.5em",
            top: 0,
            width: "1.5em",
            textAlign: "center",
            color: theme.background,
          }}
        >
          {props.on ? props.labelOn || "on" : props.labelOff || "off"}
        </span>
        <span
          style={{
            ...kToggleInnerStyle,
            backgroundColor: props.on ? theme.foreground : theme.base00,
            left: props.on ? "1.7em" : "-0.2em",
          }}
        />
      </label>
    </div>
  );
};
