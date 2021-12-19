import React from "react";
import { ThemeContext } from "./ThemeProvider";

interface SettingsMenuProps {
  open: boolean;
}

export const SettingsMenu: React.FunctionComponent<SettingsMenuProps> = (
  props
) => {
  const theme = React.useContext(ThemeContext);

  return (
    <div
      style={{
        background: theme.boldBackground,
        color: theme.foreground,
        height: "100vh",
        textAlign: "left",
        padding: "2rem",
        margin: 0,
        boxSizing: "border-box",
        position: "absolute",
        top: 0,
        left: 0,
        transition: "transform 0.3s ease-in-out",
        zIndex: 5,
        transform: props.open
          ? "translateX(0)"
          : "translateX(-100%) translateX(-1em)",
        boxShadow: "rgba(0, 0, 0, 0.5) 0.5em 0 1em",
      }}
    >
      {props.children}
    </div>
  );
};
