import React from "react";
import { ThemeContext } from "./ThemeProvider";

export const About: React.FunctionComponent = (props) => {
  const theme = React.useContext(ThemeContext);

  return (
    <div
      style={{
        position: "absolute",
        width: "50em",
        height: "80vh",
        top: "10vh",
        left: "calc(50vw - 25em",
        boxSizing: "border-box",
        background: theme.background,
        boxShadow: "rgba(0, 0, 0, 50%) 0em 0.5em 1em",
        display: "flex",
      }}
    >
      <iframe style={{ flex: 1, border: "none" }} src="./static/about.html" />
    </div>
  );
};
